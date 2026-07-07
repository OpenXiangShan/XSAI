#!/usr/bin/env python3

import argparse
import sqlite3 as sql
import subprocess
import tempfile


STAGES = {
    "AtFetch": "f",
    "AtDecode": "d",
    "AtRename": "r",
    "AtDispQue": "D",
    "AtIssueQue": "i",
    "AtIssueArb": "a",
    "AtIssueReadReg": "g",
    "AtFU": "e",
    "AtBypassVal": "b",
    "AtWriteVal": "w",
    "AtCommit": "c",
}

LATENCIES = (
    ("issue-to-readreg", "AtIssueQue", "AtIssueReadReg"),
    ("readreg-to-fu", "AtIssueReadReg", "AtFU"),
    ("issue-to-writeback", "AtIssueQue", "AtWriteVal"),
    ("issue-to-commit", "AtIssueQue", "AtCommit"),
)


class DasmQuery:
    def __init__(self, cmd="riscv64-linux-gnu-objdump", enable_cache=True):
        self.cmd = cmd
        self.cache = {} if enable_cache else None
        if cmd.endswith("objdump"):
            self.mode = "objdump"
            self.spike = None
        elif cmd == "spike-dasm":
            self.mode = "spike-dasm"
            self.spike = subprocess.Popen(
                [self.cmd],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
        else:
            raise ValueError(f"unsupported disassembler: {cmd}")

    def close(self):
        if self.spike is not None:
            self.spike.terminate()
            self.spike.wait()
            self.spike = None

    def dasm(self, instr):
        if self.cache is not None and instr in self.cache:
            return self.cache[instr]
        if self.mode == "spike-dasm":
            self.spike.stdin.write(f"DASM(0x{instr:x})\n")
            self.spike.stdin.flush()
            res = self.spike.stdout.readline().strip()
        else:
            with tempfile.NamedTemporaryFile() as f:
                f.write(instr.to_bytes(4, byteorder="little"))
                f.flush()
                proc = subprocess.run(
                    [self.cmd, "-b", "binary", "-m", "riscv:rv64", "-M,max", "-D", f.name],
                    capture_output=True,
                    text=True,
                    check=False,
                )
                res = ""
                for line in proc.stdout.splitlines():
                    if line.startswith("   0:\t"):
                        res = "\t".join(line.split("\t")[2:])
                        break
        if self.cache is not None:
            self.cache[instr] = res
        return res


def parse_int(value):
    return int(value, 0)


def unsigned64(value):
    return value + (1 << 64) if value < 0 else value


def non_stage():
    return "."


def stage(name):
    return STAGES[name]


def record_header(records, dasm):
    instr = records.get("DisAsm", "")
    if dasm is not None and isinstance(instr, int):
        instr = f"{instr:08x} {dasm.dasm(instr)}"
    pc = records.get("PC", "")
    sn = records.get("SN", "")
    uop_idx = records.get("UopIdx", "")
    return f"sn={sn} uop={uop_idx} pc={pc}: {instr}"


def dump_visual(pos, records, args, dasm):
    empty_line = list(f"[{non_stage() * args.cycle_per_line}]")
    line_buffer = empty_line.copy()
    line = ""
    last_key = None
    last_line = None

    def fill_line_pos(offset, char):
        line_buffer[1 + offset] = char

    def commit_last_line(offset=None):
        nonlocal line, line_buffer
        if offset is None or (
            last_line is not None
            and offset // args.cycle_per_line != last_line
            and line_buffer != empty_line
        ):
            prefix = f"{last_line * args.cycle_per_line:07d}:" if args.cycle else ""
            line += prefix + "".join(line_buffer) + "\n"
            line_buffer = empty_line.copy()

    for key, value in sorted(pos.items(), key=lambda item: item[1]):
        if value == 0:
            continue
        if last_key is not None:
            if args.singleline:
                for i in range(pos[last_key], value):
                    fill_line_pos(i % args.cycle_per_line, stage(last_key))
            else:
                for i in range(pos[last_key], value):
                    commit_last_line(i)
                    fill_line_pos(i % args.cycle_per_line, stage(last_key))
                    last_line = i // args.cycle_per_line
        last_key = key

    if last_key is None:
        print(record_header(records, dasm))
        return
    if not args.singleline and last_line is not None:
        commit_last_line(pos[last_key])
    else:
        last_line = pos[last_key] // args.cycle_per_line
    fill_line_pos(pos[last_key] % args.cycle_per_line, stage(last_key))
    commit_last_line()
    line = line.strip()
    print(f"{line} {record_header(records, dasm)}")


def dump_txt(pos, records, args, dasm):
    for key in STAGES:
        if key in pos:
            print(f"{stage(key)}{pos[key]}", end=" ")
    print(record_header(records, dasm))


def dump_latencies(pos):
    parts = []
    for name, start, end in LATENCIES:
        if pos.get(start, 0) != 0 and pos.get(end, 0) != 0:
            parts.append(f"{name}={pos[end] - pos[start]}")
    if parts:
        print("  " + " ".join(parts))


def build_query(args):
    where = []
    params = []
    if not args.spec or args.committed:
        where.append("AtCommit != 0")
    if args.pc is not None:
        where.append("PC = ?")
        params.append(args.pc)
    if args.sn is not None:
        where.append("SN = ?")
        params.append(args.sn)
    if args.uop_idx is not None:
        where.append("UopIdx = ?")
        params.append(args.uop_idx)
    clause = " WHERE " + " AND ".join(where) if where else ""
    return f"SELECT * FROM LifeTimeCommitTrace{clause} ORDER BY SN, UopIdx, ID", params


def select_index(rows, col_names, index):
    if index is None:
        return rows
    sn_idx = col_names.index("SN")
    selected = None
    seen = []
    for row in rows:
        sn = row[sn_idx]
        if sn not in seen:
            seen.append(sn)
        if len(seen) == index:
            selected = sn
            break
    if selected is None:
        return []
    return [row for row in rows if row[sn_idx] == selected]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("sqldb")
    parser.add_argument("-v", "--visual", action="store_true", default=False)
    parser.add_argument("-z", "--zoom", action="store", type=float, default=1)
    parser.add_argument("-p", "--period", action="store", type=int, default=1)
    parser.add_argument(
        "-d",
        "--dasm",
        action="store",
        help='disassembler command, "spike-dasm" or an objdump path',
        default=None,
    )
    parser.add_argument("-s", "--spec", action="store_true", default=False, help="show speculative execution")
    parser.add_argument("-l", "--singleline", action="store_true", default=False, help="single line per instruction")
    parser.add_argument("-c", "--cycle", action="store_true", default=False, help="show cycle number in visual mode")
    parser.add_argument("--pc", type=parse_int, help="filter by PC, for example 0x80000e48")
    parser.add_argument("--sn", type=parse_int, help="filter by sequence number")
    parser.add_argument("--uop-idx", type=parse_int, help="filter by uop index")
    parser.add_argument("--committed", action="store_true", default=False, help="show committed records only")
    parser.add_argument("--index", type=int, help="select the Nth dynamic instruction after filters, 1-based")
    parser.add_argument("--limit", type=int, help="limit number of output rows")
    parser.add_argument("--latency", action="store_true", default=False, help="print common per-uop latency metrics")
    args = parser.parse_args()
    args.cycle_per_line = int(100 * args.zoom)

    dasm = DasmQuery(args.dasm) if args.dasm is not None else None
    try:
        with sql.connect(args.sqldb) as con:
            cur = con.cursor()
            query, params = build_query(args)
            cur.execute(query, params)
            col_names = [col[0] for col in cur.description]
            rows = select_index(cur.fetchall(), col_names, args.index)
            if args.limit is not None:
                rows = rows[: args.limit]

            dump = dump_visual if args.visual else dump_txt
            for row in rows:
                pos = {}
                records = {}
                for name, val in zip(col_names, row):
                    if name == "ID":
                        continue
                    if name in STAGES:
                        pos[name] = val // args.period
                    elif name == "PC":
                        records[name] = f"{unsigned64(val):016x}"
                    else:
                        records[name] = val
                dump(pos, records, args, dasm)
                if args.latency:
                    dump_latencies(pos)
    finally:
        if dasm is not None:
            dasm.close()


if __name__ == "__main__":
    main()
