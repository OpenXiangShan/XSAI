# XSAI Agent Notes

This document is for automated coding agents working in this repo.
Follow it before making changes.

## Ground Rules
- Primary build system: Mill (see `build.sc`), invoked via Makefile helpers.
- Scala formatting: scalafmt configs live in multiple submodules; root uses `.scalafmt.conf`.
- Scala style: `scalastyle-config.xml` and `scalastyle-test-config.xml` define naming, imports, and size limits.

## Build, Lint, Test
All commands below are from repo root unless noted.

### Initialize submodules
```
make init
# or force re-sync submodules
make init-force
```

### Compile / generate Verilog
```
make init                     # fetch required submodules first
make comp                     # mill xiangshan.compile + xiangshan.test.compile
make verilog                  # generate FPGA / synthesis RTL at build/rtl/XSTop.sv
make sim-verilog              # generate simulation RTL (SimTop, with difftest-friendly logic)
```

Common build-time knobs used across `verilog`, `sim-verilog`, and `emu`:
```
CONFIG=MinimalMatrixConfig    # choose XSAI config; default is DefaultMatrixConfig
NUM_CORES=2                   # generate multi-core RTL / simulator
ISSUE=E.b                     # CHI issue selection
JVM_XMX=40G JVM_XSS=256m      # tune Mill / Scala memory usage for large builds
YAML_CONFIG=path/to/file.yaml # override parameters from YAML
```

Useful generation notes from xs-env workflow:
- `make verilog` is for FPGA / synthesis style RTL; it strips simulation-only debug pieces such as DiffTest.
- `make sim-verilog` is the normal frontend for simulation RTL; `make emu`, `make simv`, and `make xsim` depend on it.
- `make verilog NUM_CORES=2` generates dual-core synthesis RTL.
- Long RTL generation jobs are usually run under `tmux` / `screen` on a machine with enough RAM.

### Simulators
```
make emu                      # Verilator-based simulator build (default software simulation flow)
make simv                     # VCS-based build
make xsim                     # GalaxSim / X-EPIC build
make pldm-build               # Palladium build
```

Common `make emu` options worth knowing:
```
make emu CONFIG=MinimalConfig EMU_THREADS=8 -j16
make emu EMU_TRACE=fst EMU_THREADS=8 RELEASE=1 -j32
make emu -j16 EMU_THREADS=8 WITH_CHISELDB=1 EMU_TRACE=fst CONFIG=DefaultMatrixConfig
```

- `CONFIG=...`: choose the hardware config to elaborate.
- `EMU_THREADS=<n>`: enable multi-threaded RTL simulation in Verilator; for XSAI / XiangShan AI, `8` or `16` is the preferred range for `make emu`.
- `EMU_TRACE=fst|FST`: compile waveform support with FST output; smaller than VCD.
- `RELEASE=1`: build a faster, more stripped-down simulator (`--fpga-platform --disable-all --remove-assert --reset-gen` path in `Makefile`).
- `WITH_CHISELDB=1`, `WITH_ROLLINGDB=1`, `WITH_CONSTANTIN=1`: enable extra database / analysis facilities when needed.
- `CONFIG=DefaultMatrixConfig` is a common XSAI matrix-extension configuration for emulation.
- `WITH_DRAMSIM3=1 DRAMSIM3_HOME=...`: switch simulated memory model to DRAMSim3.
- `WITH_RESETGEN=1`, `DISABLE_PERF=1`, `DISABLE_ALWAYSDB=1`: extra debug / performance toggles passed into `SimTop` generation.
- `FIRRTL_COVER=...`: extract FIRRTL coverage when doing coverage-oriented builds.

Unless otherwise specified, Verilator-based `emu` is the default recommendation.

### Run a workload on `emu`
Basic run:
```
./build/emu -i ./ready-to-run/coremark-2-iteration.bin \
  --diff ../NEMU/build/riscv64-nemu-interpreter-so
```

Frequently used runtime options (from `./build/emu --help` / `difftest` args):
```
-i <bin>                     # workload image
--diff <ref.so>              # reference model shared library for DiffTest
--no-diff                    # disable DiffTest
-C, --max-cycles <n>         # stop after at most n cycles
-I, --max-instr <n>          # stop after at most n instructions
-s, --seed <n>               # set random seed
-b, --log-begin <n>          # enable log / wave window start cycle
-e, --log-end <n>            # enable log / wave window end cycle
--dump-wave                  # dump waveform when log window is active
--dump-wave-full             # dump full waveform when log window is active
--wave-path <file>           # write waveform to a specific path
--force-dump-result          # force final perf counter dump
--enable-fork                # enable LightSSS-style fork debugging
--dump-ref-trace             # dump REF trace in the logging window
--dump-commit-trace          # dump commit trace in the logging window
```

Typical debug / waveform commands:
```
./build/emu -i ./ready-to-run/linux.bin --diff ../NEMU/build/riscv64-nemu-interpreter-so

./build/emu -i ./ready-to-run/linux.bin --diff ../NEMU/build/riscv64-nemu-interpreter-so \
  -b 10000 -e 11000 --dump-wave

./build/emu -i ./ready-to-run/linux.bin --no-diff -C 500000 --force-dump-result \
  2>&1 | tee emu.log
```

Waveform notes:
- Wave dumping at runtime only works if waveform support was compiled in via `EMU_TRACE=...` during `make emu`.
- `-b` / `-e` default to `0`; a real wave window needs `-e > -b`.
- Default wave files are written under `build/`; `--wave-path` overrides that.

### Lint / format
```
make check-format             # scalafmt check via mill xiangshan.checkFormat
make reformat                 # scalafmt rewrite via mill xiangshan.reformat
```
Submodules also have check/format targets (for example `utility/Makefile` and `coupledL2/Makefile`).

## Code Style
These rules combine repository conventions and scalastyle/scalafmt settings.

### Formatting
- Use scalafmt with max line length 120 (see `.scalafmt.conf`).
- Root config uses `preset = defaultWithAlign`, `rewrite.rules = [RedundantBraces, RedundantParens, SortModifiers, Imports]`.
- Trailing commas are disabled in root config.
- Keep existing local `.scalafmt.conf` in submodules; do not mix versions across subprojects.

### Imports
- Avoid block imports (`import pkg.{a, b}`) per scalastyle.
- Avoid wildcard imports (`import pkg._`) except `chisel3._` and `chisel3.util._`.
- Imports are sorted using the scalastyle order (`rewrite.imports.sort = scalastyle`).

### Naming
- Classes/objects: UpperCamelCase.
- Methods: lowerCamelCase or UpperCamelCase (methods can act as constants).
- Variables/fields: lowerCamelCase; constants in objects: UpperCamelCase.
- Allowed field prefixes for pipeline signals: `sx_`, `perf_`, `debug_`.
- Package names: all lower-case.

### Types and signatures
- Public methods must have explicit type annotations (scalastyle rule).
- Prefer type aliases already used in the code (see `src/main/scala/xiangshan/Parameters.scala`).
- Keep implicit parameters explicit in constructors where used, e.g. `(implicit val p: Parameters)`.

### Error handling and assertions
- Use `require(...)` for configuration invariants (common in core modules).
- Prefer `Option.when(...)` for optional IO/fields instead of nulls.
- Do not swallow errors; surface with `require` or propagate via module IOs.

### Chisel/RTL patterns
- Use `dontTouch` only when necessary (e.g. keeping debug/flush signals).
- Prefer `Decoupled`/`Valid` interfaces for handshaked signals; keep naming consistent.
- Keep IO bundles in a single `IO(new Bundle { ... })` block for modules.

### File hygiene
- License headers are expected (see `scalastyle-config.xml`); keep the existing XiangShan header style in Scala sources.
- Avoid tabs, trailing whitespace, and missing newline at EOF.
- Avoid TODO/FIXME comments unless tracked and short-lived.
- Avoid line-ending semicolons (`;`).

## Useful entry points
- Core top: `src/main/scala/xiangshan/XSCore.scala`
- Global parameters: `src/main/scala/xiangshan/Parameters.scala`
- L2 cache (coupledL2): `coupledL2/src/main/scala/coupledL2/CoupledL2.scala`

## Commit message format
- Follow the existing Conventional Commits-style summary used in this repo: `type(scope): subject`
- `type` should be lowercase (common in history: `fix`, `feat`, `refactor`, `ci`, `timing`, `submodule`).
- `scope` is optional but recommended for non-trivial changes (examples seen: `matrix`, `backend`, `rob`, `cache`).
- Keep the subject imperative, concise, and without a trailing period; prefer <= 72 characters when possible.
- The subject line is required; an optional multi-line body is allowed and recommended for non-obvious changes.

Examples:
- ```
  fix(matrix): Reset value for xmcsr

  - Root cause: xmcsr was left X after reset, causing nondeterminism in difftest
  - Fix: initialize xmcsr to 0 and gate reset under AME when disabled
  - Test: run `make emu CONFIG=DefaultMatrixConfig` with coremark and difftest enabled
  ```
- ```
  feat(difftest): Support refill check for XSAI

  - Update global mem at matrix store.
  - Enable Refill Check.
  ```
- `submodules: Bump HBL2 and difftest`

## Notes for agents
- This repo is large and uses submodules. Run `make init` before building.
- Prefer Makefile entry points (they wrap mill with args/config).
- Only build full XiangShan RTL / simulators when necessary. A full `make verilog`, `make sim-verilog`, or `make emu` often takes tens of minutes to more than an hour and can consume substantial CPU and memory; prefer targeted checks, code inspection, or narrower tests first when they are sufficient.
- For XSAI / XiangShan AI, prefer `EMU_THREADS=8` or `EMU_THREADS=16` when building `make emu`, unless a task specifically needs a different setting.
- Do not change build tools or formatting configs unless the task requires it.
