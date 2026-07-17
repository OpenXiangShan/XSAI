# XSAI Agent Notes

This document is for automated coding agents working in this repo.
Follow it before making changes.

## Ground Rules
- Primary build system: Mill (see `build.sc`), invoked via Makefile helpers.
- Scala formatting: scalafmt configs live in multiple submodules; root uses `.scalafmt.conf`.
- Scala style: `scalastyle-config.xml` and `scalastyle-test-config.xml` define naming, imports, and size limits.

## Build, Lint, Test
All commands below are from repo root unless noted.

Initialize submodules before the first build:

```bash
make init
# Use make init-force to re-sync them when necessary.
```

### Compile and generate RTL

```bash
make comp          # compile main and test Scala sources
make verilog       # FPGA/synthesis RTL at build/rtl/XSTop.sv
make sim-verilog   # simulation RTL with Difftest support
```

- Common overrides are `CONFIG`, `NUM_CORES`, `ISSUE`, and `YAML_CONFIG`; the default hardware config is `DefaultMatrixConfig`.
- `make verilog` strips simulation-only logic; `make emu`, `make simv`, and `make xsim` use `sim-verilog`.
- Treat generated RTL, sources, Difftest profiles, and Verilator outputs as configuration-specific. After changing hardware or simulation knobs, confirm the relevant artifacts were regenerated rather than trusting timestamps.

### Simulators

```bash
make emu          # Verilator; default software simulation flow
make simv         # VCS
make xsim         # GalaxSim / X-EPIC
make pldm-build   # Palladium
```

Typical matrix-emulator build:

```bash
make emu -j16 EMU_THREADS=8 WITH_CHISELDB=1 EMU_TRACE=fst CONFIG=DefaultMatrixConfig
```

- Prefer `EMU_THREADS=8` or `16`. Compile FST support with `EMU_TRACE=fst`; enable ChiselDB or rolling/constantin facilities only when needed.
- `RELEASE=1` produces a faster, stripped-down simulator. Use `WITH_DRAMSIM3=1 DRAMSIM3_HOME=...` only when DRAMSim3 is required.
- Use the CPU MMA reference by default; enable CUDA with `SIM_ARGS="--difftest-config C"` only when explicitly requested, since transfer, launch, and synchronization overhead usually eliminates non-FPGA `emu` speedups.
- CUDA runs need host GPU access; if a sandbox reports no device, retry with elevated or unsandboxed access before diagnosing the backend.
- Full RTL and simulator builds are expensive; prefer targeted checks or incremental builds when sufficient.

### Run a workload on `emu`

```bash
./build/emu -i ./ready-to-run/coremark-2-iteration.bin \
  --diff ../NEMU/build/riscv64-nemu-interpreter-so
```

- Use `./build/emu --help` for the complete option list. Key controls are `-i`, `--diff`/`--no-diff`, `-C`/`-I`, and `-s`.
- Wave dumping requires `EMU_TRACE` at build time and a non-empty `-b`/`-e` runtime window. Files default to `build/`; override with `--wave-path`.

```bash
./build/emu -i ./ready-to-run/linux.bin --diff ../NEMU/build/riscv64-nemu-interpreter-so \
  -b 10000 -e 11000 --dump-wave
```

### Lint / format

```bash
make check-format
make reformat
```

Use each submodule's local formatting target and configuration.

## Code Style

- Follow the nearest `.scalafmt.conf` and scalastyle configuration; do not mix formatter versions across subprojects.
- Keep public method types explicit and reuse existing aliases and implicit-parameter patterns.
- Use `require` for configuration invariants and `Option.when` for optional hardware rather than nulls.
- Prefer `Decoupled`/`Valid` handshakes, keep IO bundles together, and use `dontTouch` only when necessary.
- Preserve license headers and existing local style; avoid tabs, trailing whitespace, untracked TODOs, and missing final newlines.

## Useful entry points
- Core top: `src/main/scala/xiangshan/XSCore.scala`
- Global parameters: `src/main/scala/xiangshan/Parameters.scala`
- L2 cache (coupledL2): `coupledL2/src/main/scala/coupledL2/CoupledL2.scala`

## Commit message format

- Follow the existing Conventional Commits-style summary used in this repo: `type(scope): subject`
- Use lowercase types such as `fix`, `feat`, `refactor`, `ci`, or `submodule`; add a meaningful scope for non-trivial changes.
- Keep the imperative subject at 72 characters or fewer with no trailing period. For non-obvious changes, explain the root cause, fix, and validation in the body.

Example: `fix(matrix): initialize xmcsr on reset`

## Notes for agents

- Do not change build tools or formatting configs unless the task requires it.
