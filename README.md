# XSAI

XSAI (XiangShan · XiangLuFeng) is an open-source RISC-V processor project for general-purpose computing and AI inference.

XSAI is developed by extending XiangShan (XS): **XSAI = XS + AI**. It is based on the XiangShan Kunminghu V2 microarchitecture and adds configurable AI matrix and memory capabilities while retaining the general-purpose processor programming model.

Chinese README is [readme.zh-cn.md](readme.zh-cn.md).

## Documentation

- [XSAI design documentation (Chinese)](https://docs.xiangshan.cc/projects/design/zh-cn/kunminghu-v2r2a/)

## Follow us

Wechat/微信：香山开源处理器

<div align=left><img width="340" height="117" src="images/wechat.png"/></div>

Zhihu/知乎：[香山开源处理器](https://www.zhihu.com/people/openxiangshan)

Weibo/微博：[香山开源处理器](https://weibo.com/u/7706264932)

You can contact us through [our mailing list](mailto:xiangshan-all@ict.ac.cn). All mails from this list will be archived [here](https://www.mail-archive.com/xiangshan-all@ict.ac.cn/).

## Architecture

### Kunminghu V2 control core and AME extensions

XSAI extends the XiangShan Kunminghu V2 control core with AME matrix instruction decoding, configuration, control, synchronization, and matrix memory support.

### AI Matrix Unit (AMU)

The [CUTE](https://github.com/OpenXiangShan/CUTE) submodule implements the AI Matrix Unit (AMU). AmuCtrl connects it to the control core through `mload`, `mma`, `mstore`, and `mrelease`.

### High-Bandwidth L2 Cache (HBL2)

The [XSAICache](https://github.com/OpenXiangShan/XSAICache) submodule implements the XSAI cache subsystem. HBL2 is the high-bandwidth L2 architecture used by matrix workloads.

### Overall architecture

![XSAI overall architecture](images/xsai-architecture.svg)

[SVG](images/xsai-architecture.svg) · [draw.io source](images/xsai-architecture.drawio)

### XSAI extensions

The comparison diagram shows components retained from Kunminghu V2 at 50% opacity to highlight XSAI additions and modifications.

![Architecture comparison between XSAI and Kunminghu V2](images/xsai-architecture-comparison.svg)

[SVG](images/xsai-architecture-comparison.svg) · [draw.io source](images/xsai-architecture-comparison.drawio)

The diagrams are from the [XSAI design documentation](https://docs.xiangshan.cc/projects/design/zh-cn/kunminghu-v2r2a/overview/) by the XiangShan Team · Beijing Institute of Open Source Chip, licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Programming model and software ecosystem

XSAI keeps a CPU programming model and combines general-purpose, vector, and matrix computation in one RISC-V system. This supports AI inference together with ordinary software execution.

## Repository overview

The key directories and repositories are:

```text
.
|-- src                 # XSAI top-level integration and XiangShan core changes
|-- CUTE                # AMU matrix unit (submodule)
|-- XSAICache           # XSAI cache subsystem (submodule)
|-- difftest            # differential co-simulation infrastructure (submodule)
|-- ready-to-run        # reference simulation images and binaries (submodule)
`-- scripts             # build, simulation, and analysis scripts
```

Related repositories include [xsai-env](https://github.com/OurCompArchGroup/xsai-env) for build and evaluation environments and [NEMU](https://github.com/OpenXiangShan/NEMU) for the reference model and debugging support.

## IDE support

Generate project files for BSP or IntelliJ IDEA with:

```bash
make bsp
make idea
```

## Generate Verilog

Run `make verilog` to generate SystemVerilog RTL in `build/rtl/`, including `build/rtl/XSTop.sv`. See the `Makefile` for configuration options.

## Run programs by simulation

### Prepare the environment

- Set `NEMU_HOME` to the absolute path of the [NEMU project](https://github.com/OpenXiangShan/NEMU).
- Set `NOOP_HOME` to the absolute path of this XSAI repository.
- Set `AM_HOME` to the absolute path of the [nexus-am project](https://github.com/OpenXiangShan/nexus-am) when running AM workloads.
- Install [Mill](https://mill-build.org/mill/cli/installation-ide.html#_bootstrap_scripts).
- Clone XSAI and run `make init` to initialize its submodules.

### Run with the simulator

Install [Verilator](https://verilator.org/guide/latest/) and run:

```bash
make emu
./build/emu --help
```

For example:

```bash
make emu CONFIG=MinimalConfig EMU_THREADS=2 -j10
./build/emu -b 0 -e 0 -i ./ready-to-run/coremark-2-iteration.bin --diff ./ready-to-run/riscv64-nemu-interpreter-so
```

### Run with xspdb

The repository also provides `pdb` and `pdb-run` targets based on picker:

```bash
make pdb
make pdb-run
```

Install [picker](https://github.com/XS-MLVP/picker) and the Python dependencies listed in `scripts/xspdb/requirements.txt` before using these targets.

## Acknowledgement

XSAI is developed by the XiangShan team. It extends the XiangShan Kunminghu V2 control core with the CUTE matrix unit and the XSAICache subsystem.

## License

Copyright © 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences.

Copyright © 2021-2025 Beijing Institute of Open Source Chip

Copyright © 2020-2022 by Peng Cheng Laboratory.

XSAI is licensed under the [Mulan PSL v2](LICENSE).
