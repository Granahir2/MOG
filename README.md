# MOG main repository
This repository is here as the main source of documentation and 'getting-started' place for understanding the MOG architecture, including documentation of the architecture, of the different hardwares available, of an assembler of the MOG assembly instruction set, and an emulator of different hardwares.

## MOG Instruction set
The MOG instruction set is an 8 bit targeted, RISC instruction set with fixed 32 bit instructions. While it may not enable some uncommon operations (most notably modifying/reading the stack pointer directly), it is designed to work on really different processor components, which makes it easy to switch a part of the processor by another with the same interface, which may enable some clock rate increase without having to re-wire things. More information will soon be available on the *instruction set* sub-directory
