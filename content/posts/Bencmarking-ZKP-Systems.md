---
title: "On Benchmarking Zero Knowledge Proof Systems"
date: 2022-11-30
draft: false
---

In the Zero Knowledge sphere, there exists a plethora of Zero Knowledge Systems and languages. From zk-SNARK languages like [Zokrates](https://zokrates.github.io/), [Leo](https://leo-lang.org/), and [Vamp-IR](https://github.com/anoma/vamp-ir); to STARK machines like [Triton VM](https://triton-vm.org/), [Miden VM](https://polygon.technology/solutions/polygon-miden/), and [RISC Zero](https://www.risczero.com/). It is difficult in the current landscape to make an informed decision about which system fits their use case best. [This benchmarking endeavor](https://github.com/anoma/zkp-compiler-shootout) seeks to alleviate these issues by readily making performance numbers between the various systems be known and provide an easy way to compare the same program across languages and systems.

Currently, the amount of backends and programs that are compared is limited. However, community contributions should amplify the value provided by the benchmarking project.

In particular, the benchmarking results and programs will be most useful to five kinds of individuals:

1. Users who are building an application with a Zero Knowledge System.
2. Compiler writers who wish to target a Zero Knowledge System.
3. Users who have a choice of the Zero Knowledge System in their application.
4. Zero Knowledge System implementers who wish to know optimization points.
5. Performance-oriented users who wish to see optimal code in their desired system.

Both Individuals `4.` and `5.` can use these results in a straightforward manner, seeing what in their programs and compilers to improve and as a comparison point if any results seem suspect.

Individuals `1.` through `3.` all have the system as a free variable and can wisely pick the language/system that is fit for their problem domain. It is important to note that the benchmarking results to these individuals is not the full story in making decisions, it is important to take the benchmark results as a data point in a more full analysis:

- What is my speed versus expressivity trade-off?
- What kind of tooling does my application need? Do I have the time to develop said tooling?
- How much storage space per proof generated am I willing to spend?
- Do I want to work within a particular proving system?

These are all equally important questions, and no singular system wins in every single category. For example, `RISC zero` may be slower than another STARK system like `Miden`, however, it offers your users the ability to use a language they are more familiar with (`Rust`, `C`, etc.) with little effort, while `Miden` has the advantage of being faster but requires writing tooling or using a language your users are unlikely to be familiar with.

<!-- comment: Less is more. It should be fine to end this here !-->

With these caveats made apparent, we can now discuss the data collection methodology.

## Methodology

Most Zero Knowledge Systems are implemented in Rust. This gives the benchmarking initiative an easy through line to implement a general framework.

In particular, we define the `ZeroKnowledge` trait within rust to act as our API across any Zero Knowledge System. Future work will be had to incorporate systems/languages which do not expose a Rust API into the benchmark.

```rust
pub trait ZeroKnowledge {
    type C;
    type R;
    fn name(&self) -> String;
    fn compile(&self) -> Self::C;
    fn prove(&self, setup: &Self::C) -> Self::R;
    fn verify(&self, receipt: Self::R, program: &Self::C) -> ();
    fn prove_and_verify(&self) -> () {
        let circuit = self.compile();
        let receipt = self.prove(&circuit);
        self.verify(receipt, &circuit);
    }
}
```
The`compile`, `prove`, and `verify` functions are responsible for compiling the circuit, running the prover and the verifier respectively, with `prove_and_verify` running all of the functions. The `name` function is there to name the circuit for data reporting reasons.

The `prove` and `verify` functions, in particular, are where most of the benchmarking interests lay. Since every Zero Knowledge Proof System offers some kind of prover and verifier, this interface is easy to implement for any such system.

For the benchmarking aspect in `Rust`, we have decided to go with the [criterion micro benchmakrer](https://docs.rs/criterion/latest/criterion/). We use criterion to aggregate the average times of each interface functions (`compile`, `prove`, `verify`, and `prove_and_verify`). For the sake of speed, we only collect twenty samples each with the default benchmarking settings.

### On Different Zero Knowledge Systems

As alluded to earlier, in analyzing the data, it is important to understand the capability of the machinery of each system. For example, we can break down the base machinery into a few broad categories:

1. zk-SNARKs
2. zk-STARKs
3. Recursive zk-SNARKs

Even within these categories, great differences can be seen. For example within zk-SNARKs, there is a divide between the R1CS style of systems and more Plonkish style of systems. Typically Plonk style circuits are universal (due to the polynomials making up the circuit being of a fixed degree) while many R1CS style circuits are not universal (and thus require a setup for every new circuit).

With that said, this categorization is useful in the rough properties we can surmise. A good comparison table can be found [on the awesome zero knowledge proof repository](https://github.com/matter-labs/awesome-zero-knowledge-proofs#comparison-of-the-most-popular-zkp-systems). Recursive `ZK-SNARKS` are not included in the table; for analysis purposes, one can lump them in with `ZK-SNARKs`. However, where the difference comes in is in the expressivity of each of the systems:

1. zk-SNARKs: Limited Expressivity Power
2. zk-STARKs: Turing Complete Systems
3. Recursive zk-SNARKs: Turing Complete Systems

For non recursive `ZK-SNARKS` doing something basic like

```sml
if (2 = 3)
then 3 + 5
else 5 + 8
```

requires executing both the then (`3 + 5`), and the else (`5 + 8`). This is in contrast to both `ZK-STARKS` and recursive `ZK-SNAKRS` which allow execution of only one branch [^1]. Due to the difference in expressivity, this is why projects like:

- [Lurk (A recursive zk-SNARK)](https://github.com/lurk-lang)
- [RISC Zero (A zk-STARK)](https://www.risc0.com/)
- [Miden (A zk-STARK)](https://polygon.technology/solutions/polygon-miden/)
- [Triten (A zk-STARK)](https://triton-vm.org/)

Can all be turing complete.

Another point to consider when comparing data, is that many of these projects are on different levels of abstraction. For example, we may have benchmark results of `RISC0` code and `Miden` code. The `RISC0` code is generated by Rust, that is to say we have taken a Von Neuman programming language compiled down to the `RISCV` instruction set, which is then converted into an underlying `ZK-STARK` circuit. While the `Miden` code may have been written directly in `Miden`, meaning that the code just has to be translated from `Miden`'s stack representation to the underlying `ZK-STARK` representation. Thus, the `Miden` code used for benchmarking will be closer to the underlying `ZK-STARK` representation and will likely have better performance characteristics.

Further, machines like `Miden` and `Triton` have static [call graphs](https://en.wikipedia.org/wiki/Call_graph) ([MAST for Miden](https://hackmd.io/yr-ieh7SSKOzWw7Kdo9gnA), [Triton](https://github.com/anoma/zkp-compiler-shootout/issues/16#issuecomment-1275861505)), which means that all the jumps/branches on the machines must be known statically. This results in machines which have various limitations when building on top of them. Due to the limitations and also trying to maintain multiple non-trivial programs, we have also provided minimal languages on top of the assembly languages. Compiler writers, audience `2.`, should be interested in these to see what kinds of langauges are easy to build on these machines.

- [Miden](https://github.com/anoma/zkp-compiler-shootout/tree/main/miden-assembler).
   + See [`src/programs.lisp`](https://github.com/anoma/zkp-compiler-shootout/blob/main/miden-assembler/src/programs.lisp) to see the program generators
   + Further also see [Scribe which should allow Solidity to compile down to Miden via transforming its IR language yule](https://github.com/ControlCplusControlV/Scribe)
- [Triton](https://github.com/anoma/zkp-compiler-shootout/tree/main/triton-assembler)
   + See [`src/programs.lisp`](https://github.com/anoma/zkp-compiler-shootout/blob/main/triton-assembler/src/programs.lisp) to see some programs
   + Note that the Triton code generator is still quite imature, thus for audience `2.` it may be good to look at issues [16](https://github.com/anoma/zkp-compiler-shootout/issues/16) and [17](https://github.com/anoma/zkp-compiler-shootout/issues/17) instead.
   + Since Triton is easily recursive, one should be able to mimic higher level languages easier.

### The Current Data

The data currently collected is in the preliminary stage, so the following caveats should be had when looking at the data:

1. An effort has not *currently* been made to measure startup time.
2. Not all examples are representative of real-world use cases.
    - Many are designed around showing certain behaviors and edge cases of particular systems.
3. Not all programs may be written in the most optimal way.

The data can be seen in an HTML rendered format: [here](https://anoma.github.io/zkp-compiler-shootout/).

All results are taken from a `AMD Ryzen 7 5700X 8-Core @ 16x 3.4GHz` CPU.


| Sudoku       | `Miden`                    | `Plonk: 3 by 3`                   | `Risc`                            | `Halo: 3 by 3`                    |
|------------ |-------------------------- |--------------------------------- |--------------------------------- |--------------------------------- |
| All Combined | `321.75 ms` (✅ **1.00x**) | `128.18 ms` (🚀 **2.51x faster**) | `1.39 s` (❌ *4.32x slower*)      | `353.85 ms` (✅ **1.10x slower**) |
| Compile      | `2.92 ms` (✅ **1.00x**)   | `60.78 ms` (❌ *20.84x slower*)   | `726.71 us` (🚀 **4.01x faster**) | `271.36 ms` (❌ *93.06x slower*)  |
| Prove        | `313.32 ms` (✅ **1.00x**) | `64.96 ms` (🚀 **4.82x faster**)  | `1.37 s` (❌ *4.36x slower*)      | `84.19 ms` (🚀 **3.72x faster**)  |
| Verify       | `1.94 ms` (✅ **1.00x**)   | `4.89 ms` (❌ *2.52x slower*)     | `1.78 ms` (✅ **1.09x faster**)   | `3.21 ms` (❌ *1.65x slower*)     |



| Fibonacci    | `Miden: iter-93`           | `Miden: fixed-92`                 | `Miden: fixed-50`                 | `Risc0: iter-93`                  | `Risc0: iter-50`                  | `Risc0: fixed-50`                 | `Risc0: fixed-92`                 |
|:------------ |:-------------------------- |:--------------------------------- |:--------------------------------- |:--------------------------------- |:--------------------------------- |:--------------------------------- |:--------------------------------- |
| All Combined | `317.49 ms` (✅ **1.00x**) | `151.65 ms` (🚀 **2.09x faster**) | `154.28 ms` (🚀 **2.06x faster**) | `336.80 ms` (✅ **1.06x slower**) | `333.00 ms` (✅ **1.05x slower**) | `332.51 ms` (✅ **1.05x slower**) | `333.55 ms` (✅ **1.05x slower**) |
| Compile      | `64.50 us` (✅ **1.00x**)  | `53.27 us` (✅ **1.21x faster**)  | `41.98 us` (✅ **1.54x faster**)  | `113.36 us` (❌ *1.76x slower*)   | `111.91 us` (❌ *1.74x slower*)   | `114.57 us` (❌ *1.78x slower*)   | `114.30 us` (❌ *1.77x slower*)   |
| Prove        | `316.77 ms` (✅ **1.00x**) | `151.03 ms` (🚀 **2.10x faster**) | `150.90 ms` (🚀 **2.10x faster**) | `334.39 ms` (✅ **1.06x slower**) | `332.21 ms` (✅ **1.05x slower**) | `330.52 ms` (✅ **1.04x slower**) | `332.26 ms` (✅ **1.05x slower**) |
| Verify       | `1.90 ms` (✅ **1.00x**)   | `1.88 ms` (✅ **1.01x faster**)   | `1.88 ms` (✅ **1.01x faster**)   | `1.67 ms` (✅ **1.14x faster**)   | `1.67 ms` (✅ **1.14x faster**)   | `1.65 ms` (✅ **1.15x faster**)   | `1.65 ms` (✅ **1.15x faster**)   |



| Fibonacci    | `Miden: iter-1000`        | `Risc0: iter-1000`              |
|:------------ |:------------------------- |:------------------------------- |
| All Combined | `2.76 s` (✅ **1.00x**)   | `2.74 s` (✅ **1.00x faster**)  |
| Compile      | `63.84 us` (✅ **1.00x**) | `108.18 us` (❌ *1.69x slower*) |
| Prove        | `2.72 s` (✅ **1.00x**)   | `2.70 s` (✅ **1.01x faster**)  |
| Verify       | `2.11 ms` (✅ **1.00x**)  | `12.08 ms` (❌ *5.73x slower*)  |

### Foot Notes
[^1]: This does not imply it is cheaper to only execute one branch like on a Von Neuman machine. For simpler branches, it is still more efficient to execute both branches.

[^2]: [GEB](https://github.com/anoma/geb) is much more general, as it seeks to be a categorical compiler pipeline from any language to another, allowing programmers to write code in more flexible ways while also allowing them to give up power for various mathematical properties.
