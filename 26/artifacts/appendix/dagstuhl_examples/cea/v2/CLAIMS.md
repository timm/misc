# Interesting claims, v2 (three EuroSys 2022 papers)

Rubric: ../RUBRIC_INTERESTING.md on top of ../DISCIPLINE.md. One generator agent per paper; claims that are bare measurements or design announcements were discarded (list at end).

## Executive summary

**42 claims kept** (Virtines 11, SafePM 15, VMSH 16), replacing 44 trite ones (15 appendix + 29 body). Every kept claim names a cause, a condition, a denied default belief, a refuted prior result, or a rebuttal.

**Scope:** law 11, method 28, product 3. Earlier round: 29 product, 0 law.

**Who says it:** stated by the paper 27, implied by its data and framing 15, reader synthesis 0. Nothing here is attributed to a paper that does not say it.

**Generators that fired most:** G2 ×25, G3 ×14, G8 ×13, G6 ×6, G4 ×5, G1 ×5, G7 ×3, G5 ×2. G2 (Aristotle's topics: cause, division, antecedent) and G8 (Toulmin rebuttal) carry the load; G3 (Davis, denied default belief) supplies the headline claims.

**The laws (product-free, conditional, two or more instances):**

- virtines:I1 — Hardware virtualization is not what makes a virtual machine slow to start; the software booted inside it is. With a guest image of kilobytes and no platform emulation, creating and entering a hardware-virtualized context costs on the order of the host's VM-enter syscall (microseconds), three to five orders of magnitude below a Linux VM boot.
- virtines:I3 — Per-invocation hardware isolation adds a fixed cost c that is independent of the work w done inside the isolated unit, so slowdown = 1 + c/w; for compute-bound functions the overhead becomes negligible (<3%) once w is roughly ten times c, which with snapshot-restored contexts of tens of kilobytes is about 100µs of work.
- virtines:I5 — Snapshotting an initialized isolated context pays off only when the initialization work it skips exceeds the cost of restoring the snapshot image; when in-context initialization is trivial or memory bandwidth is scarce, snapshotting is neutral or harmful.
- virtines:I7 — When an isolated context can retain initialized runtime state across invocations, the isolated path can be faster end-to-end than the native path that must initialize and tear down that state on every call; the gain equals the init+teardown cost minus the fixed isolation cost, and comes from executing less code, not faster code.
- virtines:I8 — Hardware isolation mechanisms share a create-once/enter-many cost structure: allocating the hardware-managed context (VMCS/VMCB, enclave) is expensive because the kernel must allocate and validate it, while entering an existing one is cheap; therefore pooling and recycling contexts, not a faster mechanism, is what brings per-invocation isolation to the hardware floor.
- safepm:I2 — A heap sanitizer protects only the allocators it intercepts: if an application's objects move to an allocator the sanitizer does not wrap, exploitability rises back toward the unprotected level, and it returns to the protected level only when that allocator's (de)allocations are made to update the sanitizer's metadata.
- safepm:I8 — If a sanitizer rebuilds its metadata from scratch at every process start, then accesses on the recovery path to objects allocated in earlier runs are unchecked; only metadata that persists with the data — and is itself updated crash-consistently — can check them.
- vmsh:I2 — Interposing on a guest's VM exits from outside the hypervisor with a debugger (ptrace on KVM_RUN) charges every exit, not only the ones addressed to the interposer, so the guest's own devices slow down in proportion to their exit rate: small-block IO (IOPS) suffers several times more than large-block IO (bandwidth); an in-kernel filter that forwards only the interposer's MMIO range removes the charge.
- vmsh:I5 — If a block-level interposer adds a fixed per-request cost, then a workload's slowdown falls with the fraction of its IO served from the guest page cache: direct-IO workloads see the full cost (up to 3.7×), metadata- and read-heavy cached workloads see little or none; and a cache on the path with zero reuse (sequential, never re-read) only adds cost.
- vmsh:I12 — Trace-based pruning of an image removes mostly the distribution userland (package managers, coreutils, shells), so the gain is a property of how much the image depends on an OS userland, not of the application: images that ship a distro userland shrink by half or more, images that are a single static binary shrink by under 10%; and because VM images carry more such tooling than containers, container results are a lower bound for VMs.
- vmsh:I13 — For an interactive console the acceptability bound is human perception, not the transport: once the echo round-trip is an order of magnitude below the visual-comprehension threshold (~13 ms per Potter et al. [91]), a console served through a side-loaded VirtIO device is indistinguishable in use from SSH, so console throughput is irrelevant to interactive usability.

**What changed versus the trite list.** Same evidence, different questions. 'VMSH is 1.5× slower on block I/O' became 'exit-interposition taxes every exit, so IOPS suffer far more than bandwidth'. 'Virtines start in 3 µs' became 'virtualization is not what makes VMs slow to start; guest software is'. 'SafePM costs 2.5×' became a rule about which allocators a sanitizer protects and a stated rebuttal (temporal safety is probabilistic without quarantine).

**Caveat.** Interest judged by one reader per paper against the filter; no second rater. Registry additions (baselines and prior systems named in refutations) are listed per paper, not yet merged into the shared registries.

## All claims

### virtines:I1  ·  scope=law  ·  G3(seems-expensive-is-cheap)+G4  ·  stated_by_paper=yes  ·  cea=3
**Claim.** Hardware virtualization is not what makes a virtual machine slow to start; the software booted inside it is. With a guest image of kilobytes and no platform emulation, creating and entering a hardware-virtualized context costs on the order of the host's VM-enter syscall (microseconds), three to five orders of magnitude below a Linux VM boot.
**Denies / refutes.** Default belief (paper, S8 @ p656 l27-31): 'the received wisdom is that when using hardware virtualization, creating a new isolated context for every isolation boundary crossing is too expensive.' Prior claim refuted: Faasm [70] (Shillaker & Pietzuch, USENIX ATC 2020) is 'partly based on the premise that hardware virtualization is simply too expensive' (S8 @ p657 l70-73). Registry: Faasm (new entry).
```
latency(minimal-runtime) < 30K cycles ≈ 12µs | mode=64-bit, machine=tinker ⊢ T1 @ p648 l29-30
latency(vs(Wasp+C, vmrun)) ≤ +4% | machine=tinker ⊢ F8 @ p650 l60-62
latency(vs(Wasp+C, Linux pthread)) < 0 ("much lower") ⊢ F8 @ p650 l52-54  [vague]
latency(vs(vmrun, Linux process)) < 0 ("far outstrip") ⊢ F2 @ p647 l23-28  [vague]
latency(Linux-VM-boot) ∈ hundreds of ms | hypervisor=QEMU/KVM ⊢ S4.2 @ p647 l37-38
fraction(Linux-VM-boot) ≈ 30% | component=ACPI-scan+ACPI-config+PCI-enumeration+rootfs ⊢ S4.2 @ p647 l40-42
latency(Unikraft) ∈ [10µs, 100µs] | workload=trivial image ⊢ S6.2 @ p653 l15-16
latency(MirageOS/Solo5-HVT) ≈ 12ms | workload=no-op ⊢ S6.2 @ p653 l21-24
latency(OSv) ≈ 600ms | workload=no-op, machine=tinker ⊢ S6.2 @ p653 l19-21
```
**Warrant.** Table 1 bounds the entire mode-transition boot at <30K cycles and Figure 8 puts a pooled context within 4% of the KVM_RUN ioctl, while the same hypervisor (KVM) takes hundreds of ms to boot Linux and 30% of that time is ACPI/PCI/rootfs work that a function image never needs. The ordering Wasp < Unikraft < Solo5/MirageOS < OSv < Linux VM tracks guest-software size, not the virtualization mechanism, which is constant across them.
**Rebuttal.** Fails once the image exceeds ~2MB (start-up becomes memory-copy bound, I4) or once per-invocation hypercalls dominate (I6). Under a Type-II VMM the host ioctl and ring transitions remain a floor of ~5µs per crossing (T2 @ p652 l10; S7.2 @ p656 l38-42).
**Instances.** This paper: Wasp on KVM (F8, T1) and on Hyper-V ('similar', S4.1 @ p647 l41-45); SGX enclave create/ECALL 'similar behavior' (F8 @ p650 l63-68).; Unikraft [42] (Kuenzer et al., EuroSys 2021): 10s-100s µs boot for trivial images (cited p653 l15-16).; Solo5 HVT [12] / MirageOS [54] ~12ms no-op vs OSv [40] ~600ms on the same testbed (p653 l19-24): larger runtime, longer boot, same KVM.; Firecracker [13] (Agache et al., NSDI 2020): omits PCI/ACPI/network features to cut boot (p647 l45-47).
**Evidence strength.** Strong for the product instance (artifact-reproduced F8, T1; E1/E4). The cross-system ordering rests on the authors' own OSv/MirageOS measurements plus Unikraft's published numbers; Figure 2 numbers are not stated in the text, so the process/pthread comparisons are qualitative.

### virtines:I2  ·  scope=method  ·  G2(definition/division; cause)+G8  ·  stated_by_paper=yes  ·  cea=3
**Claim.** The cost of bringing up a hardware-virtualized context is dominated by one component, page-table construction for the identity map (28K cycles; Table 1 minima sum to ~36K, the paper quotes the whole boot at <30K), so an isolated function that can run without paging (real or protected mode) skips most of the boot cost; mode selection is a performance knob for isolated-function toolchains.
**Denies / refutes.** Denies the implicit belief that VM boot cost is diffuse and mechanism-bound; it is one avoidable software step (paging setup) plus a surprisingly costly PE-bit flip.
```
latency(minimal-runtime) = 28109 cycles | component=Paging identity mapping ⊢ T1 @ p648 l5
latency(minimal-runtime) = 4118 cycles | component=Load 32-bit GDT (lgdt) ⊢ T1 @ p648 l10
latency(minimal-runtime) = 3217 cycles | component=Protected transition ⊢ T1 @ p648 l6
latency(minimal-runtime) = 681 cycles | component=Long transition (lgdt) ⊢ T1 @ p648 l7
latency(minimal-runtime) = 74 cycles | component=First Instruction ⊢ T1 @ p648 l11
latency(vs(16-bit mode, 64-bit mode)) ≈ -10K cycles | workload=fib(20), machine=tinker ⊢ F3 @ p648 l24-27
latency(vs(32-bit mode, 64-bit mode)) ≈ 0 ("essentially the same") | workload=fib(20) ⊢ F3 @ p648 l22-24  [vague]
```
**Warrant.** Table 1 gives per-component minima; paging is 28109 of the sum (~77%), and the paper states the most significant Table 1 costs 'are not incurred when executing in 16-bit mode' (p648 l21-22), which is why 16-bit fib(20) is ~10K cycles cheaper while 32- and 64-bit are equal (both pay paging + protected setup). The paging cost is explained mechanistically: three levels of page tables (12KB of references), control-register writes, and EPT construction in KVM (p648 l31-33).
**Rebuttal.** Only if 'the virtine is short-lived (on the order of microseconds) and can feasibly execute in real-mode' (p648 l24-27); the artifact warns that 'on some [machines] there is little difference' between modes (E2 @ p662 l36-38). The saving is bounded by ~10K cycles ≈ 4µs, so it matters only against a µs-scale budget.
**Instances.** This paper: T1, F3 (E1, E2 reproduce on 9 machines per artifact).; Echo server keeps the context in protected mode without paging and reaches C main in ~10K cycles (F4 @ p648 l48-50).
**Evidence strength.** Moderate-strong: minima rather than distributions; the 10K-cycle mode saving is read from Figure 3 with a false origin, and E2 concedes machine dependence.

### virtines:I3  ·  scope=law  ·  G2(antecedent/consequent)+G8  ·  stated_by_paper=implied  ·  cea=3
**Claim.** Per-invocation hardware isolation adds a fixed cost c that is independent of the work w done inside the isolated unit, so slowdown = 1 + c/w; for compute-bound functions the overhead becomes negligible (<3%) once w is roughly ten times c, which with snapshot-restored contexts of tens of kilobytes is about 100µs of work.
**Denies / refutes.** Denies the belief that hardware isolation imposes a proportional (multiplicative) tax on execution; the tax is additive and amortizable. Rebuts the appendix's own 'about 10×' amortization-point claim (C5 @ p661 l49-52), which the body's 2.5× (F11) does not support.
```
slowdown(vs(virtine+snap-fib, native-fib)) = 6.6× | n=0, machine=tinker ⊢ F11 @ p652 l26-27
slowdown(vs(virtine+snap-fib, native-fib)) = 1.03× | n=25 ⊢ F11 @ p652 l30-31
slowdown(vs(virtine+snap-fib, native-fib)) = 1.01× | n=30 ⊢ F11 @ p652 l31
amortization-work(virtine+snap-fib) ≈ 100µs ⊢ F11 @ p652 l33-34
speedup(vs(virtine+snap-fib, virtine-fib)) = 2.5× | n=0 ⊢ F11 @ p652 l47-50
latency(virtine+snap-openssl) ≈ 16µs | image-size=21KB (per invocation, read off F12) ⊢ S6.4 @ p654 l8-10
```
**Warrant.** Slowdown falls monotonically from 6.6× at zero work to 1.01× at fib(30) while c (shell provisioning + snapshot restore + marshalling) is unchanged across n; that is exactly the 1 + c/w curve. With c ≈ 5-16µs (T2 @ p652 l10; p654 l8-10), c/w < 0.03 at w = 100µs matches the reported crossover.
**Rebuttal.** Fails when the isolated unit's cost is not fixed per invocation: every host interaction adds ~5µs of exits that scale with I/O count, not with w (I6); and when invocations are 'many thousands of times per second' with tiny w (OpenSSL AES blocks) the fixed cost is 'amplified' to 17× (S6.4 @ p653 l53-55, p654 l6-8). Snapshot bars in F11 include the initial snapshot, so steady-state c is smaller than shown (p652 l50-53).
**Instances.** This paper: F11 (E5), corroborated by S6.4 OpenSSL (17× at 16KB blocks, i.e. w ≪ c).; Table 2 systems (T2 @ p652 l5-10): fixed crossing costs of 0.1µs (Hodor [32]), 0.5µs (SeCage [51]), 0.9µs (Enclosures [27]), 2.01µs (LwC [48]), ~60µs (Wedge [20]) each define their own amortization point under the same law.; Enclosures [27] observed the same amortization for connection sandboxing (cited p653 l22-24).
**Evidence strength.** Strong within the paper (E5 reproducible, monotone series over n = 0..30); the general law is the reader's fit of the additive model, which the paper states only as 'as expected' amortization.

### virtines:I4  ·  scope=method  ·  G2(cause)+G8  ·  stated_by_paper=yes  ·  cea=3
**Claim.** Snapshot-restore by memory copy makes isolated-context start-up latency linear in image size once the image exceeds a few MB, with slope 1/memcpy-bandwidth; beyond that knee the hypervisor is irrelevant and only copy-on-write (or a smaller image) can lower start-up cost.
**Denies / refutes.** Denies the belief that start-up latency is a hypervisor property; above the knee it is a memory-system property, and the 'tailor the image to the function' design rule is a performance requirement, not only an isolation nicety.
```
latency(virtine-halt) = 2.3ms | image-size=16MB, machine=tinker ⊢ F12 @ p653 l4-5
throughput(virtine-halt) ≈ 6.8GB/s | image-size=16MB ⊢ F12 @ p653 l5
throughput(memcpy) = 6.7GB/s | machine=tinker ⊢ S6.2 @ p653 l5-7
knee-image-size(virtine-halt) ≈ 2MB ⊢ F12 @ p661 l55-56
knee-image-size(virtine-halt) ∈ [1MB, 2MB] ⊢ E6 @ p662 l22-23
latency(virtine+snap-openssl) ≈ 16µs | image-size=21KB ⊢ F12 @ p654 l8-10
size(virtine) ≈ 16KB | typical image ⊢ S2 @ p645 l55-57
```
**Warrant.** At 16MB the measured start-up throughput (6.8GB/s) equals the machine's memcpy bandwidth (6.7GB/s) within 2%, and the paper's mechanism statement is explicit: 'Wasp's snapshotting mechanism currently uses memcpy to populate a virtine's memory image with the snapshot' (S7.2 @ p656 l26-30). The OpenSSL case is diagnosed the same way: 'virtine creation in this example is memory bound, since copying the snapshot comprises the dominant cost' (p654 l10-12).
**Rebuttal.** 'Where exactly the knee occurs depends on the memory copy bandwidth of the machine' (E6 @ p662 l24-25). Below the knee, fixed provisioning cost dominates and image size is nearly free. Copy-on-write as in SEUSS [21] is expected to 'reduce [this cost] drastically' (p653 l9-11), which would flatten the slope.
**Instances.** This paper: F12 (E6), S6.4 (21KB → 16µs), S6.5 Duktape (~578KB image, p655 l53-54).; SEUSS [21] (Cadden et al., EuroSys 2020) uses copy-on-write precisely to escape this bound (cited p653 l9-11, p656 l33-36).
**Evidence strength.** Strong: two independent measurements (start-up throughput vs memcpy bandwidth) agree; memcpy method not described; only one machine reported for the knee.

### virtines:I5  ·  scope=law  ·  G2(contraries)+G3(seems-necessary-is-unnecessary)+G8  ·  stated_by_paper=yes  ·  cea=3
**Claim.** Snapshotting an initialized isolated context pays off only when the initialization work it skips exceeds the cost of restoring the snapshot image; when in-context initialization is trivial or memory bandwidth is scarce, snapshotting is neutral or harmful.
**Denies / refutes.** Denies the belief that checkpoint/snapshot is an unconditional win for cold starts (the serverless literature's default: SEUSS [21], Catalyzer [26], SOCK [60]). The paper's own appendix supplies the contrary case.
```
speedup(vs(virtine+snap-fib, virtine-fib)) = 2.5× | n=0 ⊢ F11 @ p652 l47-50
overhead(vs(virtine+snap-js, virtine-js)) ≈ -50% ("roughly 2×" reduction) ⊢ F14 @ p654 l21-25
throughput(vs(virtine+snap-http, native-http)) = -12% ⊢ F13 @ p653 l56-58
throughput(vs(virtine+snap-http, virtine-http)) < 0 | memory-bandwidth=limited ⊢ E7 @ p662 l36-38  [vague]
latency(virtine-halt) ∝ image-size | image-size > 2MB ⊢ F12 @ p653 l4-7
```
**Warrant.** Snapshot gain scales with skipped initialization: 2.5× for a libc-only fib image, ~2× for Duktape whose init 'avoids many calls to malloc and other expensive functions' (p654 l21-22), and the appendix generalizes: 'Snapshotting improves performance when environment setup in the virtual context is non-trivial' (C8 @ p661 l72-73). Snapshot cost scales with image size at memcpy bandwidth (I4). The sign of the net effect is therefore the sign of (init skipped) − (image/bandwidth).
**Rebuttal.** The harmful case is stated only as an expectation for evaluators ('may actually reduce performance ... on machines with limited memory bandwidth', E7 @ p662 l36-38) and is not measured in the body; F11 snapshot bars include the initial snapshot cost, so steady-state gains are understated (p652 l50-53).
**Instances.** This paper: F11 (fib), F14 (Duktape), F13 (HTTP), E7 contrary condition.; SEUSS [21], SOCK [60], Catalyzer [26] achieve sub-20ms cold starts by caching initialized runtime state (p655 l15-18; p657 l4-10), i.e. the high-init regime where the law predicts a win.
**Evidence strength.** Moderate: the positive branch is measured three times; the negative branch is asserted, not measured.

### virtines:I6  ·  scope=method  ·  G2(division; cause)+G1(policy)+G8  ·  stated_by_paper=yes  ·  cea=3
**Claim.** For I/O-bound functions under a hosted (Type-II) hypervisor, the overhead of hardware isolation is set by the number of host interactions, not by context creation: each hypercall costs a VM exit plus two ring transitions, so the design rule is to expose few, high-level hypercalls rather than emulated devices.
**Denies / refutes.** Denies the belief that once creation is cheap the isolation problem is solved; the residual cost moves to the boundary. Also rebuts the appendix headline C7 ('less than 20% drop', p661 l62-66): that holds only with snapshotting; without it the appendix itself expects ~2× (E7 @ p662 l34-35).
```
count(virtine-http) = 7 hypercalls/request ⊢ S6.3 @ p653 l46-50
throughput(vs(virtine+snap-http, native-http)) = -12% | machine=tinker ⊢ F13 @ p653 l56-58
throughput(vs(virtine-http, native-http)) ≈ -50% ("2× drop") ⊢ E7 @ p662 l34-35
latency(vs(virtine-http, native-http)) ≈ +100% ("a little more than 2×") ⊢ E7 @ p662 l34-35
latency(virtine) = 5µs | measure=boundary-cross, mechanism=syscall+VMRUN ⊢ T2 @ p652 l10
latency(Hodor) = 0.1µs | measure=boundary-cross, mechanism=VMFUNC-no-exit ⊢ T2 @ p652 l9
latency(SeCage) = 0.5µs | measure=boundary-cross, mechanism=VMFUNC-no-exit ⊢ T2 @ p652 l8
latency(echo-server) < 300µs | workload=HTTP echo, optimizations=none ⊢ F4 @ p649 l34-38
```
**Warrant.** Mechanism is stated: 'The exits generated by these hypercalls are doubly expensive due to the ring transitions necessitated by KVM' (p653 l54-55) and 'Most of the performance drop is caused by hypercall interactions' (E7 @ p662 l37-38). Design insight (3): host interactions 'must be limited to keep costs low' (p649 l52-54); hypercalls are 'high-level hypervisor services with as few exits as possible' (p649 l38-41). The 50× gap between virtines (5µs) and Hodor (0.1µs) in T2 is attributed to the user-space KVM_RUN path (p652 l43-45).
**Rebuttal.** Costs 'would be reduced in a more realistic HTTP server, as more work unrelated to I/O would be involved' (p653 l59, l22-24); a Type-I VMM 'can mitigate some software latencies' (S7.2 @ p656 l42-44). The 12% figure is a harmonic mean over a single-file localhost workload on one machine.
**Instances.** This paper: F13 (E7), T2, F4.; Enclosures [27] report the same effect for connection sandboxing (p653 l23-24).; Solo5/ukvm [12][75] use hypercall-based I/O 'in a similar way to virtines' (p653 l22-24; p657 l56-60).
**Evidence strength.** Strong on direction and mechanism; the per-hypercall cost is not isolated experimentally (no hypercall-count sweep), and the 2× no-snapshot figure comes only from the appendix expectation.

### virtines:I7  ·  scope=law  ·  G3(seems-expensive-is-cheap)+G2(cause)  ·  stated_by_paper=implied  ·  cea=3
**Claim.** When an isolated context can retain initialized runtime state across invocations, the isolated path can be faster end-to-end than the native path that must initialize and tear down that state on every call; the gain equals the init+teardown cost minus the fixed isolation cost, and comes from executing less code, not faster code.
**Denies / refutes.** Denies the belief that isolation always costs latency relative to native. The paper never says 'faster than native', but its numbers do: 137µs isolated vs 419µs native.
```
latency(native-js) = 419µs | workload=Duktape base64, machine=tinker ⊢ F14 @ p654 l16
latency(vs(virtine-js, native-js)) = +125µs ⊢ F14 @ p654 l49-50
latency(virtine+snap-js-NT) = 137µs ⊢ F14 @ p654 l36-38
slowdown(vs(virtine+snap-js-NT, native-js)) = 0.33× (derived: 137/419) ⊢ F14 @ p654 l16, l37
overhead(vs(virtine+snap-js, virtine-js)) ≈ -50% ⊢ F14 @ p654 l21-25
count(virtine-js) = 3 hypercalls ⊢ S6.5 @ p654 l35-36
```
**Warrant.** The authors give the cause: retaining the Duktape context is 'something that cannot be done when executing in the client environment' (p654 l29-31) and 'the virtine is not executing code any faster than native, but ... executing less code' (p654 l33-36); 137µs 'effectively constitutes the parsing and execution of the JavaScript code' (p654 l37-38).
**Rebuttal.** Requires that retained state be safe to share across invocations; a snapshot's private state 'is exposed to all future virtines that are created using that reset state' (S5.2 @ p650 l47-52). Requires a runtime whose init is heavy relative to the work (Duktape, no JIT); production engines (V8) differ and are untested (S7.2 @ p655 l47-53). The native baseline is chosen to tear down the engine each call; a native design that pooled engines would remove the gap.
**Instances.** This paper: F14 (E8).; SEUSS [21] applies 'similar optimizations' with V8 and 'avoids even more initialization overhead' (p654 l39-41).; Cloudflare V8 isolates [23] reuse runtime state to cut cold start (p657 l73-74).
**Evidence strength.** Moderate: single workload, single machine, and the comparison hinges on an unpooled native baseline; the 0.33× ratio is the reader's arithmetic from stated numbers.

### virtines:I8  ·  scope=law  ·  G7(scene: VT-x/SVM -> SGX)+G2(cause)  ·  stated_by_paper=yes  ·  cea=2
**Claim.** Hardware isolation mechanisms share a create-once/enter-many cost structure: allocating the hardware-managed context (VMCS/VMCB, enclave) is expensive because the kernel must allocate and validate it, while entering an existing one is cheap; therefore pooling and recycling contexts, not a faster mechanism, is what brings per-invocation isolation to the hardware floor.
**Denies / refutes.** Denies the belief that the Figure 8 result is a KVM artefact; the same shape appears on Intel SGX (enclave create ≫ ECALL) and on a different vendor/OS (Hyper-V).
```
latency(vs(Wasp+C, vmrun)) ≤ +4% ⊢ F8 @ p650 l60-62
latency(vs(Wasp, Wasp+C)) > 0 | cause=KVM_CREATE_VM host allocation of VMCS/VMCB ⊢ F8 @ p650 l38-47
latency(vs(SGX Create, ECALL)) > 0 ("similar behavior") | machine=Dell XPS 9500 ⊢ F8 @ p650 l63-68  [vague]
latency(Wasp | host=Hyper-V) ≈ same-as(KVM) ("similar") ⊢ S4.1 @ p647 l41-45  [vague]
```
**Warrant.** The paper names the cause of the create/enter gap ('the host kernel's internal allocation of the VM state (VMCS on Intel/VMCB on AMD)', p650 l40-43) and shows that recycling removes it (within 4% of vmrun). It then reports the same behaviour on SGX and Hyper-V, two independent mechanisms/hosts.
**Rebuttal.** SGX and Hyper-V numbers are not given in the text (only in the bottom half of F8 and as 'similar'); the artifact does not reproduce the SGX panel (hardware list omits SGX, A.2.2 @ p661 l25-29). Recycling adds a clean step whose cost is hidden in Wasp+CA (p650 l56-60) and a state-leak risk (p650 l43-45).
**Instances.** This paper: KVM/AMD (F8 top), SGX/Intel (F8 bottom), Hyper-V (text only).; SOCK [60] and SEUSS [21] pool/reuse containers or runtime state for the same reason (p655 l14-18).
**Evidence strength.** Weak-moderate: one quantified instance (KVM); SGX and Hyper-V instances are qualitative in the text.

### virtines:I9  ·  scope=method  ·  G3(seems-necessary-is-unnecessary)+G5(kairos)  ·  stated_by_paper=yes  ·  cea=2
**Claim.** Serverless cold-start latency is an artefact of using the container (a process-granularity abstraction) for function-granularity isolation; a hardware-virtualized context purpose-built for one function starts faster than a process on the same host, so the container is unnecessary for isolation and is the wrong unit for it.
**Denies / refutes.** Denies the default belief that containers are the natural isolation unit for serverless functions (Amazon Lambda 'constructs a container to achieve the desired level of isolation [73]', p655 l35-38). Kairos: serverless made function-granularity isolation 'particularly salient today' (S1 @ p644 l63-69).
```
latency(vs(vmrun, Linux process)) < 0 ("far outstrip any start-up performance that processes (and by proxy, containers) will achieve") ⊢ F2 @ p647 l23-28  [vague]
latency(vs(Wasp+C, Linux pthread)) < 0 ("much lower") ⊢ F8 @ p650 l52-54  [vague]
latency(Vespid) < latency(OpenWhisk) | load=bursty, engine=Duktape vs V8 ("low-latency responses") ⊢ F15 @ p655 l8-11  [vague]
latency(SOCK|SEUSS|Faasm|Catalyzer) < 20ms | measure=cold-start ⊢ S7.1 @ p655 l14-18
```
**Warrant.** Cold-start challenges 'stem from contorting the container abstraction to fit an unintended usage model' (p644 l67-69); Figures 2 and 8 order creation costs vmrun < Wasp+C < pthread < process; Figure 15 shows a virtine-based platform beating vanilla OpenWhisk under burst.
**Rebuttal.** Vespid 'is a prototype that lacks many features offered by OpenWhisk, including the high-performance V8 engine', and OpenWhisk lacks reuse/snapshot optimizations that SOCK/SEUSS/Faasm/Catalyzer have (p655 l12-18); Figure 15 therefore compares an optimized prototype to an unoptimized baseline. No process/container creation number is stated in the text.
**Instances.** This paper: F2, F8, F15.; Firecracker [13] already replaces containers with µVMs for Lambda isolation (p647 l45-47; p657 l53-56).
**Evidence strength.** Weak: all atoms are qualitative orderings; Figure 15 is unreproduced by the artifact.

### virtines:I10  ·  scope=method  ·  G6(enthymeme)+G1(policy)  ·  stated_by_paper=implied  ·  cea=3
**Claim.** Whether a hardware-isolation slowdown is 'acceptable' is a property of where the isolation boundary is cut in the call graph, not of the mechanism: the unstated premise behind calling 17× (AES block) and 1.3× (JS function) acceptable is that the practitioner will move the cut outward until each isolated invocation carries ≥100µs of work and few host interactions.
**Denies / refutes.** Surfaces the enthymeme behind the abstract's 'acceptable slowdowns' (p644 l48) and the appendix's 'acceptable (< 1.5×)' (C8 @ p661 l70-72): acceptability presupposes re-cutting, which the paper states only as an expectation.
```
slowdown(vs(virtine+snap-openssl, native-openssl)) = 17× | block-size=16KB, tool=openssl speed -evp aes-128-cbc ⊢ S6.4 @ p654 l6-8
time-to-port(OpenSSL) ≈ 1 hour | developer=experienced ⊢ S6.4 @ p653 l45-46
slowdown(vs(virtine-js, native-js)) = 1.30× (derived: (419+125)/419) ⊢ F14 @ p654 l16, l49-50
amortization-work(virtine+snap-fib) ≈ 100µs ⊢ F11 @ p652 l33-34
count(virtine-http) = 7 hypercalls/request ⊢ S6.3 @ p653 l46-50
```
**Warrant.** The paper concedes the AES cut 'would not be a good candidate for running in virtine context from a performance perspective' (p653 l35-37) and that 'in a realistic scenario, the developer would likely include more functionality in virtine context, amortizing those overheads' (p654 l4-5); the cut is currently 'made by the programmer' (S2 @ p645 l49-51). Together with I3 and I6 this yields the policy: choose the cut so that work ≥ ~100µs and hypercalls are few.
**Rebuttal.** Moving the cut outward enlarges the trusted-code surface inside the virtine and the image (raising snapshot cost, I4); the C extension cannot cross compilation units (S7.2 @ p655 l65-71), which limits where the cut can be placed today.
**Instances.** This paper: S6.4 (17×), S6.5 (1.3×), F11 (100µs), F13 (7 hypercalls, 12%).; Enclosures [27] and SeCage [51] make the same cut-placement decision at package/secret granularity (S8 @ p656 l44-63).
**Evidence strength.** Moderate: numbers are the paper's; the policy is the reader's synthesis of three separate results and the authors' stated expectation.

### virtines:I11  ·  scope=method  ·  G7(agency: Type-II -> Type-I VMM)+G2(cause)  ·  stated_by_paper=yes  ·  cea=2
**Claim.** The ~5µs floor on a hardware-isolated function call is a cost of hosting the hypervisor in user space (ioctl + ring transitions per exit), not of the VMRUN instruction; published in-kernel or exit-free crossings (0.1-0.5µs) bound what a Type-I virtine hypervisor could reach, roughly a 10× reduction.
**Denies / refutes.** Denies reading Table 2 as 'virtines are 10-50× slower than VMFUNC-based isolation'; the gap is attributable to measurement scope and hosting, which the paper states.
```
latency(virtine) = 5µs | measure=boundary-cross ⊢ T2 @ p652 l10
latency(Hodor) = 0.1µs | measure=boundary-cross ⊢ T2 @ p652 l9
latency(SeCage) = 0.5µs | measure=boundary-cross ⊢ T2 @ p652 l8
latency(Enclosures) = 0.9µs | measure=boundary-cross ⊢ T2 @ p652 l7
latency(LwC) = 2.01µs | measure=boundary-cross ⊢ T2 @ p652 l6
latency(Wedge) ≈ 60µs | measure=boundary-cross ⊢ T2 @ p652 l5
latency(vs(Wasp+C, vmrun)) ≤ +4% ⊢ F8 @ p650 l60-62
```
**Warrant.** 'SeCage and Hodor measure only the latency of the VMFUNC instruction without a VMEXIT event. Virtine latency is measured from userspace on the host, surrounding the KVM_RUN ioctl, thus incurring system call and ring-switch overheads' (p652 l41-45); 'Some of these costs are unavoidable because they maintain userspace control over the VM. However, a Type-I VMM like Palacios or Xen can mitigate some software latencies' (S7.2 @ p656 l38-44). Wasp+C is already within 4% of vmrun, so the remaining cost is in the ioctl path.
**Rebuttal.** Table 2 mixes systems with different isolation guarantees ('the types of isolation these systems provide is slightly different', p652 l37-38) and quotes published numbers on other hardware; the Type-I projection is untested. Giving up user-space control loses the embeddable-library deployment model that motivates Wasp (S5.1 @ p649 l7-11).
**Instances.** This paper: T2, F8, S7.2.; Hodor [32] and SeCage [51] (VMFUNC, no exit) as the exit-free instances of the same crossing.
**Evidence strength.** Weak-moderate: T2 numbers for other systems are quoted, not measured; the attribution is the authors' explanation, not an ablation.

### safepm:I1  ·  scope=method  ·  G3(seems-necessary-is-unnecessary)  ·  stated_by_paper=implied  ·  cea=3
**Claim.** A volatile-memory shadow sanitizer can be extended to a persistent heap without modifying its compiler pass or its runtime library, provided the persistent metadata is kept in the sanitizer's own shadow byte format and mmap-overlaid onto the address the sanitizer's fixed shadow-translation formula already expects.
**Denies / refutes.** Default belief (the paper's own framing): 'the existing approaches are incompatible for PM' (ABS p506 l14-15) and mapping pool addresses to in-pool metadata 'would require changes to ASan's compiler pass' (S4.2 p512 l25-26). Registry: AddressSanitizer; new entry 'overmap'.
```
count(modifications, AddressSanitizer) = 0 | component=compiler-pass ⊢ S5 @ p515 l28-29
count(modifications, AddressSanitizer) = 0 | component=runtime-library, mechanism=overmap ⊢ S4.1 @ p511 l40-42
safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | outcome=always, workload=RIPE, repeats=3 ⊢ T5 @ p518 l11-13
slowdown(vs(SafePM, ASan)) < 1.20× | workload=persistent indices, except=hashmap-get ⊢ S6.2 @ p516 l42-45
size(persistent shadow memory) = 1/8 × pool | format=same-as(AddressSanitizer) ⊢ S4.1 @ p510 l62-64 ∧ p511 l54-55
```
**Warrant.** The unmodified ASan checks reach the same RIPE outcome on the PM heap (27/1/1306) as on the volatile heap, so the checks are consulting the overlaid persistent metadata; slowdown within 20% of ASan shows the checks were not degraded by the overlay. Zero-modification is a structural fact checkable in the artifact (ASan is stock gcc 9.3.0; only PMDK is forked).
**Rebuttal.** Requires (a) a sanitizer whose shadow address is a fixed linear function of the data address (GET_SM, p511 l48) so that a page-aligned mmap can land on it, and (b) the pool mapped at 8×page alignment and padded (p514 l112-117 right column). The paper itself limits applicability to shadow-memory tools (p514 l50-53); pointer-based schemes (SoftBound/CETS [68,70]) keep metadata per pointer and cannot be overlaid.
**Instances.** This paper: SafePM over ASan (only demonstrated instance).; Paper asserts other shadow-memory tools [27,78] could be handled the same way but does not demonstrate it (p514 l52-55) — hence method, not law.
**Evidence strength.** Measured (RIPE counts ×3 repeats, slowdowns averaged over ≥3 runs), no variance reported; zero-modification is verifiable structurally in the artifact.

### safepm:I2  ·  scope=law  ·  G2(antecedent/consequent)+G3(seems-general-is-local)  ·  stated_by_paper=implied  ·  cea=3
**Claim.** A heap sanitizer protects only the allocators it intercepts: if an application's objects move to an allocator the sanitizer does not wrap, exploitability rises back toward the unprotected level, and it returns to the protected level only when that allocator's (de)allocations are made to update the sanitizer's metadata.
**Denies / refutes.** Default belief: 'compile with ASan and the heap is protected' — the paper reads Table 5 as 'ASan is able to prevent most attacks' (S6.4 p518 l56-58) and then shows this is local to the system heap. Registry: AddressSanitizer, ASan w/ system heap, ASan w/ PM pool heap, Intact.
```
safety(Intact) = 306 attacks | outcome=always, workload=RIPE, repeats=3, compiler=gcc-9.3.0 ⊢ T5 @ p518 l10
safety(ASan w/ system heap) = 27 attacks | outcome=always, workload=RIPE ⊢ T5 @ p518 l11
safety(ASan w/ PM pool heap) = 119 attacks | outcome=always, workload=RIPE ⊢ T5 @ p518 l12
safety(vs(ASan w/ PM pool heap, ASan w/ system heap)) = +92 attacks | outcome=always ⊢ T5 @ p518 l11-12
safety(vs(ASan w/ PM pool heap, ASan w/ system heap)) = +11 attacks | outcome=sometimes ⊢ T5 @ p518 l11-12
safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | outcome=always ⊢ T5 @ p518 l11-13 ∧ safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | outcome=sometimes ⊢ T5 @ p518 l11-13
```
**Warrant.** Same exploit suite, same compiler and stack protections (p518 l38-40); only the heap the victim allocates from changes. Moving to the un-wrapped PM heap multiplies always-successful attacks 4.4× (27→119); wrapping the PM allocator (Table 1) returns the count exactly to 27. The paper names the mechanism: 'the layout of the persistent heap is not available to ASan' (p518 l60-63).
**Rebuttal.** Coverage does not drop to the unprotected level (119, not 306) because stack and global instrumentation are allocator-independent; so the law is about heap objects only. For pointer-based bounds checkers the same coupling exists at allocation time (bounds must be recorded when the object is created), but the paper offers no evidence for them.
**Instances.** This paper: ASan × PM pool heap (Table 5) — 27 → 119 → 27.; memcheck on PM [21]: works only through 'instrumentation built into PMDK' (S7 p519 l32-33), i.e. Valgrind also had to be told about the PM allocator to see that heap.; KASAN [23] (cited p506 l55): the kernel sanitizer exists precisely because userspace ASan does not intercept the kernel's slab allocator — reader-supplied instance, not argued by the paper.
**Evidence strength.** Measured counts, 3 repeats per exploit, suite re-run 'several times' for stability (p518 l48-50), artifact script run_ripe.sh; no per-attack breakdown by heap vs stack.

### safepm:I3  ·  scope=method  ·  G3(seems-expensive-is-cheap)+G2(division)  ·  stated_by_paper=implied  ·  cea=3
**Claim.** In a persistent shadow-memory sanitizer the crash-consistent metadata bookkeeping (transactions, undo-log snapshots of shadow bytes on every alloc/realloc/free) is a minor cost; the dominant cost is the inline access check the sanitizer already imposed on volatile memory. Persistence of the safety metadata is nearly free relative to sanitization itself.
**Denies / refutes.** Default belief set up by the paper: 'it is imperative to design a crash consistent safety mechanism' (ABS p506 l17-18) and that PM sanitizers carry 'prohibitive' overhead (S3 p508 l36-38). Registry: SafePM w/o ASan (the ablation), ASan, Native.
```
slowdown(SafePM w/o ASan) < 1.20× | workload=persistent indices, op∈{insert,get,remove}, except=rtree-insert ⊢ S6.2 @ p515 l73-77 ∧ p516 l42
slowdown(SafePM w/o ASan) = 1.34× | workload=rtree, op=insert ⊢ S6.2 @ p515 l75-76
slowdown(SafePM) ∈ [1.68,2.00]× | workload=persistent indices, op=insert ⊢ F3 @ p515 l71-72 ∧ slowdown(SafePM) ∈ [1.16,1.50]× | op=get ⊢ F3 @ p515 l71-72 ∧ slowdown(SafePM) ∈ [1.68,1.87]× | op=remove ⊢ F3 @ p515 l72-73
latency(vs(SafePM w/o ASan, Native)) < +0.3 ms | workload=recovery process, log∈[4KB,4MB], repeats=100 ⊢ T3 @ p517 l59-61
latency(vs(ASan, Native)) ≈ +10 ms | workload=recovery process, log∈[4KB,4MB] ⊢ T3 @ p517 l61-63
latency(vs(SafePM w/o ASan, Native)) ≈ 0 [vague: 'close to'] | workload=PM pool create/open, op=open ⊢ F6 @ p517 l32-35
slowdown(vs(SafePM, ASan)) ≤ 1.08× | workload=pmemkv, threads∈[1,24], derived from 2.55/2.36 ⊢ F4 @ p516 l64-66
```
**Warrant.** Table 2's four variants form a 2×2 ablation (ASan on/off × wrappers on/off). In three independent experiments — indices (Fig 3), recovery (Table 3), pool open (Fig 6) — the wrappers-only term is within a few percent of native while the ASan term accounts for essentially the whole gap; SafePM ≈ ASan + ε.
**Rebuttal.** Fails for allocation-heavy operations: free costs 3.5-7.0× and atomic alloc/free become transactions with PSM snapshots (F5 p516 l64-71, l75-79); for these the paper attributes cost to both checks and wrappers and reports no ablation, so the wrapper term may dominate there. Also fails where red zones change alignment (hashmap get, p516 l45-47).
**Instances.** This paper: three experiments (Fig 3, Table 3, Fig 6) on one product — method, not law.
**Evidence strength.** Measured with an explicit ablation variant; ≥3 runs (100 for recovery); no variance or CI; pool-open 'close to' is unquantified.

### safepm:I4  ·  scope=method  ·  G2(cause)  ·  stated_by_paper=yes  ·  cea=3
**Claim.** If sanitizer metadata must stay crash-consistent with a persistent allocator's own heap metadata, the allocator's non-transactional (single-atomic-store) operations cannot be preserved: the two updates cannot be one atomic store, so every 'atomic' allocation or free is silently promoted to a logged transaction and pays more than its already-transactional counterpart.
**Denies / refutes.** Default belief: an allocator's atomic fast path survives instrumentation. The paper names the cause: shadow modification 'cannot be performed with a single atomic operation in conjunction with the actual PM heap metadata modification' (S4.2 p513 l30-33). Registry: PM operations benchmark; cond mode=atomic|transactional.
```
slowdown(SafePM) ∈ [2.4,5.8]× | workload=PM operations benchmark, op=alloc, mode∈{atomic,transactional}, decreasing-in object-size∈[64B,16KB], repeats=10 ⊢ F5 @ p516 l64-67
slowdown(SafePM) ∈ [1.85,2.25]× | workload=PM operations benchmark, op=realloc ⊢ F5 @ p516 l67-69
slowdown(SafePM) ∈ [3.5,7.0]× | workload=PM operations benchmark, op=free ⊢ F5 @ p516 l69-71
slowdown(vs(SafePM|mode=atomic, SafePM|mode=transactional)) > 1× | workload=PM operations benchmark, op∈{alloc,realloc,free} ⊢ F5 @ p516 l75-79
count(non-transactional operations preserved, SafePM) = 0 ⊢ S4.2 @ p513 l26-28
```
**Warrant.** The paper states the conversion is 'inevitable' (p513 l30) and reads Fig 5 as the atomic curves lying above the transactional ones 'as it transparently converts them into their transactional counterpart' (p516 l75-79). The per-object cost is a constant (snapshot + log entry), which is why alloc overhead falls with object size while free stays high.
**Rebuttal.** Would not hold for an allocator whose metadata word and the shadow bits fit in one atomic update (or a log-free design that colocates them), nor for read-only paths (get) that allocate nothing (F7 p518 l64-67). The atomic-vs-transactional gap is shown graphically only; no number is given.
**Instances.** This paper (Fig 5).; Poseidon [31] (p519 l6-9) protects persistent allocator metadata with per-operation MPK domain switches — a different mechanism for the same coupling; not the same relation, so method not law.
**Evidence strength.** Measured, 100K ops/experiment, average of 10 runs, no variance; mechanism stated by the paper; atomic-vs-transactional difference not quantified in text.

### safepm:I5  ·  scope=method  ·  G2(division)+G2(contraries)  ·  stated_by_paper=yes  ·  cea=3
**Claim.** The space cost of per-object guard zones is set by the allocator's size-class padding, not by the guard size: where padding already absorbs two red zones the guards are free and the shadow array (1/8 of the pool) is the entire overhead; where object sizes sit tight against a size-class boundary the guards spill into the next class.
**Denies / refutes.** Default belief: 2×16 B of red zone per object is a per-object space tax. Paper: red zones 'occupy space which is wasted to padding by the native PMDK allocator' (S6.3 p517 l58-61). Registry: persistent red zones, persistent shadow memory.
```
space-overhead(SafePM) = 12.5% | workload∈{ctree,rbtree,hashmap}, op∈{insert,remove,get}, at=peak ⊢ T4 @ p517 l37-40
space-overhead(SafePM) ∈ [13.8,14.25]% | workload=rtree, op∈{insert,remove,get} ⊢ T4 @ p517 l38
size(persistent shadow memory) = 1/8 × pool ⊢ S6.3 @ p517 l55-57
size(persistent red zones) = 16 B | count=2 per object ⊢ S6.2 @ p515 l55-57
space-overhead(vs(SafePM, persistent shadow memory)) = 0% | workload∈{ctree,rbtree,hashmap} ⊢ T4 @ p517 l37-40 ∧ p517 l57-61
```
**Warrant.** 12.5% equals exactly the PSM fraction, so in three of four indices red zones added zero bytes at peak; only rtree, whose node sizes do not leave 32 B of padding, shows the extra 1.3-1.75 points.
**Rebuttal.** Fails for allocators without size-class rounding (bump or exact-fit allocators), which would pay the full 32 B per object; and the same red zones that are free in space are not free in time — for hashmap get they shift object alignment and add cache-line accesses (S6.2 p516 l45-47), a seems-independent-is-coupled effect the paper reports only for that case.
**Instances.** This paper (Table 4).; ASan's own allocator sizes red zones by size class [80] — same design rationale, but the zero-cost relation is not reported there; method not law.
**Evidence strength.** Measured at peak usage via artifact utils/pool_overhead.cpp and table_4.sh; single number per cell, no repeats stated.

### safepm:I6  ·  scope=method  ·  G2(cause)+G3(seems-independent-is-coupled)  ·  stated_by_paper=implied  ·  cea=2
**Claim.** Sanitizer slowdown is not a fixed multiplier of baseline throughput: under write contention, instrumentation slows each thread and thereby relieves contention on the shared structure, so the measured slowdown shrinks as thread count grows beyond the contention knee.
**Denies / refutes.** Default belief: overhead ratios measured single-threaded transfer to multi-threaded runs. Paper: 'a significant drop in the overheads beyond 8 threads in the update-intensive workloads ... attributed to the native application suffering from increasing level of contention while the instrumentation decreases this stress' (S6.2 p516 l75-79). Registry: pmemkv.
```
slowdown(ASan) ∈ [1.14,2.36]× | workload=pmemkv, threads∈[1,24], mix∈{50/50,95/5,random-read,seq-read} ⊢ F4 @ p516 l64-65
slowdown(SafePM) ∈ [1.20,2.55]× | workload=pmemkv, threads∈[1,24] ⊢ F4 @ p516 l66
slowdown(SafePM) decreasing-in threads | workload=pmemkv, mix=50/50, threads>8 ⊢ F4 @ p516 l75-77
throughput-scaling(SafePM) = same-as(Native) [vague] | workload=pmemkv, threads∈[1,24] ⊢ F4 @ p516 l72-75
```
**Warrant.** The paper names the mechanism and Fig 4 shows the ratio falling with thread count only for the update-intensive mix; read-only mixes show no such knee, which is what the contention explanation predicts.
**Rebuttal.** No contention measurement (lock wait, cache-line bouncing) is reported; the attribution is inferred from the shape of the curve. Expect the effect to vanish on machines with fewer cores than the knee (8 here on 24 cores) and for read-dominated workloads.
**Instances.** This paper (Fig 4, 50/50 and 95/5 mixes).; No second product; the paper cites none. Method.
**Evidence strength.** Measured (10M ops, 1M entries, ≥3 runs) but the drop is not quantified and the cause is asserted, not tested; level 2.

### safepm:I7  ·  scope=method  ·  G2(cause)+G2(contraries)  ·  stated_by_paper=yes  ·  cea=3
**Claim.** Partial coverage cannot drive a shadow sanitizer's overhead to zero: a residual floor is set by the sanitizer's interception of the runtime library's volatile heap calls (malloc/free inside the PM library), independent of how many persistent objects are excluded; operations that trigger no volatile allocation have no floor.
**Denies / refutes.** Default belief: excluding 100% of objects from checks recovers native speed. Paper: 'there is still an inevitable overhead that stems from ASan intercepting the volatile heap management functions, which are used by PMDK internally' (S6.6 p518 l58-63 right column). Registry: partial safety coverage.
```
slowdown(partial safety coverage) decreasing-in unsafe-objects-% | workload=hashmap, op∈{insert,remove,get}, unsafe-objects∈[0,100]% ⊢ F7 @ p518 l53-58
slowdown(partial safety coverage) > 1× | workload=hashmap, op∈{insert,remove}, unsafe-objects=100% ⊢ F7 @ p518 l58-63
slowdown(partial safety coverage) ≈ 1× | workload=hashmap, op=get, unsafe-objects=100% ⊢ F7 @ p518 l64-67
```
**Warrant.** The floor appears only for operations that call PMDK routines which internally malloc/free (insert/remove) and is absent for get; the paper's mechanism (libc allocator interception by ASan's runtime) predicts exactly that split.
**Rebuttal.** The floor is a property of ASan's allocator replacement, not of the PM design; running with ASan's allocator interception disabled for the library, or an uninstrumented libc, would remove it. Partial coverage also carries a correctness trap: an 'unsafe' object touched from instrumented code is reported as an error because its PSM bytes stay poisoned (S4.3 p513 l55-59 right column).
**Instances.** This paper (Fig 7, hashmap only).; No other index or product tested. Method.
**Evidence strength.** Measured (Fig 7; artifact figure_7.sh), single index, no numbers given in text, no variance.

### safepm:I8  ·  scope=law  ·  G2(antecedent/consequent)  ·  stated_by_paper=yes  ·  cea=2
**Claim.** If a sanitizer rebuilds its metadata from scratch at every process start, then accesses on the recovery path to objects allocated in earlier runs are unchecked; only metadata that persists with the data — and is itself updated crash-consistently — can check them.
**Denies / refutes.** Default belief: a heap sanitizer covers 'the heap' wherever it is accessed. Paper: 'Unlike with ASan, where the memory safety metadata is volatile and reconstructed from scratch on each application run, SafePM's memory safety metadata remains consistent and can be retrieved across reboots or failures' (S3.2 p510 l38-46). Registry: AddressSanitizer, memcheck, persistent shadow memory.
```
count(detections, AddressSanitizer) = 0 | path=recovery, objects=allocated-in-prior-run [argued, not measured] ⊢ S3.2 @ p510 l39-46
count(errors, pmemcheck) = 0 | workload∈{persistent indices, PM operations benchmark}, ASan=disabled, ops=10000 ⊢ S6.5 @ p518 l27-31 (right column)
count(vs(memcheck|SafePM w/o ASan, memcheck|Native)) = 0 errors | workload=PM operations benchmark ⊢ S6.5 @ p518 l31-35 (right column)
crash-consistency(persistent shadow memory) = validated [vague] | tool∈{pmemcheck,memcheck} ⊢ A.4.1 @ p523 l54-57
```
**Warrant.** Definitional: a checker can only consult metadata it holds; after restart a volatile sanitizer holds none for pre-existing objects until they are re-registered, and PMDK recovery touches them before any application allocation. The pmemcheck/memcheck runs show the persistent metadata survives crashes consistently, which is the precondition for the 'only' half.
**Rebuttal.** A volatile sanitizer could rebuild metadata by walking the persistent heap at open (re-poisoning from allocator metadata); the paper neither measures nor rules out that alternative, and it would cost a full-heap scan per open. The paper also never runs an experiment that plants a violation on the recovery path (only artifact tests do).
**Instances.** ASan [80]: volatile shadow, reconstructed per run (p510 l39-41).; memcheck [16,21]: 'no persistent memory overhead' i.e. volatile metadata (S7 p519 l35-36).; SafePM: the persistent contrast instance (this paper).
**Evidence strength.** Argument plus crash-consistency validation runs; no measurement of recovery-path detection in the paper; level 2.

### safepm:I9  ·  scope=method  ·  G6(enthymeme)+G2(contraries)  ·  stated_by_paper=yes  ·  cea=1
**Claim.** 'Transparent' here means source-transparent, not data-transparent: because the sanitizer replaces the pool's root object with its own shadow root, pools are not interchangeable between sanitized and unsanitized builds, so adopting or dropping the sanitizer forces pool recreation — which, whatever its runtime overhead, makes it a development-phase tool.
**Denies / refutes.** Unstated premise behind 'no source code modifications' (S1 p507 l26-27): that a drop-in library is drop-in for existing data. Internal tension: 'a persistent pool created by SafePM is a valid PMDK pool' (S1 p507 l50-51) versus 'Persistent pools created by PMDK are not compatible with SafePM, and vice versa' (S4.3 p514 l27-28). Registry: PM pool; new entry 'shadow root object'.
```
count(source modifications, SafePM) = 0 ⊢ S1 @ p507 l26-27
compatibility(vs(PM pool|SafePM, PM pool|PMDK)) = no [vague] ⊢ S4.3 @ p514 l27-28
count(detections, SafePM w/o ASan) = 0 | ASan=disabled ⊢ S4.3 @ p514 l32-33
size(PM pool) ≤ 128 GB | cause=PSM≤16GB ⊢ S4.3 @ p514 l11-21
```
**Warrant.** The shadow root object is 'from libpmemobj's perspective ... the root object of the PM pool' and the wrappers hide its extra fields (S4.1 p511 l39-48 right column); an unwrapped PMDK sees the wrong root, and a SafePM open of an unwrapped pool finds no PSM and treats creation as torn (S4.2 p512 l42-45). The paper's own usage model is 'during the development phase' (S3 p509 l22-25 right column).
**Rebuttal.** Data-transparency could be bought by storing the shadow in a side file keyed by pool id; the paper chooses in-pool placement so that the same PMDK transactions give crash consistency for free (S3.2 p510 l27-32). The trade is therefore: free crash consistency ⇔ pool incompatibility.
**Instances.** This paper only.
**Evidence strength.** Stated design facts; no measurement; level 1.

### safepm:I10  ·  scope=method  ·  G4(dissoi logoi)  ·  stated_by_paper=yes  ·  cea=3
**Claim.** pmem-valgrind memcheck's standing as the state of the art for persistent-memory violation detection does not survive on effectiveness: a compile-time shadow sanitizer with persistent metadata stops 35 more RIPE attacks (62→27 always-successful) — though memcheck keeps two advantages the paper concedes and never measures against.
**Denies / refutes.** Prior claim refuted: memcheck [21] as 'the current state-of-the-art for detecting memory violations in persistent memory' (S6.4 p518 l36-38); also the paper's unquantified 'prohibitive ... overheads (e.g., memcheck [16])' (S3 p508 l36-38). Registry: memcheck, Valgrind.
```
safety(memcheck) = 62 attacks | outcome=always, workload=RIPE ⊢ T5 @ p518 l14
safety(vs(SafePM, memcheck)) = -35 attacks | outcome=always ⊢ T5 @ p518 l13-14
safety(vs(SafePM, memcheck)) = +1 attacks | outcome=sometimes ⊢ T5 @ p518 l13-14
slowdown(memcheck) = much-larger [vague, never measured] ⊢ S7 @ p519 l36-37
space-overhead(memcheck) = 0% [stated] ⊢ S7 @ p519 l35-36
count(compiler support, memcheck) = 0 [binary-only] ⊢ S7 @ p519 l34-35
```
**Warrant.** Same RIPE suite, same gcc, memcheck run as variant (v) alongside SafePM; SafePM's 27 equals ASan's volatile-heap figure while memcheck leaves 62 always-exploitable. The paper adds that memcheck's spatial detection 'is not as precise' (p519 l37-38).
**Rebuttal.** The refutation is on effectiveness only. memcheck needs no compiler support and no persistent space (12.5% for SafePM); the paper never measures memcheck's runtime overhead, so the 'prohibitive' half of the prior belief is not tested. memcheck also scores 0 on 'sometimes' vs SafePM's 1.
**Instances.** This paper (Table 5).
**Evidence strength.** Measured RIPE counts (3 repeats); overhead comparison absent; level 3 for the effectiveness atoms only.

### safepm:I11  ·  scope=method  ·  G3(seems-necessary-is-unnecessary)+G4  ·  stated_by_paper=implied  ·  cea=3
**Claim.** Neither a memory-safe language nor hardware protection keys are necessary to obtain PM heap memory safety and allocator-metadata protection at test time: a persistent shadow array initialised to 'inaccessible' gives both, without changing the allocator — but only as dynamic detection on executed paths with red-zone granularity, never as a static guarantee.
**Denies / refutes.** Refutes the premise of Corundum [44] that PM memory safety requires a Rust library with static enforcement (S3 p508 l38-39; S7 p519 l24-31), and of Poseidon [31] that persistent-metadata protection requires Intel MPK and allocator changes (S4.3 p513 l38-41 right column; S7 p519 l6-9). New registry entries: Corundum, Poseidon.
```
safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | outcome=always, workload=RIPE ⊢ T5 @ p518 l11-13
count(modifications, PMDK) = 0 | purpose=metadata-protection, mechanism=PSM-initialised-inaccessible ⊢ S4.3 @ p513 l34-41 (right column)
count(source modifications, SafePM) = 0 ⊢ S1 @ p507 l26-27
count(bugs, PMDK) = 2 | found-by=SafePM, classes={off-by-one overflow, invalid free} ⊢ S6.7 @ p518 l70-82 (right column)
count(detections, SafePM) = 0 | class=intra-object-overflow ⊢ S4.3 @ p514 l5-8 ∧ count(detections, SafePM) = 0 | class=OOB-landing-inside-other-object ⊢ S4.3 @ p514 l9-10
size(persistent red zones) = 16 B | detects overflow ≤ 16 B only ⊢ S4.1 @ p511 l36-38 (right column)
```
**Warrant.** PMDK's heap-metadata region is never allocated through the API, so its shadow bytes stay poisoned and any application access is caught (p513 l34-41); RIPE equivalence with ASan on the volatile heap shows the dynamic protection is as good as the field's default tool. Two real PMDK bugs found without any Rust or MPK.
**Rebuttal.** Explicit limits: intra-object overflows and out-of-bounds accesses that land in another object are missed (p514 l4-10), temporal safety is probabilistic (p513 l10-11), and detection only covers executed paths — Corundum's static guarantee excludes these entirely. Metadata protection also relies on the PSM staying poisoned rather than on mprotect (p514 l57-59 right column), so uninstrumented code (PMDK internals) can still corrupt it.
**Instances.** This paper.; Contrast systems: Corundum [44] (static, Rust), Poseidon [31] (MPK) — cited by the paper, not measured against.
**Evidence strength.** Measured RIPE equivalence and two found bugs; metadata-protection claim is argued, not tested; level 3.

### safepm:I12  ·  scope=product  ·  G6(enthymeme)+G2(division)  ·  stated_by_paper=implied  ·  cea=3
**Claim.** Persistent metadata moves the sanitizer's setup cost from every process start (ASan rebuilds its shadow each run) to once per pool lifetime — a create cost of seconds that grows with pool size because 1/8 of the pool is written as poisoned shadow — plus a fixed ~20 ms per open; 'one-time, hence irrelevant' presumes pools outlive their creation by orders of magnitude, which is false for test suites that create a fresh pool per test.
**Denies / refutes.** Unstated premise behind 'pool creation is an one-time operation, hence, the high overhead is largely irrelevant to application performance' (S6.2 p517 l40-42). Registry: PM pool create/open, persistent shadow memory.
```
latency(SafePM) ≈ 30 ms | workload=PM pool create/open, op=open, pool-size∈[256MB,128GB] ⊢ F6 @ p517 l30-32
latency(Native) ≈ 10 ms | workload=PM pool create/open, op=open ⊢ F6 @ p517 l31-32
slowdown(SafePM) ≤ 3× | workload=PM pool create/open, op=open ⊢ F6 @ p517 l32
latency(SafePM) increasing-in pool-size | workload=PM pool create/open, op=create, pool-size∈[256MB,128GB] ⊢ F6 @ p517 l35-38
latency(SafePM) = a-few-seconds [vague] | op=create ⊢ S6.2 @ p517 l37-38
size(persistent shadow memory written at create) = 1/8 × pool | initialised=inaccessible ⊢ S4.1 @ p511 l54-57
```
**Warrant.** The paper names the create mechanism ('overmap and initialize the PSM object, which grows with the size of the pool', p517 l38-40) and the open mechanism (ASan checks, since w/o-ASan is close to native, p517 l32-35). At 128 GB the create step must write 16 GB of shadow.
**Rebuttal.** From the paper: create is one-time for a long-lived pool. From the artifact: its own tests (A.5 p524 l50-56) and pmembench runs create pools per experiment, so the create cost is paid per test and dominates short tests; at 128 GB it is tens of seconds.
**Instances.** This paper (Fig 6).
**Evidence strength.** Measured, ≥3 runs, no variance; create times given only as 'a few seconds' in text (Fig 6 log scale); the 10 ms vs 15 ms native-open discrepancy between Fig 6 and Table 3 is unexplained.

### safepm:I13  ·  scope=product  ·  G2(cause)  ·  stated_by_paper=implied  ·  cea=3
**Claim.** The sanitizer's recovery-time penalty is a fixed per-open cost, not a per-log-entry cost: undo-log replay runs inside uninstrumented library code, so the ASan term stays ≈10 ms across a 1000× range of log sizes while native recovery itself grows with the log.
**Denies / refutes.** Default belief: instrumentation slows recovery in proportion to the work recovered. The paper says only 'approximately 10 ms' (S6.2 p517 l61-63); the constancy across log sizes and its cause are reader-derived from Table 3 and §5. Registry: recovery process.
```
latency(Native) = 15.00 ms | workload=recovery process, log=4KB, repeats=100 ⊢ T3 @ p517 l30 ∧ latency(Native) = 19.13 ms | log=4MB ⊢ T3 @ p517 l30
latency(vs(ASan, Native)) = +10.23 ms | workload=recovery process, log=4KB ⊢ T3 @ p517 l30-32
latency(vs(ASan, Native)) = +10.32 ms | workload=recovery process, log=4MB ⊢ T3 @ p517 l30-32
latency(vs(SafePM, ASan)) ∈ [-0.01,+0.34] ms | workload=recovery process, log∈[4KB,4MB] ⊢ T3 @ p517 l32-33
count(functions compiled with ASan, PMDK) = 0 | reason=internals manipulate PSM ⊢ S5 @ p515 l34-38
```
**Warrant.** An additive constant (10.23-10.34 ms) over log sizes 4 KB-4 MB while the native time rises 15.00→19.13 ms means the instrumented part does no per-entry work; PMDK internals, which perform the replay, are deliberately not compiled with ASan (p515 l34-38), so the 10 ms is the instrumented application's open/overmap path.
**Rebuttal.** If PMDK internals were instrumented, or if the application's own recovery code touched every recovered object, the penalty would scale with log size. The fixed term also disagrees between experiments (≈10 ms here vs ≈20 ms in Fig 6 pool open), which the paper does not reconcile.
**Instances.** This paper (Table 3).
**Evidence strength.** Measured, 100 repetitions per cell, no variance; the recovery microbenchmark is not in the artifact (per the bob reader's inspection); level 3 on the paper's numbers.

### safepm:I14  ·  scope=method  ·  G1(definition)  ·  stated_by_paper=yes  ·  cea=1
**Claim.** 'Persistent-memory memory safety' is not a new class of bug but the familiar spatial and temporal classes under three changed conditions — a fat persistent-pointer representation, a separate persistent allocator with persistent heap metadata, and a recovery code path — so a PM sanitizer is an allocator-integration and metadata-lifetime problem, not a new-checker problem.
**Denies / refutes.** Default belief: PM needs a new kind of memory-safety checker. Paper: 'although the types of memory safety vulnerabilities on PM remain the same as those on volatile memory ... memory safety needs to be further ensured for the recovery code paths' (S1 p507 l13-18); 'the state-of-the-art memory safety approaches for volatile memory are insufficient for PM. Unlike volatile memory ... persistent pointer representation ... allocators designed for PM' (S1 p507 l7-13).
```
count(new vulnerability classes, PM) = 0 ⊢ S1 @ p507 l13-15
count(bugs, PMDK) = 2 | found-by=SafePM, classes∈{spatial: off-by-one overflow, temporal: invalid free} ⊢ S6.7 @ p518 l70-82 (right column)
count(modifications, AddressSanitizer) = 0 | component∈{compiler-pass, runtime-library} ⊢ S5 @ p515 l28-29 ∧ S4.1 @ p511 l40-42
```
**Warrant.** The two real bugs found are a classic overflow and a classic invalid free; everything SafePM adds (Table 1 wrappers, PSM placement, shadow root) touches the allocator and metadata lifetime, and nothing touches the checker. That is the definitional content of the paper's claim.
**Rebuttal.** If PM introduced violations with no volatile analogue (e.g. stale persistent pointer after pool relocation, torn-write partial objects), a new checker class would be needed; the paper does not look for such classes and its RIPE suite is volatile-heap-derived.
**Instances.** This paper.; Same diagnosis is the premise of memcheck-for-PM [21] (allocator hooks added to an existing checker, p519 l32-33).
**Evidence strength.** Argument, supported by the composition of the design and two found bugs; level 1.

### safepm:I15  ·  scope=method  ·  G6(enthymeme)  ·  stated_by_paper=yes  ·  cea=1
**Claim.** Without an explicit quarantine, a shadow-memory sanitizer's temporal-safety detection is exactly as strong as the allocator's reuse delay: use-after-free and double-free are caught only if they occur before the region is handed out again, so the temporal guarantee rests on an allocator policy the paper observes ('based on our experience') but never measures.
**Denies / refutes.** Field default (ASan's own design) that a quarantine is needed for temporal safety (S2.3 p508 l52-55); the paper drops it and states the unmeasured premise: 'SafePM has no explicit quarantine ... but based on our experience, libpmemobj delays reallocating a deallocated region of PM' (S4.2 p513 l8-10). Registry: SafePM, PMDK.
```
count(quarantine, SafePM) = 0 ⊢ S4.2 @ p513 l8-9
safety(SafePM) = probabilistic [vague] | class=temporal ⊢ S4.3 @ p513 l10-11 (right column)
count(detections, SafePM) ∃ | class∈{use-after-free, double-free}, cond=before-region-reallocated ⊢ S4.3 @ p513 l17-21 (right column)
count(detections, SafePM) = 0 | class=double-free, cond=within-single-transaction, mechanism=libpmemobj-deferred-free (explicit wrapper check added) ⊢ S4.2 @ p513 l4-8
```
**Warrant.** The paper's own text bounds the detection window by 'before the PM region is allocated again' (p513 l20-21); Table 5 does not separate temporal from spatial attacks, and no experiment varies allocation pressure after a free.
**Rebuttal.** An allocator with immediate (LIFO free-list) reuse defeats detection entirely; under allocation pressure PMDK's delay shrinks. Conversely, a persistent quarantine could be added at the cost of persistent space and of a quarantine that itself must be crash-consistent.
**Instances.** This paper.; ASan [80]: the contrast instance that keeps a quarantine (p508 l52-55).
**Evidence strength.** No measurement of temporal detection rate or of PMDK's reuse delay; level 1.

### vmsh:I1  ·  scope=method  ·  G3(seems-necessary-is-unnecessary)  ·  stated_by_paper=yes  ·  cea=2
**Claim.** Run-time attachment of tools to a running VM needs neither a cooperative in-guest agent nor a hypervisor-specific API; access to the hypervisor process's KVM file descriptors (via debugger-style syscall injection) is sufficient to side-load kernel code and VirtIO devices into any KVM guest that uses the MMIO transport.
**Denies / refutes.** 'Side-loading code into the guest VM would traditionally require a cooperative guest agent running inside the VM or hypervisor-specific APIs.' (S3.3 @ p682 l44-46); cloud-agent ecosystem [8,40,41,82]; debugger/hot-plug route via QEMU (p682 l48-50).
```
support(VMSH) = yes | hypervisor∈{QEMU,kvmtool,Firecracker,crosvm}, guest-agent=none, hypervisor-api=none ⊢ T1 @ p686 l4
count(VMSH) = 4 of 5 hypervisors ⊢ S6.2 @ p686 l58
support(VMSH) = yes | guest-kernel∈{v5.10,v5.4,v4.19,v4.14,v4.9,v4.4}, hypervisor=QEMU ⊢ T1 @ p686 l6
support(VMSH) = no | hypervisor=Cloud Hypervisor, transport=PCIe-MSI-X ⊢ S6.2 @ p686 l59-61
support(VMSH) = yes | hypervisor=Firecracker, seccomp=disabled ⊢ S6.2 @ p687 l8-10
```
**Warrant.** Four hypervisors with no shared management API (Firecracker and kvmtool expose none, p682 l50-51) all accept the same attach path, so the path cannot depend on hypervisor-specific APIs; the guest side is a side-loaded library, not an agent (p683 l36-37).
**Rebuttal.** Fails when the hypervisor does not use MMIO as VirtIO transport (Cloud Hypervisor, PCIe MSI-X), sandboxes its own syscalls (Firecracker seccomp must be disabled), or the guest is memory-encrypted/attested (AMD SEV, p683 l10-12). Linux/x86_64/KVM only (p686 l40-49).
**Instances.** this paper: QEMU, kvmtool, Firecracker, crosvm (T1); prior kernel-module injection [88,103,124]: guest-side half is old, hypervisor-agnostic host side is new (p690 l9)
**Evidence strength.** moderate: pass/fail attach tests (E2/E3); hypervisor versions not in paper; 3 of 6 Table-1 kernels lack a shipped test (bob C4)

### vmsh:I2  ·  scope=law  ·  G2(cause)+G8  ·  stated_by_paper=yes  ·  cea=3
**Claim.** Interposing on a guest's VM exits from outside the hypervisor with a debugger (ptrace on KVM_RUN) charges every exit, not only the ones addressed to the interposer, so the guest's own devices slow down in proportion to their exit rate: small-block IO (IOPS) suffers several times more than large-block IO (bandwidth); an in-kernel filter that forwards only the interposer's MMIO range removes the charge.
**Denies / refutes.** none stated; cause named by the paper (p688 l69-70; p688 l42-46 right col.), rebuttal = ioregionfd.
```
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 1.5× | mode=wrap_syscall, metric=throughput, op=read, workload=fio, bs=256KiB ⊢ F6 @ p688 l68-69
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 6× | mode=wrap_syscall, metric=iops, workload=fio, bs=4KiB ⊢ F6 @ p688 l69
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 1× | mode=ioregionfd, metric∈{throughput,iops} ⊢ F6 @ p688 l63-66
iops(vs(native, qemu-blk)) ≥ 2× | workload=fio, bs=4KiB, io=direct ⊢ F6 @ p688 l63-65 (right col.)
throughput(vs(native, qemu-blk)) ≈ 1× | workload=fio, bs=256KiB, io=direct ⊢ F6 @ p688 l62-63 (right col.)
```
**Warrant.** Same device, same workload, only the trap mechanism differs: 6× (IOPS) vs 1.5× (bandwidth) under wrap_syscall collapses to 1× under ioregionfd; paper's cause: 'overhead added to every system call performed by QEMU [...] For every VMEXIT triggered by an MMIO access, VMSH has to check if it is related to a vmsh-blk device'. Native-vs-virtualised shows the same shape one layer down: per-operation interposition costs IOPS ≥2× while bandwidth is preserved.
**Rebuttal.** Holds for exit-heavy IO; a CPU-bound or page-cache-resident guest workload that rarely exits would show little wrap_syscall penalty (not measured). The in-kernel filter is an out-of-tree patch (p686 l4-5).
**Instances.** VMSH wrap_syscall vs ioregionfd on qemu-blk (this paper, F6 dagger rows); KVM/QEMU vs native on the same fio (this paper, F6; second product); virtio rationale cited by the paper [99,118]: 'Emulating physical hardware is slow and causes significant overheads' (p679 l46-48 right col.)
**Evidence strength.** moderate: two products, one machine, one workload family (fio libaio), no repetition count, error bars only

### vmsh:I3  ·  scope=product  ·  G3(seems-general-is-local)+G8  ·  stated_by_paper=implied  ·  cea=3
**Claim.** VMSH's headline 'no overhead for the applications running in the VM' is local to the ioregionfd build, which needs an out-of-tree kernel patch the appendix calls optional; on a stock kernel (wrap_syscall) the guest's own block device loses 1.5× read bandwidth and 6× IOPS while VMSH is attached.
**Denies / refutes.** the paper's own unconditional statements: Abs @ p678 l26-27; S1 @ p679 l52-54; A.4.1 @ p695 l53-55 (right col.) 'The VM and devices not connected to VMSH experience no slowdown'.
```
overhead(vs(qemu-blk+VMSH, qemu-blk)) = "no overhead" | mode=unstated ⊢ Abs @ p678 l26-27 [vague]
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 1× | mode=ioregionfd ⊢ F6 @ p688 l63-66
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 1.5× | mode=wrap_syscall, metric=throughput, op=read ⊢ F6 @ p688 l68-69
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 6× | mode=wrap_syscall, metric=iops ⊢ F6 @ p688 l69
support(ioregionfd) = "under review for inclusion into the Linux kernel" ⊢ S5 @ p686 l4-5 [vague]
support(ioregionfd) = "optionally" ⊢ A.2.3 @ p695 l55-56 [vague]
```
**Warrant.** The only zero-overhead row in F6 is the ioregionfd row; the paper: 'The overheads of the ptrace implementation violate the goal of non-invasiveness' (p688 l47-48 right col.). Policy (G1): attach to production guests only with ioregionfd; wrap_syscall is a development mode.
**Rebuttal.** If ioregionfd is merged upstream the locality disappears; if the guest workload is not IO-exit-bound the wrap_syscall penalty may be small (unmeasured).
**Instances.** this paper only (product claim)
**Evidence strength.** moderate: disaggregated numbers are in the paper; headline omits the condition (claim-map M-1)

### vmsh:I4  ·  scope=method  ·  G2(cause)  ·  stated_by_paper=yes  ·  cea=3
**Claim.** When a VirtIO device is served from a process other than the hypervisor, its per-request cost is context switches between the guest driver, the hypervisor process and the device process, not data copying: with copy time held equal the device does about twice the context switches and runs at about half the speed of the in-hypervisor device, and moving the copy into the host kernel (process_vm_readv/writev) recovers a factor of two.
**Denies / refutes.** implicit expectation that the cross-process copy is the cost; paper: 'the time spent copying data [...] is identical for qemu-blk and vmsh-blk, thus leaving the number of context switches as the main reason' (p689 l33-36).
```
count(vs(vmsh-blk, qemu-blk)) = 2× | metric=context-switches, workload=fio, io=direct ⊢ S6.3C @ p689 l36-38
time(vs(vmsh-blk, qemu-blk)) = same-as(qemu-blk) | phase=guest-to-host-page-cache-copy ⊢ S6.3C @ p689 l33-34 [vague]
slowdown(vs(vmsh-blk, qemu-blk)) = 2× | metric∈{throughput,iops}, mode∈{wrap_syscall,ioregionfd}, io=direct ⊢ F6 @ p688 l68-70 (right col.)
speedup(vmsh-blk) = 2× | change=in-kernel-copy(process_vm_readv/writev), workload=Phoronix Test Suite ⊢ S5 @ p685 l44-48 (right col.)
overhead(vs(vmsh-blk, qemu-blk)) = 14% | metric=iops ⊢ S6.3C @ p689 l51 [conflicts with the 2× atom; paper-internal]
```
**Warrant.** Two direct measurements (copy time equal; context-switch count 2×) plus the 2× performance gap; the implementation change that removed one user-space copy doubled Phoronix performance, as a per-request-boundary-cost model predicts.
**Rebuttal.** Two magnitudes for the same gap ('halved' p688 l68-70 vs 7% read / 14% IOPS p689 l48-51) without saying which implementation each refers to; context-switch count reported once, without method. Fails for page-cache-resident workloads where no request crosses the boundary (I5).
**Instances.** this paper: vmsh-blk vs qemu-blk; vhost-user devices [78,108,128]: same architecture with hypervisor cooperation, not measured here
**Evidence strength.** moderate: mechanism backed by a direct count; magnitudes inconsistent in the text

### vmsh:I5  ·  scope=law  ·  G2(antecedent)+G8  ·  stated_by_paper=implied  ·  cea=3
**Claim.** If a block-level interposer adds a fixed per-request cost, then a workload's slowdown falls with the fraction of its IO served from the guest page cache: direct-IO workloads see the full cost (up to 3.7×), metadata- and read-heavy cached workloads see little or none; and a cache on the path with zero reuse (sequential, never re-read) only adds cost.
**Denies / refutes.** none stated; the paper states the mechanism per benchmark (p687 l50-59, l62-66) but not the law.
```
slowdown(vs(vmsh-blk, qemu-blk)) = 3.7× | workload=fio-2MB, io=direct ⊢ F5 @ p687 l48-50 (right col.)
slowdown(vs(vmsh-blk, qemu-blk)) = 1.5× ±0.6 | workload=Phoronix Test Suite, statistic=mean ⊢ F5 @ p687 l47-48 (right col.)
slowdown(vs(vmsh-blk, qemu-blk)) = "less or no overhead" | workload∈{Compile Bench,PostMark,FS-Mark,DBENCH,Sqlite}, io=page-cache ⊢ F5 @ p687 l53-59 (right col.) [vague]
page-cache-hit-rate(IOR) = 20% ⊢ S6.3A @ p687 l62-64 (right col.)
slowdown(vs(vmsh-blk, qemu-blk)) decreasing-in page-cache-hit-rate | workload=IOR ⊢ S6.3A @ p687 l64-66 (right col.)
throughput(vs(qemu-9p, qemu-blk)) < 1× | op=read, io=file, workload=fio-sequential-no-reuse ⊢ F6 @ p689 l39-43 [vague: 'significantly drops']
```
**Warrant.** Within one suite the only direct-IO benchmark is the worst case and the cached ones the best; IOR with a stated 20% hit rate sits between; the 9p file-IO case shows the contrary condition (cache present, reuse absent) as a pure loss. Same relation across two products (vmsh-blk, qemu-9p).
**Rebuttal.** The ±0.6 is undefined and no per-benchmark hit rates except IOR are given; Sqlite is called 'unexpectedly' not write-heavy, i.e. the classification is post hoc.
**Instances.** vmsh-blk over Phoronix (this paper, F5); qemu-9p file IO under fio (this paper, F6 double-dagger rows); 9p stacked-cache explanation (p689 l44-47)
**Evidence strength.** moderate: consistent direction across benchmarks; hit rates mostly unmeasured

### vmsh:I6  ·  scope=method  ·  G3(seems-expensive-is-cheap)  ·  stated_by_paper=yes  ·  cea=2
**Claim.** Side-loading against Linux's unstable internal kernel API is cheap to keep portable if the loaded code touches a minimal function set: with about a dozen kernel functions, five years of LTS kernels cost one person-week, only 2 of 10 functions and 2 of 4 structures need version variants, and the two symbol-layout changes are absorbed by trying all layouts with consistency checks.
**Denies / refutes.** 'there is no stable internal kernel API or ABI' hence 'it is not trivial to build a side-loadable library that would work for all kernel versions' (S3.3 @ p682 l68-71; S6.2 @ p687 l21-25).
```
time-to-port(VMSH) = 1 person-week | kernels=v4.4..v5.10, span=5 years ⊢ S6.2 @ p687 l55-56
count(VMSH) = 12 kernel functions | split=2 driver-registration + 4 file-IO + 5 process/threads ⊢ S5 @ p686 l19-21
count(VMSH) = 2 of 10 kernel functions needing variants | functions={kernel_read,kernel_write} ⊢ S6.2 @ p687 l49-51
count(VMSH) = 2 of 4 kernel structures conditioned on version ⊢ S6.2 @ p687 l53-55
count(VMSH) = 2 symbol-layout changes | span=5 years ⊢ S6.2 @ p687 l44-46
support(VMSH) = yes | guest-kernel∈{v5.10,v5.4,v4.19,v4.14,v4.9,v4.4} ⊢ T1 @ p686 l6
```
**Warrant.** Churn is bounded by the API surface; the paper's counts show a small surface and a small churn, and the effort number is the cost of covering that churn.
**Rebuttal.** Function count stated as 12 (p686 l19) and 10 (p687 l49); structures are 'more brittle'; forward compatibility is extrapolated (p687 l56-58) not measured; half the Table-1 kernels have no shipped test (bob C4).
**Instances.** this paper; kernel-module-injection introspection systems [88,103,124] as prior class, no effort numbers reported
**Evidence strength.** weak-to-moderate: a single self-reported effort figure

### vmsh:I7  ·  scope=method  ·  G6(enthymeme)+G8  ·  stated_by_paper=implied  ·  cea=3
**Claim.** For an on-demand tooling overlay, acceptability of performance is two-tiered: the guest's own devices must show zero slowdown, while the overlay's device need only be 'usable'; the paper's judgment that a 1.5× tool slowdown is acceptable rests on the unstated premise that attached-tool workloads are interactive/developer workloads insensitive to a ~2× IOPS loss, a premise in tension with its own statement that IOPS is the metric that matters most for attached devices.
**Denies / refutes.** implicit premise surfaced. Design goal S3.1 @ p681 l48-51 ('Performance of the attached tools and services is secondary, but they need to be usable'); judgment S6.3A @ p687 l67-74 ('should not impact developers' productivity significantly').
```
slowdown(vs(vmsh-blk, qemu-blk)) = 1.5× ±0.6 | workload=Phoronix Test Suite ⊢ F5 @ p687 l47-48 (right col.)
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 1× | mode=ioregionfd ⊢ F6 @ p688 l63-66
slowdown(vs(vmsh-blk, qemu-blk)) = 2× | metric=iops, io=direct ⊢ F6 @ p688 l68-70 (right col.)
overhead(vs(vmsh-blk, qemu-blk)) = 14% | metric=iops ⊢ S6.3C @ p689 l51
quality(vmsh-blk) = "acceptable" | judge=authors ⊢ S6.3A @ p687 l67-68 [vague]
quality(iops) = "the most important metric for VMSH because attached devices would be more prone to small sized IOs" ⊢ S6.3C @ p689 l52-54 [vague]
```
**Warrant.** The acceptability judgment is licensed only if tier 1 holds (it does, under ioregionfd) and tier-2 users tolerate the measured loss; the paper asserts the tolerance without a user or task measurement.
**Rebuttal.** An attached tool that is IOPS-bound (security scanner walking many small files, use-case #3) pays the loss the paper itself calls most important; if the 2× figure (p688) rather than 14% (p689) is right, 'usable' rests on the weaker number.
**Instances.** this paper; console tier uses the same two-tier logic with a perception threshold (I13)
**Evidence strength.** moderate for the tier-1 atom, weak for the tolerance premise (no user/task data)

### vmsh:I8  ·  scope=method  ·  G4  ·  stated_by_paper=yes  ·  cea=2
**Claim.** Password/rescue recovery for a locked-out VM does not require a pre-installed guest agent, a reboot into a recovery system, or a recovery VM with disk access; a rescue image containing chpasswd can be attached to the VM while it keeps running.
**Denies / refutes.** rescue-via-reboot: 'Existing implementations of such services require rebooting into a recovery system [26, 47]' (S2.3 @ p680 l12-15); 'this usually requires a user-installed agent in the VM image, a reboot to access the file system directly, or booting a recovery virtual machine' (S6.5-2 @ p690 l23-26). Registry: rescue-via-reboot (DigitalOcean Recovery Console [26], Hetzner Rescue System [47]).
```
support(VM rescue system) = yes | agent=none, reboot=no, vm-state=running, tool=chpasswd ⊢ S6.5-2 @ p690 l27-29
support(rescue-via-reboot) = yes | reboot=required ⊢ S2.3 @ p680 l14-15 [prior, qualitative]
```
**Warrant.** E9 is a unit test that resets a guest password through VMSH (p696 l15-17); one working agent-less, reboot-less path refutes 'requires'.
**Rebuttal.** Only for KVM/Linux/x86_64 guests the attach path supports (I1); provider must run VMSH with elevated host privileges (p684 l48-52 right col.); the guest's own state is not measured during the rescue.
**Instances.** this paper (use-case #2); dual-VM inspection shells [36,88] reach guest state without reboot but through a separate VM (p690 l12-17 right col.)
**Evidence strength.** weak-to-moderate: one pass/fail unit test, no numbers in the paper

### vmsh:I9  ·  scope=method  ·  G4+G3(seems-necessary-is-unnecessary)  ·  stated_by_paper=yes  ·  cea=3
**Claim.** Serving a VirtIO device from a process outside the hypervisor does not require hypervisor modification (the vhost-user premise): a trap on the hypervisor's KVM_RUN (ptrace) or an in-kernel MMIO forward (ioregionfd) plus cross-process memory access suffices to run a block device that passes the same conformance suite as the hypervisor's own device, at roughly half its direct-IO performance.
**Denies / refutes.** vhost-user [78,108,128]: 'While vhost still requires modifications on the hypervisor side, VMSH does not and operates non-cooperatively' (S7 @ p690 l49-50 right col.); 'the devices have to run outside the hypervisor process, without its cooperation' (S3.3 @ p682 l29-30 right col.).
```
support(vmsh-blk) = yes | hypervisor∈{QEMU,kvmtool,Firecracker,crosvm}, hypervisor-modified=no ⊢ T1 @ p686 l4
correctness(vs(vmsh-blk, qemu-blk)) = same-as(qemu-blk) | workload=xfstests, group=quick, tests=619, failures=3 ⊢ S6.1 @ p686 l40-48 (right col.)
slowdown(vs(vmsh-blk, qemu-blk)) = 2× | io=direct, metric∈{throughput,iops} ⊢ F6 @ p688 l68-70 (right col.)
count(vs(vmsh-blk, qemu-blk)) = 2× | metric=context-switches ⊢ S6.3C @ p689 l36-38
```
**Warrant.** Four unmodified hypervisors, the explicit xfstests criterion ('failure [...] as vmsh-blk failing any test that succeeds on native or qemu-blk', p686 l35-36 right col.), and a measured cost; the cost (I4) is the price of non-cooperation.
**Rebuttal.** Non-cooperation is conditional: Firecracker's seccomp filter must be disabled (p687 l8-10), PCI-transport hypervisors are unsupported (p686 l59-61 right col.), and the zero-guest-overhead variant needs the ioregionfd kernel patch (I3).
**Instances.** this paper: vmsh-blk on 4 hypervisors; vhost-user / SPDK vhost [78,108,128] as the cooperative counterpart
**Evidence strength.** moderate: conformance criterion explicit; failing tests unnamed and retry policy undisclosed (bob C5)

### vmsh:I10  ·  scope=method  ·  G2(division)+G7(agency)  ·  stated_by_paper=yes  ·  cea=3
**Claim.** The MMIO-trap mechanism decides the overhead the guest's own devices pay, but not the overhead the attached device pays: switching from ptrace to ioregionfd takes the guest's block device from 6× IOPS loss to none, while the attached device stays at about half the hypervisor device's speed under both; the two costs have different causes (exit interposition vs cross-process request handling) and must be engineered separately.
**Denies / refutes.** seems-coupled-is-independent: a faster trap path might be expected to speed up the device it serves; paper: 'indifferent to the used implementation' (S6.3C @ p688 l68-70 right col.).
```
slowdown(vs(vmsh-blk, qemu-blk)) = 2× | mode=wrap_syscall, io=direct ⊢ F6 @ p688 l68-70 (right col.)
slowdown(vs(vmsh-blk, qemu-blk)) = 2× | mode=ioregionfd, io=direct ⊢ F6 @ p688 l68-70 (right col.)
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 6× | mode=wrap_syscall, metric=iops ⊢ F6 @ p688 l69
slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 1× | mode=ioregionfd ⊢ F6 @ p688 l63-66
```
**Warrant.** A 2x2 of {device: guest's, attached} x {mode: wrap_syscall, ioregionfd} in F6: mode moves one row pair and not the other.
**Rebuttal.** If the attached device's cost were dominated by MMIO notifications rather than virtqueue data handling the independence would break; the paper does not vary notification frequency.
**Instances.** this paper only (two modes x two devices)
**Evidence strength.** moderate: one figure, directions clear, magnitudes textually inconsistent (I4)

### vmsh:I11  ·  scope=method  ·  G5(kairos)+G2(antecedent)  ·  stated_by_paper=implied  ·  cea=1
**Claim.** The value of a below-hypervisor (KVM-level) attach mechanism rises as hypervisors shed management APIs: the microVM generation removed the debugger and hot-plug interfaces QEMU had, and orchestration layers hide the ones that remain, so run-time extension of a VM now has to go beneath the hypervisor to be portable.
**Denies / refutes.** none; kairos. Evidence S3.3 @ p682 l48-54 (API inventory; 'obscured by orchestration frameworks, such as OpenStack or Containerd'); p682 l55-56 ('sparse, heterogeneous and incomplete').
```
count(QEMU) = 2 management APIs | api∈{debugger-sideload, runtime-disk-attach} ⊢ S3.3 @ p682 l48-50
count(crosvm) = 1 management API | api=debugger-sideload ⊢ S3.3 @ p682 l50
count(Firecracker) = 0 management APIs ⊢ S3.3 @ p682 l51
count(kvmtool) = 0 management APIs ⊢ S3.3 @ p682 l51
support(VMSH) = yes | hypervisor∈{Firecracker,kvmtool} ⊢ T1 @ p686 l4
```
**Warrant.** The hypervisors with zero APIs are exactly the ones the KVM-level path still reaches; the fewer APIs, the fewer alternatives to going below the hypervisor.
**Rebuttal.** If microVMs adopt a common management API (paper hopes for 'new virtualisation standards', p680 l46-48) the antecedent stops holding; going below the hypervisor costs host privileges (eBPF, p684 l48-52 right col.).
**Instances.** this paper's API inventory (four products); no measurement
**Evidence strength.** weak: an inventory, not an experiment

### vmsh:I12  ·  scope=law  ·  G2(division)+G8  ·  stated_by_paper=implied  ·  cea=3
**Claim.** Trace-based pruning of an image removes mostly the distribution userland (package managers, coreutils, shells), so the gain is a property of how much the image depends on an OS userland, not of the application: images that ship a distro userland shrink by half or more, images that are a single static binary shrink by under 10%; and because VM images carry more such tooling than containers, container results are a lower bound for VMs.
**Denies / refutes.** none stated; division of the reduction is the paper's own observation (S6.4 @ p689 l50-56 right col.); the a fortiori is stated (p689 l57-62 right col.).
```
size(vs(lightweight VM image, top-40 Docker images)) ∈ [-97%, -50%] ⊢ F8 @ p689 l49-50 (right col.)
size(vs(lightweight VM image, top-40 Docker images)) = -60% | statistic=mean ⊢ F8 @ p689 l50 (right col.)
count(top-40 Docker images) = 3 of 40 | size-reduction<10%, app=single-static-Go-binary ⊢ S6.4 @ p689 l53-56 (right col.)
count(top-40 Docker images) = "a number of tools [...] package managers, coreutils and shells" | role=removed ⊢ S6.4 @ p689 l50-53 (right col.) [vague]
size(vs(lightweight VM image, VM image)) ≤ size(vs(lightweight VM image, container image)) | reason="VM images package a higher number of tools" ⊢ S6.4 @ p689 l57-62 (right col.) [vague, a fortiori]
```
**Warrant.** The 37 distro-based images and the 3 static-binary images differ in the one variable (userland dependence) and split cleanly on the outcome; the removed-file inventory names the mechanism.
**Rebuttal.** Paper-internal conflict: 'between 50% and 97%' vs '3 of the 40 [...] less than 10%' (claim-map M-4); correctness after pruning checked only by 'the application still works' (port probes, bob C2) so untraced code paths may break; dataset is the live Docker Hub top-40 (claim-map M-5).
**Instances.** 37 vs 3 images across independent vendors on Docker Hub (this paper, F8); a fortiori for VM images asserted, not measured; lightweight-VM literature cited as motivation [14,97,120,123]
**Evidence strength.** moderate: 40 real images with pinned digests in the artifact; the static-binary split is the paper's explanation, not a controlled test

### vmsh:I13  ·  scope=law  ·  G8+G1(quality)  ·  stated_by_paper=yes  ·  cea=4
**Claim.** For an interactive console the acceptability bound is human perception, not the transport: once the echo round-trip is an order of magnitude below the visual-comprehension threshold (~13 ms per Potter et al. [91]), a console served through a side-loaded VirtIO device is indistinguishable in use from SSH, so console throughput is irrelevant to interactive usability.
**Denies / refutes.** none; quality judgment and bound stated: 'throughput is less relevant than latency' (S6.3D @ p689 l55-56); 'an order of magnitude faster than the capabilities of the human eye [91], making it sufficient for real life use cases' (p689 l66-68).
```
latency(vmsh-console) ≈ 0.9 ms | workload=echo-round-trip, samples=32 (artifact) ⊢ F7 @ p689 l64
latency(vs(vmsh-console, ssh)) ≈ 1× | workload=echo-round-trip ⊢ F7 @ p689 l64-66
latency(vs(vmsh-console, human-eye)) ≤ 0.1× ⊢ S6.3D @ p689 l66-68
latency(human-eye) = 13 ms | source=[91] title 'Detecting meaning in RSVP at 13 ms per picture' ⊢ Refs @ p693 [reader-supplied value]
```
**Warrant.** Two consoles with different transports (VirtIO console via VMSH; TCP/SSH) land at the same ~0.9 ms, both ≥10x under the perception bound; usability is bounded by perception, so the transport difference cannot be perceived.
**Rebuttal.** Bulk output (streaming a large log) is throughput-bound and was not measured; a loaded host or a wrap_syscall-mode guest with heavy exits could raise console latency (not measured); the perception figure is for image comprehension, not keystroke echo.
**Instances.** vmsh-console and ssh (this paper, F7); threshold from Potter et al. 2014 [91]
**Evidence strength.** adequate for the narrow relation: one VM, 32 samples per the artifact, sd bars only

### vmsh:I14  ·  scope=method  ·  G2(cause)+G8  ·  stated_by_paper=yes  ·  cea=3
**Claim.** Sharing host files into a guest at block level through an external device beats file-protocol sharing (9p) by about 7× in IOPS but not in bandwidth, because a file protocol stacks two file systems and two page caches on every operation (per-op cost), whereas large sequential transfers amortise that stack.
**Denies / refutes.** none stated; cause named: 'qemu-9p has poor IOPS compared to qemu-blk (7.8× lower) because of the use of two stacked file systems' (S6.3C @ p689 l43-47).
```
slowdown(vs(qemu-9p, qemu-blk)) = 7.8× | metric=iops, io=file ⊢ F6 @ p689 l43-44
speedup(vs(vmsh-blk, qemu-9p)) = 7× | metric=iops ⊢ F6 @ p689 l51-52
overhead(vs(vmsh-blk, qemu-9p)) = 40% | metric=throughput, op=write ⊢ F6 @ p689 l49-50
overhead(vs(vmsh-blk, qemu-9p)) = 2.3% | metric=throughput, op=read ⊢ F6 @ p689 l49-50
count(qemu-9p) = 2 stacked file systems + page caches | per-operation ⊢ S6.3C @ p689 l44-47
```
**Warrant.** IOPS (4 KiB) maximises per-operation software overhead by construction (p688 l51-53); the stacked-cache path pays per operation, so the gap appears in IOPS and vanishes (or reverses, write bandwidth) in the 256 KiB case.
**Rebuttal.** On write bandwidth vmsh-blk is 40% behind 9p, so block-level sharing is not a dominant choice; one fio configuration on one host; vmsh-blk's own 94% write-bandwidth overhead vs qemu-blk (p689 l48-49) is unexplained.
**Instances.** qemu-9p vs qemu-blk and vmsh-blk vs qemu-9p (this paper, F6 file-IO rows); two products (qemu-9p, vmsh-blk)
**Evidence strength.** moderate: clear mechanism, single configuration, no repetition count

### vmsh:I15  ·  scope=method  ·  G3(seems-necessary-is-unnecessary)+G8  ·  stated_by_paper=yes  ·  cea=2
**Claim.** KASLR does not stop host-side code injection into a guest kernel: because the kernel is placed in a fixed number of slots within a known range, a host with access to the guest's page tables (through KVM) locates the kernel and its exported symbol tables by walking page-table entries; only memory encryption with attestation (AMD SEV) prevents such injection.
**Denies / refutes.** KASLR as a barrier ('Because of KASLR, mapping the kernel library into the correct location is challenging', S4.2 @ p683 l27-29 right col.); prior KASLR breaks needed side channels (Jang et al. [53], p683 l35).
```
support(VMSH) = yes | guest-kernel∈{v5.10,v5.4,v4.19,v4.14,v4.9,v4.4}, kaslr=default ⊢ T1 @ p686 l6 [kaslr state not stated by the paper]
count(KASLR) = "a fixed number of slots [...] in a fixed address range [53]" ⊢ S4.2 @ p683 l33-35 (right col.) [vague]
∃ prevention(host-side injection) = {memory encryption + attestation (AMD SEV)} ⊢ S4.1 @ p683 l10-12 (right col.)
```
**Warrant.** Attach succeeds on six kernels with no mention of disabling KASLR; the symbol-location mechanism (.ksymtab_strings, p683 l38-49 right col.) is the one the paper ships.
**Rebuttal.** Fails under SEV/attested guests; fails if the guest kernel is not Linux or lacks an exported symbol table (p686 l36-39); the paper does not state KASLR was enabled in the tested guests.
**Instances.** this paper; Lares/kernel-module injection [88,103,124] as prior class assuming a cooperative or hypervisor-integrated loader
**Evidence strength.** weak: mechanism described, KASLR condition not explicitly verified in the evaluation

### vmsh:I16  ·  scope=method  ·  G6(enthymeme)+G1(quality)  ·  stated_by_paper=yes  ·  cea=1
**Claim.** Replacing a network-facing guest agent with an agent-less, host-side attach moves the attack surface from the guest's network (authentication, key management, remote-code-execution bugs) to two host-local channels (a backing file and a terminal); the paper's 'does not increase the TCB' rests on the unstated premise that an attacker who already controls the guest kernel gains nothing from side-loaded kernel code, and on trusting the provider, which it names as an assumption rather than a result.
**Denies / refutes.** adding functionality on the hypervisor side increases attack surface (paper concedes, S4.5 @ p684 l20-22 right col.); network agents as the default (S7 @ p690 l44-50).
```
tcb(vs(VMSH, guest-agent)) = "does not increase" | premise=no-guest-network-access ⊢ S4.5 @ p685 l29-32 [vague]
count(VMSH) = 2 host channels | channels={block-backing-file, terminal} ⊢ S4.5 @ p684 l36-40 (right col.)
privilege(attached service) = same-as(guest kernel) ⊢ S4.5 @ p684 l27-32 (right col.) [vague]
privilege(VMSH host process) > unprivileged | reason=eBPF, dropped-after-setup ⊢ S4.5 @ p684 l48-52 (right col.) [vague]
```
**Warrant.** The two-channel inventory and the privilege-domain argument are the paper's; the claim is that the security judgment is a relocation of surface, not a reduction, and is conditional on the trust assumptions of section 3.2 (p681 l40-52 right col.).
**Rebuttal.** If a colocated tenant can reach the VMSH process (host privilege before drop), or if the rust-vmm device code shared with Firecracker/crosvm has an exploitable bug (p684 l42-44 right col.), the host-local channel is worse than a network agent; no measurement or audit is reported.
**Instances.** this paper's argument; dual-VM inspection [36,88] as the alternative that keeps the tool out of the guest (p690 l12-17 right col.)
**Evidence strength.** weak: argument only, no atoms beyond inventories

## Discarded (trite) claims

- virtines bob:C1 / claim1:C1: — bare measurement; kept as I2 atom
- virtines bob:C2 / claim1:C2: — absorbed into I2 with rebuttal
- virtines bob:C3 / claim1:C3: — bare measurement; absorbed into I6
- virtines bob:C4 / claim1:C4: — bare measurement; absorbed into I1/I8
- virtines bob:C5 / claim1:C5: — restated as law I3; 10× unsupported
- virtines bob:C6 / claim1:C6: — bare measurement; mechanism kept I4
- virtines bob:C7 / claim1:C7: — bare measurement; contradiction kept I6
- virtines bob:C8: — design announcement; premise kept I10
- virtines bob:C9 / claim1:C8: — bare measurement; reversal kept I7
- virtines bob:C10: — design announcement, no relation
- safepm :bob:eurosys2022-safepm:C1 — design announcement; novelty token vague
- safepm :bob:eurosys2022-safepm:C2 — bare RIPE equivalence; absorbed I2,I10,I11
- safepm :bob:eurosys2022-safepm:C3 — bare slowdown ranges; abstract restatement
- safepm :bob:eurosys2022-safepm:C4 — measurement plus cause; absorbed I4
- safepm :bob:eurosys2022-safepm:C5 — measurement plus cause; absorbed I12
- safepm :bob:eurosys2022-safepm:C6 — bare recovery numbers; absorbed I3,I13
- safepm :bob:eurosys2022-safepm:C7 — measurement plus cause; absorbed I5
- safepm :bob:eurosys2022-safepm:C8 — zero-error tally; no mechanism
- safepm :bob:eurosys2022-safepm:C9 — measurement plus cause; absorbed I7
- safepm :bob:eurosys2022-safepm:C10 — bug tally; no general relation
- safepm :claim1:eurosys2022-safepm:C1 — abstract restatement; 'reasonable' vague
- safepm :claim1:eurosys2022-safepm:C2 — bare equivalence; absorbed I2
- safepm :claim1:eurosys2022-safepm:C3 — qualitative 'preserves'; no atom
- safepm :claim1:eurosys2022-safepm:C4 — bug tally; no relation
- vmsh bob:bob:eurosys2022-vmsh:C1 — bare headline; condition absorbed I3
- vmsh bob:bob:eurosys2022-vmsh:C2 — bare measurement; mechanism absorbed I12
- vmsh bob:bob:eurosys2022-vmsh:C3 — design announcement; part absorbed I8
- vmsh bob:bob:eurosys2022-vmsh:C4 — bare count; absorbed I1, I6
- vmsh bob:bob:eurosys2022-vmsh:C5 — bare measurement; atom in I9
- vmsh bob:bob:eurosys2022-vmsh:C6 — bare measurement; absorbed I5, I7
- vmsh bob:bob:eurosys2022-vmsh:C7 — bare measurements; cause absorbed I4, I14
- vmsh bob:bob:eurosys2022-vmsh:C8 — bare measurement; absorbed I13
- vmsh bob:bob:eurosys2022-vmsh:C9 — design announcement, no atom
- vmsh claim1:claim1:eurosys2022-vmsh:C1 — restates Table 1; absorbed I1
- vmsh claim1:claim1:eurosys2022-vmsh:C2 — restates abstract; condition absorbed I3
- vmsh claim1:claim1:eurosys2022-vmsh:C3 — 'correct and functional', vague