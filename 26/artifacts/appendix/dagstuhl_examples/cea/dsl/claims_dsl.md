# All claims, both sources, DSL form (see ../DISCIPLINE.md)



# virtines

# Claims DSL — eurosys2022-isolating-at-the-hardware-limit-with-virtines

Sources: `claim1` = Artifact Appendix A.4.1 Major Claims (p661) plus the matching A.4.2 experiment expectation (p662, locus `En`); `bob` = paper-body reader (cea/virtines.json). Loci are printed proceedings pages, lines from `pdftotext -layout`. Registry names only (registry.json). Qualitative values are quoted and tagged `[vague]`. Atoms derived by arithmetic from stated numbers carry `derived=...` in their conditions.

```
claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C1   scope=product   cea=3
  quote: "The core components of virtual context creation comprise only a few tens of thousands of cycles. We show this in experiment (E1) [...] whose results are shown in Table 1."   ⊢ appendix p661 l20
  atoms:
    latency(minimal-runtime) = "a few tens of thousands" cycles | statistic=per-component-min, host=KVM [vague] ⊢ T1 @ p648
    ∧ latency(minimal-runtime) < 100K cycles | statistic=mean-total, exclude=first-run ⊢ E1 @ p662 l25
    ∧ latency(minimal-runtime) = "most expensive components" | component∈{Paging identity mapping, Protected transition} [vague] ⊢ E1 @ p662 l27
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C1   scope=product   cea=3
  quote: "The row labeled “Paging/ident. map” is by far the most expensive at ∼28K cycles."   ⊢ p647 l59
  quote: "a minimal long-mode boot sequence costs less than 30K cycles (∼12 𝜇s)"   ⊢ p648 l30
  atoms:
    latency(minimal-runtime) = 28109 cycles | component=Paging identity mapping, statistic=min, machine=tinker, repeats=1000 ⊢ T1 @ p648 l5
    ∧ latency(minimal-runtime) = 3217 cycles | component=Protected transition, statistic=min ⊢ T1 @ p648 l6
    ∧ latency(minimal-runtime) < 30K cycles | mode=long, statistic=sum-of-component-minima ⊢ S4.2 @ p648 l30
    ∧ latency(minimal-runtime) ≈ 12 µs | mode=long, machine=tinker ⊢ S4.2 @ p648 l30
    ∧ latency(minimal-runtime) = 36564 cycles | statistic=sum-of-T1-rows, derived=28109+3217+681+175+190+4118+74 ⊢ T1 @ p648 l5

claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C2   scope=method   cea=4
  quote: "The latency to run a function in different processor modes can vary (e.g., 16-bit mode is cheaper on some microarchitectures), presenting an opportunity for optimization when building virtual contexts."   ⊢ appendix p661 l26
  atoms:
    latency(vs(16-bit mode, 32-bit mode, 64-bit mode)) < 0% | workload=fib(20), machine="some microarchitectures" [vague] ⊢ F3 @ p648
    ∧ latency(vs(16-bit mode, 32-bit mode, 64-bit mode)) ≈ 0% | machine="some machines", value="little difference" [vague] ⊢ E2 @ p662 l37
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C2   scope=method   cea=4
  quote: "These results suggest—provided that the virtine is short-lived (on the order of microseconds) and can feasibly execute in real-mode—that 10K cycles may potentially be saved."   ⊢ p648 l24
  atoms:
    latency(vs(16-bit mode, 64-bit mode)) ≈ -10K cycles | workload=fib(20), machine=tinker, repeats=1000, outliers=Tukey-1.5IQR-removed, hedge="may potentially" ⊢ F3 @ p648 l26
    ∧ latency(vs(32-bit mode, 64-bit mode)) ≈ 0% | workload=fib(20), machine=tinker, value="essentially the same" [vague] ⊢ F3 @ p648 l22

claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C3   scope=product   cea=4
  quote: "A runtime system that boots a basic server in a minimal execution environment can achieve response times <1ms, even without optimizations."   ⊢ appendix p661 l34
  atoms:
    latency(echo-server) < 1 ms | optimizations=none, measure=HTTP-response ⊢ F4 @ p648
    ∧ latency(echo-server) ∈ [100K, 500K] cycles | measure=HTTP-response ⊢ E3 @ p662 l48
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C3   scope=product   cea=4
  quote: "Even when leveraging the underlying host OS, and when adding the from-scratch virtual context creation time from Figure 2, we can achieve sub-millisecond HTTP response latencies (<300 𝜇s) without optimizations (§5.2)."   ⊢ p649 l34
  atoms:
    latency(echo-server) < 300 µs | optimizations=none, includes=F2-context-creation+F4-milestones, machine=tinker ⊢ F4 @ p649 l37
    ∧ latency(echo-server) ≈ 10K cycles | milestone=C-entry-point, mode=protected-no-paging, statistic=mean ⊢ F4 @ p648 l48

claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C4   scope=product   cea=3
  quote: "Virtual context creation latencies with Wasp approach the hardware limit of the vmrun/vmcall instruction by employing optimizations."   ⊢ appendix p661 l43
  atoms:
    latency(vs(Wasp+C, vmrun)) = "relatively close" | optimizations=caching [vague] ⊢ F8 @ p662 l57
    ∧ latency(vs(Wasp+CA, vmrun)) = "relatively close" | optimizations=caching+async-clean [vague] ⊢ F8 @ p662 l57
    ∧ latency(vs(Wasp+C, Linux pthread)) < 0% ⊢ F8 @ p662 l4
    ∧ latency(vs(Wasp+CA, Linux pthread)) < 0% ⊢ F8 @ p662 l4
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C4   scope=product   cea=3
  quote: "By recycling virtines, we can reach latencies much lower than Linux thread creation and much closer to the hardware limit, i.e., the vmrun instruction. [...] This measurement shows that the caching mechanism brings the cost of provisioning a virtine shell to within 4% of a bare vmrun."   ⊢ p650 l52
  atoms:
    latency(vs(Wasp+CA, vmrun)) ≤ +4% | machine=tinker, repeats=1000, clean-cost=excluded, variant-attribution=inferred-from-sentence-order ⊢ F8 @ p650 l61
    ∧ latency(vs(Wasp+C, Linux pthread)) < 0% | value="much lower", machine=tinker [vague] ⊢ F8 @ p650 l52
    ∧ latency(vs(Wasp+C, Wasp)) < 0% | pooling=on ⊢ F8 @ p650 l49

claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C5   scope=product   cea=4
  quote: "Virtine creation overheads can be amortized with roughly 100𝜇s of work. In finer-grained scenarios, snapshotting can reduce overheads significantly, pushing the amortization point down by about 10×."   ⊢ appendix p661 l49
  atoms:
    amortization-work(virtine-fib) ≈ 100 µs | workload=fib ⊢ F11 @ p661 l50
    ∧ amortization-work(vs(virtine+snap-fib, virtine-fib)) ≈ 0.1× | workload=fib ⊢ F11 @ p661 l52
    ∧ latency(vs(virtine+snap-fib, virtine-fib)) < 0% | image-size=small, value="significantly" [vague] ⊢ E5 @ p662 l14
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C5   scope=product   cea=4
  quote: "At first, the relative slowdown between native function invocation and virtines with snapshotting is 6.6×. [...] the slowdown drops to 1.03× for 𝑛 = 25 and 1.01× for 𝑛 = 30. [...] Here we can amortize start-up overheads with ∼100𝜇s of work. [...] producing an overall speedup of 2.5× relative to virtines without snapshotting for 𝑓 𝑖𝑏 (0)."   ⊢ p652 l26
  atoms:
    slowdown(vs(virtine+snap-fib, native-fib)) = 6.6× | n=0, machine=tinker, repeats=1000, steady-state=no ⊢ F11 @ p652 l27
    ∧ slowdown(vs(virtine+snap-fib, native-fib)) = 1.03× | n=25 ⊢ F11 @ p652 l31
    ∧ slowdown(vs(virtine+snap-fib, native-fib)) = 1.01× | n=30 ⊢ F11 @ p652 l31
    ∧ slowdown(vs(virtine+snap-fib, native-fib)) decreasing-in n | workload=fib ⊢ F11 @ p652 l30
    ∧ amortization-work(virtine+snap-fib) ≈ 100 µs | workload=fib ⊢ S6.1 @ p652 l34
    ∧ speedup(vs(virtine+snap-fib, virtine-fib)) = 2.5× | n=0, includes=initial-snapshot-cost ⊢ F11 @ p652 l49

claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C6   scope=product   cea=2
  quote: "Once virtine image size reaches around 2MB, start-up latency becomes bottlenecked by memory bandwidth."   ⊢ appendix p661 l55
  atoms:
    knee-image-size(virtine-halt) ≈ 2 MB ⊢ F12 @ p661 l55
    ∧ throughput(vs(virtine-halt, memcpy)) ≈ 0% | image-size≥2MB, meaning=memory-bandwidth-bound ⊢ F12 @ p661 l56
    ∧ knee-image-size(virtine-halt) ∈ [1, 2] MB | machine-dependent=memory-copy-bandwidth ⊢ E6 @ p662 l22
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C6   scope=product   cea=2
  quote: "With a 16MB image size, the start-up cost is 2.3ms. This amounts to roughly 6.8GB/s, which is in line with our measurement of the memcpy bandwidth on our tinker machine, 6.7GB/s."   ⊢ p653 l4
  atoms:
    latency(virtine-halt) = 2.3 ms | image-size=16MB, padding=zeroes, machine=tinker ⊢ F12 @ p653 l4
    ∧ throughput(virtine-halt) ≈ 6.8 GB/s | image-size=16MB, derived=16MB/2.3ms ⊢ S6.2 @ p653 l5
    ∧ throughput(memcpy) = 6.7 GB/s | machine=tinker, method=unstated ⊢ S6.2 @ p653 l6
    ∧ throughput(vs(virtine-halt, memcpy)) ≈ 0% | image-size=16MB, derived=+1.5% ⊢ S6.2 @ p653 l5
    ∧ latency(virtine-halt) ∝ image-size | image-size≤16MB, snapshot=simple-copy ⊢ F12 @ p652 l50

claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C7   scope=product   cea=3
  quote: "An HTTP server using our virtine compiler extensions experiences a less than 20% drop in throughput relative to a native environment."   ⊢ appendix p661 l62
  atoms:
    throughput(vs(virtine-http, native-http)) > -20% | variant=unspecified ⊢ F13 @ p653
    ∧ throughput(vs(virtine-http, native-http)) ≈ -50% | value="2× drop" ⊢ E7 @ p662 l35
    ∧ latency(vs(virtine-http, native-http)) ≈ +100% | value="a little more than 2× increase" ⊢ E7 @ p662 l34
    ∧ throughput(vs(virtine+snap-http, virtine-http)) < 0% | machine="limited memory bandwidth", hedge=may [vague] ⊢ E7 @ p662 l36
    ∧ overhead(virtine-http) = "mostly hypercall interactions" [vague] ⊢ E7 @ p662 l37
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C7   scope=product   cea=3
  quote: "However, despite the cost of these host interactions, virtines with snapshots incur only a 12% decrease in throughput relative to the baseline."   ⊢ p653 l56
  atoms:
    throughput(vs(virtine+snap-http, native-http)) = -12% | snapshots=on, workload=single-static-file, client=localhost, threads=1, machine=tinker ⊢ F13 @ p653 l57
    ∧ count(virtine-http) = 7 n | unit=hypercalls-per-request ⊢ S6.3 @ p653 l47

claim1:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C8   scope=product   cea=3
  quote: "Virtines can be integrated with an off-the-shelf Javascript engine, with acceptable (< 1.5×) slow-downs (∼2×). Snapshotting improves performance when environment setup in the virtual context is non-trival."   ⊢ appendix p661 l68
  atoms:
    slowdown(vs(virtine-js, native-js)) < 1.5× | engine=Duktape ⊢ F14 @ p661 l70
    ∧ slowdown(vs(virtine-js, native-js)) ≈ 2× | engine=Duktape ⊢ F14 @ p661 l72
    ∧ slowdown(vs(virtine-js, native-js)) = "acceptable" [vague] ⊢ F14 @ p661 l70
    ∧ latency(vs(virtine+snap-js, virtine-js)) < 0% | setup=non-trivial ⊢ F14 @ p661 l72
    ∧ slowdown(vs(virtine-js, native-js)) ∈ [1.5, 2]× | optimizations=none ⊢ E8 @ p662 l48
    ∧ latency(vs(virtine+snap-js, virtine-js)) < 0% | value="a real effect" [vague] ⊢ E8 @ p662 l45
bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C9   scope=product   cea=3
  quote: "The virtine trial without snapshotting takes 125𝜇s longer to execute than the baseline. [...] virtines can enjoy a significant reduction in overhead–roughly 2×. [...] These optimizations cause the overall latency to drop to 137𝜇s [...]"   ⊢ p654 l49
  quote: "The baseline latency is 419𝜇s."   ⊢ p654 l16
  atoms:
    latency(native-js) = 419 µs | engine=Duktape, workload=base64, machine=tinker ⊢ F14 @ p654 l16
    ∧ latency(vs(virtine-js, native-js)) = +125 µs | snapshot=off, teardown=on ⊢ F14 @ p654 l49
    ∧ slowdown(vs(virtine-js, native-js)) = 1.30× | derived=(419+125)/419 ⊢ F14 @ p654 l49
    ∧ overhead(vs(virtine+snap-js, virtine-js)) ≈ -50% | overhead=latency-excess-over-native, value="roughly 2×" ⊢ F14 @ p654 l24
    ∧ latency(virtine+snap-js-NT) = 137 µs | snapshot=on, teardown=off ⊢ F14 @ p654 l37
    ∧ slowdown(vs(virtine+snap-js-NT, native-js)) = 0.33× | derived=137/419, reason=executes-less-code ⊢ F14 @ p654 l37

bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C8   scope=product   cea=2
  quote: "Compiling OpenSSL using virtines was straightforward. [...] In all, the change took roughly one hour for an experienced developer. [...] with our optimizations and a 16KB cipher block size, virtines only incur a 17× slowdown relative to native execution with snapshotting."   ⊢ p653 l40
  atoms:
    time-to-port(OpenSSL) ≈ 1 hour | developer=experienced, change=annotate-block-cipher+swap-compiler ⊢ S6.4 @ p653 l45
    ∧ slowdown(vs(virtine+snap-openssl, native-openssl)) = 17× | block-size=16KB, benchmark="openssl speed -elapsed -evp aes-128-cbc", cipher=AES-128-CBC ⊢ S6.4 @ p654 l6
    ∧ size(virtine+snap-openssl) ≈ 21 KB | unit=image ⊢ S6.4 @ p654 l8
    ∧ latency(virtine+snap-openssl) ≈ 16 µs | per=invocation, derived-from=F12 ⊢ S6.4 @ p654 l9

bob:eurosys2022-isolating-at-the-hardware-limit-with-virtines:C10   scope=product   cea=2
  quote: "We present a prototype embeddable hypervisor framework, Wasp, that implements the virtine abstraction. Wasp runs as a Type-II micro-hypervisor on both Linux and Windows."   ⊢ p645 l38
  quote: "[...] for brevity we only show KVM’s performance on Linux, as Hyper-V performance was similar for our experiments."   ⊢ p647 l43
  atoms:
    portability(Wasp) = {Linux/KVM, Windows/Hyper-V} platforms | windows-status=prototype ⊢ S1 @ p645 l42
    ∧ latency(Wasp) = "similar" | host=Hyper-V, baseline-host=KVM [vague] ⊢ S4.1 @ p647 l44
```


# safepm

# claims_dsl — eurosys2022-safepm (DISCIPLINE v0.1)
# Pages are printed proceedings pages; lines from `pdftotext -layout -f N -l N | cat -n` per page.
# Sources: claim1 = Artifact Appendix A.4.1 (p523); bob = cea/safepm.json (paper-body reader).
# Names are registry.json names; conditions after |; qualitative values quoted and tagged [vague].

claim1:eurosys2022-safepm:C1   scope=product   cea=n/a
  quote: "SafePM provides comprehensive memory safety, both temporal and spatial, while incurring reasonable performance overheads. This is proven by the experiments (E1 & E2) described in Section 6.2 whose results are illustrated in Figure 3, Figure 4, Figure 5 and Figure 6. These overheads can be further reduced via SafePM's partial coverage approach as shown in Figure 7."   ⊢ appendix p523 l43
  atoms:
    safety(SafePM) = "comprehensive" [vague] | kind=spatial+temporal ⊢ A.4.1 @ p523
    ∧ slowdown(vs(SafePM, Native)) = "reasonable" [vague] | workload=persistent-indices ⊢ F3 @ p515
    ∧ slowdown(vs(SafePM, Native)) = "reasonable" [vague] | workload=pmemkv ⊢ F4 @ p516
    ∧ slowdown(vs(SafePM, Native)) = "reasonable" [vague] | workload=PM-operations-benchmark ⊢ F5 @ p516
    ∧ slowdown(vs(SafePM, Native)) = "reasonable" [vague] | workload=PM-pool-create/open ⊢ F6 @ p517
    ∧ slowdown(vs(partial safety coverage, Native)) decreasing-in unsafe-objects-percentage | index=hashmap ⊢ F7 @ p518

bob:eurosys2022-safepm:C1   scope=product   cea=4
  quote: "We introduce SafePM, a memory safety mechanism that transparently and comprehensively detects both spatial and temporal memory safety violations for PM-based applications. [...] We present the design (§ 4) of SafePM, the first solution for comprehensive spatial and temporal memory safety for PMDK-based PM applications."   ⊢ p506 l20; p507 l23
  atoms:
    safety(SafePM) = "comprehensive" [vague] | kind=spatial+temporal, source-changes=none ⊢ ABS @ p506
    ∧ novelty(SafePM) = "first" [vague] | class=comprehensive-spatial+temporal, target=PMDK-based ⊢ S1 @ p507
    ∧ safety(SafePM) = "probabilistic" [vague] | kind=temporal, quarantine=none, reuse-delay=PMDK-allocator ⊢ S4.3 @ p513
    ∧ count(SafePM) = 0 detections | violation=intra-object-overflow ⊢ S4.3 @ p514
    ∧ count(SafePM) = 0 detections | violation=out-of-bounds-landing-inside-another-object ⊢ S4.3 @ p514
    ∧ size(persistent shadow memory) ≤ 16 GB | limit=PMDK-object ⊢ S4.3 @ p514
    ∧ size(PM pool) ≤ 128 GB | tool=SafePM ⊢ S4.3 @ p514
    ∧ size(persistent red zones) = 16 B | setting=default ⊢ S4.1 @ p511

claim1:eurosys2022-safepm:C2   scope=product   cea=4
  quote: "SafePM offers the same memory safety guarantees for persistent memory as ASan provides for volatile memory. This is proven by experiment (E3) described in Section 6.4 whose results are reported in Table 5."   ⊢ appendix p523 l50
  atoms:
    safety(vs(SafePM, ASan w/ system heap)) = "same guarantees" [vague] | memory=PM-vs-volatile ⊢ A.4.1 @ p523
    ∧ safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | benchmark=RIPE, outcome=always ⊢ T5 @ p518
    ∧ safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | benchmark=RIPE, outcome=sometimes ⊢ T5 @ p518
    ∧ safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | benchmark=RIPE, outcome=never ⊢ T5 @ p518

bob:eurosys2022-safepm:C2   scope=product   cea=4
  quote: "SafePM achieves memory safety effectiveness for the PM heap equivalent to that achieved by ASan for the volatile (system) heap. [...] Finally, we observe that SafePM is able to detect and prevent a higher number of memory vulnerabilities compared to the state-of-the-art memcheck [21]."   ⊢ p518 l70
  atoms:
    safety(Intact) = 306 attacks | benchmark=RIPE, outcome=always, repeats=3, compiler=gcc-9.3.0 ⊢ T5 @ p518
    ∧ safety(ASan w/ system heap) = 27 attacks | benchmark=RIPE, outcome=always, repeats=3, compiler=gcc-9.3.0 ⊢ T5 @ p518
    ∧ safety(ASan w/ system heap) = 1 attacks | benchmark=RIPE, outcome=sometimes, repeats=3 ⊢ T5 @ p518
    ∧ safety(ASan w/ system heap) = 1306 attacks | benchmark=RIPE, outcome=never, repeats=3 ⊢ T5 @ p518
    ∧ safety(ASan w/ PM pool heap) = 119 attacks | benchmark=RIPE, outcome=always, repeats=3 ⊢ T5 @ p518
    ∧ safety(SafePM) = 27 attacks | benchmark=RIPE, outcome=always, repeats=3, compiler=gcc-9.3.0 ⊢ T5 @ p518
    ∧ safety(SafePM) = 1 attacks | benchmark=RIPE, outcome=sometimes, repeats=3 ⊢ T5 @ p518
    ∧ safety(SafePM) = 1306 attacks | benchmark=RIPE, outcome=never, repeats=3 ⊢ T5 @ p518
    ∧ safety(memcheck) = 62 attacks | benchmark=RIPE, outcome=always, repeats=3 ⊢ T5 @ p518
    ∧ safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | benchmark=RIPE, outcome=always, repeats=3 ⊢ T5 @ p518
    ∧ safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | benchmark=RIPE, outcome=sometimes, repeats=3 ⊢ T5 @ p518
    ∧ safety(vs(SafePM, ASan w/ system heap)) = 0 attacks | benchmark=RIPE, outcome=never, repeats=3 ⊢ T5 @ p518
    ∧ safety(vs(SafePM, ASan w/ PM pool heap)) = -92 attacks | benchmark=RIPE, outcome=always, repeats=3 ⊢ T5 @ p518
    ∧ safety(vs(SafePM, memcheck)) = -35 attacks | benchmark=RIPE, outcome=always, repeats=3 ⊢ T5 @ p518
    ∧ safety(vs(SafePM, ASan w/ system heap)) = "same guarantees" [vague] | memory=PM-heap-vs-system-heap ⊢ S6.4 @ p518

claim1:eurosys2022-safepm:C3   scope=product   cea=3
  quote: "SafePM preserves the crash consistency property for both the PM residing data and its memory safety metadata. This is proven by the experiment (E4) described in Section 6.5."   ⊢ appendix p523 l54
  atoms:
    crash-consistency(SafePM) = "preserved" [vague] | scope=PM-data+safety-metadata ⊢ S6.5 @ p518
    ∧ count(SafePM) = 0 errors | tool=pmemcheck+memcheck, workload=§6.5-configurations ⊢ S6.5 @ p518

bob:eurosys2022-safepm:C8   scope=product   cea=3
  quote: "We validate the crash-consistency property for both the application data and SafePM metadata using existing tools, pmemcheck [22] and memcheck [74]. [...] We observe that for the tested indices, neither pmemcheck nor memcheck report any error. For the PM operations benchmark, pmemcheck again reports no error, while memcheck does not report any error beyond the ones also reported for the case of unmodified PMDK."   ⊢ p518 l79; p518 l27
  atoms:
    count(SafePM w/o ASan) = 0 errors | tool=pmemcheck, workload=persistent-indices, ops=10000, ASan=disabled ⊢ S6.5 @ p518
    ∧ count(SafePM w/o ASan) = 0 errors | tool=memcheck, workload=persistent-indices, ops=10000, ASan=disabled ⊢ S6.5 @ p518
    ∧ count(SafePM w/o ASan) = 0 errors | tool=pmemcheck, workload=PM-operations-benchmark, ASan=disabled ⊢ S6.5 @ p518
    ∧ count(vs(SafePM w/o ASan, Native)) = 0 errors | tool=memcheck, workload=PM-operations-benchmark, ASan=disabled ⊢ S6.5 @ p518
    ∧ crash-consistency(SafePM w/o ASan) = "validated" [vague] | scope=application-data+SafePM-metadata, ASan=disabled ⊢ S6.5 @ p518

claim1:eurosys2022-safepm:C4   scope=product   cea=4
  quote: "SafePM has been used to uncover two new bugs in the PMDK software. These can be reproduced by the experiment (E5) described in Section 6.7."   ⊢ appendix p523 l58
  atoms:
    count(SafePM) = 2 bugs | in=PMDK, novelty=new ⊢ S6.7 @ p518

bob:eurosys2022-safepm:C10   scope=product   cea=4
  quote: "Through SafePM we have also identified two memory safety bugs in the widely-used PMDK library. [...] (i) in the btree example of PMDK version 1.9.2, a call to memmove on line 378 of btree_map.c causes an off-by-one overflow on PM residing data objects and (ii) in the transactional operations benchmark, shipped as part of pmembench, a configuration file lacks the configuration setting nestings. This causes the transaction to not be aborted, which triggers invalid frees at line 295 in pmemobj_tx.cpp, that is detected by SafePM, when the benchmark attempts the cleanup."   ⊢ p507 l20; p518 l71
  atoms:
    count(SafePM) = 2 bugs | in=PMDK, kind=memory-safety ⊢ S6.7 @ p518
    ∧ count(SafePM) = 1 bugs | in=btree-example, version=PMDK-1.9.2, kind=spatial-off-by-one-overflow, location=btree_map.c:378 ⊢ S6.7 @ p518
    ∧ count(SafePM) = 1 bugs | in=transactional-operations-benchmark, shipped-with=pmembench, kind=invalid-free, location=pmemobj_tx.cpp:295, cause=missing-nestings-setting ⊢ S6.7 @ p518

bob:eurosys2022-safepm:C3   scope=product   cea=3
  quote: "Our evaluation shows that SafePM offers the same memory safety guarantees for persistent memory as ASan provides for volatile memory with reasonable performance overheads, e.g., 1.20 − 2.62× slowdown for the KV store [...] In general, SafePM is 1.68-2.00×, 1.16-1.50× and 1.68-1.87× slower than the native PMDK for the insert, get and remove operations, respectively. [...] The respective values for SafePM are 1.20-2.55×."   ⊢ p507 l15; p515 l71; p516 l66
  atoms:
    slowdown(vs(SafePM, Native)) ∈ [1.20, 2.62]× | workload=pmemkv ⊢ S1 @ p507
    ∧ slowdown(vs(SafePM, Native)) ∈ [1.20, 2.55]× | workload=pmemkv, engine=cmap, entries=1M, ops=10M, key=16B, value=1024B, threads=1-24, repeats≥3 ⊢ F4 @ p516
    ∧ slowdown(vs(ASan, Native)) ∈ [1.14, 2.36]× | workload=pmemkv, threads=1-24, repeats≥3 ⊢ F4 @ p516
    ∧ slowdown(vs(SafePM, ASan)) = "marginally higher" [vague] | workload=pmemkv ⊢ F4 @ p516
    ∧ slowdown(vs(SafePM, Native)) = "does not affect scalability" [vague] | workload=pmemkv, threads=1-24 ⊢ F4 @ p516
    ∧ slowdown(vs(SafePM, Native)) decreasing-in threads | workload=pmemkv, mix=50/50, threads>8 ⊢ F4 @ p516
    ∧ slowdown(vs(SafePM, Native)) ∈ [1.68, 2.00]× | workload=persistent-indices, op=insert, ops=1M, key=8B, dist=uniform, red-zone=16B, repeats≥3 ⊢ F3 @ p515
    ∧ slowdown(vs(SafePM, Native)) ∈ [1.16, 1.50]× | workload=persistent-indices, op=get, ops=1M, key=8B, dist=uniform, red-zone=16B, repeats≥3 ⊢ F3 @ p515
    ∧ slowdown(vs(SafePM, Native)) ∈ [1.68, 1.87]× | workload=persistent-indices, op=remove, ops=1M, key=8B, dist=uniform, red-zone=16B, repeats≥3 ⊢ F3 @ p515
    ∧ slowdown(vs(SafePM w/o ASan, Native)) = 1.34× | index=rtree, op=insert ⊢ F3 @ p515
    ∧ slowdown(vs(SafePM w/o ASan, Native)) < 1.20× | workload=persistent-indices, except=rtree-insert ⊢ F3 @ p515
    ∧ slowdown(vs(SafePM, ASan)) = "very close" [vague] | workload=persistent-indices, except=hashmap-get ⊢ F3 @ p515

bob:eurosys2022-safepm:C4   scope=product   cea=3
  quote: "For object allocation, we observe that the overhead decreases for both atomic and transactional allocation as the object size grows (2.4-5.8×). The reallocation operation maintains a relatively constant overhead for all the tested data sizes (1.85-2.25×). SafePM incurs a higher performance overhead for the free operation (3.5-7.0×) compared to alloc and realloc for every object size. [...] SafePM poses a higher overhead for the atomic versions of the operations, as it transparently converts them into their transactional counterpart."   ⊢ p516 l64
  atoms:
    slowdown(vs(SafePM, Native)) ∈ [2.4, 5.8]× | workload=PM-operations-benchmark, op=alloc, mode=atomic+transactional, object-size=64B-16KB, ops=100K, repeats=10 ⊢ F5 @ p516
    ∧ slowdown(vs(SafePM, Native)) decreasing-in object-size | workload=PM-operations-benchmark, op=alloc, mode=atomic+transactional ⊢ F5 @ p516
    ∧ slowdown(vs(SafePM, Native)) ∈ [1.85, 2.25]× | workload=PM-operations-benchmark, op=realloc, object-size=64B-16KB, ops=100K, repeats=10 ⊢ F5 @ p516
    ∧ slowdown(vs(SafePM, Native)) ∈ [3.5, 7.0]× | workload=PM-operations-benchmark, op=free, object-size=64B-16KB, ops=100K, repeats=10 ⊢ F5 @ p516
    ∧ slowdown(vs(SafePM, Native)) = "higher than alloc and realloc" [vague] | workload=PM-operations-benchmark, op=free, object-size=each ⊢ F5 @ p516
    ∧ slowdown(vs(SafePM, Native)) = "higher than transactional" [vague] | workload=PM-operations-benchmark, mode=atomic ⊢ F5 @ p516

bob:eurosys2022-safepm:C5   scope=product   cea=3
  quote: "We observe that opening a pool with SafePM takes ∼30ms instead of 10ms with native PMDK, a slowdown of up to 3×. The slowdown appears to be largely caused by the introduced ASan checks because the performance of the SafePM w/o Asan variant is close to that of the native PMDK. Further, during pool creation, SafePM incurs significant slowdown which increases with the pool size, causing the create operation to take a few seconds to complete. [...] pool creation is an one-time operation, hence, the high overhead is largely irrelevant to application performance."   ⊢ p517 l30
  atoms:
    latency(SafePM) ≈ 30 ms | workload=PM-pool-create/open, op=pool-open, pool-size=256MB-128GB, repeats≥3 ⊢ F6 @ p517
    ∧ latency(Native) = 10 ms | workload=PM-pool-create/open, op=pool-open, pool-size=256MB-128GB, repeats≥3 ⊢ F6 @ p517
    ∧ slowdown(vs(SafePM, Native)) ≤ 3× | workload=PM-pool-create/open, op=pool-open ⊢ F6 @ p517
    ∧ latency(vs(SafePM w/o ASan, Native)) = "close" [vague] | op=pool-open ⊢ F6 @ p517
    ∧ slowdown(vs(SafePM, Native)) = "significant" [vague] | op=pool-create ⊢ F6 @ p517
    ∧ latency(SafePM) increasing-in pool-size | op=pool-create, pool-size=256MB-128GB ⊢ F6 @ p517
    ∧ latency(SafePM) = "a few seconds" [vague] | op=pool-create ⊢ F6 @ p517

bob:eurosys2022-safepm:C6   scope=product   cea=2
  quote: "With ASan disabled, SafePM's wrappers introduce insignificant overhead (<300 μs) in the recovery time compared to PMDK. When we enable ASan, the shadow memory checks incur an inevitable, but minor, overhead (approximately 10 ms). Overall, SafePM does not introduce any further delays in the recovery process other than those of ASan."   ⊢ p517 l59
  atoms:
    latency(vs(SafePM w/o ASan, Native)) < +300 µs | workload=recovery, log-size=4KB-4MB, object=1KB, repeats=100 ⊢ T3 @ p517
    ∧ latency(vs(SafePM, Native)) ≈ +10 ms | workload=recovery, log-size=4KB-4MB, repeats=100 ⊢ T3 @ p517
    ∧ latency(vs(ASan, Native)) ≈ +10 ms | workload=recovery, log-size=4KB-4MB, repeats=100 ⊢ T3 @ p517
    ∧ latency(vs(SafePM, ASan)) ∈ [-0.01, +0.34] ms | workload=recovery, log-size=4KB-4MB, repeats=100 ⊢ T3 @ p517
    ∧ latency(Native) ∈ [15.00, 19.13] ms | workload=recovery, log-size=4KB-4MB, repeats=100 ⊢ T3 @ p517
    ∧ latency(SafePM) ∈ [25.44, 29.79] ms | workload=recovery, log-size=4KB-4MB, repeats=100 ⊢ T3 @ p517
    ∧ latency(SafePM) increasing-in log-size | workload=recovery, log-size=4KB-4MB ⊢ T3 @ p517

bob:eurosys2022-safepm:C7   scope=product   cea=2
  quote: "The persistent shadow memory always occupies one eighth of the pool which corresponds to an overhead of 12.5%. For the ctree, rbtree and hashmap_tx, we observe that this is the only considerable space overhead as the persistent object red zones occupy space which is wasted to padding by the native PMDK allocator. For the rtree index the object red zones increase persistent memory usage leading to slightly higher space overheads."   ⊢ p517 l53
  atoms:
    space-overhead(persistent shadow memory) = 12.5% | of=pool-size ⊢ T4 @ p517
    ∧ space-overhead(SafePM) = 12.5% | index=ctree, op=insert+remove+get, measure=peak ⊢ T4 @ p517
    ∧ space-overhead(SafePM) = 12.5% | index=rbtree, op=insert+remove+get, measure=peak ⊢ T4 @ p517
    ∧ space-overhead(SafePM) = 12.5% | index=hashmap, op=insert+remove+get, measure=peak ⊢ T4 @ p517
    ∧ space-overhead(SafePM) = 14.25% | index=rtree, op=insert, measure=peak ⊢ T4 @ p517
    ∧ space-overhead(SafePM) = 13.8% | index=rtree, op=remove+get, measure=peak ⊢ T4 @ p517
    ∧ space-overhead(persistent red zones) ≈ 0% | index=ctree+rbtree+hashmap, cause=allocator-padding ⊢ T4 @ p517
    ∧ space-overhead(persistent red zones) ∈ [1.3, 1.75]% | index=rtree ⊢ T4 @ p517

bob:eurosys2022-safepm:C9   scope=product   cea=3
  quote: "We observe that for all three operations the relative overhead decreases as more objects are excluded from the ASan instrumentation and runtime checks. However, there is still an inevitable overhead that stems from ASan intercepting the volatile heap management functions, which are used by PMDK internally. Note that with get operation there is no overhead as there are no intercepted malloc/free calls."   ⊢ p518 l53
  atoms:
    slowdown(vs(partial safety coverage, Native)) decreasing-in unsafe-objects-percentage | index=hashmap, op=insert, unsafe-objects-percentage=0-100% ⊢ F7 @ p518
    ∧ slowdown(vs(partial safety coverage, Native)) decreasing-in unsafe-objects-percentage | index=hashmap, op=remove, unsafe-objects-percentage=0-100% ⊢ F7 @ p518
    ∧ slowdown(vs(partial safety coverage, Native)) decreasing-in unsafe-objects-percentage | index=hashmap, op=get, unsafe-objects-percentage=0-100% ⊢ F7 @ p518
    ∧ slowdown(vs(partial safety coverage, Native)) > 1.0× | index=hashmap, op=insert+remove, unsafe-objects-percentage=100%, cause=ASan-malloc/free-interception-in-PMDK ⊢ F7 @ p518
    ∧ slowdown(vs(partial safety coverage, Native)) = 1.0× | index=hashmap, op=get ⊢ F7 @ p518


# vmsh

# eurosys2022-vmsh — claims under DISCIPLINE.md v0.1

Sources: `claim1` = Appendix A.4.1 (p695), `bob` = cea/vmsh.json (paper-body reader). Page/line from `pdftotext -layout -f N -l N`.
Conditions common to all performance atoms unless stated: machine=testbed (i9-9900K, P4600 NVMe, host Linux 5.12.14), hypervisor=QEMU/KVM, vm=8GiB/4vCPU (p686 l55-61).

## Generality

```
claim1:eurosys2022-vmsh:C1   scope=product   cea=2
  quote: "VMSH supports a wide range of KVM-based hypervisors and guest kernel versions (see Section 6.2) listed in Table 1. This claim is confirmed by the unit tests of experiments (E2) and (E3)."   ⊢ appendix p695 l40-43
  atoms:
    count(VMSH) = "wide range" hypervisors [vague] ⊢ A.4.1 @ p695 l40
  ∧ count(VMSH) = 4 hypervisors | set={QEMU, kvmtool, Firecracker, crosvm} ⊢ T1 @ p686 l4
  ∧ count(VMSH) = 6 LTS kernels | set={v5.10, v5.4, v4.19, v4.14, v4.9, v4.4}, hypervisor=QEMU ⊢ T1 @ p686 l6
bob:eurosys2022-vmsh:C4   scope=product   cea=2
  quote: "We demonstrate VMSH approaches its goal of generality by successfully testing 4 industry leading KVM-based hypervisors and all current long-term support versions of the Linux kernel (§ 6.2)."   ⊢ p679 l49-52
  atoms:
    count(VMSH) = 4 hypervisors | set={QEMU, kvmtool, Firecracker, crosvm}, tested=5 ⊢ S6.2 @ p686 l58
  ∧ support(VMSH) = no | hypervisor=Cloud Hypervisor, reason=MSI-X-interrupts ⊢ S6.2 @ p686 l58-61
  ∧ support(VMSH) = yes | hypervisor=Firecracker, seccomp=disabled ⊢ S6.2 @ p687 l8-10
  ∧ count(VMSH) = "all current LTS" kernels [vague] ⊢ S1 @ p679 l51
  ∧ count(VMSH) = 6 LTS kernels | set={v5.10, v5.4, v4.19, v4.14, v4.9, v4.4}, hypervisor=QEMU, dev-kernel=5.12 ⊢ T1 @ p686 l6
  ∧ time-to-port(VMSH) = 1 person-week | kernel-span=5 years ⊢ S6.2 @ p687 l55-56
```

## Performance

```
claim1:eurosys2022-vmsh:C2   scope=product   cea=3
  quote: "We claim that attached tools running through VMSH are on average 1.5× slower (see Figure 5, Section 6.3). The VM and devices not connected to VMSH experience no slowdown (see Figure 6, Section 6.3)."   ⊢ appendix p695 l50-55
  atoms:
    slowdown(vs(vmsh-blk, qemu-blk)) = 1.5× | workload=Phoronix Test Suite, stat=mean ⊢ F5 @ p687
  ∧ slowdown(vs(qemu-blk+VMSH, qemu-blk)) = "no slowdown" [vague] | workload=fio ⊢ F6 @ p688
bob:eurosys2022-vmsh:C1   scope=product   cea=3
  quote: "(i) VMSH adds no overhead for the applications running in the VM"   ⊢ p678 l26-27
  quote: "when VMSH is attached to a VM, the throughput and IOPS of qemu-blk devices on the VM are the same as without VMSH when using the ioregionfd implementation. However, with the wrap_syscall implementation, both throughput and IOPS on the qemu-blk device are negatively impacted. Read throughput is reduced by 1.5× and IOPS by 6×."   ⊢ p688 l63-69
  atoms:
    overhead(vs(qemu-blk+VMSH, qemu-blk)) = "no overhead" [vague] ⊢ Abs @ p678 l27
  ∧ throughput(vs(qemu-blk+VMSH, qemu-blk)) = "same-as(qemu-blk)" [vague] | workload=fio, mode=ioregionfd, bs=256KiB, access=sequential, iotype=direct ⊢ F6 @ p688 l63-66
  ∧ iops(vs(qemu-blk+VMSH, qemu-blk)) = "same-as(qemu-blk)" [vague] | workload=fio, mode=ioregionfd, bs=4KiB, access=sequential, iotype=direct ⊢ F6 @ p688 l63-66
  ∧ slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 1.5× | workload=fio, mode=wrap_syscall, metric=throughput, op=read, bs=256KiB, iotype=direct ⊢ F6 @ p688 l68-69
  ∧ slowdown(vs(qemu-blk+VMSH, qemu-blk)) = 6× | workload=fio, mode=wrap_syscall, metric=iops, bs=4KiB, iotype=direct ⊢ F6 @ p688 l69
bob:eurosys2022-vmsh:C6   scope=product   cea=3
  quote: "On average, VMSH is 1.5 × ±0.6 slower than qemu-blk. The fio tests accessing large chunks of data (2 MB) are the slowest benchmarks, being up to 3.7× slower than qemu-blk."   ⊢ p687 l47-50
  atoms:
    slowdown(vs(vmsh-blk, qemu-blk)) = 1.5× ±0.6 | workload=Phoronix Test Suite, stat=mean, vcpus=4 ⊢ F5 @ p687 l47-48
  ∧ slowdown(vs(vmsh-blk, qemu-blk)) ≤ 3.7× | workload=Phoronix Test Suite, benchmark=fio, bs=2MB, iotype=direct ⊢ F5 @ p687 l48-50
bob:eurosys2022-vmsh:C7   scope=product   cea=3
  quote: "As for vmsh-blk, throughput and IOPS are halved compared to qemu-blk, indifferent to the used implementation [...] Finally, vmsh-blk suffers a 94% write and 7% read overhead in throughput compared to qemu-blk (40% write/2.3% read overhead compared to qemu-9p), but still has good IOPS (14% degradation compared to qemu-blk and is 7× better than qemu-9p)."   ⊢ p688 l68-69, p689 l48-52
  atoms:
    slowdown(vs(vmsh-blk, qemu-blk)) = 2× | workload=fio, metric=throughput, mode∈{wrap_syscall, ioregionfd}, iotype=direct ⊢ F6 @ p688 l68-69
  ∧ slowdown(vs(vmsh-blk, qemu-blk)) = 2× | workload=fio, metric=iops, mode∈{wrap_syscall, ioregionfd}, iotype=direct ⊢ F6 @ p688 l68-69
  ∧ overhead(vs(vmsh-blk, qemu-blk)) = 94% | workload=fio, metric=throughput, op=write, bs=256KiB, iotype=direct ⊢ F6 @ p689 l48-49
  ∧ overhead(vs(vmsh-blk, qemu-blk)) = 7% | workload=fio, metric=throughput, op=read, bs=256KiB, iotype=direct ⊢ F6 @ p689 l48-49
  ∧ overhead(vs(vmsh-blk, qemu-9p)) = 40% | workload=fio, metric=throughput, op=write, iotype=file ⊢ F6 @ p689 l49-50
  ∧ overhead(vs(vmsh-blk, qemu-9p)) = 2.3% | workload=fio, metric=throughput, op=read, iotype=file ⊢ F6 @ p689 l49-50
  ∧ overhead(vs(vmsh-blk, qemu-blk)) = 14% | workload=fio, metric=iops, bs=4KiB, iotype=direct ⊢ F6 @ p689 l51
  ∧ speedup(vs(vmsh-blk, qemu-9p)) = 7× | workload=fio, metric=iops, bs=4KiB ⊢ F6 @ p689 l51-52
  ∧ count(vs(vmsh-blk, qemu-blk)) = 2× | metric=context-switches, sampling=same-period ⊢ S6.3C @ p689 l36-38
  note: the two "halved" atoms (= 50% overhead) and the 7% read / 14% IOPS atoms are on the same measure/subject pair and disagree; the paper does not reconcile them.
bob:eurosys2022-vmsh:C8   scope=product   cea=4
  quote: "Our measurements show that, with around 0.9ms, the latency of the VMSH console is very similar to the one of SSH (see Figure 7). The latency of the VMSH console is an order of magnitude faster than the capabilities of the human eye [91], making it sufficient for real life use cases."   ⊢ p689 l64-68
  atoms:
    latency(vmsh-console) ≈ 0.9 ms | workload=echo-round-trip-via-pts ⊢ F7 @ p689 l64
  ∧ latency(vs(vmsh-console, ssh)) = "very similar" [vague] | workload=echo-round-trip-via-pts ⊢ F7 @ p689 l64-66
  ∧ latency(vs(vmsh-console, human-eye)) ≈ 0.1× ⊢ S6.3D @ p689 l66-68
```

## Functionality

```
claim1:eurosys2022-vmsh:C3   scope=product   cea=2-3
  quote: "VMSH has a correct and functional implementation. We evaluate its robustness in Section 6.1 (E1), measure the potential of VMSH to reduce the size of boot images in Section 6.4 (E7) and test the use-cases from Section 6.5 in (E8-10)."   ⊢ appendix p695 l58-62
  quote: "(E7) Figure 8: VM size reduction for the top-40 Docker images (average reduction: 60%)."   ⊢ appendix p696 l9-10
  atoms:
    correctness(VMSH) = "correct and functional" [vague] ⊢ A.4.1 @ p695 l58-59
  ∧ correctness(vmsh-blk) = "robust" [vague] | workload=xfstests ⊢ S6.1 @ p686
  ∧ size(vs(lightweight VM image, top-40 Docker images)) = -60% | stat=mean, n=40 ⊢ F8 @ p689
  ∧ count(VMSH) = 3 use-cases | set={serverless debug shell, VM rescue system, package security scanner}, outcome=tested ⊢ S6.5 @ p690
bob:eurosys2022-vmsh:C5   scope=product   cea=3
  quote: "Out of the 619 tests, all succeed natively. For both qemu-blk and vmsh-blk, three tests (0.5%) fail. [...] since vmsh-blk passes all tests that are passed by known-good devices, we conclude that the vmsh-blk device has no regressions w.r.t. qemu-blk."   ⊢ p686 l40-41, l46-48
  atoms:
    count(native) = 0 failed tests | workload=xfstests, group=quick, of=619, fs=XFS ⊢ S6.1 @ p686 l40
  ∧ count(qemu-blk) = 3 failed tests | workload=xfstests, group=quick, of=619, fs=XFS, cause=quota-reporting ⊢ S6.1 @ p686 l40-42
  ∧ count(vmsh-blk) = 3 failed tests | workload=xfstests, group=quick, of=619, fs=XFS, cause=quota-reporting ⊢ S6.1 @ p686 l40-42
  ∧ count(vs(vmsh-blk, qemu-blk, native)) = 0 regressions | workload=xfstests, group=quick ⊢ S6.1 @ p686 l46-48
bob:eurosys2022-vmsh:C2   scope=method   cea=3
  quote: "(ii) de-bloating images from the Docker registry can save up to 60% of their size on average"   ⊢ p678 l28-29
  quote: "Image sizes are reduced by between 50% and 97%, on average by 60%. [...] Only 3 of the 40 containers are reduced by less than 10%."   ⊢ p689 l49-54
  atoms:
    size(vs(lightweight VM image, top-40 Docker images)) ≥ -60% | stat=mean ⊢ Abs @ p678 l28-29
  ∧ size(vs(lightweight VM image, top-40 Docker images)) = -60% | stat=mean, n=40, method=sysdig-trace-of-opened-files, hypervisor=runq ⊢ F8 @ p689 l49-50
  ∧ size(vs(lightweight VM image, top-40 Docker images)) ∈ [-97%, -50%] | per-image ⊢ S6.4 @ p689 l49-50
  ∧ count(lightweight VM image) = 3 images | reduction<10%, of=40, cause=static-Go-binary ⊢ S6.4 @ p689 l53-54
  note: the range atom [-97%, -50%] and the "3 images < 10%" atom cannot both hold; the paper does not reconcile them.
bob:eurosys2022-vmsh:C3   scope=product   cea=2
  quote: "(iii) VMSH enables cloud providers to offer services to customers, such as recovery shells, without interfering with their VM’s execution."   ⊢ p678 l29-31
  atoms:
    count(VMSH) = 3 use-cases | set={serverless debug shell, VM rescue system, package security scanner} ⊢ S6.5 @ p689 l68-69
  ∧ support(VMSH) = yes | use-case=serverless debug shell, platform=vHive, hypervisor=Firecracker, interactive=yes ⊢ S6.5-1 @ p690 l6-13
  ∧ support(VMSH) = yes | use-case=VM rescue system, agent=none, attached-while-running=yes ⊢ S6.5-2 @ p690 l27-29
  ∧ support(VMSH) = yes | use-case=package security scanner, guest=Alpine Linux ⊢ S6.5-3 @ p690 l37-40
  ∧ overhead(VMSH) = "without interfering with VM execution" [vague] | context=use-cases ⊢ Abs @ p678 l31
```

## Design (bob only)

```
bob:eurosys2022-vmsh:C9   scope=product   cea=1
  quote: "We design a system for hypervisor-independent side-loading into a VM of a generic guest-overlay that does not impose limitations on both the original guest application or the spawned service, and a device that can be attached to hypervisors non-cooperatively (§ 4)."   ⊢ p679 l12-16
  atoms:
    support(VMSH) = "hypervisor-independent" [vague] | hypervisor=KVM ⊢ S1 @ p679 l12-13
  ∧ overhead(VMSH) = "no limitations on guest application or spawned service" [vague] ⊢ S1 @ p679 l13-15
  ∧ support(VMSH) = "non-cooperative attach" [vague] | hypervisor=KVM ⊢ S1 @ p679 l15-16
```
