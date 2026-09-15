# CEA read of the three Dagstuhl example papers (EuroSys 2022, Distinguished Artifact winners)

Rubric: cea/RUBRIC.md (Dagstuhl Triple-A spec, 15 Sep 2026). One reader per paper, paper + artifact README/file listing, nothing executed. Levels are articulation, not quality.

## Level distribution

| paper | claims | CEA1 | CEA2 | CEA3 | CEA4 | causal overreach | reader min |
|---|---|---|---|---|---|---|---|
| Virtines | 10 | 0 | 3 | 4 | 3 | 0 | 35 |
| SafePM | 10 | 0 | 2 | 5 | 3 | 1 | 30 |
| VMSH | 9 | 1 | 2 | 5 | 1 | 0 | 6 |

## Claim types by paper

| paper | claim type | n |
|---|---|---|
| Virtines | comparative | 5 |
| Virtines | descriptive | 2 |
| Virtines | design | 2 |
| Virtines | explanatory | 1 |
| SafePM | descriptive | 2 |
| SafePM | design | 1 |
| SafePM | comparative (with generalization to 'same guarantees') | 1 |
| SafePM | comparative (descriptive of overhead) with explanatory sub-claims | 1 |
| SafePM | comparative (descriptive) | 1 |
| SafePM | descriptive with an explanatory sub-claim | 1 |
| SafePM | descriptive (negative-result style: no errors found) supporting a design property | 1 |
| SafePM | comparative | 1 |
| SafePM | existence | 1 |
| VMSH | comparative | 2 |
| VMSH | design | 2 |
| VMSH | comparative (descriptive) | 2 |
| VMSH | descriptive (measurement of potential); used as motivation for VMSH | 1 |
| VMSH | descriptive | 1 |
| VMSH | comparative (descriptive) with an explanatory sub-claim | 1 |

## Every claim

| paper | id | level | type | cause? | overreach | statement | stuck at |
|---|---|---|---|---|---|---|---|
| Virtines | C1 | 3 | descriptive | no |  | The core components of virtual context creation comprise only a few tens of thousands of c | No uncertainty statement for Table 1; no discussion of why minimum is representative; no reconciliation with t |
| Virtines | C2 | 4 | comparative | no |  | These results suggest—provided that the virtine is short-lived (on the order of microsecon |  |
| Virtines | C3 | 4 | descriptive | no |  | Even when leveraging the underlying host OS, and when adding the from-scratch virtual cont |  |
| Virtines | C4 | 3 | comparative | no |  | By recycling virtines, we can reach latencies much lower than Linux thread creation and mu | No uncertainty for the 4% number; SGX comparison not traceable; no discussion of the ioctl/ring-transition flo |
| Virtines | C5 | 4 | comparative | no |  | At first, the relative slowdown between native function invocation and virtines with snaps |  |
| Virtines | C6 | 2 | explanatory | yes |  | With a 16MB image size, the start-up cost is 2.3ms. This amounts to roughly 6.8GB/s, which | The memcpy-bandwidth measurement (6.7 GB/s) that supports the 'memory bound' conclusion has no method, code, o |
| Virtines | C7 | 3 | comparative | no |  | However, despite the cost of these host interactions, virtines with snapshots incur only a | Uncertainty on the 12% figure; explanation of the paper-vs-appendix discrepancy (12% vs 'a little more than 2x |
| Virtines | C8 | 2 | design | no |  | Compiling OpenSSL using virtines was straightforward. [...] In all, the change took roughl | The modified OpenSSL source/diff (external unpinned repo, not archived in the Zenodo package); any record supp |
| Virtines | C9 | 3 | comparative | no |  | The virtine trial without snapshotting takes 125μs longer to execute than the baseline. [. | Attribution of the 125 us overhead to specific sources is asserted, not decomposed by measurement; no uncertai |
| Virtines | C10 | 2 | design | no |  | We present a prototype embeddable hypervisor framework, Wasp, that implements the virtine  | No Windows build instructions, no Hyper-V benchmark data documented, no numbers for 'similar'. |
| SafePM | C1 | 4 | design / existence-capabilit | no |  | We introduce SafePM, a memory safety mechanism that transparently and comprehensively dete |  |
| SafePM | C2 | 4 | comparative (with generaliza | no |  | SafePM achieves memory safety effectiveness for the PM heap equivalent to that achieved by |  |
| SafePM | C3 | 3 | comparative (descriptive of  | no | YES | Our evaluation shows that SafePM offers the same memory safety guarantees for persistent m | Uncertainty not stated (no spread); the two causal explanations for anomalies are asserted without supporting  |
| SafePM | C4 | 3 | descriptive / comparative wi | no |  | For object allocation, we observe that the overhead decreases for both atomic and transact | Uncertainty not stated; decomposition of overhead not shown although the data to show it were collected; no ex |
| SafePM | C5 | 3 | descriptive / comparative wi | yes |  | We observe that opening a pool with SafePM takes ∼30ms instead of 10ms with native PMDK, a | Uncertainty not stated; the 64 GB vs 128 GB mismatch between artifact and figure is unexplained. |
| SafePM | C6 | 2 | comparative (descriptive) | no |  | With ASan disabled, SafePM's wrappers introduce insignificant overhead (<300 μs) in the re | Source code/script for the recovery microbenchmark; how the crash is injected; raw timings. |
| SafePM | C7 | 2 | descriptive with an explanat | no |  | The persistent shadow memory always occupies one eighth of the pool which corresponds to a | Measurement method for peak usage; script for per-index red-zone overhead; raw numbers. |
| SafePM | C8 | 3 | descriptive (negative-result | no |  | We validate the crash-consistency property for both the application data and SafePM metada | Uncertainty/limits of tool-based absence-of-error evidence not discussed; the pre-existing memcheck errors in  |
| SafePM | C9 | 3 | comparative / explanatory (c | yes |  | We observe that for all three operations the relative overhead decreases as more objects a | Uncertainty not stated. |
| SafePM | C10 | 4 | existence / descriptive | no |  | Through SafePM we have also identified two memory safety bugs in the widely-used PMDK libr |  |
| VMSH | C1 | 3 | comparative / negative-resul | no |  | (i) VMSH adds no overhead for the applications running in the VM | Statement of limits of 'no overhead' (workload types, idle vs. active attached device, dependence on out-of-tr |
| VMSH | C2 | 3 | descriptive (measurement of  | no |  | (ii) de-bloating images from the Docker registry can save up to 60% of their size on avera | Coverage assumption of dynamic tracing; reconciliation of reported range; how 'still works' was validated beyo |
| VMSH | C3 | 2 | design / demonstrative (enab | no |  | (iii) VMSH enables cloud providers to offer services to customers, such as recovery shells | A measure of non-interference for the use-cases; reported outcomes (pass counts) in the paper; version of lamb |
| VMSH | C4 | 2 | descriptive / design (compat | no |  | We demonstrate VMSH approaches its goal of generality by successfully testing 4 industry l | MISMATCH: Table 1 lists kernels 5.4, 4.14, 4.9, which have no tests in the artifact; the artifact tests 5.15 a |
| VMSH | C5 | 3 | comparative / negative-resul | no |  | Out of the 619 tests, all succeed natively. For both qemu-blk and vmsh-blk, three tests (0 | Disclosure of retry handling; statement of limits (single FS, single implementation); result of sustained load |
| VMSH | C6 | 3 | comparative (descriptive) | no |  | On average, VMSH is 1.5 × ±0.6 slower than qemu-blk. | Definition of the ±; implementation used; number of runs. |
| VMSH | C7 | 3 | comparative (descriptive) wi | yes |  | Finally, vmsh-blk suffers a 94% write and 7% read overhead in throughput compared to qemu- | Reconciliation of the two sets of numbers; explicit implementation labels for the quoted percentages. |
| VMSH | C8 | 4 | comparative (descriptive) | no |  | Our measurements show that, with around 0.9ms, the latency of the VMSH console is very sim |  |
| VMSH | C9 | 1 | design | no |  | We design a system for hypervisor-independent side-loading into a VM of a generic guest-ov | Definition of 'limitations'; any measure; conditions under which the overlay cannot see guest namespaces (Sec  |

## Notes per paper

- **Virtines**: Well-packaged artifact (make artifacts.tar regenerates Table 1 and Figs 3,4,8,11,12,13,14 with gold + 9-machine reference data); gaps: appendix target 'figure1_data' absent, gold table1.csv ~10x paper values, OpenSSL code in external unpinned repo, SGX/Hyper-V/Vespid(Fig 15)/memcpy-bandwidth data absent, paper 12% HTTP throughput drop vs appendix ~2x expectation; time period taken from stated software versions (exact dates unstated for all claims) rather than failing every claim at CEA2.
- **SafePM**: Well-scripted artifact for Figs 3-7, Table 5, valgrind checks and bug repro, but the PMDK fork (with all pmembench .cfg files) is fetched by mutable branch name and absent from the zip, no raw results or variance are shipped, and Table 3 (recovery time) and Table 4 (space overhead) have no artifact support; the paper also carries an internal 2.55x/2.62x inconsistency and 'XXX' citation placeholders for the bug reports.
- **VMSH**: Artifact is code + scripts only (no raw results/figures); kernel list in Table 1 (5.4/4.14/4.9) does not match artifact tests (5.15/5.16 instead); abstract's 'no overhead' holds only for ioregionfd; several internal numeric inconsistencies (halved vs 7%/14%; 50-97% range vs <10% cases; 12 vs 10 kernel functions; host kernel 5.12.14 vs 5.15.14).