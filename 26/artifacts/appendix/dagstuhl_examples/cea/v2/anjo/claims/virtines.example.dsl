paper: eurosys2022-isolating-at-the-hardware-limit-with-virtines
title: Isolating Functions at the Hardware Limit with Virtines
venue: EuroSys 2022
system: Wasp
name: system Wasp — runtime that creates and runs virtines
name: baseline vmrun — the bare VM-entry instruction; the hardware floor
name: baseline Native — the same program without virtines
name: baseline Linux — a full Linux VM booted under QEMU/KVM
name: benchmark fib — recursive Fibonacci(n) as a virtine call
name: benchmark js — a Duktape JavaScript function inside a virtine
name: benchmark openssl — OpenSSL aes-128-cbc with the block cipher in a virtine
name: config snapshot — start the virtine from a pre-initialised memory image
name: config cache — reuse a pooled, pre-created context
name: config noteardown — keep the context after the call
note: v2 interesting claims (cea/v2/CLAIMS.md) translated by hand from SPEC v1.0; example only

# virtines:I1
explain(performance)
  explain(latency(vm, start))
    latency(vm, start) depends-on guest-software @ §4.2 p647 l37-42 -- not on the virtualization hardware
      latency(vm, start, cold, image=KB) ∈ [1, 40] µs @ table1 fig8 p648 p650 -- context create + enter ≈ one host VM-enter syscall
        latency(vm, start, cold, image=KB, Wasp) < 12 µs @ C1 E1 table1 p648 l29-30 -- 30K cycles, 64-bit mode
        latency(vm, start, cold, image=KB, Wasp[cache]) ≤ 1.04× vs vmrun @ C4 E4 fig8 p650 l60-62 -- warm pool, same image
      latency(vm, start, cold, image=GB, Linux) ∈ [100, 1000] ms @ §4.2 p647 l37-38 -- the denied default: QEMU/KVM full boot; ACPI + PCI + rootfs ≈ 30% of it
      latency(vm, start, cold, image>2MB) increasing-in image-size @ fig12 p653 -- rebuttal: past the copy knee the image, not the hardware, is the cost

# virtines:I3
bound(overhead)
  bound(overhead(isolation, call))
    slowdown(isolation, call) decreasing-in work @ §6.1 fig11 p652 -- fixed cost c per call; slowdown = 1 + c/w
      slowdown(isolation, call, fib, n=0, Wasp[snapshot]) = 6.6× vs Native @ C5 E5 fig11 p652 l26-27
      slowdown(isolation, call, fib, n=25, Wasp[snapshot]) = 1.03× vs Native @ fig11 p652 l30-31 -- w ≈ 10c: negligible
      slowdown(isolation, call, fib, n=30, Wasp[snapshot]) = 1.01× vs Native @ fig11 p652 l31
      ! slowdown(isolation, call, openssl, Wasp[snapshot]) = 17× vs Native @ §6.4 p654 l3-6 -- rebuttal: w ≪ c (16 KB AES blocks, thousands of calls per second)
    ! slowdown(isolation, call, io-bound) increasing-in exits @ §6.3 p653 -- rebuttal: host exits add ≈5 µs each and scale with I/O count, not with w
explain(performance)
  explain(amortization(isolation, call))
    amortization(isolation, call, Wasp[snapshot]) ≈ 100 µs @ C5 p652 l33-34 -- work after which a call costs about what a native call costs

# virtines:I5
explain(performance)
  explain(speedup(isolation, snapshot))
    speedup(isolation, snapshot) increasing-in init-work @ §6.1 §6.5 p652 p654 -- pays only when skipped init exceeds image restore
      speedup(isolation, snapshot, fib, n=0, Wasp) = 2.5× vs Wasp @ fig11 p652 l47-50
      speedup(isolation, snapshot, js, Wasp) ≈ 2× vs Wasp @ fig14 p654 l21-25 -- Duktape init skipped
      speedup(isolation, snapshot, image>2MB) decreasing-in image-size @ fig12 p653 l4-7 -- restore cost grows with the image
      speedup(isolation, snapshot, memory-bandwidth=limited) ? @ E7 p662 l36-38 -- appendix warns it "may actually reduce performance"; never measured

# virtines:I7
improve(performance)
  improve(performance(isolation))
    latency(isolation, call) < 1× vs Native @ §6.5 fig14 p654 -- with retained runtime state the isolated path beats native by skipping init + teardown
      latency(isolation, call, js, Wasp[snapshot,noteardown]) = 137 µs @ fig14 p654 l36-38
      latency(isolation, call, js, Native) = 419 µs @ fig14 p654 l16
      latency(isolation, call, js, Wasp[snapshot,noteardown]) = 0.33× vs Native @ fig14 p654 -- derived 137/419
    ! safety(isolation, call, state=shared) ? @ §5.2 p650 l47-52 -- rebuttal: retained state is exposed to every later virtine; not measured
