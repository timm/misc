paper: eurosys2022-vmsh
title: VMSH: Hypervisor-agnostic Guest Overlays for VMs
venue: EuroSys 2022
system: VMSH
name: system VMSH — side-loads a guest overlay into a running VM without hypervisor cooperation
name: baseline Qemu — QEMU's built-in virtio block device
name: baseline Docker — unmodified top-40 Docker Hub images
name: benchmark fio — fio block benchmark, direct I/O
name: benchmark docker — shrinking the top-40 Docker Hub images to the files they open
name: config wrapsyscall — attach by ptrace-wrapping the hypervisor's KVM_RUN
name: config ioregionfd — attach through the in-kernel ioregionfd filter
note: v2 interesting claims translated by hand from SPEC v1.0; example only

# vmsh:I2
explain(overhead)
  explain(overhead(vm, interposition))
    slowdown(vm, interposition) increasing-in exit-rate @ §6.3 fig6 p688 -- every exit is charged, not only the interposer's
      slowdown(vm, interposition, fio, bs=256KiB, VMSH[wrapsyscall]) = 1.5× vs Qemu @ fig6 p688 l68-69 -- bandwidth
      slowdown(vm, interposition, fio, bs=4KiB, VMSH[wrapsyscall]) = 6× vs Qemu @ fig6 p688 l69 -- IOPS: four times the bandwidth penalty
      slowdown(vm, interposition, fio, VMSH[ioregionfd]) = 1× vs Qemu @ C2 E5 fig6 p688 l63-66 -- in-kernel MMIO filter removes the charge
      slowdown(vm, interposition, cpu-bound) ? @ §6.3 -- rebuttal: few exits, penalty expected small, not measured

# vmsh:I12
reduce(footprint)
  reduce(footprint(image, pruned))
    size(image, pruned) depends-on userland-share @ §6.4 fig8 p689 l50-56 -- what is removed is the distro userland, not the application
      size(image, pruned, docker, VMSH) = -60% vs Docker @ C3 E7 fig8 p689 l50 -- mean over the top-40 Docker Hub images
      size(image, pruned, docker, userland=distro, VMSH) ∈ [-97, -50]% vs Docker @ fig8 p689 l49-50
      ! size(image, pruned, docker, userland=static-go, VMSH) > -10% vs Docker @ §6.4 p689 l53-56 -- 3 of 40 images; outside the sibling's range
      size(image, pruned, vm) ≤ size(image, pruned, container) ? @ §6.4 p689 l57-62 -- a fortiori: VM images carry more tooling; asserted, not measured
