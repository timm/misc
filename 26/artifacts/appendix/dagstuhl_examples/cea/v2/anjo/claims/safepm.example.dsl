paper: eurosys2022-safepm
title: SafePM: A Sanitizer for Persistent Memory
venue: EuroSys 2022
system: SafePM
name: system SafePM — memory-safety sanitizer for persistent memory built on PMDK and ASan
name: baseline ASan — AddressSanitizer on the system heap
name: baseline Native — unprotected binary
name: benchmark ripe — RIPE buffer-overflow attack suite
name: config pmheap — objects allocated from the PM pool allocator instead of the system heap
name: config noasan — SafePM's wrappers only, ASan disabled
note: v2 interesting claims translated by hand from SPEC v1.0; example only

# safepm:I2
provide(safety)
  provide(safety(memory, heap))
    attacks(memory, heap) depends-on allocator-interception @ §6.4 table5 p518 -- a sanitizer covers only the allocators it wraps
      attacks(memory, heap, ripe, outcome=always, Native) = 306 @ table5 p518 l10
      attacks(memory, heap, ripe, outcome=always, ASan) = 27 @ table5 p518 l11 -- system heap
      ! attacks(memory, heap, ripe, outcome=always, ASan[pmheap]) = 119 @ table5 p518 l12 -- same sanitizer, objects moved to the PM pool allocator: +92 attacks
      attacks(memory, heap, ripe, outcome=always, SafePM) = 27 @ C2 E3 table5 p518 l13 -- PM allocator made to update the metadata: back to ASan's level
  provide(safety(memory, stack))
    attacks(memory, stack, ripe, ASan[pmheap]) < 306 @ table5 p518 -- rebuttal: stack and global checks are allocator-independent, so heap coverage loss never reaches the unprotected level

# safepm:I8
provide(safety)
  provide(safety(memory, persistent))
    detections(memory, persistent, recovery) depends-on metadata-persistence @ §3.2 p510 l39-46 -- metadata rebuilt per run cannot see objects from earlier runs
      detections(memory, persistent, recovery, ASan) = 0 @ §3.2 p510 l39-41 -- argued, not measured
      detections(memory, persistent, recovery, SafePM) ? @ §6.5 -- rebuttal: no experiment plants a violation on the recovery path
      detections(memory, persistent, recovery, metadata=rebuilt-at-open) ? -- rebuttal: a heap walk at open could rebuild volatile metadata; not ruled out
preserve(correctness)
  preserve(correctness(memory, persistent))
    crash-consistency(memory, persistent, metadata, SafePM[noasan]) = preserved @ C3 E4 §6.5 p518 l27-35 -- the precondition the paper does measure: pmemcheck + memcheck, 10K ops, 0 errors beyond PMDK
