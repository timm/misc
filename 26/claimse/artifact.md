# Claims ↔ Evidence

Claim = indicator op value, about a construct. Data →d2e→ evidence →e2r→ result; data →d2c→ indicator. Example: Virtines (EuroSys 2022); paths from its artifact, other cells illustrative.

**Claims**
| id | contribution | construct | indicator | op | value | paper @ | reasoning @ | results | judgment |
|---|---|---|---|---|---|---|---|---|---|
| C1 | fast isolated functions | K1 | boot time to echo reply | < | 1 ms | §4.2 fig4 p648 | p649 l15-18 | R1 | to assess |

**Constructs** (indicator = measurable stand-in)
| id | construct | meaning | indicator | unit | why it represents @ |
|---|---|---|---|---|---|
| K1 | cold-start latency | request to function running in fresh context | boot milestone time per trial | µs | p649 l15-18 |

**Items** (data, evidence, result)
| id | kind | what | location | cached |
|---|---|---|---|---|
| D1 | data | raw boot timestamps, 1,000 trials | made at run time | no |
| E1 | evidence | per-trial echo timings | `data_example/gold/data/fig4/echo-server.csv` | yes |
| R1 | result | median cold-start latency | fig4 p648 | redrawable |

**Steps** (status: established, speculative, missing, inaccessible, not examined)
| id | kind | from | to | code | run | cost | status |
|---|---|---|---|---|---|---|---|
| T1 | d2c | D1 | K1 | `test/echo_server/` | `make fig4_data` | 5 min | not examined |
| T2 | d2e | D1 | E1 | `Makefile:fig4_data l124-128` | `make fig4_data` | 5 min | not examined |
| T3 | e2r | E1 | R1 | `plotgen/fig4-boot-milestones.py` | `python plotgen/fig4-boot-milestones.py` | 1 min | not examined |

**Rubric** (per claim; level = highest level whose checks, and all lower ones, pass; article = lowest level over its core claims, plus counts per level; `old #` = check number in the earlier three-depth rubric)
| # | level | check | runs | old # |
|---|---|---|---|---|
| 1 | iron | claim names a contribution, and its construct exists in Constructs | nothing | 1 |
| 2 | iron | paper @ gives section and page, plus figure, table or line where possible | nothing | 4 |
| 3 | iron | op value is a number with unit or tolerance, or a qualitative op (up, down) | nothing | 5 |
| 4 | bronze | claim lists at least one result, or a step with status missing | nothing | 2 |
| 5 | bronze | every referenced id exists, and every item and step is used by some claim | nothing | 3 |
| 6 | bronze | result traces e2r ← evidence ← d2e ← data, and the indicator has a d2c | nothing | new |
| 7 | bronze | every step's code and run name a real file or Makefile target | nothing | 13 |
| 8 | silver | reasoning @ and why-it-represents @ give page locations | nothing | new |
| 9 | silver | result value at its paper location satisfies op value | nothing | 6 |
| 10 | gold | cached evidence file exists in the deposit | nothing | 7 |
| 11 | gold | value read from cached evidence satisfies op value | nothing | 8 |
| 12 | gold | e2r code exists and reads only cached evidence | nothing | 9 |
| 13 | gold | reading the code shows each step does what it claims (status established) | nothing | new |
| 14 | gold | d2e output path matches the cached evidence location, apart from the cache folder | nothing | 14 |
| 15 | gold | every README setup step used by a run has a pass/fail check | nothing | 11 |
| 16 | gold | every run's setup steps exist in the README | nothing | 12 |
| 17 | gold | every step states its cost | nothing | 15 |
| 18 | gold | running e2r on cached evidence redraws the result in the paper | plotting on shipped data | 10 |
| 19 | diamond | rerunning d2e and d2c from data regenerates evidence matching the cache within tolerance | full experiments | new |

A step with status missing fails check 6, which caps the claim at iron. Judgment: supports, insufficient, conflicting, unresolved. Setup lives in the README.
