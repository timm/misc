# q.md — SECRET exam bank: questions 101-120

Companion to lull/tut.md's revision guide (gates 4-233 are
public there, with answers). These 20 cover the highest
gates: lecture 10's statistics (236-261) and the Lua
appendix (1002-1045). Protocol as in the guide: part a =
definitional, attempted from memory; part b = a small
artifact or protocol with ONE conceptual error — name the
error, its consequence, and the fix in English (never
code). Both parts open with context; answers follow.

---

**Q236** — Lecture 10 judges optimizers by comparing score
DISTRIBUTIONS, starting with an effect-size measure.
a. Cliff's delta: what does it measure, in plain words, and
what do 0 and 1 mean?
b. A 20-page evaluation reports, for every pair of methods,
only the p-value of a significance test — no effect sizes
anywhere — and recommends the "most significant" method.
Mistake, consequence, fix ([EFF](#glossary))?

**Q238** — The KS test compares two samples by their
cumulative distributions ([PDF/CDF](#glossary)).
a. Define the KS statistic geometrically — two staircases.
b. A team verifies "our new test data matches production"
by comparing only the two means. The distributions differ
wildly in shape (production is bimodal). Mistake,
consequence, fix?

**Q240** — `[240]` reseeds before building the very samples
the noise-floor demo compares.
a. Why must a published comparison's random inputs be
reseeded ([SEED](#glossary))?
b. A benchmark harness generates fresh unseeded workloads
for every run; week-to-week dashboards then "show" the
service getting faster and slower. Mistake, consequence,
fix?

**Q243** — At `[243]`, two samples drawn from the SAME
distribution showed Cliff's delta 0.10 at n=50.
a. What is that phenomenon called here, and what is the
lesson for reading raw effect sizes?
b. A blog post compares two JS frameworks on n=15 page
loads each, finds delta 0.24, and headlines "Framework A
is measurably faster." Mistake, consequence, fix?

**Q245** — `l.same(xs, ys)` is not one test but a chain.
a. Name its three gates, in order, and what each asks.
b. A team replaces the chain with "different if the means
differ by more than 5%" — one gate, no ranks, no scaling
by n. On noisy small samples it fires constantly; on huge
samples it misses real shifts. Mistake, consequence, fix?

**Q248** — At `[250]`-`[252]`, the same 0.1 shift passed
`same` at n=10 and failed at n=1000.
a. Why — what does the KS threshold do as n grows?
b. A reliability team concludes "the regression is gone"
after a retest with one-tenth the original sample size
comes back "no difference". Mistake, consequence, fix
([SAMP](#glossary))?

**Q251** — [SAMP](#glossary): the course's closing claim is
that every result is a claim about a sample.
a. Sample SIZE controls one thing, sample SOURCE another.
Which two?
b. A config study samples workloads exclusively from the
team's own CI runs, then claims the tuned config is best
"for this software generally". Mistake, consequence, fix?

**Q253** — `[253]` accepted a statistically real difference
as "engineering-same" via the eps argument.
a. Reconcile: how can "different" and "same" both be
right?
b. A student sets eps AFTER seeing the results, choosing
the exact value that makes their method tie the leader.
Mistake, consequence, fix?

**Q255** — topTier sorts methods by mean, then walks down
admitting methods while they remain `same` as the best.
a. What ends the walk, and what does membership in the
tier mean?
b. A leaderboard reports only rank 1 — "the winner" — from
data where the top four methods are statistically
indistinguishable. Mistake, consequence, fix
([TIER](#glossary))?

**Q259** — In lecture 8's single seed-1 run at a 1000-eval
budget, ls BEAT sa (0.0746 vs 0.0926); yet the five-seed
tier at `[261]` holds sa alone ({0.09..0.15} vs
{0.14..0.46}).
a. Reconcile: why is excluding ls correct?
b. A team adopts the method that produced "the best run
we've ever seen" over the method with the tighter
distribution and equal mean. Six months later results are
erratic. Mistake, consequence, fix?

**Q261** — The course closes: "statistics proposes,
engineering disposes."
a. Unpack that, citing where engineering judgment enters
`l.same`.
b. A capstone reports "our mean beat the baseline's mean
over 20 seeds, therefore ours is better" — no spread, no
effect size, no tie test. Mistake, consequence, fix?

**Q1002** — Lua 101 (`[1002]`+): the language has one data
structure and 1-based arrays.
a. Which five types does this course touch, and which one
is "the only data structure"?
b. A C programmer ports a loop assuming arrays start at 0;
the code runs without error and quietly processes one
element less than intended everywhere. Mistake,
consequence, fix?

**Q1007** — `#t` is Lua's length operator (`[1007]`-ish
territory: tables as arrays vs dictionaries).
a. What part of a table does `#t` measure, and when is its
answer undefined?
b. A function "counts the settings" with `#the` and reports
0; the developer concludes the config failed to load and
adds a retry loop. Mistake, consequence, fix?

**Q1012** — lull sorts keys before printing because Lua's
dictionary iteration has no guaranteed order (`[1012]`).
a. pairs vs ipairs: which is ordered, over what?
b. The classic Lua ternary `cond and a or b` is used where
`a` is a boolean flag that can be false. The expression
silently returns `b` in exactly those cases. Mistake,
consequence, fix?

**Q1018** — Closures — functions that capture surrounding
variables — are lull's main structuring tool (`[1018]`).
a. Define a closure; name two lull values that are
closures.
b. A loop builds three "independent" counters but declares
the counted variable OUTSIDE the loop (a global). All
three counters turn out to share one count. Mistake,
consequence, fix?

**Q1031** — lull's house style declares locals in the
signature after a 4-space gap (`[1031]`).
a. In `function m.slice(t,lo,hi,    u,n)`, what are u and
n?
b. A refactor "cleans up" a helper by deleting what looked
like a stray `local` keyword. The function still passes
its own tests, but a week later two unrelated features
corrupt each other's state. Mistake, consequence, fix —
and which lull exit-time check exists to catch exactly
this?

**Q1036** — Lua has no class keyword; lull's entire object
system is one line of metatable wiring (`[1036]`).
a. What does `__index` mean, and how does `l.new` use it?
b. A new class is added by copying Account.new but the
author omits the line pointing the metatable's `__index`
at the class. Constructing works; the first METHOD call
crashes with "attempt to call a nil value". Mistake,
consequence, fix?

**Q1040** — Method-call sugar: lull writes `obj:add(v)`
everywhere (`[1040]`).
a. What exact call is `acc:add(50)` sugar for, and what is
`i` in the method bodies?
b. A student calls `s.mid()` (dot, no argument) on a Sym
and hits "index a nil value (local 'i')". They conclude
the library is broken. Mistake, consequence, fix?

**Q1044** — lull survives its own failing tests: each eg
runs under protected call (`[1044]`).
a. What does `pcall(f)` return, in what order — and which
lull function uses it so `--all` survives one bad test?
b. An error handler captures only pcall's FIRST return
value, then tries to print "the error message". The
handler itself then crashes, masking the original error.
Mistake, consequence, fix?

**Q1045** — `require` finds modules via a search path that
includes the current directory (`[1045]`).
a. Why must the course REPL start inside the lull
checkout?
b. From ~/Downloads, `require"lib"` fails with "module not
found"; the student reinstalls Lua twice. Mistake,
consequence, fix?

---

# Answers

**236.** a) How often one sample's values beat the
other's, beyond chance symmetry: 0 = coin flip, 1 = total
separation. b) Mistake: significance without effect size.
Consequence: with enough samples everything is
"significant"; the recommendation may rest on a
practically irrelevant difference. Fix: report and gate on
effect size first, significance second ([EFF]).

**238.** a) Plot both samples' empirical CDFs as rising
staircases; KS = the largest vertical gap between them,
anywhere. b) Mistake: comparing distributions by means
alone. Consequence: a bimodal production load "matches" a
unimodal test load — tests pass, production surprises.
Fix: compare SHAPES (KS or similar), not single moments.

**240.** a) So the comparison's inputs are identical and
the result is re-runnable; otherwise differences may be
the inputs', not the system's. b) Mistake: unseeded
workload generation in a longitudinal benchmark.
Consequence: the dashboard tracks workload luck, not the
service; trends are noise. Fix: fixed seeded workloads per
comparison; vary seeds only deliberately, across repeats.

**243.** a) The noise floor: finite same-source samples
show nonzero effect sizes; raw deltas mean nothing without
thresholds. b) Mistake: reading 0.24 at n=15 as signal.
Consequence: a headline built inside the noise floor; the
next 15 loads could reverse it. Fix: thresholds + a
shape test ([EFF]), or much more data, before claims.

**245.** a) Median gap <= eps ("too small to matter?");
Cliff's delta <= threshold ("effect big enough?"); scaled
KS ("shapes plausibly differ?"). b) Mistake: one
unscaled mean-gap gate. Consequence: over-alarms on small
noisy samples, under-alarms on huge ones — exactly the two
failure modes the chain's n-aware gates prevent. Fix: rank
-based effect size plus an n-scaled shape test; keep eps
as the engineering gate.

**248.** a) The threshold shrinks ~1/sqrt(n): more data,
less slack, so fixed shifts become detectable. b) Mistake:
"absence of evidence" at low power read as "evidence of
absence". Consequence: a real regression declared gone
because the retest couldn't see it. Fix: retest at
comparable (or computed-for-power) sample size
([SAMP]).

**251.** a) Size sets what you can DETECT; source sets
what you may GENERALIZE to. b) Mistake: sample source
narrower than the claim. Consequence: tuned-to-CI config
presented as universal; users outside that workload
inherit a worse system. Fix: sample the population the
claim names, or shrink the claim.

**253.** a) A difference can be real (detectable) yet
smaller than any difference that changes a decision; eps
encodes the decision threshold. b) Mistake: post-hoc
threshold shopping. Consequence: eps becomes a knob for
reaching desired conclusions — indistinguishable from
p-hacking. Fix: fix eps before looking, justified by
domain costs.

**255.** a) The walk ends at the first method NOT `same`
as the best; tier membership = statistically tied for
first. b) Mistake: reporting a unique winner among
statistical ties. Consequence: rank 1 is seed luck;
adopters chase noise and results don't replicate. Fix:
report the whole tier; choose within it on other grounds
(cost, simplicity).

**259.** a) One winning run is an anecdote; ls's sprawl
({0.14..0.46} at this budget; one lucky 0.0746 at another)
is indistinguishable from luck while sa's tight cluster is
reliable — tiers compare distributions.
b) Mistake: selecting on the best-ever observation.
Consequence: production inherits the variance, not the
highlight; results are erratic exactly as the spread
predicted. Fix: select on distributions ([TIER]);
celebrate tight, not lucky.

**261.** a) Tests only say "distinguishable given this
data"; whether it MATTERS is a judgment about costs —
which enters l.same as the eps argument. b) Mistake:
means without spread, effect size, or tie test.
Consequence: with overlapping distributions the ordering
can flip next week; "better" is unsupported. Fix: report
distributions; gate with same/topTier; state effect size
— then conclude.

**1002.** a) nil, boolean, number, string, table — and the
table is the only data structure. b) Mistake: 0-based
habits in a 1-based language. Consequence: t[0] is a
silent dictionary entry, loops skip element one — no
error, wrong answers everywhere. Fix: arrays start at 1;
let ipairs do the indexing.

**1007.** a) The consecutive-integer-key array part;
undefined when the array has nil holes (and 0 for pure
dictionaries). b) Mistake: # applied to a dictionary.
Consequence: 0 misread as "config missing"; a retry loop
built around a non-bug. Fix: count dictionary entries by
iterating pairs.

**1012.** a) ipairs is ordered over the array part; pairs
covers everything, unordered. b) Mistake: and-or ternary
with a falsy "then" value. Consequence: the false branch
is unreachable — flag=false silently becomes b; logic
inverts rarely and confusingly. Fix: use a real if-else
whenever the middle value can be false or nil.

**1018.** a) A function plus the live variables it
captured; e.g. `l.lt(n)` comparators, `d:dxdy()` views
(and every lapps stepper). b) Mistake: shared (global)
state where per-closure state was intended. Consequence:
all closures mutate one variable — "independent" counters
agree forever. Fix: declare the captured variable local
inside the loop so each closure owns one.

**1031.** a) Locals, pre-declared in the signature — not
arguments. b) Mistake: deleting a `local` made a
temporary global. Consequence: cross-feature state
corruption — the worst kind of bug: distant, intermittent.
Fix: temporaries live in the signature gap or behind
`local`; lull's exit-time `rogue?` check exists to name
exactly these leaks.

**1036.** a) "If a key is missing, look it up over there";
l.new sets a class as its own instances' `__index` so
methods resolve. b) Mistake: missing `__index` wiring.
Consequence: instances hold data but find no methods —
first method call dies. Fix: point the metatable's
`__index` at the class (l.new does both lines in one).

**1040.** a) `acc.add(acc, 50)` — the colon passes the
receiver, bound to `i` in the body. b) Mistake: dot call
with no receiver. Consequence: `i` is nil; the error
blames the library though the call site dropped the
object. Fix: call with the colon (or pass the object
explicitly as the first argument).

**1044.** a) ok (boolean) first, then results or the
error message; `l.run1` wraps each eg so one failure
cannot kill `--all`. b) Mistake: capturing ok but not the
message, then using an undefined variable as "the error".
Consequence: the handler's own crash masks the real
failure — debugging the debugger. Fix: capture both
returns; print the second.

**1045.** a) Because `package.path` resolves `require`
relative to the working directory — ./?.lua finds lib.lua
only from the checkout. b) Mistake: an environment problem
attacked at the wrong layer (reinstalls). Consequence:
hours lost; the path issue remains. Fix: start the REPL in
the lull directory (or extend the package path) — read
the error's path list before reaching for installers.
