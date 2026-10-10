We thank the reviewers, who are correct that the paper overstates its claim.
The revision will **not** claim a rule for every optimizer, model, or search
space. It will report evidence for an effect that merits further study: under
the tested budgets and models, giving a small classical search result to an LLM
can improve the final result.

**B: "Novelty is low."** With respect, we disagree. Our reading of the
literature is that, overwhelmingly, the default assumption in recent SE work is
"neural only". Here, we widen that design conversation to *combinations* of
neural and symbolic methods, and show that a cheap symbolic learner placed
before the LLM beats either method alone, at lower cost (Table V). This
conversation remains very under-explored in SE. With your permission, we will
state this point clearly in the introduction.

**B: "Only two combinations explored."** Respectfully, the paper compares
seven methods (EZR, RANDOM, RRP, BS_LLM, SYNTHCORE, SNAP, SNAP2) across three
regimes (opt1, opt2, opt3; Section I). In addition, as shown below, we have now
tested a second model.

**B-Q3, C-Q2: "Test other LLMs."** Reviewers note that many open-source LLMs
are available. However, unless those models are very small, they are too large
or too slow for our local machines (particularly the Qwen models mentioned by
the reviewers), so running them would require paid cloud compute. Nevertheless,
our local machines could run a limited study (5 repeats, not 20; 20 datasets,
not 105) using GLM-5.3-Flash. Compared with the same sample from the original
model, SNAP2's small classical search again improved the LLM's result:

| Model | Datasets | Repeats | SNAP2 wins | Tied | SNAP wins |
|---|--:|--:|--:|--:|--:|
| (original) GPT-OSS-120B | 20 | 5 | 25% | 60% | 15% |
| (new) GLM-5.3-Flash | 20 | 5 | 40% | 50% | 10% |

With your permission, we will add these results to the revised paper.

As to budgets, prior work concludes that larger budgets are often impractical
[3], [23], so we study small budgets, where research is most needed and most
challenging.

**B-Q1: "How does this fit neuro-symbolic search?"** This is an insightful
comment. This paper is squarely about the neuro-symbolic combinations discussed
in Kautz's landmark lecture [Kautz20]. The paper already covers SE work on such
combinations: LLMs as evolutionary operators [17], [49], surrogates [18],
partition samplers [19], [50], and algorithm designers [21], [51]–[53]. As
Fig. 1 and Sections I–II note, within SE multi-objective optimization, the
specific combination studied here has not previously been assessed against the
alternatives. With your consent, we will extend future work to cover more
methods and regimes, including Reviewer B's suggestion of feeding SNAP2's output
back into a classical optimizer (classical→LLM→classical).

[Kautz20] H. Kautz, "The Third AI Summer," Robert S. Engelmore Memorial Award
Lecture, AAAI, 2020.

**A-Q1: "Why is classical-first better, conceptually?"** With your consent,
the revision will discuss Wolpert's No Free Lunch theorem: no single optimizer
or classifier is best for all problems. It will also note that ensemble research
shows the benefit of combining the biases of multiple learners, as EZR+SNAP does
here. We will also examine Reviewer A's exploration/exploitation hypothesis
using our search histories.

**A-Q2: "Opt2 differs internally, so order is not isolated."** This is a good
point. We built SNAP2-rev, which reverses SNAP2's order while keeping the same
prompt, parser, candidate mapping, EZR acquisition rule, initial rows, and label
budget. Paired by dataset and seed, SNAP2 kept a [VALUE] advantage at 20 labels
and [VALUE] at 50. With your consent, we will add this matched reversal to the
revision.

**A-Q3: "What distinguishes the datasets where SNAP2 wins?"** We agree this is
a fascinating question. Elsewhere we have investigated extensively which