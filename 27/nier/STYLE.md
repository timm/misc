# Style guide for the NIER paper

Target: ICSE 2027 NIER. 4 pages main text + 1 page references.
`\documentclass[10pt,conference]{IEEEtran}`. Double-anonymous: no author
names, no acknowledgements in the submission (both go in camera-ready only;
cite Menzies prior work in third person).

## The 26 identifiers of LLM text (avoid all of these)

1. "delve", "delve into", "dive deep".
2. "leverage" and "utilize" where "use" would do.
3. "robust", "comprehensive", "seamless" as empty amplifiers.
4. "crucial", "pivotal", "paramount", "vital role".
5. "landscape", "realm", "ecosystem" as spatial metaphor for a field.
6. "In today's rapidly evolving world of ..." openers.
7. "It is important to note that ..." / "It is worth noting that ...".
8. "Moreover," "Furthermore," "Additionally," as paragraph starters.
9. The rule of three: "fast, cheap, and reliable" triplets everywhere.
10. "not only X but also Y" / "not just X, but Y" constructions.
11. Antithesis tic: "It's not about X; it's about Y."
12. Bullet lists in place of argument (the paper argues in prose).
13. Bold-term-plus-colon lists ("**Impact:** ...", "**Novelty:** ...").
14. Em-dash overuse — three per paragraph — like this.
15. Hedging stacks: "may potentially suggest", "could arguably indicate".
16. Vague quantifiers: "significantly", "a wide range of", "numerous",
    where a number belongs.
17. "In conclusion," and summary sentences that restate the last paragraph.
18. Restating the question before answering it.
19. "Firstly, ... Secondly, ... Finally, ..." scaffolding.
20. Excess signposting: "As mentioned above", "As we will see below".
21. Every paragraph the same length (3-4 sentences, metronome rhythm).
22. Perfectly parallel section structure; symmetric section lengths.
23. "foster", "underscore", "harness", "unlock", "empower".
24. "a testament to", "stands as", "serves as".
25. "Interestingly," / "Notably," / "Remarkably," sentence openers.
26. Uniform polished tone: no fragments, no asides, no questions, no jokes,
    nothing at stake. Human papers take positions and take risks.

## What to do instead (the Menzies register)

Ask questions, then answer them. Section titles can be questions
("Why easier AI?"). Openings can be questions ("Do all AI tasks in SE
require large and complex models?"). Then answer fast, in one short
sentence.

Short sentences. One idea per sentence. If a sentence needs a second
comma, split it.

Concrete numbers over adjectives. Not "much faster": "runs 100 times
faster". Not "many datasets": "127 datasets, 93 to 100,000 rows".

First person plural. "We ask this since...". "We hence propose...".
Direct claims, plainly staked: "The counter-evidence presented in this
paper suggests otherwise."

Argue in paragraphs, not lists. Where the talk had a bullet slide, the
paper gets a paragraph with the same facts in sequence. A table is fine;
a dot list is not.

Colloquial pivots are allowed, sparingly: "To say that another way",
"Perhaps not", "But do all SE tasks need such complexity?".

History and provenance as evidence. Ockham 1300, Pearson 1902,
prototypes 1974, active learning 2009. Old ideas lend weight; use dates.

Parenthetical asides for examples and caveats (e.g., like this one).

One caveat stated early and honestly: not everything simplifies;
generative tasks need LLMs. Then move on.

End sections with a hook to the next question, the way the talk does
("so how? ...", "and how fast? ...") but in prose, not stage direction.
