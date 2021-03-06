<img align=right width=200 src="https://acmesabertooth.com/images/under.png">

_(Incomplete quick notes. **Not** complete. Please check back in, later.)_

# Summer Research, 2019

"**If Deep Learning is the answer, what was the question?**"     
-- [Tim Menzies](http://menzies.us)

## Goal

Human-in-the-loop AI for SE. Generalize a "frugal" representation to N goals. Optimize that "frugal" reasoner.

## Background reading 

(Currently this is an incomplete list):

- My prior work on this: [FSE'18](https://arxiv.org/pdf/1803.05067.pdf)
- [R project package](https://cran.r-project.org/web/packages/FFTrees/vignettes/guide.html) (code is slow, does not 
  generalize to N goals)
- Theory: 
    - 2008: [Why Heuristics work](https://pure.mpg.de/rest/items/item_2100099/component/file_2100098/content)
    - 2003: [Naive yet Englightenned](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.412.6594&rep=rep1&type=pdf)
- Other work on "[comprehensability](http://papers.nips.cc/paper/7062-a-unified-approach-to-interpreting-model-predictions)"


## The  Manifesto

From  [FSE'18](https://arxiv.org/pdf/1803.05067.pdf)

Just look at all the stuff we want to do with software analytics:

![](https://i.imgur.com/O5AVwGF.png?width=489)

For all this to work, _people_ are going to have to _talk_ about what they need,
what they can see in the data, and how to apply what they see to what they need.
So our models must be **comprehensible**.

Why Demand Comprehensibility? This work assumes that better data mining
algorithms are better at explaining their models to humans. But is that
always the case?

The obvious counter-argument is that if no human ever needs to understand
our audited model, then it does not need to be comprehensible:

- For
example, a neural net could control the carburetor of an internal
combustion engine since that carburetor will never dispute the model or
ask for clarification of any of its reasoning.
- On the other hand, if a model is to be used to persuade software engineers
to change what they are doing, it needs to be comprehensible so humans
can debate the merits of its conclusions.

Several researchers demand that
software analytics models needs to be expressed in a simple way that is
easy for software practitioners to interpret [16, 39, 45]. 

- According
to Kim et al. [32], software analytics aim to obtain actionable
insights from software artifacts that help practitioners accomplish
tasks related to software development, systems, and users.
-  Other
researchers [64] argue that for software vendors, managers, developers
and users, such comprehensible insights are the core deliverable of
software analytics. 
- Sawyer et al. comments that actionable insight is
the key driver for businesses to invest in data analytics initiatives
[62]. 

Accordingly, much research focuses on the generation of simple
models, or make blackbox models more explainable, so that human engineers
can understand and appropriately trust the decisions made by software
analytics models [1, 19].
It turns out that if a model is not comprehensible, there are several explanation algorithms
that might mitigate that problem. For example:

- In secondary learning, the examples given to a neural network are used to train a rule-based learner and those learners could be said to “explain” the neural net [13].
- In contrast set learning for instance-based reasoning, data is clustered and users are shown the difference between a few exemplars selected from each cluster [35].

Such explanation facilities are post-processors to the original learning method. An alternative simpler approach would be to use learners that generate comprehensible models in the first place.

Hence, this work.
