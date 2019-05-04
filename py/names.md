# Understanding Design Patterns and Heuristics for AI Software

## Introduction

At first glance, it might
seem difficult
to build
an agent that wonders the world, generalizing from the
past, 
learning what it needs to look at next,  while always reviewing and revising
past ideas.
But this is not the case,

As shown in this paper, inside such AI software,
are a  small number of repeated design patterns and heuristics.
These heuristics and patterns
succinctly summarize numerous prior systems. Also, they
can quickly find
important design choices that were not explored in the past.

The end of this paper includes a long list of 
SE design choices not yet tested within AI software.
By our estimates,
that list could guide another decade (or more) of research into SE for AI.
To demonstrate the value of this list, we 
use the design patterns in this paper
to 
revise and improve prior results about:

- Management choices for agile or waterfall software systems;
- Configuraion options for complex software.

The rest of this paper is strucutred as follows.

- Some motivation notes in the next section.
- List sof applications explored  by this research.
- Design patterns and heuristics.
- Using the patterns and heuristics to get a better results.
-  Future work: lists of patterns not taken.


Before beginning, we note two restrictions to the inquiry of this aper,
Firstly, here we report on  software agents exploring digitial data. We do not dare
(yet) to apply this work to real world robots and drones. Such physically embodied devices
must deal with hundreds
of sensing and hardware issues that we do not (yet) understand (except
to say that we understand  that those issues are the focus of much current reseach).


Seconldy, we do not discuss the _data engineering_ required before the knowledge engineering
can begin.  In our experience, it is a time consuming task to situate AI into a real world context
since that requires
building all the scripts needed to join across real-world data while, usually, cleaning
up the many quirks of that data. In fact, for real-world AI, we would recommned
two to three data engineers for knowledge engineer. That said, in our experience,
many commerical organizations already have such skilled data enginners (who mght be called
database adminstrators, or cloud service engineers, or some such). So in this paper, we
do not focus on something that is done very well in industry (data engineering).
Rather, we focus on something there industrial practitioners and resaerchers might need
some more support (design principles for AI algorithms).


## Motivatin

This paper shows that  SE pracitioners and reseearchers _can_ understand more about AI
software. But do they _need_ to do so? 

We think so.  AI software
is widely deployed. It often controls What we see and do not see (on
our big data dashboards, or on our Facebook or Google feeds); Also,
it can also control What we can do or not do (as determined by polices
within software that control what functions we can access); Further,
it very useful to help us explore the world looking for new insights and
opportunities (using data mining or optimization software).  Just to say
the obvious, AI software is still software that needs to be maintained
and extended. Another property of most software systems is that they have
to be integrated with other software systems ("no man is an island").
Software engineers are always  needed when software must be patched,
extended, customized, integrated with other software tools,  deployed,
then maintained.  

The _less_  an  engineers understand what they
are patching, extending, customizing, intergrateding, deploying and
manitaining, the _worse_ they become at  accomplishing those tasks.
THis is an important point since we know of too many examples
were commerical software engineers are making poor choices
about AI software.
Until very recently, the usual tactic for applying AI methods to SE
was to to apply some off-the-shelf tool developed by AI resarchers
to SE problems. That tactic is useful for generateing quick results
(see all the software analytics wor using the Python
sci-kit-learn toolkit  or all the search-base SE work using the JAVA jMetal toolkit)
but it can lead to sub-optimal results. (XXX config choices).
burning rhu CPU like bubble sort

Recently, Blinkely et al. raised the possibility that those AI
tools are not most suited to SE applications. XXX

Blineky;'s arugment echoes our own experiences.
Recently we developed DODGE, an automated assistant to assist in the 
tuning of AI tools to SE problems (specifically: to tune
the hyperparameters of a data miner). That tool worked very well
for SE problems (defect prediction, predicting Github close times,
bad smell detection, text mining on StackOverfow data) but
very poorly for non-SE problems (the standard examples from the UCIrvive
machine learning repository such as diabetes and "cancer"). On investigation
we found that our SE data was inherently different to other data. It turns

More generally, nfl. nnumber of better solutions 


## Notation
Functions are upper case letters.

- a,b,c = throw away local vars
- D= distance
- e= entropy
- F= a function of the form ys = F(xs)
- G= a guesses function (surrogates). ys= G(xs)
- i,j= self, another instance of the same class
- K= short for "ok". checks if 
- L = the Like function. tests of we prefer eg1 over eg2
- m = mu
- n = counts
- o= genetic container class
- p = distribution. can be a function (eg. normal) or a sample
- Q = aquisition function
- R = repair function
- s= standard deviation
- T= tabu
- x= decision
- y= objective 

Data structures:

- xs= set of decisions
- ys = sef of objectives ys = f(xs)
- eg = example = a pair of xs,ys
- egs = a set of examples
- fact = an example where ys = F(xs)
- guess= an example where ys = G(xs)

currently unused:
 UVWZ

