# SE for AI: Design Patterns and Heuristics of AI Software

## Introduction

Consider the problem of 
_making an agent that wonders the world, generalizing from the past, learning what it needs to look at next,  while always reviewing and revising past ideas_.
In the general case, this is a hard problem.
But in more specific cases,
where
we restrict the problem to some  specific problem in software engineering (e.g. advise for software project
managers) then we claim that AI software becomes easily constructed, maintained, and extended.

To support that claim, this paper reflects on a  decade of research
into SE for AI.  After refactoring that work, we find that inside our
AI software, are a  small number of design patterns and heuristics.
These patterns can be used to train students of software engineering;
to define general purpose AI toolkits; and
to improve past results.  For example, in
this paper, we use these design patterns to review, revise and improve
prior results about:

- Management choices for agile or waterfall software systems;
- Configuraion options for complex software.

The above application areas are just a small sample of   how SE design
patterns and heuristics can improve AI software.  The end of this paper
we reflect on our design patterns and heuristics to offer a long list
of _paths not taken_; i.e.  SE design choices not yet tested within
AI software.  By our estimates, that list could guide another decade
(or more) of research into SE for AI.

The rest of this paper is strucutred as follows.

- Some motivation notes in the next section.
- List sof applications explored  by this research.
- Design patterns and heuristics.
- Using the patterns and heuristics to get a better results.
-  Future work: lists of patterns not taken.


Before beginning, we note two restrictions to the inquiry of this aper,
Firstly, here we report on  software agents exploring digitial data. We do
not dare (yet) to apply this work to real world robots and drones. Such
physically embodied devices must deal with hundreds of sensing and
hardware issues that we do not (yet) understand (except to say that we
understand  that those issues are the focus of much current reseach).

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
some more support (SE design principles for AI algorithms).


## Motivatin

This paper shows that  SE pracitioners and reseearchers _can_ understand more about AI
software. But do they _need_ to do so? 

We think so.  AI software
is widely deployed. It often controls What we see and do not see (on
our big data dashboards, or on our Facebook or Google feeds); Also,
it can also control What we can do or not do (as determined by polices
within software that control what functions we can access); Further,
it very useful to help us explore the world looking for new insights and
opportunities (using data mining or optimization software).  

Just to say
the obvious, AI software is still software that needs to be maintained
and extended. Software systems usually have to
be integrated with other software systems.
Software engineers are always  needed when software must be patched,
extended, customized, integrated with other software tools,  deployed,
and then maintained.  

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

## Related Work

sbse 

data mining

part of our ongoing work to unify se and AI and data mining and optimization. iprior work offered
high-level principles. here, we move to details.

cloud big data, orthogonal to this work

deep learning, again orrthgonal but we would offer 3 caustions on DL. everything we;ve looked at w cold do
mch better much simpler. may not hold for all future appcaitions but aht is our experience so far.
and others have had the same experience. secondly, see above re commissioning. time to assess ai not just on how well it does but how
east it is easy to explore options to the current methods. dl hard on taht (very bad train times). thirdly, if
dl is the answer, what was the question. here, we hae a very clear gal: support agent wondering around the
world learning from old, deciding where to look next, revising old opionions. which part of that is DL ? jsut te first third? we just dont know

## "But this isn't reallt the SE for AI"

reply1: ai is what doesnt wor. ntelligence not defined

reply2: orthogal to commerial inteest in big data, deep learning. for advocates
of those approaches we ask "if deep learning is the answer what was the question". here
we offer a rnage of services and when certain other areas can address hese services then we are in 
a combabtive situation. unti then there is nothignt o discuss since our ntentions
are so different.

reply3: many other exciting books offering core principles for AI. our personnely
fav on sorvig's paradigms of AI.

## Notation

Functions are upper case letters.

- a,b,c = throw away local vars
- D= distance
- e= entropy
- F= a function of the form ys = F(xs)
- G= a guesses function (surrogates). ys= G(xs)
- i,j= self, another instance of the same class
- K= short for "ok". checks if 
- L = the Like function. tests of we prefer eg1 over eg2. used for domination
- M = mutant
- mu = mean
- n = counts
- o= genetic container class
- p = distribution. can be a function (eg. normal) or a sample
- Q = aquisition function
- R = repair function
- sd= standard deviation
- T= tabu
- V = validation function. inputs many examples, outputs (e.g. hypervolume GD, IGD, etc)
- w= some wieght. a numeric preferenc value
- x= decision
- y= objective 

Data structures:

- ws = set of weghts
- xs= set of decisions
- ys = sef of objectives ys = f(xs)
- eg = example = a pair of xs,ys
- egs = a set of examples
- fact = an example where ys = F(xs)
- guess= an example where ys = G(xs)

currently unused:
 HUZ

## JC's tricks

Certain sampling. for all memberes of the fronet, build the local models, mutate only tose with least uncrtainty.

Linear time domination
	
- Sort examples randomly
- n = len(examples)
- a=0
- b4=egs[0]
- while a<n-1:
     - a+=1
     - now=  egs[a]
     - if L(b4,now): b4.score += 1 # shohuld this be +1 now's score?
     - if L(now,b4): b4=now; b4.score  += a

