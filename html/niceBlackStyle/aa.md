
<img align=right width=200 src="https://64.media.tumblr.com/a2f91e8e8ff64d654762fa0a0bd297ea/749dcbc3d2ac7566-d9/s1280x1920/76372b6b2a3f1865c5c9ff1a7f247f3d872604d3.png">

# SE and AI: a programmer's guide to smarter scripting
Tim Menzies   
timm@ieee.org    
Jul 17, 2023


<em>
This book is a set of 101 tips and tricks and shortcuts for software engineering and knowledge engineering.
It is based on years of teaching this stuff at a gradaute level. 
The 
tips and tricks and shortcuts and organized into different layers such as

-  working working with teams;
- doing systems-level programming;
-  for programming (in general) and for  scripting in Python;
-  for data mining
-  for optimization,
- for explainable-AI.

Why read it? Well,
AI software is still software [^me20a]. To write better AI, we need better software engineering
 that us deliver more functionality, in less time, with less code.
Also, AI software searches for solution. So to write smarter code, 
we need better knowledge engineering that finds
the shortcuts that let us search faster.

Good things happen when we combine better AI with better SE
(e.g. we use the AI tools to find tests for software systems).
For example,
here we implment a very simple, yet very effective,
multi-objective semi-supervised rule-based explanations.
That code cuts up large problems into tiny pieces, then looks for
patterns in the pieces.  That process only look at the
important differences between pieces (so you don't waste time on
irrelevancies).  This means you can (e.g.) explore 10,000 examples
by only looking at a few dozen of them.  And when you are done,
there is a nice simple set of rules that explains exactly what you
learnt.
The code has numerous applications in AI and SE (e.g. test case generation, explanation, anomaly detection, data synethsis, etc etc see rest of book)

Explaining this code lets us review the shortcuts
that simplify AI and SE.
The code base is short, simple, free to could be used as the basis for a self-taught course on SE and AI.
ALso, it could be the basis of multiple graduate-level softare engineering and knowledge engineering subjects.

Share and enjoy!
</em>

------

[TOC]

<!-- tocstop -->

-------

$a^2$

<i class="fa-solid fa-coffee"></i> 

asdasd asdd asdd asd asd asd asdas
adasasddasads
asdasdasa

```python
def showRule(rule):
   def spread(ins):
      i,outs = 0,[]
      while i < len(ins):
         c,lo,hi = ins[i]
         while i < len(a) - 1 and hi == ins[i+1][1]:
            hi = ins[i+1][2]
            i += 1
         outs += [(c,lo,hi)]
         i += 1
      return tuple(outs)
   return tuple(spread(v) for k,v in rule.items())
```

	
asd
as
das
das
a
asdasdsdasdasdasasd

## asdas das das

asdasdas asd as das das das das das das asd as dasd asdasda
asdasdasdasdas asd asd asd as das da adasda

## asdasdsad

asda


### asdasas

asdasdas

<small>


[^me20a]: Tim Menzies. 2020. The Five Laws of SE for AI. IEEE Softw. 37, 1 (Jan.-Feb. 2020), 81–85. https://doi.org/10.1109/MS.2019.2954841

