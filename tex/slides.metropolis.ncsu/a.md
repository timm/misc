---
title: "AI Playground"
author: "Ashwin Kumar Karnad"
theme: metropolis
date: "March 22, 2020"
fontsize: 9pt
fontfamily: "Fira Sans"
monofont: "Fira Code"
monofontoptions: "Scale=0.85"
aspectratio: 169
colorlinks: true
header-includes:
  - \input{header.tex}
  - \usepackage{minted}
---



# What's AI x?

- Artificial Intelligence- The ability of machine to think and behave like humans.
- How does the machine learn on its own? - That is called Machine Learning. ML is the study of computer algorithms that improve automatically with experience.
- Just like humans learn with experience - Machines also learn with experience!
- Examples of common AI? Alexa, Siri, Google Home, Self Driving Cars, Robots etc.

# What's out there?

![Verticles](art1.png)


# How do computers make decisions?


\begin{wrapfigure}{r}{0.3\textwidth}  % 'r' for right, width 30% of text width
  \centering
  \includegraphics[width=.2\textwidth]{art1.png}
\end{wrapfigure}


Conditional statements are used to perform different actions based on different conditions.

- In many programming languages, decisions (also called conditionals) take the form of an if-then construct. They start with a condition, which is then evaluated as either True or False.
- Conditional statements are used to perform different actions based on different conditions.
- In many programming languages, decisions (also called conditionals) take the form of an if-then construct. They start with a condition, which is then evaluated as either True or False.


# Let's Build that

\begingroup
\tiny
```python
# Define a numerical column with statistics.
def Num(txt: str = " ", at: int = 0) -> Obj:
   return Obj(it=Num, txt=txt, at=at, n=0, mu=0, sd=0, m2=0, hi=-BIG, lo=BIG,
             goal = 0 if txt[-1]=="-" else 1)

# Define a symbolic column with frequency counts.
def Sym(txt: str = " ", at: int = 0) -> Obj:
   return Obj(it=Sym, txt=txt, at=at, n=0, has={}, most=0, mode=None)

# Define a collection of columns with metadata.
def Cols(names: List[str]) -> Obj:
   x,y,lst,klass = [], [], [], None
   for col in [(Num if s[0].isupper() else Sym)(s,n) for n,s in enumerate(names)]:
      lst.append(col)
      if col.txt[-1] != "X":
         (y if col.txt[-1] in "+-!" else x).append(col)
         if col.txt[-1] == "!": klass=col
   return Obj(it=Cols, names=names, all=lst, x=x, y=y, klass=klass)

# Define a dataset with rows and columns.
def Data(src: List[row], txt: str = "") -> Obj:
   return adds(src, Obj(it=Data, txt=txt or "", n=0, rows=[], cols=None))
``` 

\endgroup

What we learned. - Bot.send() method - if else statements.

# Build a basic greetings bot

![Flow chart](banner.png)



# Benefits of AI Playground

- Streamlines a lot of back end operations, so that the you can just learn what AI is — and can get immediate results!
- User friendly!
- Designed to suit students needs.
- Students can see and publish new projects and thus learn from each other.

## How does learning AI help?

- Logical reasoning and Sequencing 
- Critical thinking
- Problem solving
- Mental Mathematics
    - The above skills are implicit skills that students learn along with AI. And this helps them in academics, life, etc.

# Extra 

The well known Pythagorean theorem $x^2 + y^2 = z^2$ was  proved to be invalid for other exponents. 
Meaning the next equation has no integer solutions:
$$x^n + y^n = z^n$$

Can AI, help find near misses for this equation?
