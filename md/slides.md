---
title: "My wonderful presentation"
author: "Alexey Gumirov"
institute: "My home office"
topic: "Pandoc how-to"
fonttheme: "professionalfonts"
mainfont: "Hack Nerd Font"
fontsize: "10pt"  # Slightly larger font for better readability
urlcolor: blue  # Change link color
linkstyle: bold
aspectratio: 169
date: Jan 22
lang: en-US
---

# Slide 1 title

asd asdasaddass

asd asdasaddass

asd asdasaddass

- asdas
- adas

```python
def Num(txt: str = " ", at: int = 0) -> Obj:
   return Obj(it=Num, txt=txt, at=at, n=0, mu=0, sd=0, m2=0, hi=-BIG, lo=BIG,
             goal = 0 if txt[-1]=="-" else 1)

# Define a symbolic column with frequency counts.
def Sym(txt: str = " ", at: int = 0) -> Obj:
   return Obj(it=Sym, txt=txt, at=at, n=0, has={}, most=0, mode=None)

# Define a collection of columns with metadata.
def Cols(names: List[str]) -> Obj:
   ...
    return Obj(it=Cols, names=names, all=lst, x=x, y=y, klass=klass)

# Define a dataset with rows and columns.
def Data(src: List[row], txt: str = "") -> Obj:
   return adds(src, Obj(it=Data, txt=txt or "", n=0, rows=[], cols=None))
```

# Slide 1 title

Some super quickly created demo slides. The syntax is simple and effective for creating slides on the fly.

- **No special tools needed**:
  * Use basic markdown to create slides.
  * Start slide titles with `#` and use bullet points for content.
- **Advanced Options**:
  * Add custom HTML tags like `<center>` for alignment.
  * Use LaTeX commands in Pandoc Beamer format (e.g., `\\center`, `\\large`).


# Slide 2 title

\center The slide syntax is so simple that you can quickly create a handful of slides on basically any device in any editor. E.g., on your mobile on the way to the meeting where you need the slides. Right before the meeting starts you use pandoc to create the actual slides from your source.


