---
title: Demo page
layout: default
---

## Code

```python 
# try to work in less that 55 characters wide
#       1        2        3        4        50    5
def aa(): return b()
```

## Footnotes

Four score and seven years[^1] ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

## Centered tables

| Header1 | Header2 | Header3 |
|:--------|:-------:|--------:|
| cell1   | cell2   | cell3   |
| cell4   | cell5   | cell6   |
|----
| cell1   | cell2   | cell3   |
| cell4   | cell5   | cell6   |
|=====
| Foot1   | Foot2   | Foot3|
{: rules="groups" class="center"}


## Responsive Images

![](https://images.pexels.com/photos/1133957/pexels-photo-1133957.jpeg?cs=srgb&dl=beautiful-beautiful-flowers-bird-1133957.jpg&fm=jpg){:class="img-responsive img-rounded"}

## Maths

$$
\begin{align*}
  & \phi(x,y) = \phi \left(\sum_{i=1}^n x_ie_i, \sum_{j=1}^n y_je_j \right)
  = \sum_{i=1}^n \sum_{j=1}^n x_i y_j \phi(e_i, e_j) = \\
  & (x_1, \ldots, x_n) \left( \begin{array}{ccc}
      \phi(e_1, e_1) & \cdots & \phi(e_1, e_n) \\
      \vdots & \ddots & \vdots \\
      \phi(e_n, e_1) & \cdots & \phi(e_n, e_n)
    \end{array} \right)
  \left( \begin{array}{c}
      y_1 \\
      \vdots \\
      y_n
    \end{array} \right)
\end{align*}
$$

## Tabbed content

<ul id="profileTabs" class="nav nav-tabs">
    <li class="active"><a href="#profile" data-toggle="tab">Profile</a></li>
    <li><a href="#about" data-toggle="tab">About</a></li>
    <li><a href="#match" data-toggle="tab">Match</a></li>
</ul>
  <div class="tab-content">
<div role="tabpanel" class="tab-pane active" id="profile">
<p>
This is my rifle. There are many like it, but this one is mine.
My rifle is my best friend. It is my life. I must master it as I must master my life.

Without me, my rifle is useless. Without my rifle, I am useless. I must fire my rifle true. I must shoot straighter than my enemy who is trying to kill me. I must shoot him before he shoots me. I will ...


</p>
</div>

<div role="tabpanel" class="tab-pane" id="about">
    <p>My rifle and I know that what counts in war is not the rounds we fire, the noise of our burst, nor the smoke we make. We know that it is the hits that count. We will hit ...

My rifle is human, even as I, because it is my life. Thus, I will learn it as a brother. I will learn its weaknesses, its strength, its parts, its accessories, its sights and its barrel. I will keep my rifle clean and ready, even as I am clean and ready. We will become part of each other. We will ...
Lorem ipsum ...</p></div>

<div role="tabpanel" class="tab-pane" id="match">
    <p>Before God, I swear this creed. My rifle and I are the defenders of my country. We are the masters of our enemy. We are the saviors of my life.

So be it, until victory is America's and there is no enemy, but peace!Vel vehicula ....</p>
</div>
</div>

## Notes

[^1]: i.e. 47 years
