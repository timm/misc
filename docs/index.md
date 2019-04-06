---
title: Demo page
layout: default
---

## Code

```python 
# try to work in less that 60 characters wide
#       1        2        3        4        5        60
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

## Tabbed content

<ul id="profileTabs" class="nav nav-tabs">
    <li class="active"><a href="#profile" data-toggle="tab">Profile</a></li>
    <li><a href="#about" data-toggle="tab">About</a></li>
    <li><a href="#match" data-toggle="tab">Match</a></li>
</ul>
  <div class="tab-content">
<div role="tabpanel" class="tab-pane active" id="profile">
<p>
I will maintain the utmost respect for human life.
</p><p>
I make these promises solemnly, freely and upon my honour.

</p>
</div>

<div role="tabpanel" class="tab-pane" id="about">
    <p> I wanna be a nurse and help people when they are sick! And when they are well!</p>
    <img  src="http://aux2.iconspalace.com/uploads/pharmacist-female-icon-256.png">
</div>

<div role="tabpanel" class="tab-pane" id="match">
    <ul>
    <li>
    The total health of my patients will be my first consideration.</li>
<li>
I will hold in confidence all personal matters coming to my knowledge.</li>
<li>

I will not permit consideration of religion, nationality, race or social standing to intervene between my duty and my patient.
</li>
</ul>
</div>
</div>


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

## Notes

[^1]: i.e. 47 years
