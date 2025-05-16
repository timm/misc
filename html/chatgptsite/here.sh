#!/bin/bash

mkdir -p 'site'
cat << 'EOF' > 'site/why.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - Why</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>The Philosophy of Lightweight AI</h1>

        <p>In today's AI-powered age, the tools are dazzling. Code writes itself. Systems 
        configure themselves. Everything is faster, easier, more accessible. But there's 
        a cost to this convenience – we risk forgetting how the machine actually works.</p>

        <p>Kube.py represents a counterbalance to the prevailing trend of ever-larger AI 
        models. It embodies what some call "Small AI" – lightweight, explainable 
        approaches that achieve remarkable results with minimal computational resources.</p>

        <p>Why seek such alternatives? Shouldn't "Big AI," with its massive datasets and 
        CPU-intensive methods, be the undisputed champion? Not necessarily. In multiple 
        studies, simpler methods have outperformed complex Large Language Models, 
        running significantly faster and often producing more accurate results. 
        Astonishingly, many research papers on large models don't even benchmark 
        against these more straightforward alternatives.</p>

        <h2>Compelling Reasons for Lightweight AI</h2>

        <ol>
        <li><strong>Engineering Elegance & Cost:</strong> There's inherent satisfaction in achieving more 
           with less. If everyone else can do it for $100, wouldn't you want to do it 
           for one cent? Kube.py demonstrates this principle by implementing 
           sophisticated clustering and optimization with minimal code and computational 
           overhead. Research has shown that simpler approaches often outperform complex 
           ones, as demonstrated by Fu and Menzies[1], where simpler learners found 
           better configurations more efficiently.</li>

        <li><strong>Speed & Interactivity:</strong> When data analysis is expensive and slow, iterative 
           discovery suffers. Interactive exploration, crucial for refining ideas, is 
           lost[2]. Waiting hours for cloud computations only to realize a minor tweak 
           is needed reminds us of the frustratingly slow batch processing of the 1960s.</li>

        <li><strong>Explainability & Trust:</strong> Simpler systems are inherently easier to understand, 
           explain, and audit. Living in a world where critical decisions are made by 
           opaque systems that we cannot question or verify is a disquieting prospect. 
           Kube.py prioritizes transparency, making its decision processes inspectable 
           and understandable.</li>

        <li><strong>Scientific Integrity:</strong> The harder and more expensive it is to run experiments, 
           the more challenging reproducibility becomes. This directly impacts the 
           trustworthiness of scientific findings. History warns us about relying on 
           inadequately scrutinized systems, as seen in failures like the Space Shuttle 
           Columbia disaster[3]. Kube's lightweight approach enables rapid 
           experimentation and validation.</li>

        <li><strong>Environmental Sustainability:</strong> The energy footprint of "Big AI" is alarming. 
           Projections show data center energy requirements doubling, with some AI 
           applications already consuming petajoules annually. Such exponential growth 
           is simply unsustainable[4].</li>
        </ol>

        <h2>Menzies's 4th Law: Throw Most Data Away</h2>

        <p>At the heart of Kube.py is a principle that might seem counterintuitive: For 
        many tasks in Software Engineering (SE), the best thing to do with most data is 
        to throw it away. This follows from what Tim Menzies calls his "4th Law" – the 
        recognition that for many problems, a small subset of the data contains most of 
        the signal.</p>

        <p>This isn't some new fad. These "key" variables have been appearing in AI 
        research for ages, under different guises: principal components, variable subset 
        selection, narrows, master variables, and backdoors. It echoes Vilfredo Pareto's 
        famous 80/20 principle, where 80% of effects come from 20% of causes – though in 
        practice, the ratio can be even more extreme.</p>

        <p>As Menzies has observed, the practical reality is often more dramatic than even 
        Pareto suggested – not just 80:20 but closer to 1000:1, where a tiny fraction of 
        factors dictates almost everything in complex systems[5]. This extreme 
        concentration of influence has been documented across multiple domains:</p>

        <h2>Historical Evidence</h2>

        <ul>
        <li><strong>Amarel's Narrows (1960s):</strong> Saul Amarel identified "narrows" in search problems 
          – small sets of essential variable settings that, once discovered, drastically 
          simplified the search space. His innovation was creating "macros" to jump 
          between these narrows, effectively creating shortcuts through complex problem 
          spaces[6].</li>

        <li><strong>Variable Pruning (1990s):</strong> Kohavi and John demonstrated that removing up to 80% 
          of variables in certain datasets still maintained excellent classification 
          accuracy, reinforcing the idea that most variables contribute negligible 
          information[7].</li>

        <li><strong>ISAMP and Constraint Satisfaction (1990s):</strong> Crawford and Baker's ISAMP tool 
          used random search with strategic restarts, which proved surprisingly 
          effective for constraint satisfaction problems. The key insight was that 
          models have a few "master variables" controlling overall behavior, making 
          exhaustive searching inefficient[8].</li>

        <li><strong>Backdoors in Satisfiability (2000s):</strong> Williams and colleagues found that 
          running random searches repeatedly revealed the same few variable settings in 
          successful solutions. Setting these "backdoor" variables first made previously 
          intractable problems suddenly manageable, providing another demonstration of 
          the power of identifying key variables[9].</li>

        <li><strong>Modern Feature Selection and Dimensionality Reduction:</strong> Contemporary machine 
          learning continues to build on these foundations with techniques like LASSO 
          regression (which can reduce coefficients to zero) and Random Forest 
          importance metrics that identify the handful of features driving most 
          predictions[10].</li>
        </ul>

        <h2>Empirical Support</h2>

        <p>Recent work by researchers like Kocaguneli, Tu, Peters, and Xu has shown that 
        accurate predictions for software engineering tasks like predicting GitHub issue 
        close times, effort estimation, and defect prediction remained possible even 
        after discarding labels for 80%, 91%, 97%, and sometimes 98-100% of project 
        data[11][12][13][14]. This dramatic data reduction without significant 
        performance loss demonstrates the concentrated nature of information in 
        real-world datasets.</p>

        <p>The mathematical foundation for this phenomenon is also well-established. The 
        Johnson-Lindenstrauss lemma[15] proves that high-dimensional data can be 
        projected into significantly lower dimensions while mostly preserving distances 
        between points. Similarly, research on the "naturalness" of software[16] has 
        shown that code contains significant repetition and predictable patterns, 
        explaining why small samples can capture essential behaviors of much larger 
        systems.</p>

        <p>This principle finds further support in studies of software bugs by Ostrand et 
        al.[17], which demonstrated that typically 80% of defects occur in just 20% of 
        modules, and in Lin and Robles' work[18] showing how open-source development 
        follows power laws, where a small fraction of components receive most 
        development activity.</p>

        <p>Menzies et al.[19][20] have demonstrated across multiple studies that static 
        code attributes can be highly compressed while still maintaining predictive 
        power for defect models, and that local models built on small subsets of data 
        can outperform global models trained on everything. The essence of this work is 
        that more data isn't always better – smartly selected small data often is.</p>

        <h2>Techniques in Kube.py</h2>

        <p>Kube.py leverages this insight through techniques like:</p>

        <ol>
        <li><strong>Pole Selection:</strong> Finding representative points at maximum distance from each 
           other, similar to Pearson's principal component analysis (1902) which 
           identified that even in datasets with many dimensions, a handful of 
           components captured the main "drift" of the data.</li>

        <li><strong>Locality-Sensitive Hashing:</strong> Building on insights from constraint satisfaction 
           research that found "random search with retries" surprisingly effective 
           because models often have a few "master variables" pulling the strings.</li>

        <li><strong>Projection:</strong> Drawing from the Johnson-Lindenstrauss lemma which demonstrates 
           that complex data can often be accurately approximated in lower-dimensional 
           spaces while preserving essential relationships.</li>
        </ol>

        <p>By focusing on these "key" aspects of data and using computationally efficient 
        algorithms, Kube.py demonstrates that the future of AI may lie not in attempting 
        to "boil the ocean" with every query, but in focusing on "small data" with 
        intelligent, nimble approaches.</p>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/eg.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - Example</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>Practical Example</h1>

        <p>Let's start with a practical example of how kube.py can be used to analyze and 
        cluster automobile data.</p>

        <h2>Input Data Format</h2>

        <p>Kube.py works with CSV data where the column headers contain important metadata. 
        Here's a sample of the auto93.csv dataset:</p>

<pre><code class="language-html">
 Clndrs  Volume  HpX  Model  origin  Lbs-  Acc+  Mpg+  ydist
------  ------  ---  -----  ------  ----  ----  ----  -----
4       90      48   80     2       2085  21.7  40    0.17
4       91      67   80     3       1850  13.8  40    0.17
4       86      65   80     3       2019  16.4  40    0.17
4       86      64   81     1       1875  16.4  40    0.18
...
8       429     198  70     1       4341  10.0  20    0.77
8       350     165  70     1       3693  11.5  20    0.77
8       455     225  70     1       4425  10.0  10    0.77
8       454     220  70     1       4354  9.0   10    0.79
</code></pre>

        <h2>Column Naming Conventions</h2>

        <ul>
            <li>Capitalized headers (like "Clndrs", "Volume") indicate numeric columns</li>
            <li>Lowercase headers (like "origin") indicate symbolic/categorical columns</li>
            <li>A trailing "-" (like "Lbs-") indicates values to be minimized</li>
            <li>A trailing "+" (like "Acc+", "Mpg+") indicates values to be maximized</li>
            <li>A trailing "X" (not shown) would indicate columns to be ignored</li>
        </ul>

        <h2>Data Interpretation</h2>

        <ul>
            <li>The example shows cars with different attributes (cylinders, volume, 
            horsepower, etc.)</li>
            <li>Lower rows show larger cars (8 cylinders, high weight) with poor fuel 
            economy (10-20 MPG)</li>
            <li>Upper rows show smaller cars (4 cylinders, lower weight) with better fuel 
            economy (40 MPG)</li>
            <li>The "ydist" column (distance to heaven) shows that the top cars (0.17) are 
            much closer to ideal than bottom cars (0.79)</li>
            <li>The "origin" column uses codes (1=American, 2=European, 3=Japanese)</li>
        </ul>

        <h2>Running the Example</h2>

<pre><code class="language-html">
$ python kube.py --counts

# Results might look like:
# {mid: 0.29, div: 0.11, n: 8}
# {mid: 0.45, div: 0.15, n: 12}
# {mid: 0.19, div: 0.08, n: 10}
# ...
</code></pre>

        <h2>What Happened?</h2>

        <p>In this example, kube.py has:</p>
        <ol>
            <li>Loaded the auto93.csv dataset</li>
            <li>Selected representative "poles" from the data</li>
            <li>Used locality-sensitive hashing (LSH) to cluster similar items</li>
            <li>Computed statistics for each cluster</li>
            <li>Displayed clusters with their central tendency (mid), diversity (div), 
            and size (n)</li>
        </ol>

        <p>Each cluster represents automobiles with similar characteristics, and the "mid" 
        value represents how close each cluster is to an ideal car (lower is better). 
        This demonstrates kube's ability to automatically organize complex 
        multi-dimensional data into meaningful groups.</p>

        <h2>Try More Examples</h2>

        <p>Explore other example functions to understand different capabilities:</p>

<pre><code class="language-html">
# Print the current configuration 
$ python kube.py --the

# See how kube understands your columns
$ python kube.py --data

# Find the best and worst examples in your data
$ python kube.py --ydist

# See the clustering structure
$ python kube.py --poles
</code></pre>

        <p>Check the <a href="details.html">Details</a> page for more information about these example functions.</p>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/index.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>Kube.py</h1>
        
        <div class="mantra">
            <p><strong>Simplicity is the ultimate sophistication.</strong></p>
            <p>Small data, tiny code, explainable AI.</p>
        </div>

        <pre class="docstring">#!/usr/bin/env python3 -B
# In this code "i" == "self". Also "_x" is a hidden var (to big, or to secret, to show).
"""
kube.py : barelogic, XAI for active learning + multi-objective optimization
(c) 2025, Tim Menzies &lt;timm@ieee.org&gt;, MIT License

Options:
      -b bins    number of bins                     = 5
      -m min     minPts per cluster (0=auto choose) = 0
      -P P       distance formula exponent          = 2
      -d dims    number of dimensions               = 4
      -r rseed   random number seed                 = 1234567891
      -s some    search space size for poles        = 30
      -f file    training csv file = ../../moot/optimize/misc/auto93.csv
"""</pre>

        <p>Kube.py is a Python implementation for explainable AI (XAI) focused on active 
        learning and multi-objective optimization. Created by Tim Menzies in 2025, this 
        lightweight framework provides powerful clustering, projection, and optimization 
        capabilities with minimal dependencies.</p>

        <p>The name "kube" (from the Greek "κύβος" meaning "cube") suggests the 
        multi-dimensional space handling capabilities of this tool. Its primary purpose 
        is to help navigate complex data spaces efficiently while keeping explanations 
        transparent and interpretable.</p>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/details.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - Details</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>Details: CLI Options and Example Functions</h1>

        <h2>Part 1: CLI Options</h2>

        <p>Kube.py provides several command-line options to customize its behavior:</p>

        <table>
            <tr>
                <th>Option</th>
                <th>Description</th>
                <th>Default</th>
                <th>Effect of Increase</th>
                <th>Effect of Decrease</th>
            </tr>
            <tr>
                <td><code>-b bins</code></td>
                <td>Number of bins</td>
                <td>5</td>
                <td>More granular clustering, potentially more clusters</td>
                <td>Coarser clustering, fewer clusters</td>
            </tr>
            <tr>
                <td><code>-m min</code></td>
                <td>Minimum points per cluster</td>
                <td>0 (auto)</td>
                <td>Larger minimum cluster size, fewer reported clusters</td>
                <td>Smaller minimum cluster size, more reported clusters</td>
            </tr>
            <tr>
                <td><code>-P P</code></td>
                <td>Distance formula exponent</td>
                <td>2</td>
                <td>Higher values emphasize larger differences</td>
                <td>Lower values give more equal weight to all differences</td>
            </tr>
            <tr>
                <td><code>-d dims</code></td>
                <td>Number of dimensions</td>
                <td>4</td>
                <td>More clustering dimensions, potentially more precise clusters</td>
                <td>Fewer dimensions, potentially faster but less precise</td>
            </tr>
            <tr>
                <td><code>-r rseed</code></td>
                <td>Random number seed</td>
                <td>1234567891</td>
                <td>Different random selections</td>
                <td>Different random selections</td>
            </tr>
            <tr>
                <td><code>-s some</code></td>
                <td>Search space size for poles</td>
                <td>30</td>
                <td>Larger sample for pole selection, potentially better poles</td>
                <td>Smaller sample, faster but potentially less optimal poles</td>
            </tr>
            <tr>
                <td><code>-f file</code></td>
                <td>Training CSV file</td>
                <td>../../moot/optimize/misc/auto93.csv</td>
                <td>N/A (changes input file)</td>
                <td>N/A (changes input file)</td>
            </tr>
        </table>

        <h3>Automatic MinPts Selection</h3>

        <p>When <code>-m</code> is set to 0 (default), kube automatically selects an appropriate 
        minimum cluster size:</p>
        <ul>
            <li>For datasets < 30 rows: minPts = 2</li>
            <li>For datasets < 100 rows: minPts = 3</li>
            <li>For larger datasets: minPts = 2 + the.dims</li>
        </ul>

        <p>This heuristic ensures that clusters have enough points to be statistically significant 
        without being too restrictive.</p>

        <h3>Examples of CLI Usage</h3>

<pre><code class="language-html">
# Use more bins for finer clustering
$ python kube.py --counts -b 8

# Use Manhattan distance instead of Euclidean
$ python kube.py --counts -P 1

# Use more dimensions for clustering
$ python kube.py --counts -d 6

# Set a fixed minimum cluster size
$ python kube.py --counts -m 5

# Use a different dataset
$ python kube.py --counts -f path/to/other/data.csv
</code></pre>

        <h2>Part 2: Example Functions</h2>

        <p>Kube.py includes several example functions (prefixed with <code>eg__</code>) that 
        demonstrate different capabilities of the tool. These can be run directly from the 
        command line:</p>

        <table>
            <tr>
                <th>Function</th>
                <th>Command</th>
                <th>Description</th>
            </tr>
            <tr>
                <td><code>eg__all</code></td>
                <td><code>python kube.py --all</code></td>
                <td>Runs all example functions in sequence</td>
            </tr>
            <tr>
                <td><code>eg__the</code></td>
                <td><code>python kube.py --the</code></td>
                <td>Prints the current configuration settings</td>
            </tr>
            <tr>
                <td><code>eg__csv</code></td>
                <td><code>python kube.py --csv</code></td>
                <td>Prints the raw CSV data from the input file</td>
            </tr>
            <tr>
                <td><code>eg__data</code></td>
                <td><code>python kube.py --data</code></td>
                <td>Prints column information, showing which are independent (x) and dependent (y) variables</td>
            </tr>
            <tr>
                <td><code>eg__ydist</code></td>
                <td><code>python kube.py --ydist</code></td>
                <td>Prints rows sorted by distance to heaven, showing the 4 best and 4 worst examples</td>
            </tr>
            <tr>
                <td><code>eg__poles</code></td>
                <td><code>python kube.py --poles</code></td>
                <td>Shows clustering dimensions by displaying the results of pole selection and LSH</td>
            </tr>
            <tr>
                <td><code>eg__counts</code></td>
                <td><code>python kube.py --counts</code></td>
                <td>Shows cluster counts and statistics - the main clustering functionality</td>
            </tr>
        </table>

        <h3>Example Functions in Detail</h3>

        <h4>1. <code>eg_h</code>: Help Text</h4>
<pre><code class="language-html">
$ python kube.py -h
</code></pre>
        <p>This displays the docstring and lists all available example functions.</p>

        <h4>2. <code>eg__the</code>: Current Configuration</h4>
<pre><code class="language-html">
$ python kube.py --the
# Output: {b: 5, m: 0, P: 2, d: 4, r: 1234567891, s: 30, f: ../../moot/optimize/misc/auto93.csv}
</code></pre>
        <p>This is useful for verifying your current settings.</p>

        <h4>3. <code>eg__csv</code>: Raw Data</h4>
<pre><code class="language-html">
$ python kube.py --csv
# Output: Shows all rows from the CSV file
</code></pre>
        <p>This shows the raw input data without any processing.</p>

        <h4>4. <code>eg__data</code>: Column Information</h4>
<pre><code class="language-html">
$ python kube.py --data
# Output:
# x Num(at=0, txt=Clndrs, n=398, mu=5.45, m2=2400.31, lo=3, hi=8, heaven=1)
# x Num(at=1, txt=Volume, n=398, mu=193.43, m2=1786554.97, lo=68, hi=455, heaven=1)
# ...
# y Num(at=5, txt=Lbs-, n=398, mu=2970.42, m2=19431245.76, lo=1613, hi=5140, heaven=0)
</code></pre>
        <p>This reveals how kube interprets each column (Num or Sym) and whether it's 
        considered an input (x) or output (y) variable.</p>

        <h4>5. <code>eg__ydist</code>: Ranking by Distance to Heaven</h4>
<pre><code class="language-html">
$ python kube.py --ydist
# Output:
# good [4, 90, 48, 80, 2, 2085, 21.7, 40]
# good [4, 91, 67, 80, 3, 1850, 13.8, 40]
# ...
# bad [8, 455, 225, 70, 1, 4425, 10.0, 10]
# bad [8, 454, 220, 70, 1, 4354, 9.0, 10]
</code></pre>
        <p>This shows which data points are closest to optimal (good) and furthest from 
        optimal (bad).</p>

        <h4>6. <code>eg__poles</code>: Pole Selection and Clustering</h4>
<pre><code class="language-html">
$ python kube.py --poles
# Output:
# (0, 0, 0)
# (0, 1, 1)
# ...
# 14
</code></pre>
        <p>This shows the hash keys generated for different clusters and the total 
        number of clusters found.</p>

        <h4>7. <code>eg__counts</code>: Full Clustering with Statistics</h4>
<pre><code class="language-html">
$ python kube.py --counts
# Output:
# {mid: 0.29, div: 0.11, n: 8}
# {mid: 0.45, div: 0.15, n: 12}
# ...
</code></pre>
        <p>This is the primary analysis function, showing each valid cluster (meeting 
        minimum size) with its:</p>
        <ul>
            <li><code>mid</code>: central tendency of y-distances (lower is better)</li>
            <li><code>div</code>: diversity/spread of the cluster</li>
            <li><code>n</code>: number of data points in the cluster</li>
        </ul>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/how.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - Inference Tools</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>Inference Tools</h1>

        <p>Kube provides several powerful inference tools built on its core data model. These tools 
        enable efficient clustering, optimization, and exploration of multi-dimensional data spaces.</p>

        <h2>Pole Selection</h2>

        <p>The <code>poles()</code> method selects representative data points that span the data space:</p>

<pre><code class="language-html">
def poles(i) -> Rows:
    """Select poles at max distance to poles picked so far."""
    r0, *some = many(i._rows, k=the.some + 1)
    out = [max(some, key=lambda r1: i.xdist(r1, r0))]
    for _ in range(the.dims):
        out += [max(some, key=lambda r2: sum(i.xdist(r2, r1) for r1 in out))]
    return out
</code></pre>

        <p>This algorithm:</p>
        <ol>
            <li>Samples <code>the.some</code> random rows</li>
            <li>Finds the two most distant points</li>
            <li>Iteratively adds points that maximize the sum of distances to previously 
            selected poles</li>
            <li>Continues until <code>the.dims</code> poles are selected</li>
        </ol>

        <p>This approach efficiently explores the boundaries of the data space, selecting 
        points that represent extreme or distinctive characteristics. It's similar to how 
        principal component analysis identifies dimensions of maximum variance, but uses 
        a simpler, more intuitive approach.</p>

        <h2>Projection and Hashing</h2>

        <p>The <code>project()</code> method projects a data point onto the line connecting two poles:</p>

<pre><code class="language-html">
def project(i, row: Row, a: Row, b: Row) -> int:
    """Project a row onto the line connecting two poles, a and b"""
    c = i.xdist(a, b)
    x = (i.xdist(row, a)**2 + c**2 - i.xdist(row, b)**2) / (2 * c)
    return min(int(x / c * the.bins), the.bins - 1)
</code></pre>

        <p>This method:</p>
        <ol>
            <li>Uses the law of cosines to find where a point projects onto a line</li>
            <li>Calculates the normalized position (0 to 1) along that line</li>
            <li>Discretizes this position into <code>the.bins</code> bins (default: 5)</li>
        </ol>

        <p>The collection of bin assignments across multiple pole pairs forms a unique hash 
        for clustering. This discretization step is crucial for efficient approximate clustering, 
        as it allows similar points to share the same hash despite small differences.</p>

        <h2>Locality-Sensitive Hashing</h2>

        <p>The <code>lsh()</code> method implements locality-sensitive hashing for efficient clustering:</p>

<pre><code class="language-html">
def lsh(i, poles: Rows) -> Dict[Tuple[int, ...], 'Data']:
    """Locality sensitive hashing to group rows by projection."""
    clusters: Dict[Tuple[int, ...], 'Data'] = {}
    for row in i._rows:
        k = tuple(i.project(row, a, b) for a, b in zip(poles, poles[1:]))
        clusters[k] = clusters.get(k) or i.clone()
        clusters[k].add(row)
    return clusters
</code></pre>

        <p>This approach creates efficient approximate clusterings by:</p>
        <ol>
            <li>Computing projections for each row onto the lines between consecutive poles</li>
            <li>Using these projections as a hash key</li>
            <li>Grouping rows with identical hash keys into the same cluster</li>
        </ol>

        <p>This method has O(n) complexity versus the O(n²) of traditional clustering methods, 
        making it efficient for large datasets. It's a form of dimensionality reduction that 
        preserves local relationships, similar to techniques like t-SNE or UMAP but with much 
        lower computational cost.</p>

        <h2>Distance to Heaven</h2>

        <p>The <code>ydist()</code> method calculates how far a point is from the ideal:</p>

<pre><code class="language-html">
def ydist(i, row: Row) -> float:
    """Calculate the distance to heaven for this row."""
    return dist(abs(c.norm(row[c.at]) - c.heaven) for c in i.cols.y)
</code></pre>

        <p>This creates a single objective function from multiple objectives by:</p>
        <ol>
            <li>Normalizing each attribute to a 0-1 scale</li>
            <li>Measuring how far each attribute is from its ideal value (0 for minimization, 1 for maximization)</li>
            <li>Combining these distances using the Minkowski distance formula</li>
        </ol>

        <p>This approach to multi-objective optimization is elegant in its simplicity. 
        Rather than using complex Pareto-front calculations, it creates an intuitive distance 
        metric that can be used to rank solutions. This fits with the minimalist philosophy of kube.</p>

        <h2>Minimum Points Heuristic</h2>

        <p>The <code>minPts()</code> method determines how many points are needed for a valid cluster:</p>

<pre><code class="language-html">
def minPts(i) -> int:
    """Report how many points are needed for each bucket."""
    out = the.min
    if out==0:
      if   len(i._rows) <  30: out= 2
      elif len(i._rows) < 100: out= 3
      else: out = 2 + the.dims
    return out
</code></pre>

        <p>This heuristic ensures that clusters have enough points to be statistically significant 
        without being too restrictive. It adapts to the dataset size and dimensionality, making 
        kube's clustering robust across different applications.</p>

        <h2>How It All Works Together</h2>

        <p>The complete clustering process in kube follows these steps:</p>

        <ol>
            <li>Select representative poles using <code>poles()</code></li>
            <li>Project each point onto the lines between consecutive poles using <code>project()</code></li>
            <li>Cluster points with identical projections using <code>lsh()</code></li>
            <li>Calculate statistics for each cluster</li>
            <li>Filter clusters to retain only those with at least <code>minPts()</code> points</li>
            <li>Present results sorted by cluster quality (lowest <code>ydist()</code> first)</li>
        </ol>

        <p>This pipeline exemplifies the "small data" philosophy: It efficiently identifies key 
        structures in the data without requiring exhaustive pairwise comparisons or complex 
        optimization procedures.</p>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/readme.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py Website</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
            text-align: center;
        }
        h1 {
            margin-bottom: 30px;
        }
        .files {
            text-align: left;
            border: 1px solid #ddd;
            padding: 20px;
            margin-bottom: 20px;
            background-color: #f9f9f9;
        }
        ul {
            list-style-type: none;
            padding: 0;
        }
        li {
            margin-bottom: 10px;
        }
        .download {
            display: inline-block;
            background-color: #4CAF50;
            color: white;
            padding: 10px 20px;
            text-decoration: none;
            border-radius: 5px;
            font-weight: bold;
        }
        .download:hover {
            background-color: #45a049;
        }
    </style>
</head>
<body>
    <h1>Kube.py Website Files</h1>
    
    <div class="files">
        <h2>HTML Files:</h2>
        <ul>
            <li>index.html - Home page with intro and docstring</li>
            <li>why.html - Philosophy of lightweight AI</li>
            <li>eg.html - Practical example with data format explanation</li>
            <li>maths.html - Mathematical and AI foundations</li>
            <li>what.html - Data model explanation</li>
            <li>how.html - Inference tools explanation</li>
            <li>who.html - Credits and acknowledgments</li>
            <li>details.html - CLI options and example functions</li>
            <li>references.html - Complete list of references</li>
        </ul>
        
        <h2>CSS File:</h2>
        <ul>
            <li>style.css - Brutalist style sheet for the website</li>
        </ul>
    </div>
    
    <p>All files are formatted in a brutalist style inspired by mechanize.work, with fixed-width fonts, minimal styling, and a focus on content.</p>
    
    <a href="#" class="download">Download ZIP</a>
    
    <p><small>Note: In a real implementation, the download button would point to the actual ZIP file.</small></p>
</body>
</html>

EOF

mkdir -p 'site'
cat << 'EOF' > 'site/style.css'
/* 
   style.css for Kube.py - Brutalist style website
   Inspired by mechanize.work
*/

/* Base styles */
:root {
    --primary-font: 'Courier New', monospace;
    --heading-font: 'Helvetica', sans-serif;
    --background-color: #f9f9f9;
    --text-color: #111;
    --link-color: #0000EE;
    --link-visited: #551A8B;
    --light-gray: #eee;
    --code-bg: #f5f5f5;
}

html {
    box-sizing: border-box;
}

*, *:before, *:after {
    box-sizing: inherit;
}

body {
    font-family: var(--primary-font);
    xline-height: 1.5;
    background-color: var(--background-color);
    color: var(--text-color);
    max-width: 800px;
    margin: 0 auto;
    padding: 1rem;
    font-size: 16px;
}

/* Typography */
h1, h2, h3, h4 {
    font-family: var(--heading-font);
    font-weight: bold;
    margin-top: 2rem;
    margin-bottom: 1rem;
}

h1 {
    font-size: 2rem;
    border-bottom: 2px solid #000;
    padding-bottom: 0.5rem;
}

h2 {
    font-size: 1.5rem;
}

h3 {
    font-size: 1.2rem;
}

h4 {
    font-size: 1rem;
}

p, ul, ol, table {
    margin-bottom: 1.5rem;
}

a {
    color: var(--link-color);
    text-decoration: none;
}

a:visited {
    color: var(--link-visited);
}

a:hover {
    text-decoration: underline;
}

hr {
    border: none;
    border-top: 1px solid #000;
    margin: 1rem 0;
}

small {
    font-size: 0.85rem;
}

/* Code blocks and syntax */
pre, code {
    font-family: var(--primary-font);
    overflow-x: auto;
}

pre {
    background-color: var(--code-bg);
    padding: 1rem;
    margin: 1rem 0;
    white-space: pre-wrap;
    border-left: 2px solid #ccc;
}

code {
    background-color: var(--code-bg);
    padding: 0.2rem 0.4rem;
}

/* Header and navigation */
header {
    margin-bottom: 2rem;
}

.menu {
    padding: 0.5rem 0;
    font-family: var(--heading-font);
}

.menu a {
    margin: 0 0.5rem;
}

.menu a:first-child {
    margin-left: 0;
}

/* GitHub corner */
.github-corner svg {
    fill: #151513;
    color: #fff;
    position: absolute;
    top: 0;
    border: 0;
    right: 0;
}

.github-corner:hover .octo-arm {
    animation: octocat-wave 560ms ease-in-out;
}

@keyframes octocat-wave {
    0%, 100% { transform: rotate(0); }
    20%, 60% { transform: rotate(-25deg); }
    40%, 80% { transform: rotate(10deg); }
}

@media (max-width: 500px) {
    .github-corner:hover .octo-arm {
        animation: none;
    }
    .github-corner .octo-arm {
        animation: octocat-wave 560ms ease-in-out;
    }
}

/* Home page specific styles */
.mantra {
    font-size: 1.2rem;
    margin: 2rem 0;
    font-style: italic;
    text-align: center;
}

.docstring {
    background-color: var(--light-gray);
    border-left: 3px solid #999;
    padding: 1rem;
    margin: 1rem 0;
    white-space: pre-wrap;
    font-family: var(--primary-font);
}

/* Tables */
table {
    border-collapse: collapse;
    width: 100%;
    margin: 1rem 0;
}

th {
    text-align: left;
    border-bottom: 2px solid #000;
    padding: 0.5rem;
    font-weight: bold;
}

td {
    padding: 0.5rem;
    border-bottom: 1px solid #ccc;
}

tr:nth-child(even) {
    background-color: var(--light-gray);
}

/* Lists */
ul, ol {
    padding-left: 2rem;
}

li {
    margin-bottom: 0.5rem;
}

/* Footer */
footer {
    margin-top: 4rem;
    padding-top: 1rem;
    text-align: center;
}

/* Print styles */
@media print {
    body {
        background-color: white;
        color: black;
        font-size: 12pt;
    }
    
    a {
        color: black;
        text-decoration: underline;
    }
    
    .github-corner {
        display: none;
    }
    
    pre, code {
        background-color: white;
        border: 1px solid #ddd;
    }
}

EOF

mkdir -p 'site'
cat << 'EOF' > 'site/what.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - Data Model</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>Data Model</h1>

        <p>Kube's data model consists of several key classes designed for efficiency and clarity:</p>

        <h2>Base Classes</h2>

        <h3><code>o</code> Class</h3>
        <pre><code class="language-html">
class o:
  """Base class providing dictionary update and string representation."""
  __init__ = lambda i, **d: i.__dict__.update(**d)
  __repr__ = cat
</code></pre>
        <p>This is the foundation class that all other classes inherit from. It provides:</p>
        <ul>
            <li>A simple way to update object attributes with dictionary items</li>
            <li>String representation via the <code>cat</code> function that formats objects for display</li>
        </ul>

        <h3>Type Aliases</h3>
        <pre><code class="language-html">
# Type aliases
Atom = Union[int, float, str, bool]
Row  = List[Atom]
Rows = List[Row]
Col  = Union['Sym', 'Num']
</code></pre>
        <p>These type aliases provide clear definitions for the basic data structures:</p>
        <ul>
            <li><code>Atom</code>: A primitive data type (int, float, str, bool)</li>
            <li><code>Row</code>: A list of Atoms representing a single data entry</li>
            <li><code>Rows</code>: A list of Row objects</li>
            <li><code>Col</code>: Either a Sym or Num column type</li>
        </ul>

        <h2>Column Classes</h2>

        <h3><code>Sym</code> Class</h3>
        <pre><code class="language-html">
class Sym(o):
  """Symbol class for handling categorical attributes."""

  def __init__(i, has: List[Atom] = [], at: int = 0, txt: str = " "):
    """Initialize a symbol column."""
    i.at: int = at            # Column position
    i.txt: str = txt          # Column name
    i.n: int = 0              # Count of items seen
    i.has: Dict[Atom, int] = {}  # Frequency counts of values
    [i.add(x) for x in has]</code></pre>
        <p>The <code>Sym</code> class handles categorical attributes with methods for:</p>
        <ul>
            <li>Frequency counting (<code>has</code> dictionary)</li>
            <li>Distance calculation between symbols</li>
            <li>Finding most common value (<code>mid()</code>)</li>
            <li>Calculating entropy as diversity measure (<code>div()</code>)</li>
        </ul>

        <h3><code>Num</code> Class</h3>
        <pre><code class="language-html">
class Num(o):
  """Number class for handling numeric attributes."""
  def __init__(i, has: List[Atom] = [], at: int = 0, txt: str = " "):
    """Initialize a numeric column."""
    i.at: int = at            # Column position
    i.txt: str = txt          # Column name
    i.n: int = 0              # Count of items seen
    i.mu: float = 0           # Mean of values
    i.m2: float = 0           # Sum of squared differences from mean
    i.lo: float = BIG         # Lowest value seen
    i.hi: float = -BIG        # Highest value seen
    i.heaven: int = 0 if txt[-1] == "-" else 1  # Optimization goal (0=min, 1=max)
    [i.add(x) for x in has]</code></pre>
        <p>The <code>Num</code> class handles numerical attributes with methods for:</p>
        <ul>
            <li>Online calculation of mean and standard deviation</li>
            <li>Normalization to 0-1 range</li>
            <li>Distance calculation between numbers</li>
            <li>Optimization direction via <code>heaven</code> attribute (0=minimize, 1=maximize)</li>
        </ul>
        <p>Note the clever use of the column name suffix ("-" or "+") to determine the optimization 
        direction via the <code>heaven</code> attribute.</p>

        <h2>Data Container</h2>

        <h3><code>Data</code> Class</h3>
        <pre><code class="language-html">
class Data(o):
  """Data class for handling collections of rows."""
  def __init__(i, src: Iterator[Row]):
    """Initialize from data source."""
    i._rows: Rows = []             # Storage for data rows
    i.cols = o(x=[], y=[], all=[]) # Track columns (x=independent, y=dependent, all=all)
    src = iter(src)
    [i.about(c, s) for c, s in enumerate(next(src))]
    [i.add(row) for row in src]</code></pre>
        <p>The <code>Data</code> class is the main container for rows and columns with methods for:</p>
        <ul>
            <li>Parsing CSV files</li>
            <li>Separating columns into independent (x) and dependent (y) variables</li>
            <li>Finding central tendency rows</li>
            <li>Calculating distances between rows</li>
            <li>Implementing cluster analysis via <code>lsh()</code> method</li>
        </ul>

        <h2>Design Philosophy</h2>

        <p>Kube's data model is designed with several key principles in mind:</p>

        <ol>
            <li><strong>Minimalism</strong>: Each class does one thing well, with a minimum of methods and attributes.</li>
            <li><strong>Self-documenting</strong>: Code is written to be readable and understandable, with clear variable names.</li>
            <li><strong>Type safety</strong>: Python type hints provide documentation and enable static type checking.</li>
            <li><strong>Online algorithms</strong>: Statistical calculations like mean and standard deviation use online 
            algorithms that don't require storing all data points.</li>
            <li><strong>Functional style</strong>: Many operations use functional programming idioms like list comprehensions.</li>
        </ol>

        <p>This design allows kube to handle large datasets efficiently while remaining easy to understand and modify.</p>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/references.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - References</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>References</h1>

        <ol>
            <li>Fu, W., & Menzies, T. (2017). Easy over hard: A case study on deep learning. European Software Engineering Conference, 295-306.</li>
            
            <li>Fisher, D., DeLine, R., Czerwinski, M., & Drucker, S. (2012). Interactions with big data analytics. Interactions, 19(3), 50-59.</li>
            
            <li>Columbia Accident Investigation Board. (2003). Report of the Columbia Accident Investigation Board, Vol. 1. Government Printing Office, Washington, D.C.</li>
            
            <li>Strubell, E., Ganesh, A., & McCallum, A. (2019). Energy and policy considerations for deep learning in NLP. Annual Meeting of the Association for Computational Linguistics, 3645-3650.</li>
            
            <li>Menzies, T., & Zimmermann, T. (2018). Software analytics: What's next? IEEE Software, 35(5), 64-70.</li>
            
            <li>Amarel, S. (1968). On representations of problems of reasoning about actions. Machine Intelligence, 3, 131-171.</li>
            
            <li>Kohavi, R., & John, G. H. (1997). Wrappers for feature subset selection. Artificial Intelligence, 97(1-2), 273-324.</li>
            
            <li>Crawford, J., & Baker, A. (1994). Experimental results on the application of satisfiability algorithms to scheduling problems. AAAI Conference on Artificial Intelligence, 1092-1097.</li>
            
            <li>Williams, R., Gomes, C. P., & Selman, B. (2003). Backdoors to typical case complexity. International Joint Conference on Artificial Intelligence, 1173-1178.</li>
            
            <li>Tibshirani, R. (1996). Regression shrinkage and selection via the lasso. Journal of the Royal Statistical Society: Series B, 58(1), 267-288.</li>
            
            <li>Kocaguneli, E., Menzies, T., Bener, A., & Keung, J. W. (2013). Exploiting the essential assumptions of analogy-based effort estimation. IEEE Transactions on Software Engineering, 39(7), 957-972.</li>
            
            <li>Tu, H., Menzies, T., & He, B. (2022). Data to the people: Lazy learning, data reduction, simplified reasoning. Automated Software Engineering, 29(1), 1-30.</li>
            
            <li>Peters, F., Menzies, T., & Marcus, A. (2013). Better cross company defect prediction. IEEE International Working Conference on Mining Software Repositories, 409-418.</li>
            
            <li>Xu, B., Xing, Z., Xia, X., & Lo, D. (2021). AnswerBot: automated generation of answer summary to developers' technical questions. IEEE Transactions on Software Engineering, 47(5), 998-1015.</li>
            
            <li>Johnson, W. B., & Lindenstrauss, J. (1984). Extensions of Lipschitz mappings into a Hilbert space. Contemporary Mathematics, 26, 189-206.</li>
            
            <li>Hindle, A., Barr, E. T., Su, Z., Gabel, M., & Devanbu, P. (2012). On the naturalness of software. International Conference on Software Engineering, 837-847.</li>
            
            <li>Ostrand, T. J., Weyuker, E. J., & Bell, R. M. (2004). Where the bugs are. ACM SIGSOFT Software Engineering Notes, 29(4), 86-96.</li>
            
            <li>Lin, B., & Robles, G. (2015). Power laws in FLOSS development. Journal of Systems and Software, 107, 85-99.</li>
            
            <li>Menzies, T., Greenwald, J., & Frank, A. (2007). Data mining static code attributes to learn defect predictors. IEEE Transactions on Software Engineering, 33(1), 2-13.</li>
            
            <li>Menzies, T., Butcher, A., Marcus, A., Zimmermann, T., & Cok, D. (2011). Local vs. global models for effort estimation and defect prediction. IEEE International Conference on Automated Software Engineering, 343-351.</li>
            
            <li>Cha, S. H. (2007). Comprehensive survey on distance/similarity measures between probability density functions. International Journal of Mathematical Models and Methods in Applied Sciences, 1(4), 300-307.</li>
            
            <li>Gionis, A., Indyk, P., & Motwani, R. (1999). Similarity search in high dimensions via hashing. VLDB, 99(6), 518-529.</li>
            
            <li>Settles, B. (2009). Active learning literature survey. University of Wisconsin-Madison Department of Computer Sciences.</li>
            
            <li>Deb, K. (2011). Multi-objective optimization using evolutionary algorithms: an introduction. Multi-objective evolutionary optimisation for product design and manufacturing, 1-24.</li>
            
            <li>Pearson, K. (1901). On lines and planes of closest fit to systems of points in space. The London, Edinburgh, and Dublin Philosophical Magazine and Journal of Science, 2(11), 559-572.</li>

            <li>Faloutsos, C., & Lin, K. (1995). FastMap: A fast algorithm for indexing, data-mining and visualization of traditional and multimedia datasets. ACM SIGMOD Record, 24(2), 163-174.</li>

            <li>Menzies, T., Kocagüneli, E., Minku, L., Peters, F., & Turhan, B. (2013). The PROMISE Repository of empirical software engineering data. West Virginia University, Department of Computer Science.</li>
            
            <li>Nair, V., Menzies, T., Siegmund, N., & Apel, S. (2018). Using bad learners to find good configurations. European Software Engineering Conference, 257-269.</li>
            
            <li>Xu, Z., Krushevskaja, D., Ley-Wild, R., & Xie, T. (2015). Gotcha: An interactive debugger for missing cache invalidations. IEEE Transactions on Software Engineering, 42(10), 940-958.</li>
            
            <li>Hutson, M. (2018). Artificial intelligence faces reproducibility crisis. Science, 359(6377), 725-726.</li>
        </ol>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/who.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - Credits</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>Credits and Acknowledgments</h1>

        <h2>Author</h2>
        <p>Kube.py was created by Tim Menzies (<a href="mailto:timm@ieee.org">timm@ieee.org</a>), a Professor 
        of Computer Science at North Carolina State University and a leading researcher in the field 
        of software analytics and search-based software engineering.</p>

        <h2>License</h2>
        <pre><code class="language-html">
MIT License

Copyright (c) 2025 Tim Menzies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
</code></pre>

        <h2>Intellectual Foundations</h2>

        <p>Kube.py stands on the shoulders of giants in several fields:</p>

        <h3>Dimensionality Reduction</h3>
        <ul>
            <li>Karl Pearson's work on principal component analysis (1901)</li>
            <li>Johnson-Lindenstrauss lemma on dimensionality preservation (1984)</li>
            <li>Fastmap algorithm by Faloutsos and Lin (1995)</li>
        </ul>

        <h3>Active Learning</h3>
        <ul>
            <li>Settles' work on active learning (2009)</li>
            <li>Menzies' research on sampling methods for software engineering (2007-2022)</li>
        </ul>

        <h3>Multi-Objective Optimization</h3>
        <ul>
            <li>Pareto's original work on optimality (1896)</li>
            <li>Deb's work on multi-objective evolutionary algorithms (2011)</li>
        </ul>

        <h3>Locality-Sensitive Hashing</h3>
        <ul>
            <li>Indyk and Motwani's original LSH algorithms (1998)</li>
            <li>Gionis, Indyk, and Motwani's work on similarity search (1999)</li>
        </ul>

        <h2>References</h2>

        <p>For a complete list of references, please see the <a href="references.html">References</a> page.</p>

        <h2>Contributing</h2>

        <p>Contributions to kube.py are welcome! Please fork the repository, make your changes, and submit a pull request.</p>

        <p>Key areas for future work include:</p>
        <ul>
            <li>Additional dimensionality reduction techniques</li>
            <li>Improved visualization tools</li>
            <li>More sophisticated active learning strategies</li>
            <li>Applications to new domains</li>
        </ul>

        <h2>Contact</h2>

        <p>For questions, suggestions, or collaboration opportunities, please contact Tim Menzies at <a href="mailto:timm@ieee.org">timm@ieee.org</a>.</p>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

mkdir -p 'site'
cat << 'EOF' > 'site/maths.html'
<!DOCTYPE html>
<html lang="en">
<head>
<link href="https://cdn.jsdelivr.net/npm/prismjs/themes/prism.css" rel="stylesheet" />
<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.js"></script>
<style>
code, pre {
  font-family: "Fira Code", "Source Code Pro", "Menlo", "Consolas", monospace;
  font-size: 0.95em;
  background-color: #f5f5f5;
  padding: 0.5em;
  border-radius: 6px;
  line-height: 1.5;
}
</style>

    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kube.py - Mathematical Foundations</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <div class="menu">
            <small><a href="index.html">Home</a> | <a href="why.html">Why</a> | <a href="eg.html">Eg</a> | <a href="maths.html">Maths</a> | <a href="what.html">What</a> | <a href="how.html">How</a> | <a href="who.html">Who</a> | <a href="details.html">Details</a></small>
        </div>
        <hr>
    </header>

    <div class="github-corner">
        <a href="https://github.com/timm/kube" aria-label="Fork me on GitHub">
            <svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true">
                <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
                <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path>
                <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
            </svg>
        </a>
    </div>

    <main>
        <h1>Mathematical and AI Foundations</h1>

        <h2>Minkowski Distance</h2>

        <p>Kube uses the Minkowski distance metric as the foundation for measuring 
        similarity between data points. The formula is:</p>

<pre><code class="language-html">d(x, y) = (Σ|x_i - y_i|^P)^(1/P)</code></pre>

        <p>Where:</p>
        <ul>
            <li>x and y are two data points</li>
            <li>n is the number of dimensions</li>
            <li>P is the distance formula exponent (default: 2)</li>
        </ul>

        <p>When P=2, this becomes the familiar Euclidean distance. Different P values 
        create different distance spaces, affecting how clusters form.[21]</p>

        <h2>Locality-Sensitive Hashing (LSH)</h2>

        <p>Kube implements a form of locality-sensitive hashing for efficient clustering. 
        LSH works by:</p>

        <ol>
            <li>Selecting representative "poles" from the data</li>
            <li>Projecting each data point onto the lines connecting these poles</li>
            <li>Creating a discrete hash based on these projections</li>
            <li>Grouping points with identical hashes</li>
        </ol>

        <p>This approach has O(n) complexity versus the O(n²) of traditional clustering 
        methods, making it efficient for large datasets.[22]</p>

        <h2>Active Learning</h2>

        <p>The code implements active learning principles by finding the most informative 
        examples in the data space. The <code>poles()</code> method identifies data points at 
        maximum distances from each other, effectively sampling the boundaries of the 
        data space.[23]</p>

        <p>Active learning focuses on selecting the most informative examples for labeling, 
        rather than randomly sampling the data space. This is particularly useful in domains 
        where labeling is expensive, as it maximizes the information gain from each labeled 
        example.</p>

        <h2>Multi-Objective Optimization</h2>

        <p>Kube optimizes for multiple objectives through the "heaven" concept. For each 
        column marked with "+" or "-" in the CSV header, kube attempts to maximize or 
        minimize those values, respectively. The <code>ydist()</code> method calculates how far 
        each data point is from the ideal point ("heaven").[24]</p>

        <p>In multi-objective optimization, there is rarely a single solution that optimizes all 
        objectives simultaneously. Instead, we seek Pareto-optimal solutions - those where improving 
        one objective necessarily worsens another. Kube's approach combines multiple objectives into 
        a single distance metric, allowing for intuitive ranking of solutions.</p>

        <h2>Dimensionality Reduction</h2>

        <p>At its core, kube implements a form of dimensionality reduction through projection. This 
        is related to the Johnson-Lindenstrauss lemma[15], which proves that high-dimensional data can 
        be projected into significantly lower dimensions while mostly preserving distances between points.</p>

        <p>By representing complex, high-dimensional data in a lower-dimensional space, kube makes it 
        possible to visualize and understand relationships that would otherwise be difficult to perceive.</p>
    </main>

    <footer>
        <hr>
        <small>&copy; 2025 Tim Menzies. MIT License.</small>
    </footer>
</body>
</html>
EOF

