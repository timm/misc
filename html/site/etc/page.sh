#!/usr/bin/env bash
cat<<-'EOF'
<!DOCTYPE html>
<html>
<head>
EOF

gawk 'sub(/^# /,""){print "<title>"$0"</title>"; exit}' $1

cat<<-'EOF'
  <meta charset="UTF-8">
  <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.7/css/all.css">
<link href="https://fonts.googleapis.com/css?family=Source+Code+Pro|Source+Sans+Pro:300,400,400i,700" rel="stylesheet">
  <script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      extensions: ["tex2jax.js"],
      jax: ["input/TeX", "output/HTML-CSS"],
      tex2jax: {
        inlineMath: [ ['$','$'], ["\\(","\\)"] ],
        displayMath: [ ['$$','$$'], ["\\[","\\]"] ],
        processEscapes: true
      },
      "HTML-CSS": { fonts: ["TeX"] }
    });
  </script>
  <script type="text/javascript"
     src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js">
   </script>
  <link rel="stylesheet" href="style.css">
</head>
<body>
<div class="parent">
<div class="left">
<center>
<p><a  href="index.html">home</a> :: 
<a href="">src</a> ::
<a href="">issues</a> </p>
<hr>

<h2>SE+AI: just the important bits</h2>
<p><a href="license">&copy;2023</a> by <a href="">Tim Menzies</a></p>


<img src="dots4.png" width=190  
  style="margin-bottom: 0px; padding-bottom: 0px; border-bottom: 1px solid black;">
</center>
EOF

# just print the TOC
gawk '/<div class="toc"/,/<\/div>/ {print $0; next}'  $1

cat<<'EOF'
    </div>
 <div class="right">
EOF

# print everything else except the TOC
gawk '/<div class="toc"/,/<\/div>/ {next} 1'  $1

cat<<'EOF'
    </div>
  </div>
 </body>
</html>
EOF
