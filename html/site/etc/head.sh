cat<<-'EOF'
<!DOCTYPE html>
<html>
<head>
EOF

gawk 'sub(/^# /,""){print "<title>"$0"</title>"}' $1

cat<<-'EOF'
  <meta charset="UTF-8">
  <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.7/css/all.css">
  <link rel="stylesheet" href="style.css">
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
</head>
<body>

<div id="left">
<center>
<p><a  href="index.html">home</a> :: 
<a href="">src</a> ::
<a href="">issues</a> </p>
<hr>

<font style="color:rgb(72,14,120);">
<h1>SE+AI:<br>just the important bits</h1>
<p><a href="license">&copy;2023</a> by <a href="">Tim Menzies</a></p>
<img src="dots3.png" width=200 align=left style="margin-bottom: 0px; padding-bottom: 0px;"> 
<hr style="paddiing-top:0px; margin-top:0px;">
</font>
</center>
EOF
cat tmp_toc

cat<<'EOF'
    </div>
 <div id="right">
EOF
