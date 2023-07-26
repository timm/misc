cat<<-'EOF'
<!DOCTYPE html>
<html>
<head>
EOF

gawk 'sub(/^# /,""){print "<title>"$0"</title>"}' $1

cat<<-'EOF'
  <link rel="stylesheet" href="https://fonts.googleapis.com/css?family='Roboto Mono'">
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="b.css">
  <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.7/css/all.css">
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
<img src="dots3.png" width=200 align=left
  style="margin-bottom: 0px; padding-bottom: 0px;"> 
<p style="text-align:right;">

<a  href="index.html">home</a> :: 
<a href="">src</a> ::
<a href="">issues</a><br> 
    <span style="float:left;color:rgb(72,14,120);"> <b>SE+AI: a programmer's guide</b></span>
<br>
    <span style="float:left;"><a href="license">&copy;2023</a> by <a href="">Tim Menzies</a></span>
</p>
<br clear=all>

EOF
