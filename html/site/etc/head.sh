cat<<-'EOF'
<!DOCTYPE html>
<html>
<head>
EOF

gawk 'sub(/^# /,""){print "<title>"$0"</title>"}' $1

cat<<-'EOF'
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="b.css">
  <xlink rel="stylesheet" href="css/default.css">
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
<font    style="color:rgb(72,14,120);">
<img src="dots3.png" width=250 align=left
  style="margin-bottom: 0px; padding-bottom: 0px;"> 
   <p style="text-align:right; padding-top: 0px; margin-top: 0px;">
<a  href="index.html">home</a> :: 
<a href="">src</a> ::
<a href="">issues</a> </p>
<h3>SE+AI: just the important bits</h3>
<a href="license">&copy;2023</a> by <a href="">Tim Menzies</a>
</font><br clear=all>
<hr style="paddiing-top:0px; margin-top:0px;">

EOF
