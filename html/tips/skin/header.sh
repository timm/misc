cat<<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>
EOF
head -1 $1 | sed 's/# //'
cat<<'EOF'
    are Smarter</title>
    <link rel="stylesheet" href="skin/style.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/default.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/go.min.js"></script>
    <script>hljs.highlightAll();</script>
    <link rel="icon" href="favicon.png" sizes="32x32">
</head>
<body>
    <div class="container">
        <div class="sidebar">
            <center>
              <img src ="skin/me.png" width="100%">
              <h2 style="color: #CC000;" >Shorter, Smarter, Faster: the Craft of Sleek Scripting</h2>
            </center>
            <hr>
            <h2>Felidae (Cats)</h2>
            <ul>
                <li><a href="domesticcat.html">Domestic</a></li>
                <li>Wild
                    <ul>
                        <li><a href="lions.html">Lions</a></li>
                        <li><a href="tigers.html">Tigers</a></li>
                    </ul>
                </li>
            </ul>
            <h2>Canidae (Dogs)</h2>
            <ul>
                <li><a href="domesticdog.html">Domestic</a></li>
                <li>Wild
                    <ul>
                        <li><a href="wolves.html">Wolves</a></li>
                        <li><a href="foxes.html">Foxes</a></li>
                    </ul>
                </li>
            </ul>
        </div>
        <div class="content">
          <p style="text-align: right">
              <a href="https://creativecommons.org/licenses/by/4.0/" target="_blank">(c)2024, Tim Menzies <timm@ieee.org>,  CC 4.0</a>
          </p>
EOF