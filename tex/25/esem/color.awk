#!/usr/bin/gawk -f
BEGIN {
    # groups by CSS color names
    k["red"]     = "if|else|elif|while|for|break|continue|return|pass|yield"
    k["green"]   = "def|class|import|from|as|global|nonlocal|lambda"
    k["blue"]    = "print|input|len|range|enumerate|zip|open|with"
    k["magenta"] = "True|False|None|self|__init__|__name__|__main__"

    reset = "</span>"
}

/^```/ {
    print "<pre><code>"
    while ((getline line) > 0 && line !~ /^```/) {
        # escape HTML special chars first
        gsub(/&/, "&amp;", line)
        gsub(/</, "&lt;",  line)
        gsub(/>/, "&gt;",  line)

        for (c in k) {
            gsub("\\<(" k[c] ")\\>", "<span style=\"color:"c"\">&"reset, line)
        }
        print line
    }
    print "</code></pre>"
    next
}

{ print }
