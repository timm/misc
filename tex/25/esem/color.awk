#!/usr/bin/gawk -f
    # Softer color scheme
BEGIN {
    # Softer color scheme
    k["seagreen"]   = "\\<(def|class|lambda|import|from|as|with|global|nonlocal)\\>"
    k["steelblue"]  = "\\<(if|elif|else|for|while|try|except|continue|break|return|yield|pass)\\>"
    k["darkorange"] = "\"[^\"]*\"|'[^']*'"  # Strings, both "..." and '...'
    k["mediumpurple"]= "\\<(True|False|None)\\>"
    k["dimgray"]    = "\\<(self|__init__|__name__|__main__)\\>"
    k["firebrick"]  = "\\<(print|input|len|range|enumerate|zip|open)\\>"
}


/^```/ {
    print "<pre><code>"
    while ((getline line) > 0 && line !~ /^```/) {
        # escape HTML special chars first
        gsub(/&/, "\\&amp;", line)
        gsub(/</, "\\&lt;",  line)
        gsub(/>/, "\\&gt;",  line)

        for (c in k)
            gsub(k[c], "<span style=\"color:" c "\">&</span>", line)

        print line
    }
    print "</code></pre>"
    next
}
{ print }

BEGIN {
    # groups by CSS color names
    k["red"]     = "\\<(if|else|elif|while|for|break|continue|return|pass|yield)\\>"
    k["green"]   = "\\<(def|class|import|from|as|global|nonlocal|lambda)\\>"
    k["blue"]    = "\\<(print|input|len|range|enumerate|zip|open|with)\\>"
    k["magenta"] = "\\<(True|False|None|self|__init__|__name__|__main__)\\>"
    k["orange"]  = "\"[^\"]*\"" } # Double-quoted strings, no \b }

/^```/ {
    print "<pre><code>"
    while ((getline line) > 0 && line !~ /^```/) {
        # escape HTML special chars first
        gsub(/&/, "\\&amp;", line)
        gsub(/</, "\\&lt;",  line)
        gsub(/>/, "\\&gt;",  line)
        for (c in k) gsub(k[c], "<span style=\"color:" c "\">&</span>", line)
        print line
    }
    print "</code></pre>"
    next
}
{ print }
