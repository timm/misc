#!/usr/bin/gawk -f
BEGIN {
    # Tango color scheme
    k["#A40000"] = "\\<(def|class|lambda|import|from|as|with|global|nonlocal|if|elif|else|for|while|try|except|continue|break|return|yield|pass)\\>"
    k["#204A87"] = "\\<(print|input|len|range|enumerate|zip|open)\\>"
    k["#5C3566"] = "\\<(True|False|None)\\>"
    k["#555753"] = "\\<(self|__init__|__name__|__main__)\\>"
    str_pat      = "\"[^\"]*\"|'[^']*'"   # Strings
    str_color    = "#8F5902"              # Brown
}

/^```/ {
    print "<pre><code>"
    while ((getline line) > 0 && line !~ /^```/) {
        # escape HTML first
        gsub(/&/, "\\&amp;", line)
        gsub(/</, "\\&lt;",  line)
        gsub(/>/, "\\&gt;",  line)

        # highlight strings first
        gsub(str_pat, "<span style=\"color:" str_color "\">&</span>", line)

        # highlight keywords, builtins, constants, etc.
        for (c in k)
            gsub(k[c], "<span style=\"color:" c "\">&</span>", line)

        print line
    }
    print "</code></pre>"
    next
}
{ print }


