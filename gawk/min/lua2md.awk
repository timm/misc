{a[NR]=$0}
END {
    for(i=1;i<=length(a);i++) {
      if (a[i] == "") {
        if (a[i+1]       ~ /^    /) {a[i]=a[i]"\n```lua"}
        else if (a[i-1] !~ /^     /) {a[i]="```"a[i] }}}
    for(i=1;i<=length(a);i++) {
      sub(/--\[\[/,"",a[i])
      sub(/--\]\]/,"",a[i])
      sub(/^    /,"",a[i])
      print a[i]
    }}

