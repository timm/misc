# prep.awk : small markdown -> one html page. Simpler than md2html.awk.
#   awk -f etc/prep.awk what1.md > what1.html
#
#   ------ (60+ dashes or underscores)  start/stop a centered block
#   # A .. #### D                h1 .. h4
#   ### Refs                     h3, then dump every footnote seen so far
#   ---   /  --- #id             <hr> (with id)
#   - text                       bullet
#   (3) text                     paragraph led by a round icon holding 3
#   > text                       stand-out box (blockquote)
#   | a | b |  / |---|---:|      table; separator row; ":" right-aligns
#   @ text                       reference div
#   [text](url) **b** *em*       as usual
#   word[^name]                  footnote mark; numbered in order of use
#   [^name]: text                footnote body (runs to blank line);
#                                held back until the next "### Refs"
#   <tag ...                     html passes through, unwrapped
#   blank line                   paragraph break

BEGIN { CSS = "\
@import url('https://fonts.googleapis.com/css2?family=Marcellus&display=swap');\
:root { --fg: #000; --bg: #fff }\
:root[data-theme=dark] { --fg: #eee; --bg: #000 }\
* { margin: 0; padding: 0; box-sizing: border-box }\
body { background: var(--bg); color: var(--fg);\
       font: 16px/1.45 Optima, Marcellus, 'Gill Sans', sans-serif }\
main { max-width: 600px; margin: 0 auto; padding: 20px 10px 40px 20px }\
#mode { position: fixed; top: 12px; right: 14px;\
        background: none; border: 0; color: inherit; font-size: 18px }\
h1, h2, h3, h4 { margin-top: 48px; font-size: 1em;\
                 text-transform: uppercase; letter-spacing: .08em }\
h1 + hr, h2 + hr, h3 + hr { display: none }\
p { margin-top: 16px }\
blockquote { border-left: 3px solid; padding-left: 12px; margin-top: 16px }\
hr, .center { border: 0; border-top: 2px solid; margin: 36px 0 }\
.center { border-bottom: 2px solid; text-align: center; padding: 12px 0; margin-top: 0 }\
.num { display: inline-block; width: 1.6em; height: 1.6em; line-height: 1.6em;\
       border-radius: 50%; background: var(--fg); color: var(--bg);\
       text-align: center; font-weight: 700; margin-right: .5em }\
ul { margin-top: 12px; padding-left: 1.4em }\
table { border-collapse: collapse; margin: 16px 0; font-size: .85em }\
th, td { border: 1px solid; padding: 3px 8px; text-align: left }\
.ref, .fn { font-size: .8em; margin-top: 10px; opacity: .75; overflow-wrap: anywhere }\
a { color: inherit }\
.fn a, .ref a { text-decoration: none; border-bottom: 1px dotted }\
img { max-width: 100%; margin: 0 0 8px 16px }\
img[align=right] { float: right }\
sup { font-size: .7em; line-height: 0 }\
@media (max-width: 600px) { img { float: none; width: 100% } }" }
BEGIN { JS = "\
var r=document.documentElement,b=document.getElementById('mode');\
var m=localStorage.mode||(matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light');\
r.dataset.theme=m;\
b.onclick=function(){m=r.dataset.theme=localStorage.mode=(r.dataset.theme=='dark'?'light':'dark')}" }
BEGIN { print "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
        print "<meta name=\"viewport\" content=\"width=device-width\">"
        print "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css\">"
        print "<style>" CSS "</style></head><body>"
        print "<button id=\"mode\" title=\"light/dark\"><i class=\"fa-solid fa-circle-half-stroke\"></i></button>"
        print "<script>" JS "</script><main>" }
END   { refs(); flush(); endul(); endtab(); print "</main></body></html>" }

function span(s, re, n, tag,   out, mid) {      # *x* / **x** -> <tag>x</tag>
  out = ""
  while (match(s, re)) {
    mid = substr(s, RSTART+n, RLENGTH-2*n)
    out = out substr(s, 1, RSTART-1) "<" tag ">" mid "</" tag ">"
    s = substr(s, RSTART+RLENGTH) }
  return out s }

function inline(s,   out, mid, txt, url) {
  out = ""
  while (match(s, /\[\^[A-Za-z0-9_-]+\]/)) {      # [^name] -> footnote mark
    mid = substr(s, RSTART+2, RLENGTH-3)
    if (!(mid in fnum)) { fnum[mid] = ++FN; order[FN] = mid }
    out = out substr(s, 1, RSTART-1) "<sup id=\"r-" mid "\"><a href=\"#fn-" mid "\">" fnum[mid] "</a></sup>"
    s = substr(s, RSTART+RLENGTH) }
  s = out s; out = ""
  while (match(s, /\[[^]]+\]\([^)]+\)/)) {        # [text](url) -> <a>
    mid = substr(s, RSTART, RLENGTH)
    txt = mid; sub(/^\[/, "", txt); sub(/\]\(.*$/, "", txt)
    url = mid; sub(/^.*\]\(/, "", url); sub(/\)$/, "", url)
    out = out substr(s, 1, RSTART-1) "<a href=\"" url "\">" txt "</a>"
    s = substr(s, RSTART+RLENGTH) }
  s = span(out s, "\\*\\*[^*]+\\*\\*", 2, "b")
  return span(s, "\\*[^*]+\\*", 1, "em") }

function autolink(s) {                           # bare url -> <a>
  if (match(s, /https?:\/\/[^ <>)]+/))
    s = substr(s,1,RSTART-1) "<a href=\"" substr(s,RSTART,RLENGTH) "\">" substr(s,RSTART,RLENGTH) "</a>" substr(s,RSTART+RLENGTH)
  return s }

function flush(   n) {                           # end the current paragraph
  if (fnname != "") { fn[fnname] = buf; fnname = ""; buf = ""; return }
  if (buf == "") return
  endul()
  if (match(buf, /^\([0-9]+\) /)) {              # (3) text -> round icon
    n = substr(buf, 2, RLENGTH-3); buf = substr(buf, RLENGTH+1)
    print "<p><span class=\"num\">" n "</span>" inline(buf) "</p>" }
  else if (bq) print "<blockquote>" inline(buf) "</blockquote>"
  else print "<p>" inline(buf) "</p>"
  buf = ""; bq = 0 }

function refs(   i, k) {                         # dump footnotes seen so far
  for (i = 1; i <= FN; i++) {
    k = order[i]
    if (k in done) continue
    if (!(k in fn)) continue                     # marked, body not yet seen
    print "<div class=\"fn\" id=\"fn-" k "\"><a href=\"#r-" k "\">" i "</a>. " autolink(inline(fn[k])) "</div>"
    done[k] = 1 } }

function endul() { if (inul) { print "</ul>"; inul = 0 } }
function endtab() { if (intab) { print "</table>"; intab = 0; thead = 0; split("", align) } }

/^(-{60,}|_{60,})[ \t]*$/ { flush(); endul()     # centered block on/off
  if (center) { print "</div>"; center = 0 } else { print "<div class=\"center\">"; center = 1 }
  next }
/^###[ \t]+(Refs|References)[ \t]*$/ { flush(); endul(); print "<h3>References</h3>"; refs(); next }
/^####[ \t]/ { flush(); endul(); sub(/^####[ \t]+/, ""); print "<h4>" inline($0) "</h4>"; next }
/^###[ \t]/  { flush(); endul(); sub(/^###[ \t]+/, "");  print "<h3>" inline($0) "</h3>"; next }
/^##[ \t]/   { flush(); endul(); sub(/^##[ \t]+/, "");   print "<h2>" inline($0) "</h2>"; next }
/^#[ \t]/    { flush(); endul(); sub(/^#[ \t]+/, "");    print "<h1>" inline($0) "</h1>"; next }
/^---([ \t]+#[A-Za-z0-9_-]+)?[ \t]*$/ { flush(); endul()
  id = $0; sub(/^---[ \t]*#?/, "", id)
  print (id == "" ? "<hr>" : "<hr id=\"" id "\">"); next }
/^-[ \t]+/   { flush(); if (!inul) { print "<ul>"; inul = 1 }
  sub(/^-[ \t]+/, ""); print "<li>" inline($0) "</li>"; next }
/^@[ \t]+/   { flush(); endul(); sub(/^@[ \t]+/, ""); print "<div class=\"ref\">" inline($0) "</div>"; next }
/^\|/ {                                          # table row
  flush(); endul()
  if (!intab) { print "<table>"; intab = 1; thead = 1 }
  s = $0; sub(/^\|/, "", s); sub(/\|[ \t]*$/, "", s)
  n = split(s, c, /\|/)
  if (s ~ /^[ \t:|-]+$/) { for (i = 1; i <= n; i++) align[i] = (c[i] ~ /:[ \t]*$/) ? " style=\"text-align:right\"" : ""; next }
  row = ""
  for (i = 1; i <= n; i++) { gsub(/^[ \t]+|[ \t]+$/, "", c[i]); tag = thead ? "th" : "td"
    row = row "<" tag align[i] ">" inline(c[i]) "</" tag ">" }
  print "<tr>" row "</tr>"; thead = 0; next }
/^</         { flush(); print; next }             # raw html
/^\[\^[A-Za-z0-9_-]+\]:/ { flush(); endtab()      # footnote body starts
  fnname = $0; sub(/^\[\^/, "", fnname); sub(/\]:.*$/, "", fnname)
  sub(/^\[\^[A-Za-z0-9_-]+\]:[ \t]*/, ""); buf = $0; next }
/^[ \t]*$/   { flush(); endtab(); next }          # blank line
             { endtab(); if (sub(/^>[ \t]?/, "")) bq = 1
               buf = (buf == "" ? $0 : buf " " $0) }
