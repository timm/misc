same(s) = s
int(x)  = floor(Int,x)
any(a)  = a[ int(length(a) * rand()) + 1]
few(a,n=the.divs.few) = length(a) < n  ? a : [any(a)  for _ in 1:n]

function say(i)
  s,pre="$(typeof(i)){",""
  for f in sort!([x for x in fieldnames(typeof(i)) if !("$x"[1] == '_')])
    g = getfield(i,f)
    s = s * pre * "$f=$g"
    pre=", "
  end
  print(s * "}")
end


