# data.awk -- Data: header parse + row table.
# Pure ingestion. No y/target logic, no splits. Apps add what they need.

function data_init(it) {
  arr(.it.cols); arr(.it.rows); arr(.it.all)
  arr(.it.y);    arr(.it.hdr);  arr(.it.nump)
  .it.nc = 0; .it.nrows = 0; .it.klass = 0; .it.ykind = ""
  return it }

function data_head(d, line,    i, n, f, name) {
  n = split(line, f, " *, *")
  .d.nc = n
  for (i = 1; i <= n; i++) {
    name = f[i]
    .d.hdr[i]  = name
    .d.nump[i] = (name ~ /^[A-Z]/)
    .d.cols[i] = new(.d.nump[i] ? "num" : "sym")
    if      (name ~ /-$/)  { .d.y[i] = 0; .d.ykind = "num" }
    else if (name ~ /\+$/) { .d.y[i] = 1; .d.ykind = "num" }
    else if (name ~ /!$/)  { .d.klass = i } }
  if (.d.ykind == "" && .d.klass) .d.ykind = "sym" }

function data_read(d, training,    r, i) {
  r = ++.d.nrows
  for (i = 1; i <= .d.nc; i++)
    .d.rows[r][i] = add(.d.cols[i], $i, training)
  if (training) .d.all[r] = r
  return r }
