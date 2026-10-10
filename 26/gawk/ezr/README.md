# ezr0 as a gawk pipeline

ezr0.py (149 lines of python), redone as unix stages:

    cat data.csv | bestRest.awk | test.awk | xplain.awk

    ./ezr0 [file] [seed]     # runs the pipeline above

| file         | job                                              |
|--------------|--------------------------------------------------|
| lib.awk      | norm, gap, ydist, xdist; tagged-line metadata    |
| bestRest.awk | shuffle, label 45, sort by ydist, 2 centroids    |
| test.awk     | rank unseen by xdist(best)-xdist(rest), check 5  |
| xplain.awk   | centroid gaps, biggest first                     |
| cook.awk     | optional: discretize NUMs to bins 0..6, print    |
|              | each row as a raw/cooked two-line paragraph      |

Stages talk csv with tagged metadata lines (`=col`, `=best`,
`=rest`, `=opt`, `=picked`), so any stage boundary can be
inspected with `head` or swapped out.

## Verdict: shorter? no. clearer? differently.

150 lines of gawk vs 149 of python -- a wash. What changes is
the shape, not the size:

- The algorithm becomes four nouns on a pipe; each boundary is
  plain text you can look at.
- test.awk (23 lines) and xplain.awk (18) are genuinely tiny;
  the bulk hides in bestRest (62) because sorting, shuffling,
  and centroids are hand-rolled in awk but free in python.
- Lost: the Tbl/clone machinery, so ydist here always uses
  full-table stats (python sorts its labelled rows by the
  labelled-only clone's stats). Same spirit, not bit-identical;
  also different RNG, so no seed-for-seed match.
- cook.awk is the idea python makes awkward and pipes make
  natural: discretize once (`int(7*norm())`), then every later
  stage does integer compares, and per-bin y statistics -- the
  ranges a tree would cut on -- are one array index away.
