#!/usr/bin/env ./fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

function Globals(i) {
  i.dot  = sprintf("%c",46)
  i.some.magic  = 2.56
  i.some.max    = 256
  i.stats.cliffs.small   = 0.147
}
BEGIN { Globals(FUN) }
