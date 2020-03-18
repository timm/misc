#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

BEGIN {
  MY.num="\\$"
  MY.less="<"
  MY.more=">"
  MY.klass="!"
  MY.goal=   "[" MY.less MY.more MY.klass "]"
  MY.numeric="[" MY.less MY.more MY.num   "]"
  MY.skip="\\?" 
  #--------------
  MY.step = 0.5
}


