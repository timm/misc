#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib"
@include "my"
@include "num"
@include "sym"
@include "row"
@include "rows"
@include "shared"

#BEGIN { _all() }
#BEGIN {_rows(); rogues()}
BEGIN { rogues() }
