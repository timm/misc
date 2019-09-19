#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

import re, sys
import random
from the import THE
from lib import *
from thing import Num,Sym
from tbl import Tbl,string


def main():
    what = """
    mood, $cloudCover, $temp, ?$humid, <wind,  $playHours
    happy, 100,        68,    80,    0,    3   # comments
    sad,   0,          85,    85,    0,    0
    
    happy, 0,          80,    90,    10,   0
    happy, 60,         83,    86,    0,    4
    sad  , 100,        70,    96,    0,    3
    happy, 100,        65,    70,    20,   0
    happy, 70,         64,    65,    15,   5
    sad  , 0,          72,    95,    0,    0
    happy, 0,          69,    70,    0,    4
    happy, ?,          75,    80,    0,    ?
   sad  ,  0,          75,    70,    18,   4
    happy, 60,         72,
    sad  , 40,         81,    75,    0,    2
    happy, 100,        71,    91,    15,   0
    """

    tbl = Tbl()
    tbl.read(string(what))
    [print(x) for x in tbl.cols]
    [print(x) for x in tbl.rows]


if __name__ == "__main__":
    main()


