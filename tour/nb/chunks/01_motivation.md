<!-- chunk: 01_motivation -->
<!-- prev: 00_config  next: 02_peek -->

## Why read this

You have probably been told that machine learning is hard. Most
textbooks open with linear algebra and end with a deep network you
cannot afford to train. This tour does the opposite. It walks you,
line by line, through a complete classifier that fits in 160 lines of
plain Lua. It reads a CSV file. It learns. It guesses. It is roughly
as accurate, on tabular data, as a model a thousand times its size.

The point is not Lua. The point is that a working learner is small,
mechanical, and built from parts you can hold in your head. Once you
see how `nb.lua` is put together, you can rebuild it in Python or
Java in an afternoon. The homework at the end of each lecture asks
you to do exactly that.

You will see a few ideas wear two hats. A column of values is also
its own summary. A summary is also a probability model. Training is
also testing. We will spend most of our prose budget on those
crossings, because they are the parts that pay off later when you
read a tree learner, a clusterer, or a recommender — the bones are
the same.

You are allowed to skim. K&R wrote that the first program in any
language is the same — print "hello, world" — and the hard part is
the mechanics around it. This tour is in that spirit. The complete
story is the source file; the prose is here to keep you from getting
lost.
