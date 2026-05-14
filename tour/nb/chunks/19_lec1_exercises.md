<!-- chunk: 19_lec1_exercises -->
<!-- prev: 18_data_add  next: 20_lec2_open -->

### Lecture 1 exercises

1. Run prompts [ref:l1-start], [ref:l1-end], and the
   `eg["--num"]` line in your local REPL. Paste the output into
   a file. Then add the value `"?"` to a [`Sym`](#sym) and to a
   [`Num`](#num) — what changes, and what does not?
2. Modify [`SYM.add`](#dataadd) so that a missing value `"?"`
   still increments `n` and is stored in `has` under the key
   `"?"`. Rerun the `eg["--sym"]` line; explain the new output.
3. Predict by hand what `Num(0,"X")` returns just before any
   `add` has been called. Then verify in the REPL. Why is `sd`
   initialized to `0` and not `nil`?

**Homework (due before Lecture 2):** Port [`Sym`](#sym),
[`Num`](#num), [`Col`](#col), and [`adds`](#adds) to a language
you are already fluent in (Python, Java, JavaScript). Single
file, no third-party libraries. The same input
`{10,20,30,40,50}` and `{"a","a","a","b","c"}` must produce the
same mean, sd, and counts. Submit on the LMS.
