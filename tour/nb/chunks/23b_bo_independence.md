<!-- chunk: 23b_bo_independence -->
<!-- prev: 23a_bo_logprob  next: 24_nb -->

### Breakout (AI): The naive Bayes assumption

> When we multiplied per-column likelihoods together in
> [`DATA.likes`](#datalikes), we were pretending the columns are
> *independent* given the class. In real data they almost never
> are: weight and waist-size are correlated; PLAS (plasma glucose)
> and INSU (insulin) move together.
>
> Naive Bayes ignores this. It treats every column as if knowing
> one tells you nothing about the others. That is why it is *naive*.
>
> The remarkable empirical fact is that the classifier still works.
> Even when the absolute probabilities it computes are wrong, the
> *ranking* of classes is often right, because the same correlation
> error inflates every class's score in roughly the same way. That
> is why a 160-line program can hold its own against a deep network
> on tabular data.
