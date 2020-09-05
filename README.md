# observe: simplifying observational medical research in R

Sanity-preserving tools for combinatorial research tasks.

Things can quickly get out of hand when analyses start to require multiple types of adjustment, multiple match conditions, multiple outcomes, multiple stratifications, and multiple datasets.


`observe` is a package to hide these complexities from the end user.
For example, I'm imagining something like this:

```R
df %>%
  plan_analysis() %>%
  prep_bootstrap(n, include_original = TRUE) %>%
  prep_match(list(unmatched = match_identity,
                  matched = ~match_stratified_propensity(formula, bins, ratio))) %>%
  fit_fine_gray(list(adjusted = adjusted_formula, unadjusted = unadjusted_formula)) %>%
  eval_austin_method(strata) %>%
  summ_metrics('RR', 'NNT', 'ARR', 'AR', options = list(reference = 'Not exposed')) %>%
  run_analysis()
```


Random note for myself:

Objects need:

1. Constructor/validator
2. is.*** 
3. print method (format method if complicated)

