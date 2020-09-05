# TODO: Build this evaluation step


#' Evaluate fitted models using Austin's method.
#'
#' @example
#' p <- analysis_plan(mtcars)
#' f <- fit_fine_gray(p, Surv(mpg, vs) ~ gear)
#'
#' # Typical stratification on different values of a single variable
#' eval_austin(f, list(gear = 0, gear = 1))
#'
#' # Complex stratifications on more than one variable are also possible
#' eval_austin(f, list(gear = 0, gear = 1, list(gear = 0, cyl == 6)))
#'
#' @export
eval_austin <- function(.fits, ...) {
  UseMethod("eval_austin")
}

#' @export
eval_austin.analysis_fit <- function(.fits, strata) {
  # Process ids for the evaluations (like name = value -> name:value/ or something)

}

#' @export
single_stratified_evaluation <- function(.data, ...) {
  .strat <- rlang::enquos(...)
  strat_names <- names(.strat)
  .data
  .strat
}
