#' Evaluate fitted models using Austin's method.
#'
#' @examples
#' p <- analysis_plan(mtcars)
#' f <- fit_fine_gray(p, Surv(mpg, vs) ~ gear)
#'
#' # Typical stratification on different values of a single variable
#' eval_austin(f, list(gear = 0, gear = 1))
#'
#' # Complex stratifications on more than one variable will also be possible (not yet)
#' eval_austin(f, list(gear = 0, gear = 1, list(gear = 0, cyl == 6)))
#'
#' @export
eval_austin <- function(.fits, ...) {
  UseMethod("eval_austin", object = .fits)
}

#' @export
eval_austin.analysis_fit <- function(.fits, .strata, .f = mean) {
  if (any(purrr::map_lgl(.strata, is.list))) {
    rlang::warn('.strata contained stratifications on multiple variables. These are not yet supported.')
  }

  predictions <- list()
  ids <- list()
  for (i in 1:length(.fits$data)) {
    fit_vals <- list()
    fit_ids <- list()

    prepped_data <- .fits$prep(.fits$data[[i]])
    for (j in 1:length(.strata)) {
      stratified_data <- single_stratified_evaluation(prepped_data, .strata[[j]], names(.strata)[[j]])
      stratified_evaluation <- predict(.fits$fits[[i]], stratified_data)
      fit_vals[[j]] <- .f(stratified_evaluation)

      id <- paste(names(.strata)[[j]], as.character(.strata[[j]]), sep = '=')
      fit_ids[[j]] <- id
    }
    predictions[[i]] <- fit_vals
    ids[[i]] <- purrr::map(fit_ids, ~paste(.fits$ids[[i]], '→Austin<', .x, '>', sep = ''))
  }
  predictions <- purrr::flatten(predictions)
  ids <- purrr::flatten(ids)

  # Index for which fits produced which results.
  which_fit <- rep(1:length(.fits$data), each = length(.strata))

  strata_meta <- paste(names(.strata), .strata, collapse = '/', sep = '=')
  function_name <- as.character(substitute(.f))
  meta <- paste(.fits$meta, '→Austin<', strata_meta, '>→:', function_name, sep = '')
  analysis_eval(predictions, ids, which_fit, meta)
}

#' @export
single_stratified_evaluation <- function(.data, .strata, .names) {
  # TODO: Check args
  strat <- rlang::enquos(.strata)
  for (i in 1:length(strat)) {
    name <- .names[[i]]
    value <- rlang::eval_tidy(strat[[i]], data = .data)
    .data <- mutate_value_only(.data, name, value)
  }
  .data
}
