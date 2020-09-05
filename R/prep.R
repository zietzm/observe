#' Bootstrap data preprocessing
#'
#' @export
prep_bootstrap <- function(x, n, include_original = TRUE) {
  UseMethod("prep_bootstrap")
}

#' @export
prep_bootstrap.analysis_plan <- function(.plan, n, inclue_original = TRUE) {
  new_data <- purrr::map(
    .plan$data, ~rsample::bootstraps(.x, times = n, apparent = inclue_original))

  # Each original id gets n new ids, one for each bootstrap iteration
  new_ids <- purrr::map(new_data, ~.x$id)
  new_ids <- purrr::map2(.plan$ids, new_ids, ~paste(.x, .y, sep = '->'))

  # Combine all the bootstrapped data
  new_data <- dplyr::bind_rows(new_data)

  # Assign attributes of returned object
  .plan$data <- as.list(new_data$splits)
  .plan$ids <- purrr::flatten(new_ids)
  .plan$meta <- paste(.plan$meta, '->bootstrap:', n, sep = '')

  # Add an additional preprocessing step to evaluate the rsplit objects
  preprocess <- match.fun(.plan$prep)
  .plan$prep <- function(data) rsample::analysis(preprocess(data))
  .plan
}

#' Matching data preprocessing
#'
#' @export
prep_match <- function(x, ...) {
  UseMethod("prep_match")
}

# A big limitation of this implementation is that it pulls everything into memory
# all at once. This means we lose all the benefits of rsample's optimizations,
# as well as the ability to limit peak memory use. Ideally, this should add a
# preprocessing step instead, which can be called LAZILY.
#' @export
prep_match.analysis_plan <- function(.plan, ...) {
  dots <- process_dots(...)
  auto_named_dots <- names(rlang::enquos(..., .named = TRUE))

  # Update analysis
  new_data <- purrr::cross2(dots, .plan$data)
  .plan$data <- purrr::map(new_data, ~.x[[1]](.plan$prep(.x[[2]])))

  # Reset preprocessing
  .plan$prep <- function(data) data

  # Update meta information and ids
  match_names <- paste(auto_named_dots, sep = '/', collapse = '/')
  .plan$meta <- paste(.plan$meta, '->match:', match_names, sep = '')

  auto_named_dots <- paste('match:', auto_named_dots, sep = '')
  new_ids <- purrr::cross2(.plan$ids, auto_named_dots)
  .plan$ids <- purrr::map(new_ids, ~paste(.x, collapse = '->'))
  .plan
}

#' Split analysis along a variable
#'
#'
#' @export
prep_split_by <- function(x, ...) {
  UseMethod("prep_split_by")
}

#' @export
prep_split_by.analysis_plan <- function(.plan, ...) {
  new_splits <- rlang::enquos(...)
  new_splits <- new_splits[!purrr::map_lgl(new_splits, rlang::quo_is_missing)]

  prepped_data <- purrr::map(.plan$data, .plan$prep)
  prepped_data <- purrr::cross2(prepped_data, new_splits)
  split_data <- purrr::map(prepped_data, ~split(.x[[1]], rlang::eval_tidy(.x[[2]], .x[[1]])))

  # Reset preprocessing
  .plan$prep <- function(x) x

  # `split` sets names by the value of the split variable. Capture then remove these.
  # `names_by_split` is a nested list of values for each split variable
  names_by_split <- purrr::map(split_data, names)
  .plan$data <- set_names(purrr::flatten(split_data), c())

  # Follow the same order of steps to get ids in the correct order
  new_ids <- purrr::cross2(.plan$ids, as.character(new_splits))
  new_ids <- purrr::map(new_ids, ~paste(.x[[1]], '->split:', .x[[2]], sep = ''))
  new_ids <- purrr::map2(new_ids, names_by_split, ~paste(.x, .y, sep = '='))
  .plan$ids <- purrr::flatten(new_ids)

  # Add variables split by as new meta information
  splits_meta <- paste(as.character(new_splits), collapse = '/')
  .plan$meta <- paste(.plan$meta, '->split:', splits_meta, sep = '')
  .plan
}

#' Filter analysis plan using ids or data
#'
#' @examples
#' p <- analysis_plan(mtcars)
#' s <- prep_match(p, a = ~., b = ~dplyr::sample_n(., 2), c = ~dplyr::sample_n(., 2))
#' # Return analysis with just a
#' prep_filter(s, nrow(data) > 2)
#' # Return analysis with a and c
#' prep_filter(s, nrow(data) > 2 | str_detect(id, 'c'))
#' # More complex
#' prep_filter(s, sum(data$vs) > 1)
#'
#' @export
prep_filter <- function(x, ...) {
  UseMethod("prep_filter")
}

#' @export
prep_filter.analysis_plan <- function(.plan, ...) {
  conditions <- rlang::enexprs(...)
  .plan$data <- purrr::map(.plan$data, .plan$prep)
  keep_by_condition <- purrr::map(conditions, ~filter_single(.plan, !!.x))
  keep <- purrr::pmap_lgl(keep_by_condition, all)
  .plan$data <- purrr::keep(.plan$data, keep)
  .plan$ids <- purrr::keep(.plan$ids, keep)

  full_meta <- purrr::map_chr(conditions, rlang::expr_label)
  full_meta <- paste(full_meta, collapse = '&')
  .plan$meta <- paste(.plan$meta, '->filter:', full_meta, sep = '')
  .plan
}

#' @export
filter_single <- function(.plan, condition) {
  condition <- rlang::enexpr(condition)
  build_func <- rlang::quo(function(id, data) !!condition)
  func <- rlang::eval_tidy(build_func)
  full_call <- rlang::quo(purrr::map2_lgl(.plan$ids, .plan$data, func))
  keep <- rlang::eval_tidy(full_call)
  keep
}

