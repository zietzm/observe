#' @export
process_dots <- function(...) {
  dots <- rlang::list2(...)
  dots <- purrr::map_if(dots, plyr::is.formula, purrr::as_mapper)
  dots <- purrr::map_if(dots, is.list, function(d) purrr::map(d, purrr::as_mapper))
  unlist(dots, recursive = FALSE)
}

#' @export
process_formula <- function(formula) {
  processed <- process_dots(formula)
  if (plyr::is.formula(formula)) {
    processed <- purrr::set_names(processed, c('formula'))
  }
  processed
}
