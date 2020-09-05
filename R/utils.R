#' @export
process_dots <- function(...) {
  dots <- rlang::list2(...)
  dots <- purrr::map_if(dots, plyr::is.formula, purrr::as_mapper)
  dots <- purrr::map_if(dots, is.list, function(d) purrr::map(d, purrr::as_mapper))
  unlist(dots, recursive = FALSE)
}

#' @export
process_formula <- function(formula) {
  if (inherits(formula, 'list')) {
    n_named <- 0
    for (i in 1:length(formula)) {
      if (names(formula)[[i]] == '') {
        n_named <- n_named + 1
        names(formula)[[i]] <- paste('formula', n_named, sep = '_')
      }
    }
  } else {
    formula <- list('formula' = formula)
  }
  formula
}
