#' A description of analysis_plan
#'
#' Details of analysis_plan
#'
#' @title analysis_plan: This function
#' @param .data analysis data
#' @param .name starting name for the data
#' @export
analysis_plan <- function(.data, .name) {
  UseMethod("analysis_plan")
}

#' @export
analysis_plan.data.frame <- function(.data, .name = 'original') {
  new_analysis_plan(list(.data), list(.name), function(x) x, 'data.frame')
}

#' @export
new_analysis_plan <- function(.data, .ids, .prep, .meta) {
  if (!inherits(.data, 'list')) {
    cnd <- rlang::warning_cnd("error", message = ".data must be a list of data.frames")
    rlang::cnd_signal(cnd)
  }
  if (!inherits(.ids, 'list')) {
    cnd <- rlang::warning_cnd("error", message = ".ids must be a list")
    rlang::cnd_signal(cnd)
  }
  if (!is.function(.prep)) {
    cnd <- rlang::warning_cnd("error", message = ".prep must be a function")
    rlang::cnd_signal(cnd)
  }
  if (!is.character(.meta)) {
    cnd <- rlang::warning_cnd("error", message = ".meta must be a character")
    rlang::cnd_signal(cnd)
  }
  out <- list(
    data = .data,
    ids = .ids,
    prep = .prep,
    meta = .meta
  )
  class(out) <- "analysis_plan"
  out
}

#' @export
is.analysis_plan <- function(x) inherits(x, "analysis_plan")

#' @export
print.analysis_plan <- function(x, ...) cat(format(x, ...), "\n")

#' @export
format.analysis_plan <- function(x, ...) {
  header <- '<Analysis plan>'
  title <- paste('<', x$meta, '>', sep = '', collapse = '/')
  n_datasets <- length(x$data)
  values <- paste('<', n_datasets, ' analysis datasets>', sep = '')
  out <- paste(header, title, values, sep = '\n')
  cat(out)
}
