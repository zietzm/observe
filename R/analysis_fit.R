#' Constructor for analysis fit
#'
#' @export
analysis_fit <- function(fits, fit_ids, .meta) {
  # TODO: Check type of inputs
  out <- list(
    fits = fits,
    fit_ids = fit_ids,
    meta = .meta
  )
  class(out) <- "analysis_fit"
  out
}

#' @export
is.analysis_fit <- function(x) inherits(x, "analysis_fit")

#' @export
print.analysis_fit <- function(x, ...) cat(format(x, ...), "\n")

#' @export
format.analysis_fit <- function(x, ...) {
  header <- '<Analysis fits>'
  title <- paste('<', x$meta, '>', sep = '', collapse = '/')
  n_fits <- length(x$fits)
  values <- paste('<', n_fits, ' fit models>', sep = '')
  out <- paste(header, title, values, sep = '\n')
  cat(out)
}

# TODO: Should have a separate constructor/validator, and have constructors
# for specific inputs too.
