#' Constructor for analysis fit
#'
#' @export
analysis_fit <- function(fits, .data, .prep, fit_ids, .meta) {
  # TODO: Check type of inputs. len(data) == len(fits) == len(fit_ids), etc.
  if (length(fits) != length(.data)) {
    cnd <- rlang::warning_cnd("error", message = "fits must have the same length as .data")
    rlang::cnd_signal(cnd)
  }
  if (length(fits) != length(fit_ids)) {
    cnd <- rlang::warning_cnd("error", message = "fits must have the same length as fit_ids")
    rlang::cnd_signal(cnd)
  }

  out <- list(
    fits = fits,
    data = .data,
    prep = .prep,
    ids = fit_ids,
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
