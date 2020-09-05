#' A description of analysis_plan
#'
#' A details of analysis_plan
#'
#' @title analysis_plan: This function
#' @param .data analysis data
#' @param ... other arguments
#' @export
analysis_plan <- function(.data, ...) {
  UseMethod("analysis_plan")
}

#' Build analysis plan from data
#'
#' @export
analysis_plan.data.frame <- function(.data, .name = 'original') {
  new_analysis_plan(list(x), list(.name), function(x) x)
}

#' Analysis plan constructor and validator
#'
#' @export
new_analysis_plan <- function(.data, .meta, .prep) {
  if (!inherits(.data, 'list')) {
    cnd <- rlang::warning_cnd("error", message = ".data must be a list of data.frames")
    rlang::cnd_signal(cnd)
  }
  if (!inherits(.meta, 'list')) {
    cnd <- rlang::warning_cnd("error", message = ".meta must be a list")
    rlang::cnd_signal(cnd)
  }
  if (!is.function(.prep)) {
    cnd <- rlang::warning_cnd("error", message = ".prep must be a function")
    rlang::cnd_signal(cnd)
  }
  out <- list(
    data = .data,
    meta = .meta,
    prep = .prep
  )
  class(out) <- "analysis_plan"
  out
}
