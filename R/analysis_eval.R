#' @export
analysis_eval <- function(pred, .ids, fit_ids, .meta) {
  out <- list(
    pred = as.double(pred),
    ids = as.character(.ids),
    fit_ids = as.character(fit_ids),
    meta = .meta
  )
  class(out) <- 'analysis_eval'
  out
}

#' @export
as.data.frame.analysis_eval <- function(.eval) {
  parsed_ids <- purrr::map(.eval$ids, parse_id)
  print(length(parsed_ids))
  print(length())
  out <- data.frame(id = parsed_ids, fit_id = .eval$fit_ids, pred = .eval$pred)
  attr(out, 'meta') <- .eval$meta
  out
}

#' @export
parse_id <- function(.id, self_keyed_vars = NULL) {
  steps <- stringr::str_split(.id, '→')
  steps <- purrr::flatten_chr(steps)
  steps <- purrr::keep(steps, ~stringr::str_detect(.x, '<.+>'))

  step_names <- purrr::map_chr(steps, ~stringr::str_replace(.x, '(<.+>)', ''))
  step_keys <- purrr::map_chr(steps, ~stringr::str_extract(.x, '(?<=<).+(?==)'))
  step_vals <- purrr::map_chr(steps, ~stringr::str_extract(.x, '(?<==).+(?=>)'))

  final_keys <- list()
  final_vals <- list()
  j <- 1
  for (i in seq_along(step_names)) {
    if (step_names[[i]] %in% self_keyed_vars) {
      final_keys[[j]] <- paste(step_names[[i]], step_keys[[i]], sep = '_')
      final_vals[[j]] <- step_vals[[i]]
    } else {
      final_keys[[j]] <- paste(step_names[[i]], 'var', sep = '_')
      final_vals[[j]] <- step_keys[[i]]

      j <- j + 1
      final_keys[[j]] <- paste(step_names[[i]], 'val', sep = '_')
      final_vals[[j]] <- step_vals[[i]]
    }
    j <- j + 1
  }
  purrr::set_names(final_vals, final_keys)
}
