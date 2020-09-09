#' summ_metrics(RR = ~. / ref, ARR = ~. - ref, NNT = ~1 / ARR, AR = ~., .ref = list(drug_exposed = 0))

#' @export
summ_metrics <- function(.eval, ...) {
  UseMethod("summ_metrics")
}

#' @export
summ_metrics.analysis_eval <- function(.eval, ..., .ref = NULL) {
  cnd <- rlang::warning_cnd("error", message = "Not implemented yet")
  rlang::cnd_signal(cnd)
  # dots <- rlang::list2(...)
  # dots <- purrr::map2(dots, purrr::as_mapper)

  # dots <- purrr::map(dots, rlang::as_function)
  # for (i in seq_along(dots)) {
  #   f <- rlang::as_function(rlang::eval_tidy(dots[[i]]))
  #   # dots[[i]] <- rlang::eval_tidy(f, data = list(ref = 1))
  # }
  # f
  # dots

  metric_formulae <- rlang::enexprs(...)
  metric_formulae <- purrr::map(metric_formulae, ~rlang::expr(function(x, ref) !!.x))
  # metric_formulae <- purrr::map(metric_formulae, purrr::as_mapper)
  # metric_formulae <- purrr::map_if(metric_formulae, rlang::is_quosure, rlang::quo_squash)


  # metric_functions <- list()
  # # purrr::map(metric_formulae, ~rlang::eval_tidy(.x, data = list(ref = 1)))
  # for (i in seq_along(metric_formulae)) {
  #   f <- rlang::eval_tidy(metric_formulae[[i]], data = list(ref = 1))
  #   metric_functions[[i]] <- f
  # }
  # metric_functions
  metric_formulae

  # data <- cbind(id = .eval$ids, fit = as.list(.eval$fit_ids), pred = .eval$pred)
  # data <- as.data.frame(data)
  # data <- dplyr::mutate(data, strat = stringr::str_extract(id, '(?<=\\:).+$'))
  # # # data <- tidyr::pivot_wider(data, id_cols = fit, names_from = strat, values_from = pred)
  # data
}

