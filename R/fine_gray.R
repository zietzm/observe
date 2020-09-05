#' @export
fit_fine_gray <- function(x, ...) {
  UseMethod("fit_fine_gray")
}

#' @export
fit_fine_gray.analysis_plan <- function(x, .formula, primary_outcome = NULL,
                                        censoring_outcome = NULL) {
  # Accept either a single formula or a named list of formulae.
  .formulae <- process_formula(.formula)

  # Fit the specified models
  models_to_fit <- purrr::cross2(.formulae, x$analysis_data)
  fit_objects <- purrr::map(models_to_fit, ~fit_fine_gray.formula(
    .x[[1]], x$preprocessing(.x[[2]]), primary_outcome, censoring_outcome))

  # Format corresponding names
  names_first <- paste(x$analysis_data_meta, '->Fine-Gray<', sep = '')
  names_last <- paste(names(.formulae), '>', sep = '')
  model_names <- purrr::cross2(names_first, names_last)
  model_names <- purrr::pmap(model_names, ~paste(.x, collapse = ''))
  model_names <- purrr::flatten(model_names)

  # Build resulting analysis_result object
  analysis_result(fit_objects, model_names)
}


#' @export
fit_fine_gray.formula <- function(.formula, .data, primary_outcome = NULL,
                                  censoring_outcome = NULL) {
  # Evaluate the survival object in the data environment
  surv_meta <- eval(.formula[[2]], envir = .data)
  time_to_event <- surv_meta[, 1]
  # Evaluate the outcome separately (to get it as a nice factor vector)
  outcome <- eval(.formula[[2]][[3]], envir = .data)

  # Pull the covariates from the original data using all terms on the RHS.
  # Remove the first column ('(Intercept)') because this is inappropriate
  # for Fine-Gray regression.
  covariates <- model.matrix(.formula, .data)[, -1]

  # Codes are used for the outcome of interest and censoring (2 and 1, resp.)
  if (is.null(primary_outcome)) {
    primary_outcome <- attributes(surv_meta)$inputAttributes$event$levels[[2]]
  }
  if (is.null(censoring_outcome)) {
    censoring_outcome <- attributes(surv_meta)$inputAttributes$event$levels[[1]]
  }

  # Run Fine-Gray model using `cmprsk` package
  output <- cmprsk::crr(ftime = time_to_event, fstatus = outcome, cov1 = covariates,
                        failcode = primary_outcome, cencode = censoring_outcome)

  # Add the fit formula for use with predict method
  output[['formula']] <- .formula
  output
}

#' @export
fit_fine_gray.character <- function(.formula, .data, primary_outcome,
                                    censoring_outcome) {
  .formula <- as.formula(.formula)
  fit_fine_gray.formula(.formula, .data, primary_outcome, censoring_outcome)
}
