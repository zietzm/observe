#' @export
prep_bootstrap <- function(x, n, include_original = TRUE) {
  UseMethod("prep_bootstrap")
}

#' @export
prep_bootstrap.analysis_plan <- function(x, n, inclue_original = TRUE) {
  new_data <- purrr::map(
    x$analysis_data, ~rsample::bootstraps(.x, times = n, apparent = inclue_original))

  # Each original id gets n new ids, one for each bootstrap iteration
  new_ids <- purrr::map(new_data, ~.x$id)
  new_meta <- purrr::map2(x$analysis_data_meta, new_ids, ~paste(.x, .y, sep = '->'))

  # Combine all the bootstrapped data
  new_data <- dplyr::bind_rows(new_data)

  # Assign attributes of returned object
  x$analysis_data <- as.list(new_data$splits)
  x$analysis_data_meta <- purrr::flatten(new_meta)

  # Add an additional preprocessing step to evaluate the rsplit objects
  preprocess <- match.fun(x$preprocessing)
  x$preprocessing <- function(data) rsample::analysis(preprocess(data))
  x
}


#' @export
prep_match <- function(x, ...) {
  UseMethod("prep_match")
}

# A big limitation of this implementation is that it pulls everything into memory
# all at once. This means we lose all the benefits of rsample's optimizations,
# as well as the ability to limit peak memory use. Ideally, this should add a
# preprocessing step instead, which can be called LAZILY.
#' @export
prep_match.analysis_plan <- function(x, ...) {
  dots <- process_dots(...)

  # Update analysis
  new_data <- purrr::cross2(dots, x$analysis_data)
  x$analysis_data <- purrr::map(new_data, ~.x[[1]](x$preprocessing(.x[[2]])))

  # Reset preprocessing
  x$preprocessing <- function(data) data

  # Update meta information
  new_meta <- purrr::cross2(names(dots), x$analysis_data_meta)
  x$analysis_data_meta <- purrr::pmap(new_meta, ~paste(.x, sep = '->'))
  x
}
