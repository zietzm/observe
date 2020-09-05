#' Constructor for analysis result
#'
#' @export
analysis_result <- function(fits, fits_meta) {
  # TODO: Check type of inputs
  out <- list(
    fits = fits,
    fits_meta = fits_meta
  )
  class(out) <- "analysis_result"
  out
}

# TODO: Figure out if there's anything I need to do to define objects...

# Create a construction method that checks the types of the input, and returns a
# list with the correct class label. XXX <- function(...) {}

# Write a function to check if an object is of your class: is.XXX <- function(x) inherits(x, "XXX")

# When implementing a vector class, you should implement these methods: length,
# [, [<-, [[, [[<-, c. (If [ is implemented rev, head, and tail should all work).

# When implementing anything mathematical, implement Ops, Math and Summary.
# When implementing a matrix/array class, you should implement these methods:
# dim (gets you nrow and ncol), t, dimnames (gets you rownames and colnames),
# dimnames<- (gets you colnames<-, rownames<-), cbind, rbind.

# If you’re implementing more complicated print() methods, it’s a better idea
# to implement format() methods that return a string, and then implement
# print.class <- function(x, ...) cat(format(x, ...), "\n".
# This makes for methods that are much easier to compose, because the side-effects are isolated to a single place.

