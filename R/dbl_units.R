
#' Simple Double w/ Units Class (dbl + units attribute)
#'
#' @param x a double
#' @param units character representing the units (may be mixed)
#' @import vctrs
#' @return record-style s3 `sunits` object
new_sunits_dbl <- function(value = double(), units = character()) {
  vec_assert(value, double())
  vec_assert(units, character())

  new_vctr(
    value,
    "units" = units,
    class = "sunits_dbl"
  )
}

format.sunits_dbl <- function(x, ...) {
  glue::glue_data(
    .x = list(
      .value = format(field(x, "value")),
      .units = stringr::str_c(" ", field(x, "units"))),
    "{.value}{.units}", .na = "")
}

validate_sunits_dbl <- function(x) {
  if(FALSE) stop("Undefined error.")
}

sunits_dbl <- function(x, units = NA_character_) {
  new_sunits_dbl(
    value = as.double(x),
    units = vec_recycle(units, vec_size(x))
  )
}
