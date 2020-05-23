
#' Simple Record-Style Units Class
#'
#' @param x a double
#' @param units character representing the units (may be mixed)
#' @import vctrs
#' @return record-style s3 `sunits_rcrd` object
new_sunits_rcrd <- function(value = double(), units = character()) {
  vec_assert(value, double())
  vec_assert(units, character())

  new_rcrd(
    list(
      "value" = value,
      "units" = units),
    class = "sunits_rcrd"
  )
}

format.sunits_rcrd <- function(x, ...) {
  glue::glue_data(
    .x = list(
      .value = format(field(x, "value")),
      .units = stringr::str_c(" ", field(x, "units"))),
    "{.value}{.units}", .na = "")
}

validate_sunits_rcrd <- function(x) {
  if(FALSE) stop("Undefined error.")
}

#' Make simple units
#' @export
sunits_rcrd <- function(x, units = NA_character_) {
  new_sunits_rcrd(
    value = vec_cast(x, double()),
    units = vec_recycle(units, vec_size(x))
  )
}

#' Coerce dataframes and lists to sunits
#' @param x dataframe or list to coerce
#' @export
as_sunits_rcrd <- function(x, value_from = "value", units_from = "units") {

  new_sunits_rcrd(
    value = vec_cast(x[[value_from]], double()),
    units = vec_cast(x[[units_from]], character())
  )
}
