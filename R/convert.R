
#' Convert unitful quantities
#'
#' @param x unitful sunits
#' @param new_x replacement value
#' @param from length 1 character of unit to convert from
#' @param to length 1 character of unit to convert to
#' @export
convert <- function(x, new_x, from, to) {
  UseMethod("convert")
}

#' @export
convert.default <- function(x, new_x, from, to) {
  stop("Don't know how to convert that.")
}

#' @export
convert.sunits_rcrd <- function(x, new_x, from, to) {

  locs <- vctrs::vec_as_location(vctrs::field(x, "units") == from, vctrs::vec_size(x))

  res <- x

  vec_slice(field(res, "value"), locs) <- vec_slice(field(new_x, "value"), locs)
  vec_slice(field(res, "units"), locs) <- to

  res
}
