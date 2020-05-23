# Unary math and arithmetic:

#' @export
#' @import vctrs
vec_math.sunits_rcrd <- function(.fn, .x, ...) {

  res <- vec_math_base(.fn, field(.x, "value"))

  first_unit <- field(.x, "units")[[1]]
  if(vec_size(res) != vec_size(.x)) {stopifnot(
    all(field(.x, "units") == first_unit))}

  new_sunits_rcrd(
    res,
    vec_recycle(first_unit, vec_size(res))
  )
}

#' @export vec_arith.sunits_rcrd
#' @method vec_arith sunits_rcrd
#' @export
vec_arith.sunits_rcrd <- function(op, x, y, ...) {
  UseMethod("vec_arith.sunits_rcrd", y)
}

#' @method vec_arith.sunits_rcrd default
#' @export
vec_arith.sunits_rcrd.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.sunits_rcrd numeric
#' @export
vec_arith.sunits_rcrd.numeric <- function(op, x, y, ...) {
  new_sunits_rcrd(
    vec_arith_base(op, field(x, "value"), y),
    field(x, "units")
  )
}

#' @method vec_arith.numeric sunits_rcrd
#' @export
vec_arith.numeric.sunits_rcrd <- function(op, x, y, ...) {
  new_sunits_rcrd(
    vec_arith_base(op, x, field(y, "value")),
    field(y, "units")
  )
}

# vec_arith.sunits_rcrd.sunits_rcrd <- function(op, x, y, ...) {
#   switch(
#     op,
#     "+" = ,
#     "-" = new_meter(vec_arith_base(op, x, y)),
#     "*" =
#     "/" = vec_arith_base(op, x, y),
#     stop_incompatible_op(op, x, y)
#   )
# }
