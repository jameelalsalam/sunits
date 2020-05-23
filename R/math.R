# Unary math and arithmetic:

#' @export
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

vec_arith.sunits_rcrd <- function(op, x, y, ...) {
  UseMethod("vec_arith.sunits_rcrd", y)
}

vec_arith.sunits_rcrd <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
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
