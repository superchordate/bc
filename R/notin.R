#' Not In.
#'
#' Easier alternative to ! x%in% y.
#'
#' @param x Vector
#' @param y Vector
#'
#' @return Logical vector indicating which positions in x are not in y.
#' @export
#'
#' @examples
#' #TODO
`%ni%` = function( x, y ) ! x %in% y
