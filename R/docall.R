
#' Score the TV watching model
#'
#' Simple model with two predictor \code{age} (value between 20 and 80) and \code{marital}
#'
#' @export
docall <- function(what, args, quote = FALSE){
 base::do.call(what, args, quote = FALSE, envir = parent.frame())
}