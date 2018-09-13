#' euclidean function
#'
#' This function implements the Euclidian algorithm to find the greatest common
#' divisor of two numbers. The description of the algorithm with pseudocode can
#' be found here: \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @param a An integer.
#' @param b An integer.
#'
#' @return This function return a number that shows the greatest common divisor
#'   of \code{a} and \code{b}.
#'
#' @examples
#' euclidean(123612, 13892347912) #4
#' euclidean(100, 1000) #100
#'
#'@export


euclidean <- function(a,b){
  if((is.integer(a))&(is.integer(b))){stopifnot('wrong input!')}
  t <- 0
  if(a > b){
    while(b != 0){
      t <- b
      b <- a%%b
      a <- t
    }
  }else{
    while(a != 0){
      t <- a
      a <- b%%a
      b <- t
    }
  }
  return(abs(t))
}
