#' Lotka-Volterra linear model with dispersal
#'
#' @param t time point at which the steady-state is wanted
#' @param y initial abundances
#' @param parms not used, only for compatibility
#' @param r vector of intrinsic growth rates
#' @param A interaction matrix - with negative competition coefficients
#' @param D dispersal coefficients matrix
#'
#' @returns abundances of the system, as a list
#' @export
#'
#' @examples
# GLV_dispersal = function(t,y,parms=NULL,r,A,D) {
#     y[y < 10^-8] <- 0 # prevent numerical problems
#     dy = y*(r+A%*%y) + D%*%y
#     return(as.list(dy))
# }
GLV_dispersal = function(t,y,parameters) {
    with(as.list(c(y, parameters)), {
    y[y < 10^-8] <- 0 # prevent numerical problems
    dy = y*(r+A%*%y) + D%*%y
    list(dy)
    })
}


