#' Find the steady state of a Lotka-Volterra system
#'
#' @param r vector of intrinsic growth rates
#' @param A interaction matrix
#' @param D dispersal matrix
#' @param y0 initial abundances
#' @returns steady-state abundances of the system
#' @export
#'
#' @examples
GLV_steady_state <- function(r, A, D, y0){
    # solve numerically
    parameters <- list(r = r, A = A, D = D)
    out <- rootSolve::stode(y = y0,time=0,func=GLV_dispersal,parms=parameters, positive = TRUE)
    return(out)
}