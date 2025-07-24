#' Calculate the dynamics of a Lotka-Volterra system
#'
#' @param r vector of growth rates
#' @param A interaction matrix
#' @param D dispersal matrix
#' @param y0 initial abundances
#' @param maxtime max timestep
#' @param steptime timestep length
#'
#' @returns tibble with abundances at each timestep
#' @export
#'
#' @examples
GLV_dynamics <- function(r, A, D, y0, maxtime = 100, steptime = 0.5){
    times <- seq(0, maxtime, by = steptime)
    parameters <- list(r = r, A = A, D = D)
    # solve numerically
    out <- deSolve::ode(y = y0, times = times, 
               func = GLV_dispersal, parms = parameters, 
               method = "ode45")
    # make into tidy form
    out2 <- tibble::as_tibble(out) %>% tidyr::pivot_longer(-time, names_to = "species", values_to = "density")
    return(out2)
}