
library(tidyverse)

source("R/GLV_dispersal.R")
source("R/GLV_dynamics.R")
source("R/GLV_jacobian.R")
source("R/GLV_steady_state.R")
# -------------------------------------------------------------------------

num.sp <- 5

# initial abundances
y0 <- rep(1,num.sp)

# growth rates
# r <- runif(num.sp)
r <- rep(1,num.sp)

# build a LD-stable matrix
A <- matrix(0, num.sp, num.sp)
A[upper.tri(A)] <- rnorm(num.sp * (num.sp - 1) / 2)
# make symmetric
A <- A + t(A)
# now find the largest eigenvalue
l1A <- max(eigen(A, only.values = TRUE, symmetric = TRUE)$values)
if (l1A > 0){
    # set the diagonal to make it stable
    diag(A) <- diag(A) - l1A - 0.01
}

D <- matrix(0, num.sp, num.sp)

# -------------------------------------------------------------------------

example_dynamics <- GLV_dynamics(r = r,A = A,D = D,y0 = y0,maxtime = 1000)
example_steady_state <- GLV_steady_state(r = r,A = A,D = D,y0 = y0)
example_jacobian <- GLV_jacobian(r = r,A = A,D = D,y0 = y0)

# largest eigenvalue
le <- max(as.double(eigen(example_jacobian)$values))

# plot the dynamics
pl <- ggplot(data = example_dynamics) +
    aes(x = time, y = density, colour = species) +
    geom_line()
