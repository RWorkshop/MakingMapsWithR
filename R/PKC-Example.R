#
# This file illustrates how to call the various functions 
# for computing several quantities of interest in connection with 
# the test of hypotheses
# H: Q(p.0) >= delta.0 versus K: Q(p.0) < delta.0, or 
# equivalently, H: F(delta.0) <= p.0 versus K: F(delta.0) > p.0.
# The MPI data is used as an example.
#
# Requires the files: (a) exact_test.R, (b) approx_test.R
# and (c) bootstrap_ucb.R. 
#
# Date: June 24, 2005
#

############# BEGIN EXAMPLE ################

# MPI data

n <- 15
mu.mle <- 0.011
sigma.mle <- 0.044
alpha <- 0.05

########## EXACT TEST ###########
# exact stuff may take a few minutes

source("exact_test.R")


# exact critical value

critval.exact(n, p.0=0.95, alpha)

# output:
#> critval.exact(n, p.0=0.95, alpha)
#[1] 0.9960502
#>

# exact UCB for Q(p.0)

ucb.exact(n, p.0=0.95, mu.mle, sigma.mle, alpha)

# output:
#> ucb.exact(n, p.0=0.95, mu.mle, sigma.mle, alpha)
#[1] 0.1305183
#>


############# MODIFIED NUT APPROXIMATION ######

source("approx_test.R")

# approximate critical value

critval.approx(p.0=0.95, n, alpha)

# output:
#> critval.approx(p.0=0.95, n, alpha)
#[1] 0.996047
#> 

# approximate ucb for Q(p.0)

ucb.approx(n, p.0=0.95, mu.mle, sigma.mle, alpha)

# output:
#> ucb.approx(n, p.0=0.95, mu.mle, sigma.mle, alpha)
#[1] 0.1305069
#>

# approximate lcb for F(delta.0)

lcb.approx(n, delta.0=0.10, mu.mle, sigma.mle, llim=0.5, alpha)

# output:
#> lcb.approx(n, delta.0=0.10, mu.mle, sigma.mle, llim=0.5, alpha)
#[1] 0.8694322
#>

# approximate pvalue for (H, K)

pval.approx(n, delta.0=0.10, p.0=0.95, mu.mle, sigma.mle)

# output:
#> pval.approx(n, delta.0=0.10, p.0=0.95, mu.mle, sigma.mle)
#[1] 0.3458825
#> 


################## BOOTSTRAP-t UCB for Q(p.0) ###


source("bootstrap_ucb.R")
set.seed(59955)

ucb.boot(n, p.0=0.95, mu.mle, sigma.mle, nboot=1999, alpha)

# output:
#> ucb.boot(n, p.0=0.95, mu.mle, sigma.mle, nboot=1999, alpha)
#[1] 0.1271421
#>

############### END OF EXAMPLE ###############
