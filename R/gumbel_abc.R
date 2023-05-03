#' Gumbel ABC
#'
#' @description   Approximate Bayesian computation using Gumbel distribution (light-tailed) as the data generating
#'                mechanism. Finds point estimates for the posterior distribution parameters, location and scale.
#'
#' @param true.loc      The true value of the location parameter used to generate "observed" data
#' @param true.sc       The true value of the scale parameter used to generate "observed" data (must be greater than 0)
#' @param prior.loc     The prior distribution of the location parameter used to generate the simulated data
#' @param prior.sc      The prior distribution of the scale parameter used to generate the simulated data
#' @param size          The size of the "observed" and simulated data generated
#' @param nsim          The number of simulations to run/number of point estimates of location and scale found
#' @param niter         The number of iterations in ABC
#' @param epsilon       Used to accept the posterior location and scale parameter values
#'
#' @return  The point estimates for the posterior distribution parameters, location and scale
#'
#' @examples gumbel_abc(true.loc = 1, true.sc = 2, prior.loc = c(-5,5), prior.sc = c(0.5,3.5), size = 10^3, nsim = 100, niter = 10^4, epsilon = 0.01)


gumbel_abc = function(true.loc = 0, true.sc = 1.5, prior.loc = c(-5,5), prior.sc = c(0.5,3.5), size = 10^3, nsim = 10^3, niter = 10^4, epsilon = 0.01, plot = T) {

  posterior.loc.estimates = vector(length = nsim) # empty vector to store location point estimates

  posterior.sc.estimates = vector(length = nsim)  # empty vector to store scale point estimates

  library(ordinal)

  for (i in 1:nsim) {

    x.obs = rgumbel(size, location = true.loc, scale = true.sc) # generating "observed" data

    sum.stat.obs = median(x.obs)  # the summary statistic of the "observed" data

    posterior.loc = c() # empty vector to store location values from the iterations below

    posterior.sc = c()  # empty vector to store scale values from the iterations below

    for (j in 1:niter) {  # the larger the number of iterations, the higher the accuracy
      loc.prop = runif(1, prior.loc[1], prior.loc[2]) # generating location from prior distribution
      sc.prop = runif(1, prior.sc[1], prior.sc[2])  # generating scale from prior distribution
      x.sim = rgumbel(length(x.obs), location = loc.prop, scale = sc.prop) # generating our simulated data
      sum.stat.sim = median(x.sim) # calculating the summary statistic of the simulated data
      if(abs(sum.stat.obs - sum.stat.sim) < epsilon){ # storing the location and scale value if the difference
        posterior.loc = c(posterior.loc, loc.prop)  # in summary statistics is less than epsilon
        posterior.sc = c(posterior.sc, sc.prop)
      }

    }

    posterior.loc.estimates[i] = mean(posterior.loc)  # storing point estimates of location parameter
    posterior.sc.estimates[i] = mean(posterior.sc)  # storing point estimates of scale parameter

  }

  posterior.estimates = list("location" = posterior.loc.estimates, "scale" = posterior.sc.estimates)

  return(posterior.estimates) # returns the point estimates of the location and scale parameters

}
