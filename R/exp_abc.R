#' Exponential ABC
#'
#' @description   Approximate Bayesian computation using Exponential distribution as the data generating
#'                mechanism. Finds point estimates for the posterior distribution parameter, lambda.
#'
#' @param true.lambda   The true value of the parameter of lambda used to generate "observed" data (must be greater than 0)
#' @param prior.lambda  The prior distribution of lambda used to generate the simulated data
#' @param size          The size of the "observed" and simulated data generated
#' @param nsim          The number of simulations to run/number of point estimates of lambda found
#' @param niter         The number of iterations in ABC
#' @param epsilon       Used to accept the posterior lambda values
#'
#' @return  The point estimates for the posterior distribution parameter, lambda
#'
#' @examples exp_abc(true.lambda = 5, prior.lambda = c(1,10), size = 10^3, nsim = 100, niter = 10^3, epsilon = 0.01)


exp_abc = function(true.lambda = 10, prior.lambda = c(1,15), size = 10^3, nsim = 10^3, niter = 10^3, epsilon = 0.01) {

  posterior.lambda.estimates = vector(length = nsim)  # empty vector to store lambda point estimates

  for (i in 1:nsim) {

    x.obs = rexp(size, true.lambda) # generating "observed" data

    sum.stat.obs = mean(x.obs)  # the summary statistic of the "observed" data

    posterior.lambda = c()  # empty vector to store lambda values from the iterations below

    for (j in 1:niter) {  # the larger the number of iterations, the higher the accuracy
      lambda.prop = runif(1, prior.lambda[1], prior.lambda[2])  # generating lambda from prior distribution
      x.sim = rexp(length(x.obs), lambda.prop)  # generating our simulated data
      sum.stat.sim = mean(x.sim)  # calculating the summary statistic of the simulated data
      if(abs(sum.stat.obs - sum.stat.sim) < epsilon) {  # storing the lambda value if the difference
        posterior.lambda = c(posterior.lambda, lambda.prop) # in summary statistics is less than epsilon
      }
    }

    posterior.lambda.estimates[i] = mean(posterior.lambda) # storing point estimates of lambda

  }

  return(posterior.lambda.estimates)  # returns the point estimates of lambda

}
