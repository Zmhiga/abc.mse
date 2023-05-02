#' Approximate Bayesian Computation - Mean-Square Error Calculation
#'
#' @description   Calculates the mean-squared error for the posterior distribution parameter(s) and plots the pdf of the posterior distribution
#'                along with the histogram of the parameter(s) estimates
##
#' @param true.par1     The true value of parameter 1 (location parameter for Gumbel and Cauchy)
#' @param true.par2     If applicable, the true value of parameter 2 (scale parameter for Gumbel and Cauchy)
#' @param post.est1     The point estimates of the posterior distribution parameter 1 (location parameter for Gumbel and Cauchy)
#' @param post.est2     The point estimates of the posterior distribution parameter 2 (scale parameter for Gumbel and Cauchy)
#' @param distribution  Select whether the distribution of interest is Exponential ("exp"), Gumbel ("gumbel"), or Cauchy ("cauchy)
#' @param plot        If true, will plot a histogram of the posterior point estimates of the parameter(s)
#'
#' @return  The mean-squared error(s) for the posterior distribution parameter(s)
#'
#' @examples  estimates = cauchy_abc(nsim = 100)
#'  mse_abc(true.par1 = 0, post.est1 = estimates$location, true.par2 = 1.5, post.est2 = estimates$scale, distribution = "cauchy", plot_bounds = (-5,5), plot_length = 100)


mse_abc = function(true.par1, true.par2 = NULL, post.est1, post.est2 = NULL, distribution = c("exp", "cauchy", "gumbel"), plot_bounds = c(0,10), plot_length = 100) {

  if (is.null(true.par2) == T & is.null(post.est2) == T & distribution == "exp") { # if our posterior distribution has only one parameter

    estimator = mean(post.est1) # estimate of the parameter of the posterior distribution
    variance = var(post.est1) # variance of the parameter of the posterior distribution
    MSE = (estimator - true.par1)^2 + variance  # MSE of the parameter of the posterior distribution

    output = list("Estimator" = estimator, "MSE" = MSE) # the estimate and MSE returned from function

    hist(post.est1) # plot histogram of point estimates of parameter 1

    x = seq(plot_bounds[1], plot_bounds[2], length = plot_length)
    prob = dexp(x, rate = mean(estimator))
    plot(x, prob, type = "l", main = "Plot of Posterior Distribution of pdf")

    return(output)

  } else if (is.null(true.par2) == F & is.null(post.est2) == F & (distribution == "cauchy" | distribution == "gumbel")) { # if our posterior distribution has two parameters

    estimator1 = mean(post.est1)  # estimate of parameter 1 of the posterior distribution
    variance1 = var(post.est1)  # variance of parameter 1 of the posterior distribution
    MSE1 = (estimator1 - true.par1)^2 + variance1  # MSE of parameter 1 of the posterior distribution

    estimator2 = mean(post.est2)  # estimate of parameter 2 of the posterior distribution
    variance2 = var(post.est2)  # variance of parameter 2 of the posterior distribution
    MSE2 = (estimator2 - true.par2)^2 + variance2  # MSE of parameter 2 of the posterior distribution

    output = list("Estimator_1" = estimator1,"MSE_1" = MSE1, "Estimator_2" = estimator2, "MSE_2" = MSE2)
    # the estimates and MSEs returned from function

    hist(post.est1)
    hist(post.est2) # plot histogram of point estimates of parameter 1 and 2

    if (distribution == "cauchy") {

      x = seq(plot_bounds[1], plot_bounds[2], length = plot_length)
      prob = dcauchy(x, location = estimator1, scale = estimator2)
      plot(x, prob, type = "l", main = "Plot of Posterior Distribution pdf")

    }

    if (distribution == "gumbel") {

      x = seq(plot_bounds[1], plot_bounds[2], length = plot_length)
      prob = dgumbel(x, location = estimator1, scale = estimator2)
      plot(x, prob, type = "l", main = "Plot of Posterior Distribution pdf")

    }

    return(output)

  } else { # in the case where the user inputs something else in the arguments

    print("Please input valid arguments")

  }

}
