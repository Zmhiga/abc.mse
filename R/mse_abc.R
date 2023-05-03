#' Approximate Bayesian Computation - Mean-Square Error Calculation
#'
#' @description   Calculates the mean-squared error for the posterior distribution parameter(s) and plots the posterior distribution of the parameter(s)
##
#' @param true.par1     The true value of parameter 1
#' @param true.par2     If applicable, the true value of parameter 2
#' @param post.est1     The point estimates of the posterior distribution parameter 1
#' @param post.est2     The point estimates of the posterior distribution parameter 2
#' @param plot          If true, will plot a histogram of the posterior distribution of the parameter(s)
#'
#' @return  The mean-squared error(s) for the posterior distribution parameter(s) and the estimate of the parameter(s)
#'
#' @examples  estimates = cauchy_abc(nsim = 100)
#'  mse_abc(true.par1 = 0, post.est1 = estimates$location, true.par2 = 1.5, post.est2 = estimates$scale, plot = T)


mse_abc = function(true.par1, true.par2 = NULL, post.est1, post.est2 = NULL, plot = T) {

  if (is.null(true.par2) == T & is.null(post.est2) == T) { # if our posterior distribution has only one parameter

    estimator = mean(post.est1) # estimate of the parameter of the posterior distribution
    variance = var(post.est1) # variance of the parameter of the posterior distribution
    MSE = (estimator - true.par1)^2 + variance  # MSE of the parameter of the posterior distribution

    output = list("Estimator" = estimator, "MSE" = MSE) # the estimate and MSE returned from function
    
    if (plot == T) {
      hist(post.est1) # plot histogram of point estimates of parameter 1
    }
    
    return(output)

  } else if (is.null(true.par2) == F & is.null(post.est2) == F) { # if our posterior distribution has two parameters

    estimator1 = mean(post.est1)  # estimate of parameter 1 of the posterior distribution
    variance1 = var(post.est1)  # variance of parameter 1 of the posterior distribution
    MSE1 = (estimator1 - true.par1)^2 + variance1  # MSE of parameter 1 of the posterior distribution

    estimator2 = mean(post.est2)  # estimate of parameter 2 of the posterior distribution
    variance2 = var(post.est2)  # variance of parameter 2 of the posterior distribution
    MSE2 = (estimator2 - true.par2)^2 + variance2  # MSE of parameter 2 of the posterior distribution

    output = list("Estimator_1" = estimator1,"MSE_1" = MSE1, "Estimator_2" = estimator2, "MSE_2" = MSE2)
    # the estimates and MSEs returned from function

    if (plot == T) {
      hist(post.est1)
      hist(post.est2) # plot histogram of point estimates of parameter 1 and 2 
    }
    
    return(output)

  } else { # in the case where the user inputs something else in the arguments

    print("Please input valid arguments")

  }

}
