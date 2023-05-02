# abc.mse
R Package on Approximate Bayesian Computation

# Installation
First install the R package devtools available on CRAN, if it is not already installed.  This package provides us with the function ```install_github()``` that enables installing packages directly from github to R with the following command.

Type the following command in R:
```
R> install.packages("devtools")
R> library(devtools)
# install R package that abc.mse depends on before running the next line 
# see details below
R> install_github("Zmhiga/abc.mse")
```

The abc.mse package depends on the ordinal package for the data generating function of the Gumbel distribution.  As you may not have installed this package on your computer, you may use the following command line for installation before running function ```install_github()```:
```
R> install.packages("ordinal")
```

# Using abc.mse
After installation, load the statsdist package into R:
```
R> library(abc.mse)
```
To bring up the documentation of the package:
```
R> library(help = abc.mse)
```
