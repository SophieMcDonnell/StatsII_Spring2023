#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

KSTest <- function(data) {
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  n = m = length(test_data)
  p <- 2*exp(- D^2 * (2*n)/(1+n/m))
  # https://specbranch.com/posts/kolmogorov-smirnov/
  
  #plot(empiricalCDF)
  paste('D =', D, ', p-value =', p) 
}

set.seed(123)
# create empirical distribution of observed data
test_data <- rcauchy(1000, location = 0, scale = 1)

KSTest(test_data)



#####################
# Problem 2
#####################

set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

linear.lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi) -.5*n*log(sigma2)- ((t(e) %*% e)/ (2*sigma2)) 
  return(-logl)
}

linear.MLE <- optim(fn=linear.lik, par=c(1,1,1), hessian=TRUE, 
                    y= data$y, X=cbind(1, data$x), method = "BFGS")
linear.MLE$par

summary(lm(y~x, data))







