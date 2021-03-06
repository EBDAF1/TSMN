% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TSMN.R
\docType{package}
\name{TSMN}
\alias{TSMN}
\alias{TSMN-package}
\title{TSMN: Truncated Scale Mixtures of Normal Distributions}
\description{
This package includes two functions related to the truncated scale mixtures
of normal distribution. One of then is to generate random samples from TSMN
distribution, and other is to compute the first four theoretical moments.
}
\examples{
 ## A test sample to compare theoretical and empirical moments, considering the following parameters:

 mu = 2
 sigma2 = 4
 nu = 5
 lower = -3
 upper = 10
 dist = "T"
 n = 10000

 ## Theoretical moments with TSMNmoments
 theor<-TSMNmoments(mu=mu, sigma2=sigma2, nu=nu, lower=lower, upper=upper, dist=dist)

 ## Generate the sample with TSMNgenerator to compute the empirical moments
 empir<-TSMNgenerator(n=n, mu=mu, sigma2=sigma2, nu=nu, lower=lower, upper=upper, dist=dist)

 ## Compare the results
 data.frame("1st" = c("Theoretic" = theor$EY1, "Empirical" = mean(empir)),
            "2nd" = c("Theoretic" = theor$EY2, "Empirical" = mean(empir^2)),
            "3rd" = c("Theoretic" = theor$EY3, "Empirical" = mean(empir^3)),
            "4th" = c("Theoretic" = theor$EY4, "Empirical" = mean(empir^4)))

}
\references{
Aldo M. Garay, Victor H. Lachos, Heleno Bolfarine, Celso R. Cabral. "Linear censored regression models with scale mixtures of normal distributions". Statistical Papers, 2017, vol. 58, issue 1, pages 247-278
}
\seealso{
\code{\link{TSMNmoments}},\code{\link{TSMNgenerator}}
}
\author{
Eraldo B. dos Anjos Filho \email{ebdaf1@de.ufpe.br} and Aldo M. Garay \email{agaray@de.ufpe.br}

Maintainer: Eraldo B. dos Anjos Filho \email{ebdaf1@de.ufpe.br}
}
