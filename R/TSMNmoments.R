#' @export
#'
#' @title Moments of Truncated Scale Mixtures of Normal Distributions
#'
#' @description \code{TSMNmoments} return the first four moments of TSMN distributions, as
#' example Normal, Student-t, Pearson VII, Slash or Contaminated Normal.
#'
#' @param mu Location parameter.
#' @param sigma2 Scale parameter.
#' @param nu Parameter of the scale variable of the SMN family. Must be NULL in
#'   case of Normal distribution. Must be a bidimensional vector and each
#'   component of the bidimensional vector "nu" must lie on (0,1) in case of
#'   contaminated normal distribution (CNormal).
#' @param delta Second parameter of Pearson VII. Must not be provided in case of
#'   Normal, Student-t or Slash distribution.
#' @param lower Lower bounds.
#' @param upper Upper bounds.
#' @param dist Distribution to be used: "Normal" for Normal model, "T" for
#'   Student-t model, "PearsonVII" for Pearson VII model, "Slash" for slash
#'   model and "CNormal" for contaminated Normal model.
#'
#' @return \describe{ \item{EY1}{The first moment} \item{EY2}{The second moment}
#'   \item{EY3}{The third moment} \item{EY4}{The fourth moment} }
#'
#' @seealso \code{\link{TSMNgenerator}}
#'
#' @examples
#'  ## A test sample to compare theoretical and empirical moments, considering the following parameters:
#'
#'  mu = 2
#'  sigma2 = 4
#'  nu = 5
#'  lower = -3
#'  upper = 10
#'  dist = "T"
#'  n = 10000
#'
#'  ## Theoretical moments with TSMNmoments
#'  theor<-TSMNmoments(mu=mu, sigma2=sigma2, nu=nu, lower=lower, upper=upper, dist=dist)
#'
#'  ## Generate the sample with TSMNgenerator to compute the empirical moments
#'  empir<-TSMNgenerator(n=n, mu=mu, sigma2=sigma2, nu=nu, lower=lower, upper=upper, dist=dist)
#'
#'  ## Compare the results
#'  data.frame("1st" = c("Theoretic" = theor$EY1, "Empirical" = mean(empir)),
#'             "2nd" = c("Theoretic" = theor$EY2, "Empirical" = mean(empir^2)),
#'             "3rd" = c("Theoretic" = theor$EY3, "Empirical" = mean(empir^3)),
#'             "4th" = c("Theoretic" = theor$EY4, "Empirical" = mean(empir^4)))


TSMNmoments <- function(mu, sigma2, nu = NULL, delta = NULL, lower = -Inf, upper = Inf, dist = "Normal") {
  if (length(mu) > 1)
    stop("mu parameter must be a scalar.")

  if (length(sigma2) > 1)
    stop("sigma2 parameter must be a scalar.")
  if (sigma2 <= 0)
    stop("Scale parameter must be positive.")

  if (lower > upper)
    stop("lower must be lower than upper.")
  if (length(lower) > 1 || length(upper) > 1)
    stop("Range values must be scalar.")

  if ((dist != "T") && (dist != "Normal") && (dist != "PearsonVII") && (dist != "Slash") && (dist !=
                                                                                             "CNormal"))
    stop("Distribution family not supported. Check documentation!")

  if (dist == "PearsonVII") {
    if (length(delta) > 1)
      stop("delta parameter must be a scalar")
    if (length(delta) == 0)
      stop("delta parameter must be provided in case of Pearson VII distribution.")
    if (delta <= 0)
      stop("delta parameter must be positive.")
  }

  if (dist == "CNormal") {
    if (length(nu) != 2)
      stop("nu must be a bidimensional vector in case of Contaminated Normal distribution.")
    if (nu[1] <= 0 || nu[1] >= 1)
      stop("nu[1] must lies in (0,1).")
    if (nu[2] <= 0 || nu[2] >= 1)
      stop("nu[2] must lies in (0,1).")
  }

  if (lower <= -1e+05) {
    lower = -1e+05
  }
  if (upper >= 1e+05) {
    upper = 1e+05
  }

  if (dist != "CNormal" && dist != "Normal"){
  if (length(nu) > 1)
    stop("nu parameter must be a scalar.")
  if (length(nu) == 0)
    stop("initial value for nu parameter must be provided in case of Pearson VII, T and Slash.")
  if (nu <= 1)
    stop("nu must be greater than 1 for PearsonVII, T and Slash")
  if(nu >= 30){
    nu = 30
    }
  }

  if (mu > upper || mu < lower){
    stop("mu must be between upper and lower limits")
  }

  pre_moments(mu = mu, sigma2 = sigma2, nu = nu, delta = delta, lower = lower, upper = upper, dist = dist)

}
