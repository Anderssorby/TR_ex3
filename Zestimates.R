estimateZ <- function(x, n = 20) {
  arima <- arima(x,order=c(2,0,1))
  
  # n number of pi-coeff to be calculated
  l <- length(x)
  m <- l-n # number of Z values to be estimated
  
  phi1 <- arima$coef[1]
  phi2 <- arima$coef[2]
  theta1 <- arima$coef[3]
  sigma2 <- arima$sigma2
  
  # calculates n coefficients of pi
  pi <- vector(length=n) 
  pi[1] <- 1
  pi[2] <- -phi1 - theta1*pi[1]
  pi[3] <- -phi2 - theta1*pi[2]
  for (i in seq(4,n)){
    pi[i] <- -theta1*pi[i-1]
  }
  
  # estimates Z for t>=n
  Z <- rep(0,l) 
  for (t in seq(n,l)){
    Z[t] <- t(pi)%*%x[t:(t-n+1)]
  }
  res <- list(Z=Z, pi=pi, m = m, n = n, sigma2 = sigma2, sd = sqrt(sigma2), l = l)
  class(res) <- "zestimate"
  return(res)
}

plot.zestimate <- function(obj, ...) {
  # Normal Q-Q plot
  Z <- obj$Z
  sigma <- obj$sd
  
  qqnorm(Z/sigma)
  qqline(Z/sigma,col = 2) 
}

print.zestimate <- function(obj, ...) {
  print("Estimate of Z for ARMA(2,1)")
  print(obj$Z)
}



