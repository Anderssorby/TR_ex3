x <- read.table("DataEx3G8.txt")
arima <- arima(x,order=c(2,0,1))

n <- 20 # number of pi-coeff to be estimated
m <- nrow(x)-n # number of Z to be estimated

phi1 <- arima$coef[1]
phi2 <- arima$coef[2]
theta1 <- arima$coef[3]
#
#M <- matrix(data=0, ncol=10,nrow=10)
#M[1,1] <- 1
#M[2,1] <- -phi1-theta1
#M[3,1] <- 
#
#for (i in seq(2,10))
#


# calculates n coefficients of pi
pi <- vector(length=n) 
pi[1] <- 1
pi[2] <- -phi1 - theta1*pi[1]
pi[3] <- -phi2 - theta1*pi[2]
for (i in seq(4,n)){
  pi[i] <- -theta1*pi[i-1]
}

# estimates Z for t>=n
Z <- vector(length=m) 
for (i in seq(1,m)){
  Z[i] <- t(pi)%*%x[(n+i-1):i,1]
}

# Normal Q-Q plot
qqnorm(Z/sd(Z)); qqline(Z/sd(Z),col = 2) 
