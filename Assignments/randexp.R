simul <- function(lbda, n){
  X1 <- rexp(n, lbda)
  X2 <- rexp(n, lbda)
  Y <- X1 + X2
  return(Y)
}

bootstrap <- function(f, lbda, n, N){
  means <- rep(0, n)
  vars <- rep(0, n)
  for(i in 1:n){
    Y <- simul(lbda, n)
    means[i] <- mean(Y)
    vars[i] <- var(Y)
  }
  return(c(means, vars))
}

hist(simul(1, 10000), main = "Distribution of Y", xlab="Y")
a <- bootstrap(simul, 1, 10000, 1000)
means <- a[1:10000]
vars <- a[10001:20000]
hist(means, main="Distribution of mean", xlab="Mean")
hist(vars, main="Distribution of variance", ylab="Variance")
# 
# conv <- function(lbda, x){
#   if(x<0){return(0)}
#   #else(return(lbda**2 * x * exp(-lbda*x)))
#   else{return(-exp(-lbda*x)*(lbda*x+1))}
# }
# 
# draws <- rep(0, 10000)
# for(i in 1:10000){
#   draws[i] <- conv(1, runif(1, -100, 100))
# }
# hist(draws)
