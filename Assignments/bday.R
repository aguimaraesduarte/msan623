rm(list=ls())
bday <- function(n=30, N=1000)
{
  days <- 1:365 #initialize the birthdays
  win <- 0 #to keep track of number of wins
  
  for(i in 1:N)
  {
    bdays <- sample(days, n, replace=T)
    
    ## Count up your wins
    if(min(1, sum(duplicated(bdays))) == 1)
    {
      win <- win+1
    }
  }
  return(win*100/N)
}

bootstrap <- function(n=30, N=1000){
  p <- rep(0, N)
  for(i in 1:N){
    p[i] <- bday(n)
  }
  return(p)
}

p <- bootstrap()
hist(p, main = "Distribution of probability of sharing a birthday (n=30)", xlab = "Probability of sharing a birthday")
abline(v=70.5, col="red")
text(71, 280, "70.5%", col="red")

for(i in 16:30){
  p <- bootstrap(n=i)
  hist(p, main = paste("Distribution of probability of sharing a birthday (n=", i, ")", sep=""), xlab = "Probability of sharing a birthday")
}
