rm(list=ls())
monty <- function(N=1000)
{
  doors <- 1:3 #initialize the doors behind one of which is a good prize
  win <- 0 #to keep track of number of wins
  
  for(i in 1:N)
  {
    prize <- floor(runif(1, 1, 4)) #randomize which door has the good prize
    guess <- floor(runif(1, 1, 4)) #guess a door at random
    
    ## Reveal one of the doors you didn't pick which has a bum prize
    if(prize != guess)
      reveal <- doors[-c(prize, guess)]
    else
      reveal <- sample(doors[-c(prize, guess)], 1)
    
    ## Switch doors
    select <- doors[-c(reveal, guess)]
    
    ## Count up your wins
    if(select == prize)
    {
      win <- win+1
    }
  }
  return(win*100/N)
}

bootstrap <- function(N=1000){
  p <- rep(0, N)
  for(i in 1:N){
    p[i] <- monty()
  }
  return(p)
}

p <- bootstrap()
hist(p, main = "Distribution of probability of winning", xlab = "Probability of winning")
abline(v=66.666, col="red")
text(67.1, 285, "66.6%", col="red")
