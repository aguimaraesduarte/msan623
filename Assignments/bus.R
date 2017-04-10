scenario1 <- function(breakdown){
  if(breakdown <= 25){return(breakdown)}
  else if(breakdown <= 50){return(50 - breakdown)}
  else if(breakdown <= 75){return(breakdown - 50)}
  else{return(100 - breakdown)}
}

scenario2 <- function(breakdown){
  if(breakdown <= 25){return(25 - breakdown)}
  else if(breakdown <= 37.5){return(breakdown - 25)}
  else if(breakdown <= 50){return(50 - breakdown)}
  else if(breakdown <= 62.5){return(breakdown - 50)}
  else if(breakdown <= 75){return(75 - breakdown)}
  else{return(breakdown - 75)}
}

dist1 <- rep(0, 100000)
dist2 <- rep(0, 100000)
for(i in 1:100000){
  r <- runif(1, 0, 100)
  dist1[i] <- scenario1(r)
  dist2[i] <- scenario2(r)
}
hist(dist1)
hist(dist2)
mean(dist1)
mean(dist2)
