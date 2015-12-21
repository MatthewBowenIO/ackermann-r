library(foreach)  
library(doParallel)
library(parallel)

ack <- function(x, y) {
  if (x == 0) {
    return (y + 1)
  } else if (y == 0) {
    return (ack(x -1, 1))
  } else 
    return (ack(x - 1, ack(x, y - 1)))
}

acks <- function() {
  cc <- makeCluster(detectCores())
  results <- foreach(i=1:3) %dopar% {
    ack(i, i + 1)
  }
  return (results)
}

system.time(replicate(1000, ackReturn <- acks()))

for (i in ackReturn) {
  print(i)	
}