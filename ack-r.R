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
  ackVals = list(1:3)
  returnVals = list(1:3)
  cc <- makeCluster(detectCores() - 1)
  
  parLapply(cc, 1:3, function(x) returnVals[x] <- ack(x, x + 1))
  return (returnVals)
}

system.time(replicate(1000, ackReturn <- acks()))

for (i in ackReturn) {
  print(i)	
}