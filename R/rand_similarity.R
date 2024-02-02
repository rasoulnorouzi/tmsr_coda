#create a hypothetical clustering outcome with 2 distinct clusters
calc_rand <- function (group1, group2)
{
  x <- abs(sapply(group1, function(x) x - group1))
  x[x > 1] <- 1
  y <- abs(sapply(group2, function(x) x - group2))
  y[y > 1] <- 1
  sg <- sum(abs(x - y))/2
  bc <- choose(dim(x)[1], 2)
  ri <- 1 - sg/bc
  return(ri)
}
