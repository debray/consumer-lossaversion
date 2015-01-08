load("Daily_Sales.RData")
load("Sku_Stats.RData")

source("Likelihood.Refdep.R")

SubList <- read.csv("subtop100.csv")
n <- nrow(SubList)

Lambda <- rep(0,n)
Logprob <- rep(0,n)
MaxV1 <- rep(0,n)
MaxV2 <- rep(0,n)

for (i in 1:n) {
  # Lambda estimates for the substitute pairs
  Values <- Likelihood.Refdep(SubList$SKUitem[i], SubList$SKUsub[i])
  Lambda[i] <- Values$EstLambda
  Logprob[i] <- Values$Logprob
  MaxV1[i] <- Values$MaxV1
  MaxV2[i] <- Values$MaxV2
}