load("Daily_Sales.RData")
load("Sku_Stats.RData")

source("Likelihood.Refdep.R")

# If testing substitutes, use list of substitutes
SubList <- read.csv("subtop100.csv")


# Else generate random pairings as control group
TopSKU <- SkuSort$Sku[1:100]
PairSKU <- SkuSort$Sku[1:300]
PairList <- NULL
set.seed(0)
for (j in 1:100) {
  # potential pairs (not including SKU itself and known substitutes)
  pair <- setdiff(PairSKU,c(TopSKU[j], SubList$SKUsub[which(SubList$SKUitem==TopSKU[j])]))
  rndpair <- sample(pair, 2)
  thisPair <- data.frame(rep(TopSKU[j],2), rndpair)
  PairList <- rbind(PairList, thisPair)
}
names(PairList) <- c("SKUitem", "SKUsub")

#PairList <- SubList

n <- nrow(PairList)

Lambda <- rep(0,n)
Logprob <- rep(0,n)
MaxV1 <- rep(0,n)
MaxV2 <- rep(0,n)

for (i in 1:n) {
  # Lambda estimates for the substitute pairs
  Values <- Likelihood.Refdep(PairList$SKUitem[i], PairList$SKUsub[i])
  Lambda[i] <- Values$EstLambda
  Logprob[i] <- Values$LogProb
  MaxV1[i] <- Values$MaxV1
  MaxV2[i] <- Values$MaxV2
  
  print(paste(i,Lambda[i], Logprob[i], MaxV1[i], MaxV2[i]))
}