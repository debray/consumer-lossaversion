Likelihood.Refdep <- function(Sku1, Sku2) {
# Estimate the log-likelihood ratio of pairwise SKUs
# for reference dependence model
source("utility.R")

# sliding window (days)
window <- 7     # forward window
winref <- 7      # reference price memory window
# Grid values of parameters
#V <- seq(0,200,10)
#Gam <- seq(0,5,0.1)
Lam <- seq(1,5,0.1)
# private values
V1 <- seq(0,200,10)
V2 <- seq(0,200,10)

#Days <- seq(1,587,1)
Days <- seq(366,587,1)

Ind1 <- which(Daily_Sales$Sku==Sku1)
#Sku2 <- 90743
Ind2 <- which(Daily_Sales$Sku==Sku2)

Price1 <- Daily_Sales$Price[Ind1,]
Qty1 <- Daily_Sales$Qty[Ind1,]
Price2 <- Daily_Sales$Price[Ind2,]
Qty2 <- Daily_Sales$Qty[Ind2,]

# Aggregate log probability for a lambda value
# max over all private values
AggLogProb <- rep(0,length(Lam))
MaxV1 <- rep(0,length(Lam)); MaxV2 <- rep(0,length(Lam))

# Median price
mp1 <- median(Price1, na.rm=T)
mp2 <- median(Price2, na.rm=T)

# Log ratio of sales
nDays <- length(Days)
logRatioQty <- log(Qty1[2:nDays]/Qty2[2:nDays])
#PDiff <- Price1[Days[2:nDays]] - Price2[Days[2:nDays]]
#PRefDiff <- Price1[Days[1:(nDays-1)]] - Price2[Days[1:(nDays-1)]]

#AggLogProb <- matrix(rep(0,length(V)*length(Lam)),length(V),length(Lam))
VLogProb <- matrix(rep(0,length(V1)*length(V2)),length(V1),length(V2))

# Max quantity
MaxQty <- max(max(Qty1,na.rm=T), max(Qty2,na.rm=T))

# Reference dependence model Parameters
for (j in 1:length(Lam)) {
  lambda <- Lam[j]
  
  for (i1 in 1:length(V1)) {
    for (i2 in 1:length(V2)) {
  
# Default parameter values
#v <- 10; 
#lambda <- 1
# Private values should be higher than price
# otherwise customer does not buy
v1 <- mp1 + V1[i1] 
v2 <- mp2 + V2[i2]

print(paste("lambda: ", lambda, " v1: ", v1, " v2: ", v2))

Uref1 <- NULL; Unoref1 <- NULL
Uref2 <- NULL; Unoref2 <- NULL
Prob1 <- NULL; Prob2 <- NULL; Prob0 <- NULL; logProb <- NULL
for (d in (2+winref):(nDays-window)) {
  day <- Days[d]
  # Reference price is previous day's price
  #Uref1[d] <- utility(60, lambda, Price1[day], Price1[day-1])
  # Reference price is average of previous t days
  Pref1 <- mean(Price1[(day-winref):(day-1)],na.rm=T)
  # Reference price is median price
  #Pref1 <- median(Price1[Days],na.rm=T)
  Uref1[d] <- utility(v1, lambda, mean(Price1[day:(day+window)], na.rm=T), Pref1)
  # Model without reference dependence
  #Uref1[d] <- utility(v1, gamma, Price1[day], Price1[day])
  Pref2 <- mean(Price2[(day-winref):(day-1)], na.rm=T)
  #Pref2 <- median(Price2[Days], na.rm=T)
  Uref2[d] <- utility(v2, lambda, mean(Price2[day:(day+window)], na.rm=T), Pref2)
  # Calculate probability of choice using the utility model
  Prob1[d] <- exp(Uref1[d]) / (1+exp(Uref1[d]) + exp(Uref2[d]))
  Prob2[d] <- exp(Uref2[d]) / (1+exp(Uref2[d]) + exp(Uref1[d]))
  
  #Prob0[d] <- 1 - Prob1[d] - Prob2[d]
  # Calculate per period log probability
  thisQty1 <- Qty1[day]
  thisQty1 <- mean(Qty1[day:(day+window)],na.rm=T)
  thisQty2 <- Qty2[day]
  thisQty2 <- mean(Qty2[day:(day+window)],na.rm=T)
  logProb[d] <- thisQty1*log(Prob1[d]) + thisQty2*log(Prob2[d])
}

VLogProb[i1,i2] <- sum(logProb[winref:(nDays-window)], na.rm=T)

} }   # closing braces for v1 , v2

#AggLogProb[j] <- sum(logProb[winref:(nDays-window)], na.rm=T)
# Log probability for lambda value is max over private values v1, v2
MaxInd <- which(VLogProb==max(VLogProb), arr.ind=T)
if (nrow(MaxInd)>1) { 
  MaxV1[j] <- I[1,1]; MaxV2[j] <- I[1,2] 
} else {
  MaxV1[j] <- I[1]; MaxV2[j] <- I[2]
}
AggLogProb[j] <- VLogProb[MaxV1[j], MaxV2[j]]

}

EstLambda <- Lam[which(AggLogProb==max(AggLogProb), arr.ind=T)]
LogProb <- AggLogProb[which(AggLogProb==max(AggLogProb), arr.ind=T)]

RetValues <- NULL
RetValues$EstLambda <- EstLambda
RetValues$LogProb <- LogProb
RetValues$MaxV1 <- V1[MaxV1[which(AggLogProb==max(AggLogProb), arr.ind=T)]]
RetValues$MaxV2 <- V2[MaxV2[which(AggLogProb==max(AggLogProb), arr.ind=T)]]

return(RetValues)

}