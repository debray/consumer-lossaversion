rm(list=ls())
load("Regret_Score_Top1000.RData")

# Compute Regret score for a SKU using Historical data
if (!exists("Daily_Sales")) { load("Daily_Sales.RData") }
if (!exists("Sku_Stats")) { load("Sku_Stats.RData") }
Trends <- read.csv("Google_Trends/HFT_Trends.csv")
Trends <- Trends$harbor.freight.tools[28:dim(Trends)[1]]

source("ForwardDist.R")
source("Optimal_Price.R")

if (!exists("Demand")) { load("Estimated_Coeff_Demand_promo.RData") }

TopN <- 3000

#Total_Hist_Sales <- 0; Total_Opt_Sales <- 0; Total_Hist_GM <- 0; Total_Opt_GM <- 0
#NumDays <- 0; RevLoss <- 0; GMLoss <- 0; Cost <- 0
#OptPrice <- NULL; OptQty <- NULL; OptSales <- NULL; OptGM <- NULL
#HistPrice <- NULL; HistQty <- NULL; HistSales <- NULL; HistGM <- NULL

Start = 1001

for (n in Start:TopN) {

Sku <- SkuSort$Sku[n]

Ind <- which(Daily_Sales$Sku==Sku)

time_window = 10

numDays <- length(Daily_Sales$Dates)

# Create a synthetic dummy for promotions?
Volatility <- var(Daily_Sales$Qty[Ind,which(!is.na(Daily_Sales$Qty[Ind,]))])
Baseline <- mean(Daily_Sales$Qty[Ind,which(!is.na(Daily_Sales$Qty[Ind,]))])

nCoeff <- dim(Betas)[2]
Beta <- Betas[Ind,1:(nCoeff-1)]

Beta[which(is.na(Beta))]=0

NumDays[n] <- sum(!is.na(Daily_Sales$Sales[Ind,]))
DataAvail <- which(!is.na(Daily_Sales$Sales[Ind,]))
startDay <- which(cumsum(!is.na(Daily_Sales$Sales[Ind,]))>10)[1]-10   # more than 10 sale days
endDay <- which(cumsum(!is.na(Daily_Sales$Sales[Ind,]))-NumDays[n]>=0)[1]

startDay = 1; endDay = 587

thisOptPrice <- rep(NA,endDay) ; thisOptQty <- rep(NA, endDay)

# Run optimization if enough days sample
if (!is.na(startDay) & !(is.na(endDay))) {

#DatesAvail = startDay:endDay
DatesAvail = which(!is.na(Daily_Sales$Sales[Ind,]))

Cost[n] = 0.5*median(Daily_Sales$Price[Ind,DatesAvail], na.rm=TRUE)

NoPromo <- rep(TRUE,endDay)

for (i in DatesAvail) {
	
	print(paste("Top ", n, ", Day ", i, "NumDays: ", NumDays[n])) 

    current_Price <- Daily_Sales$Price[Ind, i]
    
    Promo <- 0
    # Indicator for Promo - using 3-sigma event
    if (!is.na(Daily_Sales$Qty[Ind,i])) {
        Promo <- Daily_Sales$Qty[Ind,i] > (Baseline + 3*sqrt(Volatility))
    }
    
    # Add Promo effect
    if (Promo==0) {
        Optimized <- Optimal_Price(Sku, Daily_Sales$Dates[i], time_window, current_Price, Cost[n])	
        thisOptPrice[i] <- Optimized$Price
        thisOptQty[i] <- Optimized$Qty
    } else {
        Optimized <- Optimal_Price(Sku, Daily_Sales$Dates[i], time_window, current_Price, Cost[n])	
        #thisOptPrice[i] <- Optimized$Price
        #thisOptQty[i] <- Optimized$Qty + Betas[Ind,nCoeff]  # Promo effect
        thisOptPrice[i] <- NA
        thisOptQty[i] <- NA
        NoPromo[i] <- FALSE
    }
}

OptPrice <- rbind(OptPrice, thisOptPrice)
OptQty <- rbind(OptQty, thisOptQty)
OptSales <- rbind(OptSales, thisOptPrice * thisOptQty)
OptGM <- rbind(OptGM, (OptPrice[n,]-Cost[n])*OptQty[n,])

DatesAvail <- intersect(which(NoPromo), DatesAvail)

HistSales <- rbind(HistSales, Daily_Sales$Sales[Ind,])
HistPrice <- rbind(HistPrice, Daily_Sales$Price[Ind,])
HistQty <- rbind(HistQty, Daily_Sales$Qty[Ind,])
HistGM <- rbind(HistGM, (Daily_Sales$Price[Ind,]-Cost[n])*Daily_Sales$Qty[Ind,])

Total_Hist_Sales[n] <- sum(Daily_Sales$Sales[Ind,DatesAvail], na.rm=TRUE)
Total_Hist_GM[n] <- sum(HistGM[n,DatesAvail], na.rm=TRUE)
Total_Opt_Sales[n] <- sum(OptSales[n,], na.rm=TRUE)
Total_Opt_GM[n] <- sum(OptGM[n,], na.rm=TRUE)
RevLoss[n] <- Total_Opt_Sales[n] - Total_Hist_Sales[n]
GMLoss[n] <- Total_Opt_GM[n] - Total_Hist_GM[n]

}
 
save("NumDays", "HistSales", "HistPrice", "HistQty", "OptPrice", "OptQty", 
        "OptSales", "RevLoss", "GMLoss", "Total_Opt_GM", "Total_Hist_GM",
        "Total_Hist_Sales", "Total_Opt_Sales", 
        "OptGM", "HistGM", "Cost", file="Regret_Score_Top3000.RData")

}