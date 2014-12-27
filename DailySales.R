# Pre-processing steps
# Daily Statistics : Avg Prices, Qty over a Day
load("Items_DateInfo.RData")
load("Sku_Stats.RData")
load("HFT_Items.RData")

Dates <- unique(Date_Items$Date)
numDays <- length(Dates)

Daily_Sales <- NULL

numSKU <- length(SkuSort$Sku)

# Specific SKU
TopN <- numSKU

All_Sku <- NULL; All_Qty <- NULL; All_Price <- NULL 
All_Disc <- NULL; All_Sales <- NULL

for (n in 1:TopN) {
	
	print(n)
	
SKU <- SkuSort$Sku[n]
Ind <- which(HFT_Items$sku==SKU)

Prices <- HFT_Items$price[Ind]
Qty <- HFT_Items$qty_ordered[Ind]
Disc <- HFT_Items$discount_amount[Ind]
SKU_Dates <- Date_Items$Date[Ind]

DQty <- 0; DPrice <- 0; DDisc <- 0

for (i in 1:numDays) {
	
	DInd <- which(SKU_Dates==Dates[i])
	
	if (length(DInd)==0) {
		DQty[i] <- NA
		DPrice[i] <- NA
		DDisc[i] <- NA
	}
	else {
		DQty[i] <- sum(Qty[DInd])
		DPrice[i] <- mean(Prices[DInd])
		DDisc[i] <- mean(Disc[DInd])
	}
}

DSales <- DQty * DPrice

All_Sku <- c(All_Sku, SKU)
All_Qty <- rbind(All_Qty, DQty)
All_Price <- rbind(All_Price, DPrice)
All_Disc <- rbind(All_Disc, DDisc)

}

Daily_Sales$Sku <- All_Sku
Daily_Sales$Qty <- All_Qty
Daily_Sales$Price <- All_Price
Daily_Sales$Disc <- All_Disc

Slope <- 0
for (n in 1:TopN) {
	print(n)
	n1 <- sum(!is.na(Daily_Sales$Qty[n,]))
	n2 <- sum(!is.na(Daily_Sales$Price[n,]))
	if (n1<2 || n2<2) { Slope[n] <- NA }
	else {
		R <- glm(Daily_Sales$Qty[n,] ~ Daily_Sales$Price[n,])
		Slope[n] <- R$coefficients[2]
	}
}

All_Sales <- NULL
for (n in 1:TopN) {
	All_Sales <- rbind(All_Sales, Daily_Sales$Qty[n,]*Daily_Sales$Price[n,])
}

save("Daily_Sales", file="Daily_Sales.RData")