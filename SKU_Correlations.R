# Compute Pairwise correlations for All SKUs
# Calculate effective sample size
# Compute correlation lower bound using Fisher-Z Transformation
library(psychometric)

if (!exists("Daily_Sales")) { load("Daily_Sales.RData") }

TopN <- dim(Daily_Sales$Qty)[1]
#TopN <- 10

CorrQty <- matrix(rep(0, TopN^2), TopN, TopN) 
CorrPrice <- matrix(rep(0, TopN^2), TopN, TopN) 
CorrSales <- matrix(rep(0, TopN^2), TopN, TopN) 
LB_Qty <- matrix(rep(0, TopN^2), TopN, TopN) 
LB_Sales <- matrix(rep(0, TopN^2), TopN, TopN) 

SampleSize <- matrix(rep(1, TopN^2), TopN, TopN) 

for (i in 1:TopN) {
	
	print(paste("SKU ", i))
	
	ni <- sum(!is.na(Daily_Sales$Qty[i,])) 
	
	for (j in 1:TopN) {
		
		nj <- sum(!is.na(Daily_Sales$Qty[j,])) 
		
		CorrQty[i,j] <- cor(Daily_Sales$Qty[i,], Daily_Sales$Qty[j,], 
				use="na.or.complete")
		
		CorrPrice[i,j] <- cor(Daily_Sales$Price[i,], Daily_Sales$Price[j,], 
				use="na.or.complete")
		
		CorrSales[i,j] <- cor(Daily_Sales$Sales[i,], Daily_Sales$Sales[j,], 
				use="na.or.complete")
		
		effSample <- min(ni, nj)
		SampleSize[i,j] <- effSample
		
		# Lower bound
		# Convert to Fisher's z-transform and Z * 1/sqrt(N-3)
		LB_Qty[i,j] <- CIr(CorrQty[i,j], effSample)[1]
		
		LB_Sales[i,j] <- CIr(CorrSales[i,j], effSample)[1]
	}
}