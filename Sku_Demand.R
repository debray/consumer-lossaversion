# Graphing function
# Plot price, quantity and discount levels
source('Set_Variables.R')

# Which ranked item?
top = 9

TopSku <- SkuSort$Sku[top]
Ind <- which(HFT_Items$sku==TopSku)
Ind <- Ind[floor(0.7*length(Ind)):length(Ind)]

Sample_Size = length(Ind)

# Get Variables of Interest
Prices <- HFT_Items$price[Ind]
Qty <- HFT_Items$qty_ordered[Ind]
Disc <- HFT_Items$discount_amount[Ind]

# Remove prices with 0
N0Ind <- which(Prices!=0)
Prices <- Prices[N0Ind]
Qty <- Qty[N0Ind]
Disc <- Disc[N0Ind]

# Unique price points
UPrice <- unique(Prices)
UQty=0
UDisc=0

for (i in 1:length(UPrice)) {
	indx = which(Prices==UPrice[i])
	UQty[i]=sum(Qty[indx])
	UDisc[i]=sum(Disc[indx])
}
UDisc <- UDisc / UQty
# remove NaN's and Inf's
UDisc[which(is.nan(UDisc))]=0
UDisc[which(is.infinite(UDisc))]=0

# Plot the demand functions
plot(UPrice, UQty)
resP = glm(UQty~UPrice)	# regression on Price
abline(resP)
#par(new=TRUE)
plot(UDisc, UQty, col="red")
resD = glm(UQty~UDisc)		# regression on Discount
abline(resD, col="red")