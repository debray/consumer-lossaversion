# Graphing function
# Show quantity, price and discount on a sliding window
source('Set_Variables.R')

# Which ranked item?
top = 3

TopSku <- SkuSort$Sku[top]
Ind <- which(HFT_Items$sku==TopSku)
Ind <- Ind[floor(0.5*length(Ind)):length(Ind)]

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

n = length(Prices)

Window = floor(0.005*n)		# number of transactions

AvgPrice=0
AvgQty=0
AvgDisc=0

# Run sliding window for all transactions
for (i in 1:n) {
	
	low <- max(1, i-Window)
	high <- min(n, i+Window)
	slider <- seq(low, high, by=1)
	
	AvgPrice[i] = mean(Prices[slider])
	AvgQty[i]=sum(Qty[slider])
	AvgDisc[i]=mean(Disc[slider])
}

# Plot the sliding window quantities
Trans = 1:n
plot(Trans, AvgQty)
par(new=TRUE)
plot(Trans, AvgPrice, col="red")
axis(side=4)