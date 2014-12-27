# Pre-processing data for Basket Analysis
# Count of items and order amount in same order as Sku for the top items
# modify code to specify a list of SKUs

if (!exists("HFT_Items")) { load("HFT_Items.RData") }
# Above also load SkuSort
#if (!exists("HFT_Items")) { load("HFT_Items.RData") }

# By default gets basket of items for all the top SKUs
# otherwise specify a list of individual SKUs in the code below
TopN <- length(SkuSort$Sku)
#TopN <- 1

#Sku_Baskets_Qty <- NULL
#Sku_Baskets_Amt <- NULL

for (i in 1:TopN) {
	
	print(paste("Sku: ", i))
	
	thisSku <- SkuSort$Sku[i]
	
	# Initialize all Sku counts to 0
	ThisBasket_Qty <- rep(0, length(SkuSort$Sku))
	ThisBasket_Amt <- rep(0, length(SkuSort$Sku))
	
	Ind <- which(HFT_Items$sku==thisSku)
	
	# Go through orders and add counts
	for (n in 1:length(Ind)) {
		
		thisOrder <- HFT_Items$order_id[Ind[n]]
		indOrder <- which(HFT_Items$order_id==thisOrder)
		
		listBasketSku <- HFT_Items$sku[indOrder]
		amtBasketSku <- HFT_Items$row_total[indOrder]
		qtyBasketSku <- HFT_Items$qty_ordered[indOrder]
		
		# Go through the Basket and get all the items's quantity and total amount
		for (b  in 1:length(listBasketSku)) {
			
			indBasket <- which(SkuSort$Sku==listBasketSku[b])
			ThisBasket_Qty[indBasket] <- ThisBasket_Qty[indBasket] + qtyBasketSku[b]
			ThisBasket_Amt[indBasket] <- ThisBasket_Amt[indBasket] + amtBasketSku[b]
		}
	}
	
	Sku_Baskets_Qty <- rbind(Sku_Baskets_Qty, ThisBasket_Qty)
	Sku_Baskets_Amt <- rbind(Sku_Baskets_Amt, ThisBasket_Amt)
	
	save("Sku_Baskets_Qty", "Sku_Baskets_Amt", file="Sku_Basket.RData")
}