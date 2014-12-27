# plotDailySales.R
# plots the daily sales levels for two items on the same graph
# Input: SKU for the 2 items, start and end days
plotDailySales <- function(Sku1, Sku2, startDay,endDay) {
	
	if (!exists("Daily_Sales")) { load("Daily_Sales.RData")}
	
	maxDays = dim(Daily_Sales$Qty)[2]
	Days = startDay:min(endDay,maxDays)
	numDays = length(Days)
	
	Ind1 = which(Daily_Sales$Sku==Sku1)
    Ind2 = which(Daily_Sales$Sku==Sku2)
	
	IndPos1 = which(abs(Daily_Sales$Disc[Ind1,Days])>0)
	IndNeg1 = which(abs(Daily_Sales$Disc[Ind1,Days])==0)
    
    IndPos2 = which(abs(Daily_Sales$Disc[Ind2,Days])>0)
	IndNeg2 = which(abs(Daily_Sales$Disc[Ind2,Days])==0)
	
	PosQ1 <- rep(NA, numDays); NegQ1 <- rep(NA, numDays)
	PosQ1[IndPos1] = Daily_Sales$Qty[Ind1, Days[IndPos1]]
	NegQ1[IndNeg1] = Daily_Sales$Qty[Ind1, Days[IndNeg1]]
    
    PosQ2 <- rep(NA, numDays); NegQ2 <- rep(NA, numDays)
	PosQ2[IndPos2] = Daily_Sales$Qty[Ind2, Days[IndPos2]]
	NegQ2[IndNeg2] = Daily_Sales$Qty[Ind2, Days[IndNeg2]]
    
    print(paste("Sale Days: ", length(IndPos1)))
    print(paste("P1 ",mean(PosQ1,na.rm=TRUE), ", N1 ", 
            mean(NegQ1,na.rm=TRUE), "P2 ", mean(PosQ2,na.rm=TRUE),
            "N2 ", mean(NegQ2,na.rm=TRUE)))
    
    #print(SkuSort$Name[which(SkuSort$Sku==Sku1)])
    #print(paste("SKU: ", Sku1, ", OnSale: ", length(IndPos1) / numDays, 
    #", SaleBoost: ", mean(PosQ1, na.rm=TRUE) / mean(NegQ1, na.rm=TRUE)))
    
    makePlot <- TRUE

	# show plots for the two indexes on the specified days
    if (makePlot) {
    maxQ = max(Daily_Sales$Qty[Ind1, Days],Daily_Sales$Qty[Ind2, Days], na.rm=TRUE)
        
	plot(Days, PosQ1, lwd=2, col="black", ylim=c(0,maxQ))
	par(new=TRUE)
	plot(Days, NegQ1, col="blue", ylim=c(0,maxQ))
    par(new=TRUE)
    plot(Days, PosQ2, lwd=2, col="green", ylim=c(0,maxQ))
	par(new=TRUE)
	plot(Days, NegQ2, col="red", ylim=c(0,maxQ))
    }
}