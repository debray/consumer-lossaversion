# Test of experience effects - divide dataset into different
# quartiles based on experience of customer (total purchases)
# Run tests for Discounting and Substitution effects on separate groups

if (!exists("Customer_Sales")) { load("Customer_Sales_All.RData") }

#NetSales = Customer_Sales$Cust_Sales - Customer_Sales$Cust_Shipping
NetSales = Customer_Sales$Total_Item_Sale

# Sort by Customer Sales Rank
CustRank <- order(NetSales, decreasing=TRUE)

# Remove outliers 
CustRank <- setdiff(CustRank, CustRank[which(Customer_Sales$Cust_Sales[CustRank]<1)])
CustRank <- setdiff(CustRank, CustRank[which(Customer_Sales$Cust_Sales[CustRank]>10000)])

TopFrac = 0.25

numCust = length(CustRank)
numTop = floor(TopFrac * numCust)
numBottom = floor((1-TopFrac) * numCust)
cindTop = CustRank[1:numTop]
cindMiddle = CustRank[(numTop+1):numBottom]
cindBottom = CustRank[numBottom+1:numCust]

# Contrast top 50% vs bottom 50%
SaleTop = sum(NetSales[cindTop], na.rm=TRUE)
SaleMiddle = sum(NetSales[cindMiddle], na.rm=TRUE)
SaleBottom = sum(NetSales[cindBottom], na.rm=TRUE)

# Order Indexes of the top and bottom customers
oindTop <- which(Order_CustID %in% CustID[cindTop])
oindMiddle <- which(Order_CustID %in% CustID[cindMiddle])
oindBottom <- which(Order_CustID %in% CustID[cindBottom])

# Divide dataset into Top and Bottom customers
load("Items_DateInfo.RData")
load("Sku_Stats.RData")
if (!exists("HFT_Items")) { load("HFT_Items.RData") }

# Item Indexes of the top and bottom customers
ItemIndTop <- which(HFT_Items$order_id %in% oindTop)
ItemIndMiddle <- which(HFT_Items$order_id %in% oindMiddle)
ItemIndBottom <- which(HFT_Items$order_id %in% oindBottom)

Dates <- unique(Date_Items$Date)
numDays <- length(Dates)

Daily_SalesTop <- NULL; Daily_SalesBottom <- NULL

numSKU <- length(SkuSort$Sku)
TopN <- numSKU
TopN <- 6039

All_SkuTop <- NULL; All_QtyTop <- NULL; All_PriceTop <- NULL 
All_DiscTop <- NULL; All_SalesTop <- NULL
All_SkuBottom <- NULL; All_QtyBottom <- NULL; All_PriceBottom <- NULL 
All_DiscBottom <- NULL; All_SalesBottom <- NULL

for (n in 1:TopN) {

    print(n)
    SKU <- SkuSort$Sku[n]
    
    IndTop <- intersect(ItemIndTop, which(HFT_Items$sku==SKU))
    IndBottom <- intersect(ItemIndBottom, which(HFT_Items$sku==SKU))

    PricesTop <- HFT_Items$price[IndTop]; PricesBottom <- HFT_Items$price[IndBottom]
    QtyTop <- HFT_Items$qty_ordered[IndTop]; QtyBottom <- HFT_Items$qty_ordered[IndBottom]
    DiscTop <- HFT_Items$discount_amount[IndTop]; DiscBottom <- HFT_Items$discount_amount[IndBottom]
    SKU_DatesTop <- Date_Items$Date[IndTop]; SKU_DatesBottom <- Date_Items$Date[IndBottom]

    DQtyTop<-0; DPriceTop<-0; DDiscTop<-0; DQtyBottom<-0; DPriceBottom<-0; DDiscBottom<-0; 

    for (i in 1:numDays) {

        DIndTop <- which(SKU_DatesTop==Dates[i]); DIndBottom <- which(SKU_DatesBottom==Dates[i])

        if (length(DIndTop)==0) { DQtyTop[i]<-NA; DPriceTop[i]<-NA; DDiscTop[i]<-NA
        } else {
            DQtyTop[i]<-sum(QtyTop[DIndTop]); DPriceTop[i]<-mean(PricesTop[DIndTop]); 
            DDiscTop[i]<-mean(DiscTop[DIndTop]) }

        if (length(DIndBottom)==0) { DQtyBottom[i]<-NA; DPriceBottom[i]<-NA; DDiscBottom[i]<-NA
        } else {
            DQtyBottom[i]<-sum(QtyBottom[DIndBottom]); DPriceBottom[i]<-mean(PricesBottom[DIndBottom]); 
            DDiscBottom[i]<-mean(DiscBottom[DIndBottom]) }
    }

    DSalesTop <- DQtyTop * DPriceTop; DSalesBottom <- DQtyBottom * DPriceBottom 

    All_SkuTop <- c(All_SkuTop, SKU); All_SkuBottom <- c(All_SkuBottom, SKU)
    All_QtyTop <- rbind(All_QtyTop, DQtyTop); All_QtyBottom <- rbind(All_QtyBottom, DQtyBottom)
    All_PriceTop <- rbind(All_PriceTop, DPriceTop); All_PriceBottom <- rbind(All_PriceBottom, DPriceBottom)
    All_DiscTop <- rbind(All_DiscTop, DDiscTop); All_DiscBottom <- rbind(All_DiscBottom, DDiscBottom)
    All_SalesTop <- rbind(All_SalesTop, DSalesTop); All_SalesBottom <- rbind(All_SalesBottom, DSalesBottom)

}

Daily_SalesTop$Sku <- All_SkuTop; Daily_SalesBottom$Sku <- All_SkuBottom
Daily_SalesTop$Qty <- All_QtyTop; Daily_SalesBottom$Qty <- All_QtyBottom
Daily_SalesTop$Price <- All_PriceTop; Daily_SalesBottom$Price <- All_PriceBottom
Daily_SalesTop$Disc <- All_DiscTop; Daily_SalesBottom$Disc <- All_DiscBottom
Daily_SalesTop$Sales <- All_SalesTop; Daily_SalesBottom$Sales <- All_SalesBottom
Daily_SalesTop$Dates <- as.Date(Dates); Daily_SalesBottom$Dates <- as.Date(Dates)

save("Daily_SalesTop", "Daily_SalesBottom", file="Daily_Sales_Experience25S.RData")