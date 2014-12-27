"Set_Variables.R"
# Helper function for pre-processing step

# specify working directory with the item level data
setwd("/RealData/HFT-Archive")

#if (!exists("HFT_Items")) { HFT_Items <- read.csv("sales_item_reduced.csv") }
load("HFT_Items.RData")

if (!exists("Date_Items")) { load("Items_DateInfo.RData") }

if (!exists("Sku_Stats")) { load("Sku_Stats.RData") }

# Use this if Item Trend data is available
#if (!exists("Item_Trend")) { load("Item_Trend.RData") }

if (!exists("Daily_Sales")) { load("Daily_Sales.RData") }