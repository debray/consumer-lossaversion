consumer-lossaversion
============

R programs accompanying Consumer loss and reference dependence paper.
There are programs for pre-processing the raw data set, simulation of effects, and estimating lambda (loss aversion) values.

Pre-processing
============
The raw data is in two formats: 
at the order level, with each order having a unique order id. This includes customer info for customer level analysis.
at the item level, with each item having a unqiue item id. This includes pricing and discount information.
The data has to be "compacted" / aggregated at the daily level to make analysis feasible. These pre-processing and helper functions do that.

Pre-processing:
DailySales.R - collect aggregated statistics by day of total quantity, price and discounts.

Helper functions:
SetVariables.R
Sku_Basket.R
Sku_Correlations.R

Graphing functions:
plotDaiySales.R - plot daily sales data for two SKUs between start and end days.
Sku_Demand.R - plot by SKU of demand level, price and discount
Sku_Temporal.R - plot of demand, price and discount averaged at each point over a "smoothing window".

Misc:
RegretScore.R - consumer's "regret" for missing out on a previous period's low price. Was testing this as an alternative
theory to loss aversion.

Simulation
============

Estimation of Lambda
============
