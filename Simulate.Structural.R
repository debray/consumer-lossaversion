# Simulate the structural model - discrete choice
source("utility.R")
Days = 1:90

PreSales = 1:30
SaleDays = 31:60
SaleEnd = 61:length(Days)

# Reference price, discounted price and substitute price
Porig <- 8
Pdisc <- 7
Psub <- 9

# Private value
value <- 10

# Loss aversion parameter: lambda = gamma - 1
# For gamma = 1, there is no loss aversion effect
gamma <- 3

U1 <- NULL
for (i in Days) {
	if (i %in% PreSales) { P <- Porig; Pref <- Porig; }
	else if (i %in% SaleDays) { P <- Pdisc; Pref <- Porig; }
	else if (i %in% SaleEnd) { P <- Porig; Pref <- Pdisc; }
	U1[i] <- utility(value, gamma, P, Pref)
}

# Utility of item 2 (substitute) that has no price changes
U2 <- utility(value, gamma, Psub, Psub)

P1 <- exp(U1) / (exp(U1) + exp(U2))

IndSale = rep(NA, length(Days))
IndSale[SaleDays] = 0

plot(P1, type="p", ylim=c(0, 1), xlab="Days", ylab="Prob(C=1)")
par(new=TRUE)
plot(IndSale, type="l", col="blue", ylim=c(0,1), xlab="Days", ylab="Prob(C=1)", yaxt="n")