# Simulate the structural model - discrete choice
# Values of v (private value), gamma (loss aversion) are drawn from a population
source("utility.R")
Days = 1:100

PreSales = 1:30
SaleDays = 31:60
SaleEnd = 61:length(Days)

# Indicators
IndSale = rep(NA, length(Days))
IndSale[SaleDays] = 0

# Reference price, discounted price and substitute price
Porig <- 8
Pdisc <- 7
Psub <- 9

# Private value
v <- 10

U1 <- NULL; U2 <- U1; P1 <- U1
for (d in Days) {
	# Sample a Gamma value
	# Loss aversion parameter: lambda = gamma - 1
	# For gamma = 1, there is no loss aversion effect
	gamma <- runif(1, 1, 4)
  gamma <- 0.7
	
	if (d %in% PreSales) { P <- Porig; Pref <- Porig; }
	else if (d %in% SaleDays) { P <- Pdisc; Pref <- Porig; }
	else if (d %in% SaleEnd) { P <- Porig; Pref <- Pdisc; }
	U1[d] <- utility(v, gamma, P, Pref)
	
	# Utility of item 2 (substitute) that has no price changes
	U2[d] <- utility(v, gamma, Psub, Psub)
	
	P1[d] <- exp(U1[d]) / (exp(U1[d]) + exp(U2[d]))
}

D1 <- NULL; D2 <- NULL
for (d in Days) {
	g <- sample(1:100, 1)
	D1[d] <- P1[d] * 100
	D2[d] <- (1-P1[d])*100
}

plot(D1, type="l", ylim=c(0, 100), xlab="Days", ylab="Demand", col="black")
par(new=TRUE)
plot(D2, type="l", ylim=c(0, 100), xlab="Days", ylab="Demand", col="red")
par(new=TRUE)
plot(IndSale, type="l", col="blue", ylim=c(0,1), xlab="Days", ylab="Demand", yaxt="n")