library("tseries")
library("PortfolioAnalytics")
library("quantmod")
library("Quandl")
library("DEoptim")
library('stats')



#data reading
data = read.csv("data.csv", header=T)
data$ï..date = as.Date(data$ï..date)
data = xts(data[,2:5], order.by = data$ï..date)
class(data)
View(data)

#bonds
bonds = data[,1]
mbonds=monthlyReturn(bonds, type='arithmetic')
colnames(mbonds) = "mbonds"
table.AnnualizedReturns(mbonds)
charts.PerformanceSummary(mbonds)

#stocks
stocks = data[,2]
mbse = monthlyReturn(stocks, type='arithmetic')
colnames(mbse)='mbse'
table.AnnualizedReturns(mbse)
charts.PerformanceSummary(mbse)

#international_bonds
ibonds = data[,3]
mibonds = monthlyReturn(ibonds, type='arithmetic')
colnames(mibond)='mibond'
table.AnnualizedReturns(mibonds)
charts.PerformanceSummary(mibonds)

#international_stock (NAsdeq)
istocks = data[,4]
mistocks = monthlyReturn(istocks, type='arithmetic')
colnames(mistocks)='mistocks'
table.AnnualizedReturns(mistocks)
charts.PerformanceSummary(mistocks)

#Comparision
comp1=cbind(mbonds,mbse,mibonds,mistocks)
table.AnnualizedReturns(comp1)
charts.PerformanceSummary(comp1)


**********************************************************************************************************************
#Normality test
mmean_1 = mean(mbonds)
mmedian_1 = median(mbonds)
mret1 = cbind(mmean_1,mmedian_1)
mret1

msd_1 = sd(mbonds)
mmad_1 = MeanAbsoluteDeviation(mbonds)

mriskcomp1 = cbind(msd_1,mmad_1)
mriskcomp1

mmean_2 = mean(mbse)
mmedian_2 = median(mbse)
mret2 = cbind(mmean_2,mmedian_2)
mret2

msd_2 = sd(mbse)
mmad_2 = MeanAbsoluteDeviation(mbse)

mriskcomp2 = cbind(msd_2,mmad_2)
mriskcomp2

mmean_3 = mean(mistocks)
mmedian_3 = median(mistocks)
mret3 = cbind(mmean_3,mmedian_3)
mret3

msd_3 = sd(mistocks)
mmad_3 = MeanAbsoluteDeviation(mistocks)

mriskcomp3 = cbind(msd_3,mmad_3)
mriskcomp3

mmean_4 = mean(mibonds)
mmedian_4 = median(mibonds)
mret4 = cbind(mmean_4,mmedian_4)
mret4

msd_4 = sd(mibonds)
mmad_4 = MeanAbsoluteDeviation(mibonds)

mriskcomp4 = cbind(msd_4,mmad_4)
mriskcomp4
#another property of -ve skewed : msd > mmad


#*risk normality
#skewness
mskew_bonds = skewness(mbonds)
mskew_bonds  #negative skewed

#kurtosis
mkurto_bonds = kurtosis(mbonds)
mkurto_bonds #k<3 not normal

#JB tets
mjb_bonds = jarque.bera.test(mbonds)
mjb_bonds  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal


#skewness_stock
mskew_stocks = skewness(mbse)
mskew_stocks  #negative skewed

#kurtosis_stock
mkurto_stocks = kurtosis(mbse)
mkurto_stocks #k<3 not normal

#JB tets_stock
mjb_stocks = jarque.bera.test(mbse)
mjb_stocks  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal


#skewness_int-bond
mskew_ibonds = skewness(mibonds)
mskew_ibonds  #negative skewed

#kurtosis_int-bond
mkurto_ibonds = kurtosis(mibonds)
mkurto_ibonds #k<3 not normal

#JB tets_int-bond
mjb_ibonds = jarque.bera.test(mibonds)
mjb_ibonds  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal

#skewness_int-stock
mskew_istocks = skewness(mistocks)
mskew_istocks  #negative skewed

#kurtosis_int-stock
mkurto_istocks = kurtosis(mistocks)
mkurto_istocks #k<3 not normal

#JB tets_int-stock
mjb_istocks = jarque.bera.test(mistocks)
mjb_istocks  #p lesss than critical value(0.05) so, null is rejected nd distri. is not normal

comp2 = cbind(mskew_bonds,mskew_stocks,mskew_ibonds,mskew_istocks,mkurto_bonds,mkurto_stocks,mkurto_ibonds,mkurto_istocks)
comp2

Factor = c("skewness-bond","skewness-stock","skewness_int-bond", "skewness_int-stock","kurtosis-bonds", "kurtosis_stock", "kurtosis_int-bond","kurtosis_int-stock","p-value_JBtest_bonds","p-value_JBtest_stocks","p-value_JBtest_ibonds","p-value_JBtest_istocks")
value = c(mskew_bonds,mskew_stocks,mskew_ibonds,mskew_istocks,mkurto_bonds,mkurto_stocks,mkurto_ibonds,mkurto_istocks,mjb_bonds$p.value,mjb_stocks$p.value,mjb_ibonds$p.value,mjb_istocks$p.value)
df = data.frame(Factor,value)
print(df)

bond_dist = density(data$ind10)
plot(bond_dist, main="distribusion of IND 10 data")
polygon(bond_dist, col="brown1", border="darkred")

stock_dist = density(data$bse)
plot(stock_dist, main="distribusion of BSE data")
polygon(stock_dist, col="chartreuse", border="chartreuse4")

ibond_dist = density(data$int_bond)
plot(ibond_dist, main="distribusion of international bond data")
polygon(ibond_dist, col="aquamarine", border="aquamarine4")

istock_dist = density(data$int_stock)
plot(istock_dist, main="distribusion of international stocks data")
polygon(istock_dist, col="darkorange", border="darkorange4")


#Value at risk
mvar_bonds = VaR(p=0.99,mbonds,method='modified')
colnames(mvar_bonds)=c('mvar_bonds')
mvar_stocks = VaR(p=0.99,mbse,method='modified')
colnames(mvar_stocks)=c('mvar_stocks')
mvar_ibonds = VaR(p=0.99,mibonds,method='modified')
colnames(mvar_ibonds)=c('mvar_ibonds')
mvar_istocks = VaR(p=0.99,mistocks,method='modified')
colnames(mvar_istocks)=c('mvar_istocks')

Factor = c("mVAR_bonds","mVAR_stocks","mVAR_int-bonds","mVAR_int-stocks")
Value_at_Risk=c(mvar_bonds,mvar_stocks,mvar_ibonds,mvar_istocks)
df_2 = data.frame(Factor,Value_at_Risk)
print(df_2)
*********************************************************************************************************************
#Risk and Return
#covariance
mmain = cbind(mbonds,mbse,mibonds,mistocks)
macov = cov(mmain)
macov

#corr
macor=cor(mmain)
macor

#coefficient of determination
mar2 = macor^2
mar2

*********************************************************************************************************************
#portfolio optimization
mport = cbind(mbonds,mbse,mibonds,mistocks)

# 4.2.2. Naive Global Portfolio (Monthly Rebalancing)
mnaivew = as.numeric(t(c(0.25,0.25,0.25,0.25)))
names(mnaivew) = c("mbonds","mstocks","mibonds","mistocks")
mnaive = Return.portfolio(R=mport,weights=mnaivew,geometric=F,rebalance_on="months")
colnames(mnaive) = "mnaive"

# 4.2.3. Roche Global Portfolio (Monthly Rebalancing)
mrochew = as.numeric(t(c(0.24,0.18,0.33,0.25)))
names(mrochew) = c("mbonds","mstocks","mibonds","mistocks")
mroche = Return.portfolio(R=mport,weights=mrochew,geometric=F,rebalance_on="months")
colnames(mroche) = "mroche"

# 4.2.4. Bogle U.S. Portfolio (Monthly Rebalancing)
mboglew = as.numeric(t(c(0.40,0.60,0.00,0.00)))
names(mboglew) = c("mbonds","mstocks","mibonds","mistocks")
mbogle = Return.portfolio(R=mport,weights=mboglew,geometric=F,rebalance_on="months")
colnames(mbogle) = "mbogle"

benchcomp = cbind(mnaive,mroche,mbogle)
View(benchcomp)
# 4.2.6. Benchmark Portfolios Returns Comparison
table.AnnualizedReturns(benchcomp)
charts.PerformanceSummary(benchcomp)


# 4.3. Portfolio Optimization 

# 4.3.1. Mean Maximization 

# Portfolio Specifications
mport1c = portfolio.spec(assets = colnames(mport))

# Portfolio Constraints
mport1c = add.constraint(mport1c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport1c = add.constraint(mport1c,type="long_only")

# Portfolio Objectives
mport1c = add.objective(mport1c,type="return",name="mean")

# Portfolio Optimization
mportopt1 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport1c,optimize_method="DEoptim",)
chart.Weights(mportopt1)

####
mport1w = as.numeric(mportopt1$weights)
names(mport1w) = c("mbonds","mstocks","mibonds","mistocks")
mport1 = Return.portfolio(R=mport["2015-01-31::"],weights=mport1w,geometric=F,rebalance_on="months")
colnames(mport1) = "mport1"

####

# 4.3.2. Standard deviation minimization

# Portfolio Specifications
mport2c = portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport2c = add.constraint(mport2c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport2c = add.constraint(mport2c,type="long_only")

# Portfolio Objectives
mport2c = add.objective(mport2c,type="risk",name="StdDev")

# Portfolio Optimization
mportopt2 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport2c,optimize_method="DEoptim")
chart.Weights(mportopt2)

####
mport2w = as.numeric(mportopt2$weights)
names(mport2w) <- c("mbonds","mstocks","mibonds","mistocks")
mport2 <- Return.portfolio(R=mport["2015-01-31::"],weights=mport2w,geometric=F,rebalance_on="months")
colnames(mport2) <- "mport2"

####

# 4.3.3. Mean Maximization and Standard Deviation Minimization Portfolio

# Portfolio Specifications
mport3c = portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport3c = add.constraint(mport3c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport3c = add.constraint(mport3c,type="long_only")

# Portfolio Objectives
mport3c = add.objective(mport3c,type="return",name="mean")
mport3c = add.objective(mport3c,type="risk",name="StdDev")

# Portfolio Optimization
mportopt3 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport3c,optimize_method="DEoptim")
chart.Weights(mportopt3)

####
mport3w = as.numeric(mportopt3$weights)
names(mport3w) = c("mbonds","mstocks","mibonds","mistocks")
mport3 = Return.portfolio(R=mport["2015-01-31::"],weights=mport3w,geometric=F,rebalance_on="months")
colnames(mport3) = "mport3"

####

# 4.3.4. Mean Maximization Value at Risk (VaR) Minimization Portfolio

# Portfolio Specifications
mport4c = portfolio.spec(assets=colnames(mport))

# Portfolio Constraints
mport4c = add.constraint(mport4c,type="weight_sum",min_sum=0.99,max_sum=1.01)
mport4c = add.constraint(mport4c,type="long_only")

# Portfolio Objectives
mport4c = add.objective(mport4c,type="return",name="mean")
mport4c = add.objective(mport4c,type="risk",name="VaR",arguments=list(p = 0.99,method="modified"))

# Portfolio Optimization
mportopt4 = optimize.portfolio(R=mport["::2014-12-31"],portfolio=mport4c,optimize_method="DEoptim")
chart.Weights(mportopt4)


#####
mport4w = as.numeric(mportopt4$weights)
names(mport4w) <- c("mbonds","mstocks","mibonds","mistocks")
mport4 <- Return.portfolio(R=mport["2015-01-31::"],weights=mport4w,geometric=F,rebalance_on="months")
colnames(mport4) <- "mport4"

#####
#Comparision
comp3 = cbind(mnaivew,mport1w,mport2w,mport3w,mport4w)
comp3

# 4.3.5. Optimized Portfolios Backtesting Comparison
mportcomp = cbind(mnaive["2015-01-31::"],mport1,mport2,mport3,mport4)
table.AnnualizedReturns(mportcomp)
charts.PerformanceSummary(mportcomp)

