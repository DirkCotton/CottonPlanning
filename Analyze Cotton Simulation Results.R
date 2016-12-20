# Analyze Cotton Simulation Output
#
hist(scenarios.df$tpv[which(scenarios.df$tpv<10000000)],breaks=seq(0,10000000,by=100000), main="Histogram of Terminal Portfolio Values")

print(paste("Mean TPV ",round(mean(scenarios.df$tpv),0),sep=" "))
print(paste("Median TPV ",round(median(scenarios.df$tpv),0),sep=" ")) #
q <- quantile(scenarios.df$tpv)
q2 <- q[2]
q3 <- q[3]
q4 <- q[4]

print(paste("Quartiles ",quantile(scenarios.df$tpv),sep=" "))

print(" ")
print("Mean TPVs for Quartiles 1-4")
print(mean(scenarios.df$tpv[which(scenarios.df$tpv< q2)]))
print(mean(scenarios.df$tpv[which(scenarios.df$tpv>= q2 & scenarios.df$tpv< q3)]))
print(mean(scenarios.df$tpv[which(scenarios.df$tpv>= q3 & scenarios.df$tpv< q4)]))
print(mean(scenarios.df$tpv[which(scenarios.df$tpv> q4)]))

print(" ")
print("Summary Combined years or retirement for Quartiles 1-4")
print(summary(scenarios.df$combYrs[which(scenarios.df$tpv< q2)]))
print(summary(scenarios.df$combYrs[which(scenarios.df$tpv>= q2 & scenarios.df$tpv< q3)]))
print(summary(scenarios.df$combYrs[which(scenarios.df$tpv>= q3 & scenarios.df$tpv< q4)]))
print(summary(scenarios.df$combYrs[which(scenarios.df$tpv> q4)]))

# Print TPV by quartile
#
print(" ")
print("Unmet Spending Scenarios for Quartiles 1-4")
print(scenarios.df$scenario[which(scenarios.df$unmetSpend > 0 & scenarios.df$tpv < q2)])
print(scenarios.df$scenario[which(scenarios.df$unmetSpend > 0 &scenarios.df$tpv >= q2 & scenarios.df$tpv < q3)])
print(scenarios.df$scenario[which(scenarios.df$unmetSpend > 0 &scenarios.df$tpv >= q3 & scenarios.df$tpv < q4)])
print(scenarios.df$scenario[which(scenarios.df$unmetSpend > 0 &scenarios.df$tpv > q4)])

# Multiple Linear Regression Example

scenariosdata <- scenarios.df # [which(scenarios.df$percentAnnuity == .8),]

Y <- scenarios.df$combYrs
G <- scenarios.df$geoMean
W <- scenarios.df$annualSpendPercent
A <- scenarios.df$percentAnnuity
  fit <- lm(scenarios.df$unmetSpend ~ Y + G + W + A, data=scenariosdata)
# fit <- lm(scenarios.df$unmetSpend ~ Y + W + A, data=scenariosdata)
summary(fit) # show results

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 