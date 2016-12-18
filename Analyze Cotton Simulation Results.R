# Analyze Cotton Simulation Output
#
hist(scenarios.df$tpv[which(scenarios.df$tpv<10000000)],breaks=seq(0,10000000,by=100000), main="Histogram of Terminal Portfolio Values")

print(paste("Mean TPV ",round(mean(scenarios.df$tpv),0),sep=" "))
print(paste("Median TPV ",round(median(scenarios.df$tpv),0),sep=" "))
print(paste("Quartiles ",quantile(scenarios.df$tpv),sep=" "))

print(mean(scenarios.df$tpv[which(scenarios.df$tpv< 2478754)]))
print(mean(scenarios.df$tpv[which(scenarios.df$tpv>= 2478754 & scenarios.df$tpv< 3778116)]))
print(mean(scenarios.df$tpv[which(scenarios.df$tpv>- 3778116 & scenarios.df$tpv< 6069686)]))
print(mean(scenarios.df$tpv[which(scenarios.df$tpv> 6069686)]))
      

# Multiple Linear Regression Example
fit <- lm(scenarios.df$tpv ~ scenarios.df$combYrs + scenarios.df$geoMean + scenarios.df$percentAnnuity, data=scenarios.df)
summary(fit) # show results

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 