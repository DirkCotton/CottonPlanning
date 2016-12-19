# Build 40,000 (10,000 scenarios x 40 years) of stock and bond returns

set.seed (27514)
inflation <- .025
muSP <- .053 + inflation # real expected return for S&P 500
sigmaSP <- 0.16 # expected standard deviation for S&P 500
muTIPS <- -.005 + inflation # real expected return for TIPS
sigmaTIPS <- .05 # expected standard deviation for TIPS
sdVector <- c(sigmaSP,sigmaTIPS)
b= matrix(c(1,.11,.11,1),nrow=2, ncol=2)    

rans <- rmvnorm(40000, mean=c(muSP,muTIPS), cov=b,sd=sdVector,d= 2)

print(paste("SP mean= ",mean(rans[,1]),sep=" "))
print(paste("SP sigma= ",sd(rans[,1]),sep=" "))
print(paste("TIPS mean= ",mean(rans[,2]),sep=" "))
print(paste("TIPS sigma= ",sd(rans[,2]),sep=" "))
print(paste("correlation= ",cor(rans[,1],rans[,2]),sep=" "))
colnames(rans) <- c("Stocks","Bonds")

write.csv(rans,"~/R Projects/Cotton Planning/Random Returns.csv",row.names=FALSE)