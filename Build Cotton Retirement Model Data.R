# Problems I want to solve: when to claim SS benefits, how much to annuitize, how my shortened life expectancy affects my plan. 
# 
# Strategy: model and study results. 

# Initial values and parameters

set.seed(27514)
scenarios <- 1000
return50yr <- c(.0612,.0696,.0777,.0854,.0927,.0996,.1061,.1121,.1178,.123)
sigma50yrs <-  c(.0336,.043,.0556,.0696,.0843,.0994,.1148,.1304,.1461,.162)
percentAnnuity <- vector(length=scenarios)
percentEquity <- vector(length=scenarios)
annualSpendPercent <- vector(length=scenarios)
qLAC <- vector(length=scenarios)
vcSSclaimAge <- vector(length=scenarios)
dcSSclaimAge <- vector(length=scenarios)
vcSSBenefit <- vector(length=scenarios)
dcSSBenefit <- vector(length=scenarios)
scenario <- vector(length=scenarios)
maleDeathAge <- vector(length=scenarios)
femaleDeathAge <- vector(length=scenarios)
equityAlloc <- vector(length=scenarios)
annualReturn <- vector(length=scenarios)
annuityPayoutPc <- vector(length=scenarios)
qLACpayoutPc <- vector(length=scenarios)
geoMean <- vector(length=scenarios)
inflation <- vector(length=scenarios)
combYrs <- vector(length=scenarios) # combined male and female retierment years calculated in next step
unmetSpend <- vector(length=scenarios) # blank field. unmet spending year will be stored here in next step
tpv <- vector(length=scenarios) # blank field. terminal portfolio value will be stored here in next step
minSpend <- vector(length=scenarios) # blank field. minimum annual spend value will be stored here in next step

annuityPayoutPc <- .05 # annuity payout %
qLACpayoutPc <- .05 # QLAC payout %
inflation <- .025 # expected inflation rate

mu <- .05 # expected real portfolio return
sigma = .11 # portfolio variance
marketReturns <- rnorm (scenarios,mean = mu, sigma)
maleweightedLE <- 1 # weight male's life expectancy for cancer
vcBenefitList <- c(24540,26304,28200,30228,32400) # available SS benefits claiming ages 66-70
dcBenefitList <- c(27823,29952,32112,34416,36888,39540) # available SS benefits claiming ages 65-70

# 
# Create 1,000 scenarios
# 
# Get life expectancy for husband and spouse.  Build vectors of random male and female deaths
#
expectancy <- read.csv("~/desktop/Cum Prob of Death.csv",header=TRUE)

for (i in 1:scenarios) {
maledeathprob <- runif(1,0,1)
maleDeathAge[i] <- round(expectancy$Age[min(which(expectancy$Cum.Prob.Male.Dying.this.Year > maledeathprob))] * maleweightedLE,0)
femaledeathprob <- runif(1,0,1)
femaleDeathAge[i] <- expectancy$Age[min(which(expectancy$Cum.Prob.Female.Dying.this.Year > femaledeathprob))]

# Get an asset allocation, then historical (IFA.com) market returns and sigmas for that allocation
equityAlloc [i] <- sample(1:10,1)/10 # generates equity allocation from .1 to 1 in .1 increments
annualReturn [i] <- rnorm(1,mean=return50yr [equityAlloc[i] * 10], sd=sigma50yrs [equityAlloc[i] * 10])
sigma50yr [i] <- sigma50yrs [equityAlloc[i] * 10]

# Generate uniform random % of annuity from 0 to 100% of max available in 10% increments.
percentAnnuity [i] <- sample(c(.1,.2,.3,.4,.5,.6,.7,.8),1)

# Assume a spending percentage between 3% and 5% in 0.5% increments
annualSpendPercent [i] <- sample(c(.03,.035,.04,.045,.05,.055),1)

# Generate uniform random % of QLAC from 0 to 100% of max ($125,000 for Vicki only due to my illness) available in 10% increments.
qLAC [i] <- sample(c(0,25000,50000,75000,100000,125000),1)

# Generate uniform random age of SS benefits claims.

vcSSclaimAge [i] <- sample(c(66,67,68,69,70),1)
dcSSclaimAge [i] <- sample(c(65,66,67,68,69,70),1)

# Calculate annual SS benefit for claiming at each age. If death before claiming age, claiming age = FRA (66.)

if (femaleDeathAge[i] > vcSSclaimAge [i]) modVCclaimAge <- vcSSclaimAge [i] 
else {
  # print(paste("Scenario ",i," Female death at ",femaleDeathAge[i]," SS Claim age at", vcSSclaimAge [i], "claim changed to 66.",sep=" "))
  modVCclaimAge <- 66 
}

if (maleDeathAge[i] > dcSSclaimAge [i]) modDCclaimAge <- dcSSclaimAge [i] 
else {
  # print(paste("Scenario ",i," Male death at ",maleDeathAge[i]," SS Claim age at", dcSSclaimAge [i], "claim changed to 66.",sep=" "))
  modDCclaimAge <- 66 
}

vcSSBenefit [i] <- vcBenefitList[modVCclaimAge - 65]
dcSSBenefit [i] <- dcBenefitList[modDCclaimAge - 64]

scenario [i] <- i

}  ### end of outside FOR loop

combYrs <- (scenarios.df$maleDeathAge - 65) + (scenarios.df$femaleDeathAge - 65)


scenarios.df <- data.frame(scenario,maleDeathAge,femaleDeathAge,equityAlloc,annualSpendPercent,percentAnnuity,annualReturn,sigma50yr,qLAC,vcSSclaimAge,vcSSBenefit,dcSSclaimAge,dcSSBenefit,annuityPayoutPc,qLACpayoutPc,inflation,combYrs,unmetSpend,tpv,geoMean,minSpend)
scenarios.df <- scenarios.df [1:scenarios,]
write.csv(scenarios.df,"~/desktop/CottonScenarios.csv",row.names=FALSE)

# 
# Model the worst sustainable expense shock for each year. 
# 
# Model Vicki home change after my death. 