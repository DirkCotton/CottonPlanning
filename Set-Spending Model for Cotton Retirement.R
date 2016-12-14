# Simulate Cotton Retirement using scenarios created by "Build Cotton Retirement Model Data.R" script.

# This is a second  version of the model that sets minimum acceptable annual income instead of using spending a safe 
# withdrawal rate. It doesn't need to track portfolio depletion because there are othe income sources (annuity, Social Security, etc.).
# It does, however, track and flag inadequate income in any year and stops ythe simnulations as "failed" at that point.
# 
# Initialize variables and parameters


earliestAge <- 65 # age each scenario starts
portfolio <- 4000000
ageQLAC <- 85 # year QLAC begins paying, Vicki only

unmetSpending <- vector("numeric",1000)

scenarioYr <- 0
portf <- 0
debug <- 0
set.seed(27514)

muSP <- .053 + inflation # real expected return for S&P 500
sigmaSP <- 0.16 # expected standard deviation for S&P 500
muTIPS <- -.005 + inflation # real expected return for TIPS
sigmaTIPS <- .05 # expected standard deviation for TIPS

# 
# Input scenarios from CSV file

scenarios.df <- read.csv("~/desktop/CottonScenarios.csv")

# 
# Find number of scenarios

scenarios <- length(scenarios.df$scenario)
portValues <- matrix (0,nrow=scenarios,ncol=40)  # create a matrix to store all annual portfolios for a max of 40 years ending values for all scenarios
annualSpending <- matrix (0,nrow=scenarios,ncol=40)  # create a matrix to store all annual spending for a max of 40 years ending values for all scenarios
minSpend <- 200000  # this is the minimum acceptable annual income
spendReduc <- .2  * minSpend

# scenarios <- 1000  ################# DEBUG = 1
debugScenario <- sample(1:scenarios,1)  # save and print data for this scenario unless negative

if (debugScenario > 0) debug2 <- 1

for (i in 1:scenarios) {
  
  if(i == debugScenario & debugScenario > 0) debug <-1 else debug <- 0 # set i= a negative number or a single specific scenario number to print output for
  
  if (debug == 1) {
    print(" ")
    print(paste("================== Scenario ",i,sep=" "))
    print(" ")
  }
  
# calculate max number of years in this scenario
  
  lastAge <- max(scenarios.df$femaleDeathAge [i],scenarios.df$maleDeathAge [i])
  
  inflation <- scenarios.df$inflation [i]
  payoutQLAC <- scenarios.df$qLACpayoutPc [i]
  payoutAnnuityPc <- scenarios.df$annuityPayoutPc [i]
  
#
# Subtract QLAC and Annuity costs from portfolio at beginning of scenario
#
  portf <- portfolio - scenarios.df$qLAC [i]    # take out the QLAC first, then buy annuity as a percentage of remaining asvings afre QLAC purchase
  if (debug == 1) print(paste("Subtract QLAC =",scenarios.df$qLAC [i]," portf= ",portf,sep=" "))
  if (debug == 1) print(paste("Subtract Annuity =",scenarios.df$percentAnnuity [i] * portf," from portfolio. portf= ",portf - scenarios.df$percentAnnuity [i] * portf,sep=" "))
  portf <- portf - (scenarios.df$percentAnnuity [i] * portf)
  

  #
  # generate a random market return for each year of the scenario
  #
  
  portfolioMU <- (scenarios.df$equityAlloc * muSP) + ((1 - scenarios.df$equityAlloc) * muTIPS)
  randomEquityReturns <- rnorm(40,mean=muSP, sd=sigmaSP)
  randomBondReturns <- rnorm(40,mean=muTIPS, sd=sigmaTIPS)
  randomAnnualReturns <- (scenarios.df$equityAlloc[i] * randomEquityReturns) + ((1 - scenarios.df$equityAlloc[i]) * randomBondReturns)
  
  annuityPayout <- scenarios.df$percentAnnuity[i] * (portfolio - scenarios.df$qLAC [i])  * payoutAnnuityPc
  vcSSben <- scenarios.df$vcSSBenefit [i] # VC initial SS benefit
  dcSSben <- scenarios.df$dcSSBenefit [i] # DC initial SS benefit

for (scenarioYr in earliestAge:lastAge) {
  
    spend <- 0
    
# Set Vicki and Dirk's ages
    
    ageVicki <- scenarioYr + 2
    ageDirk <- scenarioYr
    
    if (ageDirk >= scenarios.df$maleDeathAge [i] & ageVicki >= scenarios.df$femaleDeathAge [i]) break # scenario ends when second spouse dies

    if (debug == 1) {
      print(" ")
      print(" ")
      print(paste("Scenario ",i," Year ",scenarioYr,"************** ",sep=" "))
    }
    
    if (debug == 1) print(paste("Dirk age ",ageDirk," Vicki Age ",ageVicki,sep=" "))
    
    
#
# Spend from Social Security benefits, QLACs, annuities when available
#
#   Inflate SS benefits
#
    vcSSben <- scenarios.df$vcSSBenefit [i] * (1 + inflation) ^ (scenarioYr - earliestAge)
    dcSSben <- scenarios.df$dcSSBenefit [i] * (1 + inflation) ^ (scenarioYr - earliestAge)
    
# 
# modify SS benefits to survivor benefits
# 
    
    if (debug == 1) {
      print(paste("portf = ",portf,sep=" "))
      if (ageDirk == scenarios.df$maleDeathAge [i]) print("Male dies this year ///")
      if (ageVicki == scenarios.df$femaleDeathAge [i]) print("Female dies this year ///")
    }
    
    if (ageDirk == scenarios.df$maleDeathAge [i]) {
      ageDirk <- 0
  
      # At Dirk's death, Vicki's SS benfit = the larger benefit. Dirk's goes away
      
      vcSSben <- max(dcSSben,vcSSben)
      dcSSben <- 0
      
    }
    if (ageVicki == scenarios.df$femaleDeathAge [i]) {
      ageVicki <- 0
      
      # At Vicki's death, smaller SS benefit goes away

      vcSSben <- 0
      dcSSben <- max(dcSSben,vcSSben)
    }
    
#     if (ageDirk >= scenarios.df$maleDeathAge [i] | ageVicki >= scenarios.df$femaleDeathAge [i]) {
#       if (debug == 1) print(paste("Spending requirement reduced 20% to ",minSpend,"/////",sep=" "))
#       minSpend <- minSpend - spendReduc
#     }
    
    if (ageVicki >= scenarios.df$vcSSclaimAge [i] & ageVicki < scenarios.df$femaleDeathAge [i]) {
      spend <- spend + vcSSben # pay VC SS benefit
      if (ageVicki == scenarios.df$vcSSclaimAge [i] & debug == 1)  print("Vicki begins SS payments ///") 
      if (debug == 1) print(paste("Add SS Benefit VC",vcSSben," spend = ",spend,sep=" "))
    }
    
    if (ageDirk >= scenarios.df$dcSSclaimAge [i] & ageDirk < scenarios.df$maleDeathAge [i]) {
      spend <- spend + dcSSben # pay DC SS benefit
      if (ageDirk == scenarios.df$dcSSclaimAge [i] & debug == 1)  print("Dirk begins SS payments ///")
      if (debug == 1) print(paste("Add SS Benefit DC",dcSSben," spend= ",spend,sep=" "))
    }
    
    if (ageVicki >= ageQLAC & ageVicki < scenarios.df$femaleDeathAge [i]) {
      spend <- spend + (scenarios.df$qLAC [i] * payoutQLAC) # payout QLAC 
      if (debug == 1) print(paste("Add QLAC payout= ",scenarios.df$qLAC [i] * payoutQLAC," spend= ",spend,sep=" "))
    }
    
    if (debug == 1 & ageVicki == ageQLAC & (scenarios.df$qLAC [i] * payoutQLAC) > 0) print("Vicki's QLAC payouts begin this year ////")
    
    spend <- spend + annuityPayout  # pay out Life Annuity
    
    if (debug == 1) print(paste("Add annuity payout= ",annuityPayout," spend= ",spend,sep=" "))
  
# Spend from portfolio, 

    if (spend < minSpend) portfolioSpend <- min(minSpend - spend, portf) else portfolioSpend <- 0 # spend from portfolio
    if (debug == 1) print(paste("spend= ",spend," minSpend=",minSpend," portf=",portf,sep=" "))
    
    portf <- max(0,portf - portfolioSpend)
    annualSpending [i,scenarioYr - earliestAge + 1] <- round(spend + portfolioSpend,0)
    totalSpend <- round(spend + portfolioSpend,0)
    excessSpend <- max(0,totalSpend - minSpend)
     
    if (debug == 1) print(paste("Portfolio withdrawal= ",portfolioSpend," portf = ",portf,sep=" "))
    if (debug == 1) print(paste("Total spend this year= ",totalSpend," portf = ",portf,sep=" "))
    if (debug == 1) print(paste("Excess spend this year= ",excessSpend,sep=" "))
    if (totalSpend < minSpend) {
      if (debug == 1) print(paste ("//// Inadequate spending this year = ",scenarioYr,sep=" "))
      unmetSpending [i] <- scenarioYr
      break
    }
# 
# Grow portfolio by market returns
# 
    if (excessSpend > 0) {
      if (debug == 1) print(paste("Increase portf from ",portf," to ", portf + excessSpend ,sep=" "))
      portf <- portf + excessSpend
    }
    portf <- portf * (1 + randomAnnualReturns [scenarioYr - earliestAge + 1])
    if (debug == 1) print(paste("Portfolio earns ",randomAnnualReturns [scenarioYr - earliestAge + 1]," to ",portf," scenarioYr= ",scenarioYr, "Earliest Age= ",earliestAge, sep=" "))
    
# End simulation at second death
    #
    # Record results in file
    #

    portValues[i,scenarioYr - earliestAge + 1] <- round(portf,0)
    if (debug == 1) print(paste("Storing portfolio value ",portf, " in row ",i," column ",scenarioYr - earliestAge + 1,sep=" "))
    }
  
  }
  
write.csv(portValues[1:scenarios,],"~/desktop/Annual Portfolio Values.csv")

write.csv(annualSpending,"~/desktop/Annual Spending.csv")
  
if (debug2 == 1) print(" ")
if (debug2 == 1) print(cbind(scenarios.df[debugScenario,],portfolio,minSpend))

# write test scenario data()

if (debugScenario > 0) write.csv(cbind(scenarios.df[debugScenario,],portfolio,minSpend),"~/desktop/Test Scenario.csv")

if (debugScenario > 0) write.csv(randomAnnualReturns,"~/desktop/Test Scenario Returns.csv")
