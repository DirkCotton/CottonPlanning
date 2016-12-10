# Simulate Cotton Retirement using scenarios created by "Build Cotton Retirment Model Data.R" script.
# 
# Initialize variables and parameters

earliestAge <- 65 # age each scenario starts
portfolio <- 1000000
ageQLAC <- 85 # year QLAC begins paying, Vicki only
payoutQLAC <- .05 
payoutAnnuity <- .05 # Annuity annual payout rate
scenarioYr <- 0
portf <- 0
debug <- 0
set.seed(27514)

# 
# Input scenarios from CSV file

scenarios.df <- read.csv("~/desktop/CottonScenarios.csv")

# 
# Find number of scenarios

scenarios <- length(scenarios.df$scenario)
portValues <- matrix (0,nrow=scenarios,ncol=40)  # create a matrix to store all annual portfolios for a max of 40 years ending values for all scenarios
annualSpending <- matrix (0,nrow=scenarios,ncol=40)  # create a matrix to store all annual spending for a max of 40 years ending values for all scenarios


#   scenarios <- 3  ################# DEBUG = 1

for (i in 1:scenarios) {
  
  if(i == 33) debug <-1 else debug <- 0
  
  if (debug == 1) {
    print(" ")
    print(paste("*************** Scenario ",i,sep=" "))
    print(" ")
  }
  
# calculate max number of years in this scenario
  
  lastAge <- max(scenarios.df$femaleDeathAge [i],scenarios.df$maleDeathAge [i])
  
#
# Subtract QLAC and Annuity costs from portfolio at beginning of scenario
#
  portf <- portfolio - scenarios.df$qLAC [i] - (scenarios.df$percentAnnuity [i] * portfolio)
  if (debug == 1) print(paste("Subtract QLAC =",scenarios.df$qLAC [i],"and annuity =",scenarios.df$percentAnnuity [i] * portfolio," from portfolio. portf= ",portf,sep=" "))
  #
  # generate a random market return for each year of the scenario
  #
  randomAnnualReturns <- rnorm(40,mean=scenarios.df$annualReturn [i],sd=scenarios.df$sigma50yr [i])
  if (debug == 1) {
    print(paste("expected portfolio mu= ",scenarios.df$annualReturn [i]," expected sd = ",scenarios.df$sigma50yr [i],sep=" "))
    print(paste("generated portfolio mu= ",mean(randomAnnualReturns)," generated sd = ",sd(randomAnnualReturns),sep=" "))
  }
  
  for (scenarioYr in earliestAge:lastAge) {
    
    if (debug == 1) {
      print(" ")
      print(" ")
      print(paste("Scenario Year ",scenarioYr,"************** ",sep=" "))
    }
    
# Set Vicki and Dirk's ages
    
    ageVicki <- scenarioYr + 2
    ageDirk <- scenarioYr

    if (debug == 1) print(paste("Dirk age ",ageDirk," Vicki Age ",ageVicki,sep=" "))
    
# 
# If only one spouse reduce expenses by 20%
# 
    annualSpend <- min(scenarios.df$annualSpendPercent [i] * portf, portf) # spend from portfolio
    
    if (debug == 1) {
      print(paste("portf = ",portf,sep=" "))
      if (ageDirk == scenarios.df$maleDeathAge [i]) {
        ageDirk <- 0
        print("Male dies this year ///")
      }
      if (ageVicki == scenarios.df$femaleDeathAge [i]) {
        ageVicki <- 0
        print("Female dies this year ///")
      }
    }
    
    if (ageDirk >= scenarios.df$maleDeathAge [i] | ageVicki >= scenarios.df$femaleDeathAge [i]) 
    annualSpend <- .8 * annualSpend
  
    if (ageDirk >= scenarios.df$maleDeathAge [i] & ageVicki >= scenarios.df$femaleDeathAge [i]) break # scenario ends when second spouse dies
    
# Spend from portfolio, 
    
    portf <- portf - annualSpend
    annualSpending [i,scenarioYr - earliestAge + 1] <- round(annualSpend,0)
    if (debug == 1) print(paste("Annual spend ",annualSpend," portf= ",portf,sep=" "))
    
# Add Social Security benefits, QLACs, annuities when available
    
    if (ageVicki >= scenarios.df$vcSSclaimAge [i]) {
      portf <- portf + scenarios.df$vcSSBenefit [i] # pay VC SS benefit
      if (ageVicki == scenarios.df$vcSSclaimAge [i] & debug == 1)  print("Vicki begins SS payments ///") 
      if (debug == 1) print(paste("Add SS Benefit VC",scenarios.df$vcSSBenefit [i]," portf= ",portf,sep=" "))
    }
    if (ageDirk >= scenarios.df$dcSSclaimAge [i]) {
      portf <- portf + scenarios.df$dcSSBenefit [i] # pay DC SS benefit
      if (ageDirk == scenarios.df$dcSSclaimAge [i] & debug == 1)  print("Dirk begins SS payments ///")
      if (debug == 1) print(paste("Add SS Benefit DC",scenarios.df$dcSSBenefit [i]," portf= ",portf,sep=" "))
    }
    
    if (ageVicki >= ageQLAC) {
      portf <- portf + (scenarios.df$qLAC [i] * payoutQLAC) # payout QLAC 
      if (debug == 1) print(paste("Add QLAC payout= ",scenarios.df$qLAC [i] * payoutQLAC," portf= ",portf,sep=" "))
    }
    
    if (debug == 1 & ageVicki == ageQLAC & (scenarios.df$qLAC [i] * payoutQLAC) > 0) print("Vicki's QLAC payouts begin this year ////")
    
    portf <- portf + scenarios.df$percentAnnuity[i] * portfolio * payoutAnnuity # pay out Life Annuity
    
    if (debug == 1) print(paste("Add annuity payout= ",scenarios.df$percentAnnuity[i] * portfolio * payoutAnnuity," portf= ",portf,sep=" "))
    
# 
# Grow portfolio by market returns
# 
    portf <- portf * (1 + randomAnnualReturns [scenarioYr - earliestAge + 1])
    if (debug == 1) print(paste("Portfolio earns ",randomAnnualReturns [scenarioYr - earliestAge + 1]," to ",portf,sep=" "))
    
# End simulation at second death
    #
    # Record results in file
    #

    portValues[i,scenarioYr - earliestAge + 1] <- round(portf,0)
    if (debug == 1) print(paste("Storing portfolio value ",portf, " in row ",i," column ",scenarioYr - earliestAge + 1,sep=" "))
    }
  
  }
  
  write.csv(portValues,"~/desktop/Annual Portfolio Values.csv")
  write.csv(annualSpending,"~/desktop/Annual Spending.csv")
  
