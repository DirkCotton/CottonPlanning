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


scenarios <- 3  ################# DEBUG = 1

for (i in 1:scenarios) {
  
  if(i == 3) debug <-1 else debug <- 0 # set i= a negative number or a single specific scenario number to print output for
  
  if (debug == 1) {
    print(" ")
    print(paste("================== Scenario ",i,sep=" "))
    print(" ")
  }
  
# calculate max number of years in this scenario
  
  lastAge <- max(scenarios.df$femaleDeathAge [i],scenarios.df$maleDeathAge [i])
  
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
  
  randomAnnualReturns <- rnorm(40,mean=scenarios.df$annualReturn [i],sd=scenarios.df$sigma50yr [i])
  if (debug == 1) {
    print(paste("expected portfolio mu= ",scenarios.df$annualReturn [i]," expected sd = ",scenarios.df$sigma50yr [i],sep=" "))
    print(paste("generated portfolio mu= ",mean(randomAnnualReturns)," generated sd = ",sd(randomAnnualReturns),sep=" "))
  }
  
  annuityPayout <- scenarios.df$percentAnnuity[i] * (portfolio - scenarios.df$qLAC [i])  * payoutAnnuity
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
    
    # 
    # If only one spouse reduce expenses by 20%, modify SS benefits to survivor benefits
    # 
    
    if (debug == 1) {
      print(paste("portf = ",portf,sep=" "))
      if (ageDirk == scenarios.df$maleDeathAge [i]) {
        ageDirk <- 0
        print("Male dies this year ///")
        
        # At Dirk's death, Vicki's SS benfit = Dirk's. Dirk's goes away
        
        vcSSben <- dcSSben
        dcSSben <- 0
        
      }
      if (ageVicki == scenarios.df$femaleDeathAge [i]) {
        ageVicki <- 0
        vcSSben <- 0
        print("Female dies this year ///")
      }
    }
    
    if (ageDirk >= scenarios.df$maleDeathAge [i] | ageVicki >= scenarios.df$femaleDeathAge [i]) 
      annualSpend <- .8 * annualSpend
    if (ageVicki >= scenarios.df$vcSSclaimAge [i]) {
      spend <- spend + vcSSben # pay VC SS benefit
      if (ageVicki == scenarios.df$vcSSclaimAge [i] & debug == 1)  print("Vicki begins SS payments ///") 
      if (debug == 1) print(paste("Add SS Benefit VC",vcSSben," spend = ",spend,sep=" "))
    }
    if (ageDirk >= scenarios.df$dcSSclaimAge [i]) {
      spend <- spend + dcSSben # pay DC SS benefit
      if (ageDirk == scenarios.df$dcSSclaimAge [i] & debug == 1)  print("Dirk begins SS payments ///")
      if (debug == 1) print(paste("Add SS Benefit DC",dcSSben," spend= ",spend,sep=" "))
    }
    
    if (ageVicki >= ageQLAC) {
      spend <- spend + (scenarios.df$qLAC [i] * payoutQLAC) # payout QLAC 
      if (debug == 1) print(paste("Add QLAC payout= ",scenarios.df$qLAC [i] * payoutQLAC," spend= ",spend,sep=" "))
    }
    
    if (debug == 1 & ageVicki == ageQLAC & (scenarios.df$qLAC [i] * payoutQLAC) > 0) print("Vicki's QLAC payouts begin this year ////")
    
    spend <- spend + annuityPayout # pay out Life Annuity
    
    if (debug == 1) print(paste("Add annuity payout= ",annuityPayout," spend= ",spend,sep=" "))
  
# Spend from portfolio, 
    
    portfolioSpend <- min(scenarios.df$annualSpendPercent [i] * portf, portf) # spend from portfolio
    annualSpending [i,scenarioYr - earliestAge + 1] <- round(spend + portfolioSpend,0)
    portf <- portf - portfolioSpend
     
    if (debug == 1) print(paste("Portfolio withdrawal= ",portfolioSpend," portf = ",portf,sep=" "))
    if (debug == 1) print(paste("Total spend this year= ",round(spend + portfolioSpend,0)," portf = ",portf,sep=" "))

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
  
if (debug == 1) print(" ")
if (debug == 1) print(scenarios.df[3,])
