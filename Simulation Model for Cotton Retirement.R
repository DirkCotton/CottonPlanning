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
debug <- 1

# 
# Input scenarios from CSV file

scenarios.df <- read.csv("~/desktop/CottonScenarios.csv")

# 
# Find number of scenarios

scenarios <- length(scenarios.df$scenario)

scenarios <- 1  ################# DEBUG = 1

for (i in 1:scenarios) {
  
# calculate max number of years in this scenario
  
  lastAge <- max(scenarios.df$femaleDeathAge [i],scenarios.df$maleDeathAge [i])
  portf <- portfolio - scenarios.df$qLAC [i] - (scenarios.df$percentAnnuity [i] * portfolio)
  
  for (scenarioYr in earliestAge:lastAge) {
    
    if (debug == 1) {
      print(paste("Scenario Year ",scenarioYr,"************** ",sep=" "))
      print(paste("Subtract QLAC =",scenarios.df$qLAC [i],"and annuity =",scenarios.df$percentAnnuity [i] * portfolio," portf= ",portf,sep=" "))
}
      # 
# If only one spouse reduce expenses by 20%
# 
    annualSpend <- min(scenarios.df$annualSpendPercent [i] * portf, portf) # spend from portfolio
    
    if (debug == 1) {
      print(paste("portf = ",portf,sep=" "))
      if (scenarioYr == scenarios.df$maleDeathAge [i]) print("Male dies this year ///")
      if (scenarioYr == scenarios.df$femaleDeathAge [i]) print("Female dies this year ///")
    }
  if (scenarioYr >= min(scenarios.df$maleDeathAge [i],scenarios.df$maleDeathAge [i]))  {
    annualSpend <- .8 * annualSpend
  }
    if (scenarioYr > max(scenarios.df$maleDeathAge [i],scenarios.df$maleDeathAge [i])) break # scenario ends when second spouse dies
    
# Spend from portfolio, 
    
    portf <- portf - annualSpend
    if (debug == 1) print(paste("Annual spend ",annualSpend," portf= ",portf,sep=" "))
    
# Add Social Security benefits, QLACs, annuities when available
    
    if (scenarioYr > scenarios.df$vcSSclaimAge [i]) portf <- portf + scenarios.df$vcSSBenefit [i] # pay VC SS benefit
    if (scenarioYr > scenarios.df$dcSSclaimAge [i]) portf <- portf + scenarios.df$dcSSBenefit [i] # pay DC SS benefit
    
    if (debug == 1) print(paste("Add SS Benefit VC",scenarios.df$vcSSBenefit [i]," portf= ",portf,sep=" "))
    if (debug == 1) print(paste("Add SS Benefit DC",scenarios.df$dcSSBenefit [i]," portf= ",portf,sep=" "))
    
    if (scenarioYr > ageQLAC) portf <- portf + (scenarios.df$qLAC [i] * payoutQLAC) # payout QLAC 
    if (debug == 1) print(paste("Add QLAC payout= ",scenarios.df$qLAC [i] * payoutQLAC," portf= ",portf,sep=" "))
    
    
    portf <- portf + scenarios.df$percentAnnuity[i] * portfolio * payoutAnnuity # pay out Life Annuity
    
    if (debug == 1) print(paste("Add annuity payout= ",scenarios.df$percentAnnuity[i] * portfolio * payoutAnnuity," portf= ",portf,sep=" "))
    
# 
# Grow portfolio by market returns
# 
    portf <- portf * (1 + scenarios.df$annualReturn [i])
    if (debug == 1) print(paste("Portfolio earns ",scenarios.df$annualReturn [i]," to ",portf,sep=" "))
    
# End simulation at second death

}

}
