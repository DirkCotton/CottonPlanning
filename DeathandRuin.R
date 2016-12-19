cat("\014")
chunkNumber <- 1000
chunkSize <- 1000
samplesForGraphs <- 1000
yearsReturns <- 65
seed1 <- 0114
cores <- 3


#Libraries
library(gdata)
library(survival)
library(cmprsk)
library(parallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(cmprsk)
library(reshape2)


#Files
men_risk_death <- tbl_df(read.xls("~dirkcotton/Desktop/Table02.xlsx", skip = 2)) %>% #CDC US 2011 Male life table
  select(Age..years., qx) %>%
  mutate(age = row_number() - 1) %>%
  filter(age >= 65)

women_risk_death <- tbl_df(read.xls("~dirkcotton/Desktop/Table03.xlsx", skip = 2)) %>% #CDC US 2011 Female life table
  select(Age..years., qx) %>%
  mutate(age = row_number() - 1) %>%
  filter(age >= 65)


#Make random male, female, and joint times of death packaged as a "data_frame"
make.frame <- function (nc, sseed, type) {
  set.seed(sseed)
  cohort <- data_frame(age = rep(65, nc), dead = rep(0, nc), id = 1:nc)
  mcohort <- transmute(cohort, male_age = age, male_dead = dead, id = id)
  fcohort <- transmute(cohort, female_age = age, female_dead = dead, id = id)
  
  for(ageyr in 65:99) {
    dead <- filter(mcohort, male_dead == 1)
    alive <- filter(mcohort, male_dead == 0)
    
    aliveNum <- length(alive$male_dead)
    riskDeath <- filter(men_risk_death, age == ageyr)$qx
    
    alive <- alive %>%
      mutate(male_age = male_age + 1,
             male_dead = rbinom(aliveNum, 1, riskDeath))
    
    mcohort <- bind_rows(dead, alive) %>%
      mutate(passseed = sseed)
  }
  
  for(ageyr in 65:99) {
    dead <- filter(fcohort, female_dead == 1)
    alive <- filter(fcohort, female_dead == 0)
    
    aliveNum <- length(alive$female_dead)
    riskDeath <- filter(women_risk_death, age == ageyr)$qx
    
    alive <- alive %>%
      mutate(female_age = female_age + 1,
             female_dead = rbinom(aliveNum, 1, riskDeath))
    
    fcohort <- bind_rows(dead, alive)
  }
  
  cohort <- arrange(inner_join(mcohort, fcohort, by = "id"), id) %>%
    mutate(female_tail = floor(rgamma(nc, shape = 1, scale = 2.3)),
           male_tail = floor(rgamma(nc, shape = 1, scale = 2.1)),
           male_dead = ifelse(male_age == 100, 1, male_dead),
           female_dead = ifelse(female_age == 100, 1, female_dead),
           male_age = ifelse(male_age == 100, male_age + male_tail, male_age),
           female_age = ifelse(female_age == 100, female_age + female_tail, female_age))
  
  if (sum(cohort$male_dead) != nc) warning("men alive")
  if (sum(cohort$female_dead) != nc) warning("women alive")
  
  if (type == "male") {
    cohort <- transmute(cohort, age_death = male_age, id = id, seed = sseed)
  }
  
  if (type == "female") {
    cohort <- transmute(cohort, age_death = female_age, id = id, seed = sseed)
  }
  
  if (type == "joint") {
    cohort <- transmute(cohort, age_death = pmax(male_age, female_age), id = id, seed = sseed)
  }
  
  return(cohort)
}


#Add Portfolio failure
add.pfailure <- function(cohort, meanreturns, sdreturns, initialEgg, annualWithdrawal, returns = F) {
  set.seed(cohort$passseed[1])
  nc <- length(cohort$age_death)
  matrixOfReturns <- matrix(rlnorm(nc * yearsReturns, log(meanreturns), sdreturns), ncol = yearsReturns)
  
  cohort <- mutate(cohort, balance = initialEgg, failure_year = 999)
  for(i in 1:yearsReturns) {
    cohort <- mutate(cohort, balance = balance - annualWithdrawal,
                     failure_year = ifelse(failure_year == 999 & balance < 0, i + 65, failure_year),
                     balance = balance*matrixOfReturns[id, i],
                     yearevent = pmin(age_death, failure_year),
                     event = ifelse(failure_year < age_death, 1, 0))
  }
  cohort <- select(cohort, -i, -balance, -seed)
  
  if(returns == T) {
    cohort <- bind_cols(cohort, mutate_each(data.frame(matrixOfReturns), funs(mone = . - 1)))
  }
  return(cohort)
}


#Death Sims
male <- data_frame(seed = seed1:(seed1 + chunkNumber - 1)) %>%
  group_by(seed) %>%
  do(cohort = make.frame(chunkSize, .$seed, "male"),
     type = rep("Single Man", chunkSize))
female <- data_frame(seed = seed1:(seed1 + chunkNumber - 1)) %>%
  group_by(seed) %>%
  do(cohort = make.frame(chunkSize, .$seed, "female"),
     type = rep("Single Woman", chunkSize))
both <- data_frame(seed = seed1:(seed1 + chunkNumber - 1)) %>%
  group_by(seed) %>%
  do(cohort = make.frame(chunkSize, .$seed, "joint"),
     type = rep("Joint Man/Woman", chunkSize))

three.categories <- bind_rows(male, female, both) %>% unnest()

three.fit <- survfit(Surv(age_death, rep(1, length(id))) ~ type, data = three.categories)

strat <- vector("integer", 0)
for(i in 1:3) strat <- append(strat, rep(names(three.fit$strata[i]), three.fit$strata[i]))
real.death.plot <- data_frame(time = three.fit$time, surv = three.fit$surv, stratum = strat) %>%
  mutate(stratum = substr(stratum, 6, 99))
rm(strat)

filter(real.death.plot, time == 95)

pmort <- ggplot(aes(x = time, y = surv), data = real.death.plot) +
  geom_step(aes(linetype = stratum)) +
  theme_classic(base_size = 12) +
  theme(text=element_text(family="Times")) +
  theme(legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_continuous("Years of Age", limits = c(65, 105)) +
  scale_y_continuous("Proportion of Retirees Living") +
  labs(linetype = "Household Type:")
print(pmort)
ggsave("mortality.png", dpi = 400, width = 4, height = 3)