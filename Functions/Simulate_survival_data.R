################################################################################
# Function to generate simulated survival data

# Aim is to create a simulated dataset with stage specific reproductive outputs
# per year for 10 years

# Do need to track individuals here

# Arguments:
# - SurvivalRate = vector of stage specific reproductive rates
# - Stages = vector of names of the stages
# - Years = number of years of simulation
# - MaxAge = maximum age this species can live to
# - Starting population dataframe includes, ID, stage, year, covariate, survival,
# and MUST include age
################################################################################

#### SET UP ####

### Load packages

library(tidyverse)

### Load any data

### Load any function scripts

#### FUNCTION CODE ####

SimulateSurvivalData <- function(SurvivalRate = c(0.7,0.8,0.8), 
                                 Stages = c("juvenile",
                                            "subadult",
                                            "adult"),
                                 Years = 10,
                                 MaxAge = 20,
                                 StartingPopulation,
                                 YearEffect = -0.05,
                                 Covariates = FALSE,
                                 YearRandomEffectSD = 0.05,
                                 beta1 = -0.05,
                                 beta2 = 0.05
){
  
  #### CHECKS! First do checks of the inputs 
  
  # 1. Survival rate vector should be same length as the stages one
  try(if(length(SurvivalRate) != length(Stages)) 
    stop("SurvivalRate length should equal number of Stages"))
  
  # 2. Survival rate vector should contain numbers
  try(if(class(SurvivalRate) != "numeric") 
    stop("SurvivalRate must be numeric"))
  
  # 3. Survival rates should not be above 1 or below 0 TODO!
  try(if(class(SurvivalRate) != "numeric") 
    stop("SurvivalRate must be numeric"))
  
  #### SIMULATION
  
  # set up random effects outside loop
  set.seed(10)
  YearRandomEffects <- rnorm(Years, 
                             mean = 0, 
                             sd = YearRandomEffectSD)
  
  # set up covariates outside of loop too
  set.seed(10)
  covariate1 <- rnorm(Years,
                      mean = 10, 
                      sd = 3)
  set.seed(10)
  covariate2 <- rnorm(Years,
                      mean = 1:Years,
                      sd = 1)
  
  # use a loop to generate a new population each year
  # need to include survival of the population AND introduction of new individuals
  for(i in 2:Years){

  # now ready to make the next year of data
  # remember that we assume we have perfect detection
    
  # step 1: age everyone as survival is were you seen this year?
  NextYear <- StartingPopulation %>%
      filter(year == i-1 & survival == 1) %>% # remember to restrict to just the previous year 
    mutate(age = age + 1) %>%
    mutate(stage = case_when(age == 1 ~ "juvenile",
                             age >1 & age < 4 ~ "subadult",
                             age > 3 ~ "adult"), 
           covariate1 = covariate1[i],
           covariate2 = covariate2[i],
           year = i)
  # covariate doesn't seem to be updating so do manually to be sure
  NextYear$covariate1 <- rep(covariate1[i], length(NextYear$covariate1))
  NextYear$covariate2 <- rep(covariate2[i], length(NextYear$covariate2))
  #print(covariate1[i])
  
  # step 2: apply survival and change year
  # first change survival based on year effect
  if(is.null(YearEffect)){ if(isTRUE(Covariates)){
    
    logit_survival <- log(SurvivalRate/(1-SurvivalRate)) + 
      (beta1 * covariate1[i]) +
      (beta2 * covariate2[i]) + YearRandomEffects[i] # doing this on log odds scale
    
    YearSurvival <- exp(logit_survival)/
      (1 + exp(logit_survival)) # back transform to probability
  }else{
    
    logit_survival <- log(SurvivalRate/(1-SurvivalRate)) + 
      YearRandomEffects[i]  # doing this on log odds scale
    
    YearSurvival <- exp(logit_survival)/
      (1 + exp(logit_survival)) # back transform to probability
  }
  
  }else{ if(isTRUE(Covariates)){
    logit_survival <- log(SurvivalRate/(1-SurvivalRate)) + 
      (YearEffect*i) + (beta1 * covariate1[i]) +
      (beta2 * covariate2[i]) + YearRandomEffects[i] # doing this on log odds scale
    
    YearSurvival <- exp(logit_survival)/
      (1 + exp(logit_survival)) # back transform to probability
  }else{
    logit_survival <- log(SurvivalRate/(1-SurvivalRate)) + 
      (YearEffect*i) + YearRandomEffects[i]  # doing this on log odds scale
    
    YearSurvival <- exp(logit_survival)/
      (1 + exp(logit_survival)) # back transform to probability 
    
  }
  }
    NextYear <- NextYear %>%
    mutate(survival = ifelse(test = stage == "juvenile",
                             yes = rbinom(n(),1,
                                 YearSurvival[which(Stages == "juvenile")]),
                             no = ifelse(
                               test = stage == "subadult",
                               yes =  rbinom(n(),1,
                                  YearSurvival[which(Stages == "subadult")]),
                               no = rbinom(n(),1,
                                 YearSurvival[which(Stages == "adult")]))),
           year = i) %>%
    mutate(survival = case_when(age > 20 ~ 0,
                                TRUE ~ survival))# need to set all adults over max age to 0 survival
  # choosing to leave in those who do not survive so we can
  # check survival rates
  
  # step 3: add some new newborns - same as adults who survive, these newborns will survive
  # need new IDs first remove those already used
  possible_IDs <- 1:100000
  possible_IDs <- possible_IDs[- which(possible_IDs %in% StartingPopulation$ID)]
  
  # try setting up lengths outside to avoid errors
  NumberNewborns <- length(which(NextYear$stage == "adult" & NextYear$survival == 1))
  print(NumberNewborns)
  Newborns <- data.frame(ID = sample(x = possible_IDs, 
                                      size = NumberNewborns, 
                                      replace = FALSE),
                          stage = "newborn",
                          year = i,
                          covariate1 = 
                            covariate1[i],
                         covariate2 = covariate2[i],
                          age = 0,
                          survival = 1)
  #print(covariate1[i])
  
  # step 4: combine all elements into the StartingPopulation
  StartingPopulation <- bind_rows(StartingPopulation, NextYear, Newborns)
  }

  return(filter(StartingPopulation, year > 1)) # return only from second year
}
