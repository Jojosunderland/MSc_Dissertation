################################################################################
# Function to generate simulated reproductive data

# Aim is to create a simulated dataset with stage specific reproductive outputs
# per year for 10 years

# Do not need to track individuals

# Arguments:
# - ReproductiveRate = vector of stage specific reproductive rates
# - Stages = vector of names of the stages
# - Years = number of years of simulation
################################################################################

#### SET UP ####

### Load packages

library(tidyverse)

### Load any data

### Load any function scripts

#### FUNCTION CODE ####

SimulateReproductiveData <- function(ReproductiveRate = c(0,0.5,0.5), 
                                     Stages = c("juvenile",
                                                "subadult",
                                                "adult"),
                                     Years = 10,
                                     YearEffect = -0.05,
                                     Covariates = FALSE,
                                     YearRandomEffectSD = 0.05,
                                     NestIDRandomEffectSD = 0.05,
                                     beta1 = -0.05,
                                     beta2 = 0.05
                                     ){
  
  #### CHECKS! First do checks of the inputs 
  
  # 1. Reproductive rate vector should be same length as the stages one
  try(if(length(ReproductiveRate) != length(Stages)) 
    stop("ReproductiveRate length should equal number of Stages"))
  
  # 2. Reproductive rate vector should contain numbers
  try(if(class(ReproductiveRate) != "numeric") 
    stop("ReproductiveRate must be numeric"))

  
  #### SIMULATION
  
  # set up 100 stages per year at random
  # 10 years
  # set up Nest ID numbers with approx 10 with two records
  NestSampler <- 1:(100*Years)
  set.seed(10)
  markers <- sample(1:1000, Years)
  set.seed(10)
  NestSampler[markers] <- sample(1:Years, Years)
  set.seed(10)
  # covariates are set with one value per year
  Population <- data.frame(stages = sample(Stages, 100*Years, replace = TRUE),
                           year = rep(1:Years, each = 100),
                           nestID = NestSampler,
                           covariate1 = rep(rnorm(Years, mean = 50, sd = 3),
                                            each = 100),
                           covariate2 = rep(rnorm(Years, mean = Years, sd = 1),
                                            each = 100))
  
  # split into list for use in map
  PopulationSplit <- split(Population, seq(nrow(Population))) 
  
  # set up the year and nest ID random effects - same length as number of years
  set.seed(10)
  YearRandomEffects <- rnorm(length(unique(Population$year)), 
                             mean = 0, 
                             sd = YearRandomEffectSD)
  names(YearRandomEffects) <- unique(Population$year)
  set.seed(10)
  NestIDRandomEffects <- rnorm(length(unique(Population$nestID)), 
                              mean = 0, 
                              sd = NestIDRandomEffectSD)
  names(NestIDRandomEffects) <- unique(Population$nestID)

  
  # use map to fill in each row of chicks based on ReproductiveRate vector
  # save into population output vector
  PopulationOutput <- map_df(.x = PopulationSplit, ~{
    # First edit reproductive rate with YearEffect
    if(!is.null(YearEffect)){if(isTRUE(Covariates)){
      NewReproductiveRate <- exp(log(ReproductiveRate[which(Stages == 
                                                              .x$stage)])+
      (YearEffect*.x$year) + (beta1 * .x$covariate1) + (beta2 * .x$covariate2) +
        YearRandomEffects[which(names(YearRandomEffects) == .x$year)] +
        NestIDRandomEffects[which(names(NestIDRandomEffects) == .x$nestID)])}else{
      NewReproductiveRate <- exp(log(ReproductiveRate[which(Stages == 
                                    .x$stage)]) +
         (YearEffect*.x$year) + 
         YearRandomEffects[which(names(YearRandomEffects) == .x$year)] +
         NestIDRandomEffects[which(names(NestIDRandomEffects) == .x$nestID)])  
        }}else{if(isTRUE(Covariates)){
      NewReproductiveRate <- exp(log(ReproductiveRate[which(Stages == .x$stage)]) + 
                                   (beta1 * .x$covariate1) + (beta2 * .x$covariate2) + 
          YearRandomEffects[which(names(YearRandomEffects) == .x$year)] +
        NestIDRandomEffects[which(names(NestIDRandomEffects) == .x$nestID)])}else{
      NewReproductiveRate <- exp(log(ReproductiveRate[which(Stages == .x$stage)]) +
          YearRandomEffects[which(names(YearRandomEffects) == .x$year)] +
          NestIDRandomEffects[which(names(NestIDRandomEffects) == .x$nestID)])}    
      
    }
    .x$chicks <- rpois(1, NewReproductiveRate)
    return(.x)
  })
  #plot(PopulationOutput$chicks ~ PopulationOutput$year) 
  
  return(PopulationOutput)
}
