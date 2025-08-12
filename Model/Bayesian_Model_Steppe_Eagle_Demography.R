library(tidyverse)
library(nimble)
library(MCMCvis)
library(popbio)
library(readxl)

#----------------------------- REAL DATA ----------------------------------
##### Load Data #####
## REPRODUCTION ##

Reproductiondata <- read_csv("Files/Clean_Files/Reproduction_clean_files/SteppeEagle_clean_nest.csv")

## SURVIVAL ##
Survivaldata <- read_csv("Files/Clean_Files/Survival_BirdYear_clean_files/Final_data/FINAL_ALL_SteppeEagle_known_fate.csv")
# Long format known date data

Survivaldatawide <- read_csv("Files/Clean_Files/Survival_BirdYear_clean_files/Final_data/FINAL_ALL_SteppeEagle_survival_combined.csv")
# Wide format data with first and last seen


##### Data Manipulation #####

## REPRODUCTION ##

# filter to 2013 onwards 

Reproductiondata <- Reproductiondata %>% 
  filter(year >= '2013')

# index nest ID and year
Reproductiondata$nest_id_index <- as.integer(factor(Reproductiondata$nest_id))

#Reproductiondata$year_i <- as.integer(factor(Reproductiondata$year))
all_years <- c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025)
Reproductiondata <- Reproductiondata %>% 
  mutate(year_i = match(year, all_years))


View(Reproductiondata)


## SURVIVAL ##

# filter survival

# screen after 2018
# Identify individuals first seen in 2018 or later
valid_ids <- Survivaldata %>%
  group_by(id) %>%
  summarise(first_seen_year = min(year), .groups = "drop") %>%
  filter(first_seen_year >= 2018) %>%
  pull(id)

# Filter both datasets to keep only those IDs
Survivaldata <- Survivaldata %>%
  filter(id %in% valid_ids)

Survivaldatawide <- Survivaldatawide %>%
  filter(id %in% valid_ids)



# Index years #

Survivaldata$year_i <- as.integer(factor(Survivaldata$year))

allyears <- 2018:2025 # make sure first and last seen years are indexed the same
Survivaldatawide <- Survivaldatawide %>%
  mutate(
    first_seen_i = match(first_seen, allyears),
    last_seen_i = match(last_seen, allyears)
  )

# Create first and last seen (for capture history)

first_last <- Survivaldata %>% 
  group_by(id) %>% 
  summarise(first_seen = min(year_i), 
            last_seen = max(year_i),
            .groups = "drop")

# Combine with Survivaldata

Survivaldata <- left_join(first_last, Survivaldata %>% 
                            select(id, stage_obs, age_obs, year, year_i, survived), 
                          by = "id")

# Create stage columns
Survivaldata$stage_subA <- ifelse(Survivaldata$stage_obs == "subadult", 1, 0)
Survivaldata$stage_A <- ifelse(Survivaldata$stage_obs == "adult", 1, 0)
Survivaldata$stage_J <- ifelse(Survivaldata$stage_obs == "juvenile", 1, 0)

# Create separate subadult stages
Survivaldata <- Survivaldata %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(next_stage = lead(stage_obs)) %>% 
  ungroup()

# TWO subadult categories
Survivaldata <- Survivaldata %>%
  mutate(next_stage = ifelse(is.na(next_stage), stage_obs, next_stage))

Survivaldata$stage_SA_stay <- ifelse(
  (Survivaldata$stage_obs == "subadult" & Survivaldata$next_stage == "subadult"),
  1, 0)

Survivaldata$stage_SA_trans <- ifelse(Survivaldata$stage_obs == "subadult" &
                                        Survivaldata$next_stage == "adult", 1, 0)

# Define consistent ID and year order:

Survivaldata <- Survivaldata %>% 
  arrange(id)

## Create stage survival matrices ##

# JUVENILE
juvenile <- pivot_wider(Survivaldata, id_cols = c(year), 
                        
                        values_from = c(stage_J),
                        
                        names_from = c(id)) %>%
  arrange(year)

juvenile = juvenile[,-1] # to remove first column and index correctly

# convert tibble to matrix 
juvenile_mat <- as.matrix(juvenile)

# SUBADULT
subadult <- pivot_wider(Survivaldata, id_cols = c(year), 
                        
                        values_from = c(stage_subA),
                        
                        names_from = c(id)) %>%
  arrange(year)

subadult = subadult[,-1] # to remove first column and index correctly

# convert tibble to matrix 
subadult_mat <- as.matrix(subadult)

# ADULT
adult <- pivot_wider(Survivaldata, id_cols = c(year), 
                     
                     values_from = c(stage_A),
                     
                     names_from = c(id)) %>%
  arrange(year)

adult = adult[,-1] # to remove first column and index correctly

# convert tibble to matrix 
adult_mat <- as.matrix(adult)

# SUBADULT STAY
subadult_stay <- pivot_wider(Survivaldata, id_cols = c(year), 
                             
                             values_from = c(stage_SA_stay),
                             
                             names_from = c(id)) %>%
  arrange(year)

subadult_stay = subadult_stay[,-1] # to remove first column and index correctly

# convert tibble to matrix 
subadult_stay_mat <- as.matrix(subadult_stay)

# SUBADULT TRANSITION
subadult_trans <- pivot_wider(Survivaldata, id_cols = c(year), 
                              
                              values_from = c(stage_SA_trans),
                              
                              names_from = c(id)) %>%
  arrange(year)

subadult_trans = subadult_trans[,-1] # to remove first column and index correctly

# convert tibble to matrix 
subadult_trans_mat <- as.matrix(subadult_trans)

# transpose stage matrices
juvenile_mat <- t(juvenile_mat)
subadult_mat <- t(subadult_mat)
adult_mat <- t(adult_mat)
subadult_stay_mat <- t(subadult_stay_mat)
subadult_trans_mat <- t(subadult_trans_mat)


# Create a capture history

cap.dates <- sort(unique(c(Survivaldata$first_seen, Survivaldata$last_seen)))
cap.dates <- seq(cap.dates[1],cap.dates[length(cap.dates)], by = 1) 
occs <- length(cap.dates)

first <- last <- array(NA, dim = nrow(Survivaldata))

# CAPTURE HISTORY - based off long format survival data:

surv_caps <- matrix(data = NA, nrow = length(unique(Survivaldata$id)), ncol = occs) #cap.history 
for(i in 1:length(unique(Survivaldata$id))){ #for each individual
  # to make it per individual could reduce dataset here
  Tempdata <- Survivaldata[Survivaldata$id == unique(Survivaldata$id)[i],]
  # this now takes ALL the rows for this individual then use the latest one for
  # everything that follows
  last_row <- Tempdata[nrow(Tempdata), ]  # final row for this individual
  # pulls the final observation
  first[i] <- which(cap.dates == last_row$first_seen)  # correct
  last[i]  <- which(cap.dates == last_row$last_seen)   # correct
  
  surv_caps[i, first[i]:last[i]] <- 1
  if (last_row$survived == 0) {
    surv_caps[i, last[i]] <- 0
  }
}

View(surv_caps)

## Subadult transition rate ##

n_trans <- sum(Survivaldata$stage_SA_trans)
n_stay  <- sum(Survivaldata$stage_SA_stay)

transition_successes = n_trans
transition_trials = n_trans + n_stay

## Vector for shared years

shared_years <- 2018:2025
shared_years_i <- as.integer(factor(shared_years))
n_years_shared = length(shared_years_i)

# Make sure Survivaldatawide is also in the same id order (for first and last seen)
Survivaldatawide <- Survivaldatawide %>% 
  arrange(id)


## COVARIATES ##

human <- read_excel("Files/Clean_Files/Electricity_data.xlsx", sheet = "Human_Impact_Proxy")
temperature <- read_csv("Files/Clean_Files/temperature_clean.csv")

# Rename human data

human <- human %>% 
  rename("energy" = "energy_production_Mil_kW")

human <- human %>% 
  dplyr::select("year", "energy")

# Scale covariate values 

human$energy_scaled <- scale(human$energy)
temperature$temp_scaled <- scale(temperature$mean_temp_C)

human <- human %>% 
  filter(year >= '2013' & '2025' >= year)

# Add to the reproduction dataset

# filter it to remove 2025 to match reproduction data
human_filtered <- human %>% 
  filter(year >= '2013' & '2024' >= year)

Reproductiondata <- Reproductiondata %>%
  left_join(human_filtered, by = "year")

# same for temperature
temp_filtered <- temperature %>% 
  filter(year >= '2013' & '2024' >= year)

Reproductiondata <- Reproductiondata %>%
  left_join(temp_filtered, by = "year")


# filter to 2018-2025 for surv
human_surv <- human %>% 
  filter(year >= '2018' & '2025' >= year)

temperature_surv <- temperature %>% 
  filter(year >= '2018' & '2025' >= year)


# index years
human$year_i <- as.integer(factor(human$year))
temperature$year_i <- as.integer(factor(temperature$year))

# index years
human_surv$year_i <- as.integer(factor(human_surv$year))
temperature_surv$year_i <- as.integer(factor(temperature_surv$year))


## View Data ##
# Check they match structure to simulated data
View(Reproductiondata)
View(Survivaldata)
View(Survivaldatawide)
View(surv_caps)
View(juvenile_mat)
View(subadult_mat)
View(subadult_stay_mat)
View(subadult_trans_mat)
View(adult_mat)


##### MODEL WITH REAL DATA AND MPM #####

## Model ##

Eagles_model <- nimbleCode({
  
  #----------------------------------------------------------------------------
  # DEFINE PRIORS SURVIVAL
  # outside of loop
  alpha_surv ~ dbeta(1,1) # vague prior - intercept for nestling survival
  beta_year_surv ~ dnorm(0,1.5) # fixed year effect
  
  # STAGE PRIORS
  beta_juvenile ~ dnorm(0,1.5) 
  #beta_subadult ~ dnorm(0,1.5)
  beta_subadult_stay ~ dnorm(0, 1.5)
  beta_subadult_trans ~ dnorm(0, 1.5)
  beta_adult ~ dnorm(0,1.5) 
  
  # SUBADULT TRANSITION RATE
  suba_transition ~ dbeta(1,1)
  
  # COVARIATES
  beta_human_surv ~ dnorm(0,1.5) 
  beta_temp_surv ~ dnorm(0,1.5) 
  
  #----------------------------------------------------------------------------
  # DEFINE PRIORS REPRODUCTION
  
  alpha_rep ~ dgamma(1, 1) # baseline fecundity rate 
  beta_year_rep ~ dnorm(0,1.5) # Fixed year effect
  
  # BREEDING PROPORTIONS
  breeding_suba ~ dunif(0.3, 0.5) 
  breeding_adult ~ dunif(0.5, 0.9)
  
  # COVARIATES
  beta_human_rep ~ dnorm(0,1.5) 
  beta_temp_rep ~ dnorm(0,1.5)
  
  #----------------------------------------------------------------------------
  # RANDOM EFFECTS 
  sigma_year_surv ~ dunif(0,0.5) # Year standard deviation survival
  sigma_year_rep ~ dunif(0,0.5) # Year standard deviation reproduction
  sigma_nest ~ dunif(0,0.5) # Nest ID standard deviation
  
  # Survival
  for(t in 1:n_total_years){
    beta_yearRE_surv[t] ~ dnorm(0, sd = sigma_year_surv)
  }
  
  # Reproduction
  for(y in 1:n_total_years){
    beta_yearRE_rep[y] ~ dnorm(0, sd = sigma_year_rep)
  }
  
  for(j in 1:n_nest){
    beta_nestRE[j] ~ dnorm(0, sd = sigma_nest)
  }
  
  #----------------------------------------------------------------------------
  ## SURVIVAL
  
  alpha_surv_logit <- logit(alpha_surv)
  
  for(i in 1:n) {
    
    #survival_prob[i, first[i]] <- 1  # Explicitly set initial condition
    
    for(t in (first[i]+1):last[i]){
      # logit link function
      logit(p[i,t]) <- alpha_surv_logit + (beta_year_surv * year_surv[t]) +
        (beta_juvenile * juvenile[i,t]) + (beta_adult * adult[i,t]) + #(beta_subadult * subadult[i,t]) +
        (beta_yearRE_surv[year_surv[t]]) + (beta_subadult_stay * subadult_stay[i,t]) + 
        (beta_subadult_trans * subadult_trans[i,t]) +
        (beta_human_surv * human_surv[t]) + (beta_temp_surv * temp_surv[t]) 
      
      mu[i,t] <- p[i,t]*survival_prob[i, t-1]
      
      # SURVIVAL LIKELIHOOD
      survival_prob[i,t] ~ dbern(mu[i,t])
    }
    
  }
  
  # Subadult transition likelihood
  n_trans ~ dbinom(suba_transition, n_trials)
  
  # Back transform sub adult and adult survival probabilities - outside of loop (only one value printed for each)
  survival_nestling <- 1 / (1 + exp(-(alpha_surv_logit))) 
  survival_juvenile <- 1 / (1 + exp(-(alpha_surv_logit +beta_juvenile)))
  #survival_subadult <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_subadult)))
  survival_subadult_stay <- 1 / (1 + exp(-(alpha_surv_logit + beta_subadult_stay)))
  survival_subadult_trans <- 1 / (1 + exp(-(alpha_surv_logit + beta_subadult_trans)))
  survival_adult <- 1 / (1 + exp(-(alpha_surv_logit + beta_adult)))
  
  
  #----------------------------------------------------------------------------
  ## REPRODUCTION
  
  for(e in 1:N) {
    
    # LIKELIHOOD REPRODUCTION
    chicks[e] ~ dpois(reproduction_rate[e]) 
    # log link function
    log(reproduction_rate[e]) <- log(alpha_rep) + (beta_year_rep * year_rep[e]) + 
      (beta_human_rep * human_rep[e]) + (beta_temp_rep * temp_rep[e]) + 
      (beta_yearRE_rep[year_rep[e]]) + (beta_nestRE[nest[e]]) 
    
  }
  
  
  #----------------------------------------------------------------------------
  ###### MATRIX POPULATION MODEL#####
  
  # Yearly survival rate for each stage
  for(i in 1:n_years_surv){
    
    nest_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit + (beta_year_surv*year_surv[i]) +
                                     beta_temp_surv*temp_surv[i] +
                                     beta_human_surv*human_surv[i] + 
                                     beta_yearRE_surv[i])))
    
    juv_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit + beta_juvenile +
                                    beta_temp_surv*temp_surv[i] +
                                    beta_human_surv*human_surv[i] + 
                                    (beta_year_surv*year_surv[i]) + 
                                    beta_yearRE_surv[i]))) 
    
    #suba_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit) + beta_subadult +
    #                              #beta_temp_surv*temp_surv[i] +
    #                             #beta_human_surv*human_surv[i] + 
    #                            (beta_year_surv*year_surv[i]) + 
    #                           beta_yearRE_surv[i]))
    
    suba_stay_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit + beta_subadult_stay +
                                          beta_temp_surv*temp_surv[i] +
                                          beta_human_surv*human_surv[i] + 
                                          (beta_year_surv*year_surv[i]) + 
                                          beta_yearRE_surv[i]))) 
    
    suba_trans_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit + beta_subadult_trans +
                                           beta_temp_surv*temp_surv[i] +
                                           beta_human_surv*human_surv[i] + 
                                           (beta_year_surv*year_surv[i]) + 
                                           beta_yearRE_surv[i])))
    
    adult_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit + beta_adult +
                                      beta_temp_surv*temp_surv[i] +
                                      beta_human_surv*human_surv[i] + 
                                      (beta_year_surv*year_surv[i]) + 
                                      beta_yearRE_surv[i])))
  }
  
  # Yearly reproduction rate
  # Calculated for ALL years that will go into the matrix
  # Even if 2019, 2021 and 2025 are missing in reproduction data
  for(i in 1:n_total_years){
    
    reproductionRate[i] <- exp(log(alpha_rep) + (beta_year_rep*i) + 
                                 (beta_yearRE_rep[year_rep[i]]) + 
                                 (beta_temp_rep*temp_years[i]) + # annual temp data with all indexed years
                                 (beta_human_rep*human_years[i])) # using the annual human data with indexed years 2013-2025
  }
  
  # breeding probability
  subadult_BP <- breeding_suba 
  adult_BP <- breeding_adult
  
  # subadult transition rate
  #suba_T <- suba_transition
  
  ## Set up Matrix transitions [row, col, year] = [t+1 (to), t (from), year]
  
  for(y in 1:n_years_shared){ #2018-2025
    
    matrix_model[1,1,y] <- 0 # N-N 
    matrix_model[1,2,y] <- 0 # J-N 
    # Sub adult to nestling
    matrix_model[1,3,y] <- reproductionRate[y+5] * (subadult_BP) * 0.5 * (suba_stay_surv[y]) #(suba_surv[y] * (1-suba_T))
    # Adult to nestling 
    matrix_model[1,4,y] <- (reproductionRate[y+5] * (adult_BP) * 0.5 *(adult_surv[y])) + 
      (reproductionRate[y+5] * (adult_BP) * 0.5 * (suba_trans_surv[y])) #(suba_surv[y] * suba_T))
    matrix_model[2,1,y] <- nest_surv[y] # Nestling to juvenile 
    matrix_model[2,2,y] <- 0 # J-J 
    matrix_model[2,3,y] <- 0 # SA-J 
    matrix_model[2,4,y] <- 0 # A-J 
    matrix_model[3,1,y] <- 0 # N-SA
    matrix_model[3,2,y] <- juv_surv[y] # Juvenile to sub adult
    matrix_model[3,3,y] <- suba_stay_surv[y] #suba_surv[y] * (1-suba_T)  # Sub adult persistence survival 
    matrix_model[3,4,y] <- 0 # A-SA
    matrix_model[4,1,y] <- 0 # N-A
    matrix_model[4,2,y] <- 0 # J-A
    matrix_model[4,3,y] <- suba_trans_surv[y] #suba_surv[y] * (suba_T)# Sub adult to adult (transition survival)
    matrix_model[4,4,y] <- adult_surv[y] # Adult survival
    
  }
  
  # DERIVED QUANTITIES
  
  #Population growth rate
  
  for(y in 1:n_years_shared){
    lambda[y] <- nimEigen(matrix_model[1:4, 1:4, y])$values[1]
  }
  
  
})

# number of observations
n <- length(unique(Survivaldata$id))# Survival data 
N <- nrow(Reproductiondata) # Reproduction data

my.data <- list(
  chicks = Reproductiondata$alive_chicks,
  survival_prob = surv_caps,
  human_surv = as.numeric(human_surv$energy_scaled), 
  temp_surv = as.numeric(temperature_surv$temp_scaled), 
  human_rep = as.numeric(Reproductiondata$energy_scaled), 
  temp_rep = as.numeric(Reproductiondata$temp_scaled), 
  temp_years = as.numeric(temperature$temp_scaled),
  human_years = as.numeric(human$energy_scaled),
  n_trans = n_trans # subadult transition 
)

# constants
my.constants <- list(
  n = n, # number of data points (survival)
  N = N, # Number of data points (reproduction)
  year_surv = Survivaldata$year_i, # Survival data years
  year_rep = Reproductiondata$year_i, # Reproduction data years 
  n_years_surv = ncol(surv_caps),#length(unique(Survivaldata$year)), # Number of years for survival year RE
  n_nest = length(unique(Reproductiondata$nest_id_index)), # Number of nests for nest RE
  nest = Reproductiondata$nest_id_index, # Nest IDs indexed
  juvenile = juvenile_mat, # juvenile matrix
  #subadult = subadult_mat, 
  subadult_stay = subadult_stay_mat, # subadult matrix - stay
  subadult_trans = subadult_trans_mat, # subadult matrix - mature
  adult = adult_mat, # adult matrix
  first = Survivaldatawide$first_seen_i, # year each individual was first seen
  last = Survivaldatawide$last_seen_i, # year each individual was last seen
  n_trials = n_trans + n_stay,
  n_years_shared = n_years_shared, #2018-2025
  n_total_years = length(1:13)
)

# parameters

parameters.to.save <- c( "alpha_surv", "beta_year_surv","alpha_rep", 
                         "beta_year_rep", "sigma_year_surv", "sigma_nest",
                         "sigma_year_rep","beta_juvenile", #"beta_subadult",
                         "beta_adult", "beta_human_surv", "beta_human_rep",
                         "breeding_suba", "breeding_adult", "beta_temp_surv", "beta_temp_rep",
                         "beta_subadult_trans", "beta_subadult_stay", "suba_transition",
                         
                         #Matrix derived quantities
                         
                         "matrix_model",  "lambda",
                         "nest_surv", "juv_surv", "adult_surv", #"suba_surv", 
                         "suba_trans_surv", "suba_stay_surv", "reproductionRate",
                         "survival_subadult_stay", "survival_subadult_trans", "survival_adult",
                         "survival_nestling", "survival_juvenile", "reproduction_rate"
)



# initial values
# Sample from same distributions to match priors

set.seed(2002)
my.inits <- list(
  alpha_surv = rbeta(1,1,1),
  alpha_rep = rgamma(1, 1, 1),
  beta_year_surv = rnorm(1, 0, 1.5),
  beta_year_rep = rnorm(1,0,1.5),
  beta_juvenile = rnorm(1, 0, 1.5),
  #beta_subadult = rnorm(1,0,1.5),
  beta_subadult_stay = rnorm(1, 0, 1.5),
  beta_subadult_trans = rnorm(1, 0, 1.5),
  beta_adult = rnorm(1,0,1.5),
  beta_human_surv = rnorm(1,0,1.5),
  beta_temp_surv = rnorm(1,0,1.5),
  beta_human_rep = rnorm(1,0,1.5),
  beta_temp_rep = rnorm(1,0,1.5),
  sigma_year_surv = runif(1,0,0.5),
  sigma_year_rep = runif(1,0,0.5),
  sigma_nest = runif(1,0,0.5),
  breeding_suba = runif(1, 0.3, 0.5),
  breeding_adult = runif(1, 0.5, 0.9),
  suba_transition = rbeta(1,1,1)
)

# Provide MCMC details

n.iter <- 100000  # total number of iterations 
n.burnin <- 15000  # burn in period 
n.chains <- 3 

# Run the model
Eagles_model_output <- nimbleMCMC(code = Eagles_model,
                                  data = my.data,
                                  inits = my.inits,
                                  constants = my.constants,
                                  monitors = parameters.to.save,
                                  niter = n.iter,
                                  nburnin = n.burnin,
                                  nchains = n.chains
)

## Compute posterior summaries ##


Real_samples <- MCMCsummary(object = Eagles_model_output, round = 2)
View(Real_samples)

Matrix_model <- MCMCsummary(Eagles_model_output, params = "matrix_model")
Lambda <- MCMCsummary(Eagles_model_output, params = "lambda")

# Population Growth Rate with 95% CIs
mean(Lambda$mean)
mean(Lambda$`2.5%`)
mean(Lambda$`97.5%`)


# AVG reproduction/chicks produced with all predictors
repro <- MCMCsummary(Eagles_model_output, params = "reproduction_rate")
mean(repro$mean) # 0.3689951
mean(repro$`97.5%`) # 0.8254222
mean(repro$`2.5%`) # 0.1281756
