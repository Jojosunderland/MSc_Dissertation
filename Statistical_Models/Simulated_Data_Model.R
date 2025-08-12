library(tidyverse)
library(nimble)
library(MCMCvis)
library(popbio)


#----------------------------- SIMULATED DATA ----------------------------------
## Load data ##

source("./Functions/Simulate_reproductive_data.R")
source('./Functions/Simulate_survival_data.R')

##### REPRODUCTION #####

Reproductiondata <- SimulateReproductiveData(ReproductiveRate = c(0,0.5,0.5),
                                             Stages = c("juvenile",
                                                        "subadult",
                                                        "adult"),
                                             Years = 10,
                                             YearEffect = -0.1,
                                             YearRandomEffectSD = 0.1,
                                             NestIDRandomEffectSD = 0.2, 
                                             beta1 = 0.1,
                                             beta2 = -0.5,
                                             Covariates = TRUE)

## Data manipulation ##

# index nest ID and year

Reproductiondata$nestID_i <- as.integer(factor(Reproductiondata$nestID))
Reproductiondata$year_i <- as.integer(factor(Reproductiondata$year))

# rename columns to match real data 

Reproductiondata <- Reproductiondata %>% 
  rename(nest_id = nestID, alive_chicks = chicks, human = covariate1, temperature = covariate2,
         nest_id_index = nestID_i, year_index = year_i)

# keep nest indices between 1-990 (unique nests)
Reproductiondata$nest_id_index <- as.integer(factor(Reproductiondata$nest_id))

View(Reproductiondata)


##### SURVIVAL #####

# Set up #
set.seed(2002) # to make sure starting population is the same - use same for both versions 
MaxAge <- 20 

StartingPopulation <- data.frame(
  ID = sample(1:100000, 100, replace = FALSE),
  stage = sample(c("juvenile",
                   "subadult",
                   "adult"), 100,
                 replace = TRUE),
  year = 1,
  covariate1 = rnorm(1, 
                     mean = 10, 
                     sd = 0.5),
  covariate2 = rnorm(1, 
                     mean = 10, 
                     sd = 0.5),
  survival = 1) %>%
  mutate(age = case_when(stage == "juvenile" ~ 1,
                         TRUE ~ 0),
         age = ifelse(test = stage == "subadult", 
                      yes = sample(c(2,3), n(), replace = TRUE),
                      no = ifelse(stage == "adult",
                                  yes = sample(4:MaxAge, n(),
                                               replace = TRUE),
                                  no = 1)))

# add some newborns
NewBorns <- data.frame(
  ID = sample(100001:100020, 10, replace = FALSE),
  stage = "newborn",
  year = 1,
  covariate1 = rnorm(1, 
                     mean = 10, 
                     sd = 0.5),
  covariate2 = rnorm(1, 
                     mean = 10, 
                     sd = 0.5),
  survival = 1,
  age = 0) 

StartingPopulation <- bind_rows(StartingPopulation, NewBorns)

## SURVIVAL DATA ##

set.seed(2002)
Survivaldata <- SimulateSurvivalData(SurvivalRate = c(0.8, 0.9, 0.9),
                                     Stages = c("juvenile",
                                                "subadult",
                                                "adult"),
                                     Years = 10,
                                     MaxAge = 20,
                                     StartingPopulation = StartingPopulation,
                                     YearEffect = 0.2,
                                     Covariates = TRUE,
                                     beta1 = -0.01,
                                     beta2 = -0.02)

## Data manipulation ##

# Year doesn't start at 1 - indexed it:
Survivaldata$year <- Survivaldata$year - 1 

# Create first and last seen

first_last <- Survivaldata %>% 
  group_by(ID) %>% 
  summarise(first_seen = min(year), 
            last_seen = max(year),
            .groups = "drop")

# Combine with Survivaldata

Survivaldata <- left_join(first_last, Survivaldata %>% 
                            select(ID, stage, age, year, survival, covariate1, covariate2),
                          #stage_J, stage_subA, stage_A, stage_SA_trans, stage_SA_stay), 
                          by = "ID")

# EGS check: if last_seen < 9 then survived should = 0 in that final year
# UNLESS tag failed
summary(filter(Survivaldata, last_seen < 9,
               last_seen == year)) # yes, all survival = 0

# remove individuals whose first seen = last seen

Survivaldata <- Survivaldata %>% 
  filter(!first_seen == last_seen)

# Create stage columns
Survivaldata$stage_subA <- ifelse(Survivaldata$stage == "subadult", 1, 0)
Survivaldata$stage_A <- ifelse(Survivaldata$stage == "adult", 1, 0)
Survivaldata$stage_J <- ifelse(Survivaldata$stage == "juvenile", 1, 0)

# Create separate subadult stages
Survivaldata <- Survivaldata %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(next_stage = lead(stage)) %>% 
  ungroup()

# TWO subadult categories
Survivaldata <- Survivaldata %>%
  mutate(next_stage = ifelse(is.na(next_stage), stage, next_stage))

Survivaldata$stage_SA_stay <- ifelse(
  (Survivaldata$stage == "subadult" & Survivaldata$next_stage == "subadult"),
  1, 0)

Survivaldata$stage_SA_trans <- ifelse(Survivaldata$stage == "subadult" &
                                        Survivaldata$next_stage == "adult", 1, 0)
Survivaldata <- Survivaldata %>% 
  arrange(ID)

## Create stage survival matrices ##

# JUVENILE
juvenile <- pivot_wider(Survivaldata, id_cols = c(year), 
                        
                        values_from = c(stage_J),
                        
                        names_from = c(ID)) %>%
  arrange(year)

juvenile = juvenile[,-1] # to remove first column and index correctly

# convert tibble to matrix 
juvenile_mat <- as.matrix(juvenile)

# SUBADULT
subadult <- pivot_wider(Survivaldata, id_cols = c(year), 
                        
                        values_from = c(stage_subA),
                        
                        names_from = c(ID)) %>%
  arrange(year)

subadult = subadult[,-1] # to remove first column and index correctly

# convert tibble to matrix 
subadult_mat <- as.matrix(subadult)

# ADULT
adult <- pivot_wider(Survivaldata, id_cols = c(year), 
                     
                     values_from = c(stage_A),
                     
                     names_from = c(ID)) %>%
  arrange(year)

adult = adult[,-1] # to remove first column and index correctly

# convert tibble to matrix 
adult_mat <- as.matrix(adult)

# SUBADULT STAY
subadult_stay <- pivot_wider(Survivaldata, id_cols = c(year), 
                             
                             values_from = c(stage_SA_stay),
                             
                             names_from = c(ID)) %>%
  arrange(year)

subadult_stay = subadult_stay[,-1] # to remove first column and index correctly

# convert tibble to matrix 
subadult_stay_mat <- as.matrix(subadult_stay)

# SUBADULT TRANSITION
subadult_trans <- pivot_wider(Survivaldata, id_cols = c(year), 
                              
                              values_from = c(stage_SA_trans),
                              
                              names_from = c(ID)) %>%
  arrange(year)

subadult_trans = subadult_trans[,-1] # to remove first column and index correctly

# convert tibble to matrix 
subadult_trans_mat <- as.matrix(subadult_trans)


# Create a capture history

cap.dates <- sort(unique(c(Survivaldata$first_seen, Survivaldata$last_seen)))
cap.dates <- seq(cap.dates[1],cap.dates[length(cap.dates)], by = 1) 
occs <- length(cap.dates)

first <- last <- array(NA, dim = nrow(Survivaldata))

# REWRITE CAPTURE HISTORY
surv_caps <- matrix(data = NA, nrow = length(unique(Survivaldata$ID)), ncol = occs) #cap.history 
for(i in 1:length(unique(Survivaldata$ID))){ #for each individual
  # to make it per individual could reduce dataset here
  Tempdata <- Survivaldata[Survivaldata$ID == unique(Survivaldata$ID)[i],]
  # this now takes ALL the rows for this individual then use the latest one for
  # everything that follows
  last_row <- Tempdata[nrow(Tempdata), ]  # final row for this individual
  # pulls the final observation
  first[i] <- which(cap.dates == last_row$first_seen)  # correct
  last[i]  <- which(cap.dates == last_row$last_seen)   # correct
  
  surv_caps[i, first[i]:last[i]] <- 1
  if (last_row$survival == 0) {
    surv_caps[i, last[i]] <- 0
  }
}

View(surv_caps)

# Rename columns - to match real data

Survivaldata <- Survivaldata %>% 
  rename(id = ID, survived = survival, human = covariate1, 
         temperature = covariate2, stage_obs = stage, age_obs = age)


# Make wide format survival data

Survivaldatawide <- Survivaldata %>%
  rowwise() %>%
  mutate(year = list(first_seen:last_seen)) %>%
  unnest(year) %>%
  select(id, first_seen, last_seen, year, survived)

Survivaldatawide <- Survivaldatawide %>%
  group_by(id, year, first_seen, last_seen) %>%
  summarise(survived = max(survived), .groups = "drop") 

Survivaldatawide <- Survivaldatawide %>% 
  pivot_wider(
    names_from = year,
    values_from = survived,
    values_fill = 0,
    names_prefix = "year"
  )

# year order
Survivaldatawide <- Survivaldatawide %>% 
  select(id, first_seen, last_seen, year1, year2, year3, year4, year5, year6, year7, year8, year9)

# remove individuals that first seen == last seen
# do this for all datasets

Survivaldatawide <- Survivaldatawide %>% 
  filter(!first_seen == last_seen)

Survivaldatawide <- Survivaldatawide %>% 
  mutate(survived = year9)

Survivaldatawide <- Survivaldatawide %>% 
  arrange(id)

## Subadult transition rate ##

n_trans <- sum(Survivaldata$stage_SA_trans)
n_stay  <- sum(Survivaldata$stage_SA_stay)

transition_successes = n_trans
transition_trials = n_trans + n_stay

# replace NAs in stage matrices with 0s 
#juvenile_mat[is.na(juvenile_mat)] <- 0
#subadult_mat[is.na(subadult_mat)] <- 0
#adult_mat[is.na(adult_mat)] <- 0

# transpose matrices:
juvenile_mat <- t(juvenile_mat)
subadult_mat <- t(subadult_mat)
adult_mat <- t(adult_mat)
subadult_stay_mat <- t(subadult_stay_mat)
subadult_trans_mat <- t(subadult_trans_mat)

## Scale covariates

Survivaldata$human_scaled <- scale(Survivaldata$human)
Survivaldata$temperature_scaled <- scale(Survivaldata$temperature)
Reproductiondata$human_scaled <- scale(Reproductiondata$human)
Reproductiondata$temperature_scaled <- scale(Reproductiondata$temperature)


##### MODEL WITH SIMULATED DATA AND MPM #####

## Model ##

Eagles_model <- nimbleCode({
  
  #----------------------------------------------------------------------------
  # DEFINE PRIORS SURVIVAL
  # outside of loop
  alpha_surv ~ dbeta(1,1) # vague prior - intercept for nestling survival
  beta_year_surv ~ dnorm(0,1.5) # fixed year effect
  
  # STAGE PRIORS
  beta_juvenile ~ dnorm(0,1.5) 
  beta_subadult ~ dnorm(0,1.5)
  #beta_subadult_stay ~ dnorm(0, 1.5)
  #beta_subadult_trans ~ dnorm(0, 1.5)
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
  for(t in 1:n_years_surv){
    beta_yearRE_surv[t] ~ dnorm(0, sd = sigma_year_surv)
  }
  
  # Reproduction
  for(y in 1:n_years_rep){
    beta_yearRE_rep[y] ~ dnorm(0, sd = sigma_year_rep)
  }
  
  for(j in 1:n_nest){
    beta_nestRE[j] ~ dnorm(0, sd = sigma_nest)
  }
  
  #----------------------------------------------------------------------------
  ## SURVIVAL
  
  alpha_surv_logit <- logit(alpha_surv)
  
  for(i in 1:n) {
    
    for(t in (first[i]+1):last[i]){
      # logit link function
      logit(p[i,t]) <- alpha_surv_logit + (beta_year_surv * year_surv[t]) +
        (beta_juvenile * juvenile[i,t]) + (beta_adult * adult[i,t]) + (beta_subadult * subadult[i,t]) +
        #(beta_subadult_stay * subadult_stay[i,t]) + (beta_subadult_trans * subadult_trans[i,t]) +
        (beta_human_surv * human_surv[t]) + (beta_temp_surv * temp_surv[t]) + (beta_yearRE_surv[year_surv[t]])
      
      mu[i,t] <- p[i,t]*survival_prob[i, t-1]
      
      # SURVIVAL LIKELIHOOD
      survival_prob[i,t] ~ dbern(mu[i,t])
    }
    
  }
  
  # Subadult transition likelihood
  n_trans ~ dbinom(suba_transition, n_trials)
  
  # Back transform sub adult and adult survival probabilities - outside of loop (only one value printed for each)
  survival_nestling <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_year_surv))) 
  survival_juvenile <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_juvenile)))
  survival_subadult <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_subadult)))
  #survival_subadult_stay <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_subadult_stay)))
  #survival_subadult_trans <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_subadult_trans)))
  survival_adult <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_adult)))
  
  
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
    
    nest_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit) + (beta_year_surv*year_surv[i]) +
                                   beta_temp_surv*temp_surv[i] +
                                   beta_human_surv*human_surv[i] + 
                                   beta_yearRE_surv[i]))
    
    juv_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit) + beta_juvenile +
                                  beta_temp_surv*temp_surv[i] +
                                  beta_human_surv*human_surv[i] + 
                                  (beta_year_surv*year_surv[i]) + 
                                  beta_yearRE_surv[i])) 
    
    suba_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit) + beta_subadult +
                                   beta_temp_surv*temp_surv[i] +
                                   beta_human_surv*human_surv[i] + 
                                   (beta_year_surv*year_surv[i]) + 
                                   beta_yearRE_surv[i]))
    
    #suba_stay_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit) + beta_subadult_stay +
    #                                beta_temp_surv*temp_surv[i] +
    #                               beta_human_surv*human_surv[i] + 
    #                              (beta_year_surv*year_surv[i]) + 
    #                             beta_yearRE_surv[i])) 
    
    #suba_trans_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit) + beta_subadult_trans +
    #                                beta_temp_surv*temp_surv[i] +
    #                               beta_human_surv*human_surv[i] + 
    #                              (beta_year_surv*year_surv[i]) + 
    #                             beta_yearRE_surv[i]))
    
    adult_surv[i] <- 1 / (1 + exp(-(alpha_surv_logit) + beta_adult +
                                    beta_temp_surv*temp_surv[i] +
                                    beta_human_surv*human_surv[i] + 
                                    (beta_year_surv*year_surv[i]) + 
                                    beta_yearRE_surv[i]))
  }
  
  # Yearly reproduction rate
  for(i in 1:n_years_rep){
    
    reproductionRate[i] <- exp(log(alpha_rep) + (beta_year_rep*year_rep[i]) + 
                                 (beta_yearRE_rep[year_rep[i]]) +  
                                 (beta_temp_rep*temp_rep[i]) +
                                 (beta_human_rep*human_rep[i]))
    
  }
  
  # breeding probability
  subadult_BP <- breeding_suba 
  adult_BP <- breeding_adult
  
  # subadult transition rate
  suba_T <- suba_transition
  
  ## Set up Matrix transitions [row, col, year] = [t+1 (to), t (from), year]
  
  #matrix_model <- array(0, dim = c(n_stages, n_stages, n_years_surv)) # Need to define years all (2003-2025)
  for(y in 1:n_years_surv){
    
    matrix_model[1,1,y] <- 0 # N-N 
    matrix_model[1,2,y] <- 0 # J-N 
    # Sub adult to nestling
    matrix_model[1,3,y] <- reproductionRate[y] * (subadult_BP) * 0.5 * (suba_surv[y] * (1-suba_T)) #(suba_stay_surv[y]) 
    # Adult to nestling 
    matrix_model[1,4,y] <- (reproductionRate[y] * (adult_BP) * 0.5 *(adult_surv[y])) + 
      (reproductionRate[y] * (adult_BP) * 0.5 * (suba_surv[y] * suba_T)) # (suba_trans_surv[y])) 
    matrix_model[2,1,y] <- nest_surv[y] # Nestling to juvenile 
    matrix_model[2,2,y] <- 0 # J-J 
    matrix_model[2,3,y] <- 0 # SA-J 
    matrix_model[2,4,y] <- 0 # A-J 
    matrix_model[3,1,y] <- 0 # N-SA
    matrix_model[3,2,y] <- juv_surv[y] # Juvenile to sub adult
    matrix_model[3,3,y] <- suba_surv[y] * (1-suba_T) #suba_stay_surv[y] # Sub adult persistence survival 
    matrix_model[3,4,y] <- 0 # A-SA
    matrix_model[4,1,y] <- 0 # N-A
    matrix_model[4,2,y] <- 0 # J-A
    matrix_model[4,3,y] <- suba_surv[y] * (suba_T) #suba_trans_surv[y] # Sub adult to adult (transition survival)
    matrix_model[4,4,y] <- adult_surv[y] # Adult survival
    
  }
  
  # DERIVED QUANTITIES
  
  #Population growth rate
  
  for(y in 1:n_years_surv){
    lambda[y] <- nimEigen(matrix_model[1:4, 1:4, y])$values[1]
  }
  
  
})

# number of observations
n <- length(unique(Survivaldata$id)) # Survival data 
N <- nrow(Reproductiondata) # Reproduction data

my.data <- list(
  chicks = Reproductiondata$alive_chicks,
  survival_prob = surv_caps,
  human_surv = as.numeric(Survivaldata$human_scaled), 
  temp_surv = as.numeric(Survivaldata$temperature_scaled), 
  human_rep = as.numeric(Reproductiondata$human_scaled), 
  temp_rep = as.numeric(Reproductiondata$temperature_scaled), 
  n_trans = n_trans # subadult transition 
)

# constants
my.constants <- list(
  n = n, # number of data points (survival)
  N = N, # Number of data points (reproduction)
  year_surv = Survivaldata$year, # Survival data years
  year_rep = Reproductiondata$year_index, # Reproduction data years 
  n_years_surv = ncol(surv_caps),#length(unique(Survivaldata$year)), # Number of years for survival year RE
  n_years_rep = length(unique(Reproductiondata$year_index)), # Number of years for reproduction year RE
  n_nest = length(unique(Reproductiondata$nest_id_index)), # Number of nests for nest RE
  nest = Reproductiondata$nest_id_index, # Nest IDs indexed
  juvenile = juvenile_mat, # juvenile matrix
  subadult = subadult_mat, 
  #subadult_stay = subadult_stay_mat, # subadult matrix - stay
  #subadult_trans = subadult_trans_mat, # subadult matrix - mature
  adult = adult_mat, # adult matrix
  first = Survivaldatawide$first_seen, # year each individual was first seen
  last = Survivaldatawide$last_seen, # year each individual was last seen
  n_trials = n_trans + n_stay
  #n_years_all = length(1:10) # vector of all years in both survival and reproduction data (1:10)
  #n_stages = 4 # number of stages
)

# parameters

parameters.to.save <- c( "alpha_surv", "beta_year_surv","alpha_rep", 
                         "beta_year_rep", "sigma_year_surv", "sigma_nest",
                         "sigma_year_rep","beta_juvenile", "beta_subadult",
                         "beta_adult", "beta_human_surv", "beta_human_rep",
                         "beta_temp_surv", "beta_temp_rep", "breeding_suba", "breeding_adult",
                         #"beta_subadult_trans", "beta_subadult_stay",
                         "survival_juvenile", "survival_adult", "survival_nestling",
                         #"survival_subadult_stay", "survival_subadult_trans", 
                         "survival_subadult",
                         
                         #Matrix derived quantities
                         
                         "matrix_model", "reproductionRate", "lambda", "suba_transition",
                         "nest_surv", "juv_surv", "adult_surv", "suba_surv"#,
                         #"suba_trans_surv", "suba_stay_surv"
)
#Model check parameters
parameters.to.save <- c( "alpha_surv", "beta_year_surv","alpha_rep", 
                         "beta_year_rep", "sigma_year_surv", "sigma_nest",
                         "sigma_year_rep", "beta_human_surv", "beta_human_rep",
                         "beta_temp_surv", "beta_temp_rep","survival_juvenile", 
                         "survival_adult", "survival_nestling","survival_subadult", 
                         "beta_juvenile", "beta_subadult", "beta_adult" 
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
  beta_subadult = rnorm(1,0,1.5),
  #beta_subadult_stay = rnorm(1, 0, 1.5),
  #beta_subadult_trans = rnorm(1, 0, 1.5),
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

n.iter <- 30000 #100000  # total number of iterations (2n)
n.burnin <-0 #15000 # burn in period (n)
n.chains <- 3

Sim_Eagles_model_output <- nimbleMCMC(code = Eagles_model,
                                      data = my.data,
                                      inits = my.inits,
                                      constants = my.constants,
                                      monitors = parameters.to.save,
                                      niter = n.iter,
                                      nburnin = n.burnin,
                                      nchains = n.chains)



## Compute posterior summaries ##

Sim_samples <- MCMCsummary(object = Sim_Eagles_model_output, round = 2)
Sim_matrix <- MCMCsummary(Sim_Eagles_model_output, params = "matrix_model")
Sim_lambda <- MCMCsummary(Sim_Eagles_model_output, params = "lambda")

View(Sim_samples)
View(Sim_lambda)

