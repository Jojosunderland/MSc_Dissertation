################################################################################
# Script to run simulations for reproductive and survival data
################################################################################

#### SET UP ####

### Load packages

library(tidyverse)

### Load any data

### Load any function scripts
source("./Functions/Simulate_reproductive_data.R")
source("./Functions/Simulate_survival_data.R")

#### TRY IT OUT

#### Reproduction
ReproductiveData <- SimulateReproductiveData(ReproductiveRate = c(0.5,0.5,0.5),
                                             Stages = c("juvenile",
                                                        "subadult",
                                                        "adult"),
                                             Years = 10)

ReproductiveData2 <- SimulateReproductiveData(ReproductiveRate = c(0.5,0.5,0.5),
                                             Stages = c("juvenile",
                                                        "subadult",
                                                        "adult"),
                                             Years = 10,
                                             YearEffect = -0.1)

plot(chicks ~ year, data = ReproductiveData)
plot(chicks ~ year, data = ReproductiveData2) # seems to be working - but should
# set up more checks

# trying the random effects
ReproductiveData3 <- SimulateReproductiveData(ReproductiveRate = c(0.5,0.5,0.5),
                                              Stages = c("juvenile",
                                                         "subadult",
                                                         "adult"),
                                              Years = 10,
                                              YearEffect = -0.1,
                                              YearRandomEffectSD = 0.1,
                                              NestIDRandomEffectSD = 0.2)
# EGS - it at least produces something! 

# now test covariates
ReproductiveData4 <- SimulateReproductiveData(ReproductiveRate = c(0.5,0.5,0.5),
                                              Stages = c("juvenile",
                                                         "subadult",
                                                         "adult"),
                                              Years = 10,
                                              YearEffect = -0.1,
                                              YearRandomEffectSD = 0.1,
                                              NestIDRandomEffectSD = 0.2, 
                                              beta1 = 0.05,
                                              beta2 = -0.5,
                                              Covariates = TRUE)
# EGS - it at least produces something! 
plot(chicks ~ covariate1, data = ReproductiveData4)
plot(chicks ~ covariate2, data = ReproductiveData4) 
# seems to be working but might want to check

## Do all plots and viewing here, not in the function

#### Survival

set.seed(1) # to make sure starting population is the same - use same for both versions 
MaxAge <- 20 # Need to define this before?

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

set.seed(1)
SurvivalData <- SimulateSurvivalData(SurvivalRate = c(0.8, 0.8, 0.9),
                                     Stages = c("juvenile",
                                                "subadult",
                                                "adult"),
                                     Years = 10,
                                     MaxAge = 20,
                                     StartingPopulation = StartingPopulation)

# check survival rates
# juvenile 
mean(filter(SurvivalData, stage == "juvenile")$survival)

# subadult
mean(filter(SurvivalData, stage == "subadult")$survival)

# adult 
mean(filter(SurvivalData, stage == "adult")$survival)


#### Try year effect survival

set.seed(1)
SurvivalData <- SimulateSurvivalData(SurvivalRate = c(0.8, 0.8, 0.9),
                                     Stages = c("juvenile",
                                                "subadult",
                                                "adult"),
                                     Years = 10,
                                     MaxAge = 20,
                                     StartingPopulation = StartingPopulation,
                                     YearEffect = -0.1)

# plot survival over years
SurvivalPlotting <- SurvivalData %>% group_by(year, stage) %>%
  summarise(mean_survival = mean(survival))

ggplot(aes(x = year, y = mean_survival), data = SurvivalPlotting)+
  geom_line()+
  theme_minimal()+
  facet_grid(cols = vars(stage))

# try random effects and covariates

set.seed(1)
SurvivalData2 <- SimulateSurvivalData(SurvivalRate = c(0.8, 0.8, 0.9),
                                     Stages = c("juvenile",
                                                "subadult",
                                                "adult"),
                                     Years = 10,
                                     MaxAge = 20,
                                     StartingPopulation = StartingPopulation,
                                     YearEffect = 0.5,
                                     Covariates = TRUE,
                                     beta1 = -0.01,
                                     beta2 = -0.5)

# plot survival by covariates
SurvivalPlotting <- SurvivalData2 %>% group_by(covariate1, stage) %>%
  summarise(mean_survival = mean(survival))

ggplot(aes(x = covariate1, y = mean_survival), data = SurvivalPlotting)+
  geom_line()+
  theme_minimal()+
  facet_grid(cols = vars(stage))

SurvivalPlotting <- SurvivalData2 %>% group_by(covariate2, stage) %>%
  summarise(mean_survival = mean(survival))

ggplot(aes(x = covariate2, y = mean_survival), data = SurvivalPlotting)+
  geom_line()+
  theme_minimal()+
  facet_grid(cols = vars(stage))

# think it is working but much less clear than reproduction
