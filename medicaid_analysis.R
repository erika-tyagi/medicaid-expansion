setwd("~/fall2019/elections_campaigns/medicaid-expansion")

library(glmnet)
library(tidyverse)
library(plyr)
library(reshape2)
library(dplyr)
library(coefplot)


## Read in Data
lobby_1 <- read_csv("lobbyist_spending_by_state.csv") 
lobby_2 <- read_csv("lobby_spending_advocates.csv")
medicaid <- read_csv("medicaid-data.csv")
state_sc <- read_csv("state_to_statecode.csv") %>%
  rename(c("Code" = "state"))


## Clean lobbying Data
lobbying_full <- join(lobby_1, lobby_2, by = NULL, type = "full") %>%
  select(c("Filing_Year", "Filing_Jurisdiction",
           "Spender_Industry", "Spender_Sector",
           "Total_$", "#_of_Records")) %>%
  rename(c("Filing_Year" = "year",
           "Filing_Jurisdiction" = "state",
           "Total_$" = "lobbying_spend",
           "#_of_Records" = "num_contributions"))
lobbying_full <- join(lobbying_full, state_sc, by= "state", type = "left") %>%
  select(-c("state", "Abbrev", "Spender_Sector")) %>%
  rename(c("State" = "state"))

## business services are repeated (were broken up from which businesses contributed)
# group by and sum these contributions
lobbying_full <- lobbying_full %>%
  dplyr::group_by(Spender_Industry, year, state) %>% 
  dplyr::summarise(lobbying_spend = sum(lobbying_spend),
            num_contributions = sum(num_contributions))

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

reshape1 <- select(lobbying_full,-c("num_contributions"))
reshape2 <- select(lobbying_full, -c( "lobbying_spend"))

lobbying_wide1 <- spread(reshape1, Spender_Industry, lobbying_spend) %>%
  rename_at(vars(-state, -year), function(x) paste0(x, "_lobby_spend"))

lobbying_wide2 <- spread(reshape2, Spender_Industry, num_contributions) %>%
  rename_at(vars(-state, -year), function(x) paste0(x, "_lobby_num"))

lobbying_wide <- full_join(lobbying_wide1 , lobbying_wide2, by = c("year", "state")) 

## Join to medicaid data
medicaid <- read_csv("medicaid-data.csv")

## Clean Medicaid Data
medicaid$pct_urban <- medicaid$pct_urban/100
medicaid$pct_unemployed <- medicaid$pct_unemployed/100

## Join in lobbying data
medicaid <- left_join(medicaid, lobbying_wide, by = c("state", "year"))

## Read in population data and merge with states
population <- read_csv("population_states.csv") %>%
  select(c("NAME", "POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012",
           "POPESTIMATE2013", "POPESTIMATE2014", "POPESTIMATE2015", "POPESTIMATE2016",
           "POPESTIMATE2017", "POPESTIMATE2018")) 
  
population <- population %>% filter(!NAME %in% 
                       c("United States", "Northeast Region", "Midwest Region",
                           "South Region", "West Region", "Puerto Rico")) %>%
  rename(c("NAME" = "state")) %>%
  rename_at(vars(-state), function(x) substr(x, 12, nchar(x)))

pop <- reshape2::melt(population) 
pop$state[pop$state == "District of Columbia"] <- "DC"
pop <- rename(pop, c("value" = "population",
                     "variable" = "year"))
pop$year <- as.numeric(as.character(pop$year))

medicaid <- left_join(medicaid, pop, by = c("state", "year"))

## divide by population for certain variables
pop_dep_vars <- c("budget-surplus"                         
                  ,"Business Services_lobby_spend",                "Conservative Policy Organization_lobby_spend" 
                  ,"Drug Policy_lobby_spend",                       "General Trade Unions_lobby_spend"             
                  ,"Health & Welfare Policy_lobby_spend",           "Health Professionals_lobby_spend"             
                  ,"Health Services_lobby_spend",                   "Hospitals & Nursing Homes_lobby_spend"        
                  ,"Liberal Policy Organization_lobby_spend",       "Miscellaneous Health_lobby_spend"             
                  ,"Pharmaceuticals & Health Products_lobby_spend", "Public Sector Unions_lobby_spend"             
                  ,"Transportation Unions_lobby_spend",             "expansion-beneficiaries")

medicaid[,pop_dep_vars]/medicaid['population']
medicaid <- mutate_at(medicaid, pop_dep_vars, function(x) (1000*x/medicaid$population))

lobby_vars <- c("Business Services_lobby_spend",                "Conservative Policy Organization_lobby_spend" 
                ,"Drug Policy_lobby_spend",                       "General Trade Unions_lobby_spend"             
                ,"Health & Welfare Policy_lobby_spend",           "Health Professionals_lobby_spend"             
                ,"Health Services_lobby_spend",                   "Hospitals & Nursing Homes_lobby_spend"        
                ,"Liberal Policy Organization_lobby_spend",       "Miscellaneous Health_lobby_spend"             
                ,"Pharmaceuticals & Health Products_lobby_spend", "Public Sector Unions_lobby_spend"             
                ,"Transportation Unions_lobby_spend", "Conservative Policy Organization_lobby_num",    "Drug Policy_lobby_num"                        
                ,"General Trade Unions_lobby_num",                "Health & Welfare Policy_lobby_num"            
                ,"Health Professionals_lobby_num",                "Health Services_lobby_num"                    
                ,"Hospitals & Nursing Homes_lobby_num",         "Liberal Policy Organization_lobby_num"        
                ,"Miscellaneous Health_lobby_num",                "Pharmaceuticals & Health Products_lobby_num"  
                ,"Public Sector Unions_lobby_num",             "Transportation Unions_lobby_num", "Business Services_lobby_num")

## fill nas with 0 where appropriate (all lobbying variables)
medicaid <- mutate_at(medicaid, lobby_vars, function(x) replace_na(x, 0))
medicaid$pct_gop_senate <- as.numeric(medicaid$pct_gop_senate)
medicaid$pct_trump_2016 <- as.numeric(medicaid$pct_trump_2016)

## impute nebraska
medicaid$pct_gop_senate[medicaid$state == "Nebraska"] <- .625 
medicaid$pct_gop_house[medicaid$state == "Nebraska"] <- .625 

## impute trump numbers
trump_by_states <- dplyr::group_by(na.omit(medicaid), state) %>%
  dplyr::summarize(pct_trump_2016 = min(pct_trump_2016))

medicaid_im <- medicaid %>% select(-c("pct_trump_2016")) %>%
  left_join(trump_by_states, by = c("state" = "state")) %>%
  select(-c("pct_unemployed"))


########### MODEL 1 ALL YEARS ALL STATES
#drop na to see
df <- na.omit(medicaid_im)
full_model <- lm(expanded_medicaid ~ . -expanded_medicaid -state -year -population, data = df)
# store outcome variable
y <- df$expanded_medicaid
# you have to pass a model.matrix object to glmnet functions: dataset and functional form
#X <- data.matrix(df)
X <- model.matrix(full_model, df)
n <- length(y)

## LASSO WITH ALPHA = 1
# CV to find optimal lambda
# glm = generalized linear model 
# we pass model matrix, dependent variable, and parameters
# family = binomial for binary dependent variable
# nfold = 10: 10-fold cross-validation
# alpha = 1: mixing parameter. 1 is lasso, 0 is ridge, between 0 and 1 is mixture (elastic net)
cv1 <- cv.glmnet(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 1,
                 standardize = TRUE)
plot(cv1) # viz cv
# left line is minimum value of lambda, right line is one s.d. from minimum
# the plot shows us that the information loss is quite variable: the red dots show us information loss
# at every log of lambda. We want the value that MINIMIZES info loss


# Fit the model
# lambda replaces nfold
lassomod <- glmnet(X, y, 
                   family = "binomial", 
                   lambda = cv1$lambda.min,  # call minimum lambda value stored in the cv1 object
                   alpha = 1,
                   standardize= TRUE)

coefplot(lassomod)
ggsave("model1_allstates.png", dpi = 500)

######### MODEL 2 ALL STATES 2018
df_2018 <- subset(df, year == 2018)

model2 <- lm(expanded_medicaid ~ . -expanded_medicaid -state -year -population, data = df_2018)

# store outcome variable
y <- df_2018$expanded_medicaid
# you have to pass a model.matrix object to glmnet functions: dataset and functional form
#X <- data.matrix(df)
X <- model.matrix(model2, df_2018)
n <- length(y)

cv2 <- cv.glmnet(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 1,
                 standardize = TRUE)
plot(cv2) # viz cv
# left line is minimum value of lambda, right line is one s.d. from minimum
# the plot shows us that the information loss is quite variable: the red dots show us information loss
# at every log of lambda. We want the value that MINIMIZES info loss

# Fit the model
# lambda replaces nfold
lassomod2 <- glmnet(X, y, 
                    family = "binomial", 
                    lambda = cv2$lambda.min,  # call minimum lambda value stored in the cv1 object
                    alpha = 1,
                    standardize= TRUE)

coefplot(lassomod2)
ggsave("model2_allstates_2018.png", dpi = 500)

##### MODEL 3 ALL YEARS
adopted <- read_csv("medicaid-expansion-outcome.csv")
df_trun <- df %>% left_join(adopted, by = c("state" = "State"))%>% 
  rename(c("Expanded 2014" = "expanded_2014",
           "Expanded 2019" = "expanded_2019"))
df_trun <- filter(df_trun, expanded_2014 == 0) %>% select(-c(expanded_2014, expanded_2019))

trun_model <- lm(expanded_medicaid ~ . -expanded_medicaid -state -year, data = df_trun)

# store outcome variable
y <- df_trun$expanded_medicaid
# you have to pass a model.matrix object to glmnet functions: dataset and functional form
#X <- data.matrix(df)
X <- model.matrix(trun_model, df_trun)
n <- length(y)

cv3 <- cv.glmnet(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 1,
                 standardize = TRUE)
plot(cv3) # viz cv
# left line is minimum value of lambda, right line is one s.d. from minimum
# the plot shows us that the information loss is quite variable: the red dots show us information loss
# at every log of lambda. We want the value that MINIMIZES info loss

# Fit the model
# lambda replaces nfold
lassomod3 <- glmnet(X, y, 
                   family = "binomial", 
                   lambda = cv3$lambda.min,  # call minimum lambda value stored in the cv1 object
                   alpha = 1,
                   standardize= TRUE)

coefplot(lassomod3)
ggsave("model3_og_states.png", dpi = 500)

##### MODEL 4 OG 2018
adopted <- read_csv("medicaid-expansion-outcome.csv")
df_trun <- df %>% left_join(adopted, by = c("state" = "State"))%>% 
  rename(c("Expanded 2014" = "expanded_2014",
           "Expanded 2019" = "expanded_2019"))
df_trun <- filter(df_trun, expanded_2014 == 0) %>% select(-c(expanded_2014, expanded_2019))
df_trun <- subset(df_trun, year == 2018)

trun_model <- lm(expanded_medicaid ~ . -expanded_medicaid -state -year, data = df_trun)

# store outcome variable
y <- df_trun$expanded_medicaid
# you have to pass a model.matrix object to glmnet functions: dataset and functional form
#X <- data.matrix(df)
X <- model.matrix(trun_model, df_trun)
n <- length(y)

cv4 <- cv.glmnet(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 1,
                 standardize = TRUE)
plot(cv4) # viz cv
# left line is minimum value of lambda, right line is one s.d. from minimum
# the plot shows us that the information loss is quite variable: the red dots show us information loss
# at every log of lambda. We want the value that MINIMIZES info loss

# Fit the model
# lambda replaces nfold
lassomod4 <- glmnet(X, y, 
                    family = "binomial", 
                    lambda = cv4$lambda.min,  # call minimum lambda value stored in the cv1 object
                    alpha = 1,
                    standardize= TRUE)

coefplot(lassomod4)
ggsave("model4_og_2018.png", dpi = 500)










 
 