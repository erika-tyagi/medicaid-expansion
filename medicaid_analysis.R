library(glmnet)
library(tidyverse)
library(plyr)
library(reshape)

setwd("~/fall2019/elections_campaigns/medicaid-expansion")

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
lobbying_full %>%
  group_by(Spender_Industry, year, state) %>% 
  summarise(lobbying_spend = sum(lobbying_spend),
            num_contributions = sum(num_contributions))

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

reshape1 <- select(lobbying_full,-c("num_contributions"))
lobbying_wide <- spread(reshape1, Spender_Industry, lobbying_spend)

## Join to medicaid data
medicaid <- read_csv("medicaid-data.csv")

## Clean Medicaid Data
medicaid$pct_urban <- medicaid$pct_urban/100
medicaid$pct_unemployed <- medicaid$pct_unemployed/100

## Join in lobbying data
medicaid <- join(medicaid, lobbying_full, by = c("state", "year"))

## Read in population data and merge with states
population <- read_csv("population_states.csv") %>%
  select(c("NAME", "POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012",
           "POPESTIMATE2013", "POPESTIMATE2014", "POPESTIMATE2015", "POPESTIMATE2016",
           "POPESTIMATE2017", "POPESTIMATE2018")) 
  
population <- population %>% filter(!NAME %in% 
                       c("United States", "Northeast Region", "Midwest Region",
                           "South Region", "West Region", "Puerto Rico")) %>%
  rename(c("NAME" = "state")) 

population <- population[order(population$state),]
pop <- melt(population) 


table(medicaid$Spender_Industry)




