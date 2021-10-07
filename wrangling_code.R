#Wrangling data 

#load packages I will use
library(tidyverse)
library(tibble)
library(dplyr)

#import data
gini <- read.csv('gini.csv')
income <- read.csv('mincpcap_cppp.csv')

#check the data I import
#View(gini)
#View(income)

# dimension of data
dim(gini)
dim(income)

# The dimension of data are not the same, we will find which years are missing in gini data frame
year_in_income <- names(income)
year_in_gini <- names(gini)

years_not_list <- year_in_income[which(!year_in_income %in% year_in_gini)]

# We want to get rid of missing values
# Check if there is any missing value
sum(is.na(income))
sum(is.na(gini))

# Since we don't want any missing value in our data, we remove years columns that are not in gini data frame
income <- income[ , !(names(income) %in% years_not_list)]
#check the dimension of income again
dim(income)

#wrangling data into one tibble
income_tidy <- income %>%
  pivot_longer(X1799:X2039, names_to = "years", values_to = "daily_income")

gini_tidy <- gini %>%
  pivot_longer(X1799:X2039, names_to = "years", values_to = "gini_coef")

tidy <- merge(income_tidy, gini_tidy, c('years', 'country' ))

#tidy up years in our data 
tidy$years <- as.integer(substring(tidy$years,2))

tidy <- as_tibble(tidy)
