#---- NBA Draft Model 1.0 ----
#Last Updated: February 5, 2018
#Author: Alexander Powell
#Project Progress: #2 College Data (0%)
#
#
#----------------------------#

#Packages
library(dplyr)

#---- NBA Data ----

#Input Data
SalaryCap <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_SalaryCap_85_to_18.csv")
Salaries <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_Salaries_90_to_18.csv")

#Data Cleaning
Per.Cap <- merge(Salaries, SalaryCap, by.y = "Year", by.x = "season_end", all.x = TRUE)
Per.Cap$Per.Cap <- Per.Cap$salary / Per.Cap$Salary.Cap

NBA <- Per.Cap %>%
  group_by(player) %>%
  mutate( fifth = nth(season_end, 5, order_by = season_end, default = 0) ) %>%
  filter(season_end == fifth)

#---- College Data ----



#---- Modeling ----
