########### MATH OF FINANCE ############
###############################################

# remove all variable except functions in environment
rm(list = setdiff(ls(), lsf.str()))
setwd("~/Documents/edu/AWS/repos/Gen_R/math_econ")
load("~/Documents/edu/AWS/repos/Gen_R/math_econ/.RData")

#rm(list = ls())

### rjava install ###
Sys.getenv("JAVA_HOME")
Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java")
install.packages("rJava")
## in terminal:
## apt-get install lib64pcre2-devel

#####################################################
##### Hedging-Arbitrage
#####################################################

### libraries
library(astsa)
library(xts)

library(tidyverse)
dat_confirmed <- read_csv("./time_series_19-covid-Confirmed.csv")
dat_nums <- dat_confirmed[, c(5:ncol(dat_confirmed))]
confirmed_mat <- colSums(dat_nums)
march_nums <- confirmed_mat[, c('3/1/20', )]
col_dates <- as.Date(names(confirmed_mat), format = "%m/%d/%y")
plot(confirmed_mat, pch = "+")
names(confirmed_mat)
barplot(confirmed_mat)

library(tidyr)
library(magrittr)
dat_dates <- dat_confirmed %>% gather(Day, Cases, -c('Province/State', 'Country/Region', 'Lat', 'Long')) 
dat_dates %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y"))
head(dat_dates)

day_cases <- dat_dates[, c("Day", "Cases")]
day_cases %>%
  group_by(Day) %>%
  mutate(CasesTotal = sum(Cases))

test00 <- day_cases %>%
  group_by(Day) %>%
  summarise(sum(Cases))


