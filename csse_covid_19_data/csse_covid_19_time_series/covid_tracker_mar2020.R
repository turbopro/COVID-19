########### COVID-19 TRACKER ############


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

######## load data #####################

### libraries
library(astsa)
library(xts)

library(tidyverse)
# dat_confirmed <- read_csv("./time_series_19-covid-Confirmed.csv")
# dat_deaths <- read_csv("./time_series_19-covid-Deaths.csv")
# dat_recovered <- read_csv("./time_series_19-covid-Recovered.csv")

dat_confirmed <- read_csv("./time_series_covid19_confirmed_global.csv")
dat_deaths <- read_csv("./time_series_covid19_deaths_global.csv")
# dat_recovered <- read_csv("./time_series_19-covid-Recovered.csv")


## get daily figures
# dat_daily <- read_csv("./daily_03-21-2020.csv")
# str(dat_daily)
# dat_daily %<>% select(., -c(Latitude, Longitude))

# select countries
# countries <- c("China", "Italy", "Spain", "US", "Germany")
# daily_country <- filter(dat_daily, `Country/Region` == countries) 
# 
# daily_confirmed <- dat_daily %>% group_by(`Country/Region`) %>% 
#   summarise(ConfCount = sum(Confirmed, na.rm = TRUE)) %>%
#   filter(ConfCount > 5000)
# daily_confirmed
# daily_country[1:20,]

#dat_nums <- dat_confirmed[, c(5:ncol(dat_confirmed))]
#confirmed_mat <- colSums(dat_nums)
#march_nums <- confirmed_mat[, c('3/1/20', )]
#col_dates <- as.Date(names(confirmed_mat), format = "%m/%d/%y")
#plot(confirmed_mat, pch = "+")
#names(confirmed_mat)
#barplot(confirmed_mat)

########  CONFIRMED   ##########

library(tidyr)
library(magrittr)

countries <- c("China", "Italy", "US")

# load data for confirmed
dat_dates <- dat_confirmed %>% gather(Day, Cases, -c('Province/State', 'Country/Region', 'Lat', 'Long')) 

dat_tidy <- dat_confirmed %>% 
  gather(Day, Cases, -c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
  select(`Country/Region`, Day, Cases) %>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  group_by(Day, `Country/Region`) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))
  
names(dat_tidy) <- c("Day", "Country", "ConfirmedCases")
dat_countries <- NULL
for(c in countries) {
  dat_countries <- bind_rows( dat_countries, filter(dat_tidy, Country == c))
}

ggplot(dat_countries, aes(x = Day, y = ConfirmedCases, Country)) +
  geom_step(aes(color = Country), direction = "vh", type = "S") +
  annotate("text", x = as.Date("2020-03-18"), y = 79000, 
           label = paste0("China = ", dat_countries$ConfirmedCases[dim(dat_countries)[1]/3]), 
           fontface = "bold", size = 3) +
  annotate("text", x = as.Date("2020-03-20"), y = 67000, 
           label = paste0("Italy = ", dat_countries$ConfirmedCases[2 * (dim(dat_countries)[1]/3)]), 
           fontface = "bold", size = 3) +
  annotate("text", x = as.Date("2020-03-21"), y = 46000, 
           label = paste0("US = ", dat_countries$ConfirmedCases[dim(dat_countries)[1]]), 
           fontface = "bold", size = 3) +
  ggtitle("Confirmed Cases as of March 22 2020")


# deaths
dat_tidy <- dat_deaths %>% 
  gather(Day, Cases, -c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
  select(`Country/Region`, Day, Cases) %>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  group_by(Day, `Country/Region`) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

names(dat_tidy) <- c("Day", "Country", "Deaths")
head(dat_tidy)

# get data for the countries
# select countries
#countries <- c("China", "Italy", "Spain", "US", "Germany")

dat_countries <- NULL
for(c in countries) {
  dat_countries <- bind_rows( dat_countries, filter(dat_tidy, Country == c))
}
str(dat_countries)
head(dat_countries)

ggplot(dat_countries, aes(x = Day, y = Deaths, Country)) +
  geom_step(aes(color = Country), direction = "vh", type = "S") +
  annotate("text", x = as.Date("2020-03-13"), y = 3400, 
           label = paste0("China = ", dat_countries$Deaths[dim(dat_countries)[1]/3]), 
           fontface = "bold", size = 3) +
  annotate("text", x = as.Date("2020-03-19"), y = 6300, 
           label = paste0("Italy = ", dat_countries$Deaths[2 * (dim(dat_countries)[1]/3)]), 
           fontface = "bold", size = 3) +
  annotate("text", x = as.Date("2020-03-20"), y = 800, 
           label = paste0("US = ", dat_countries$Deaths[dim(dat_countries)[1]]), 
           fontface = "bold", size = 3) +
  ggtitle("Deaths as of March 22 2020")


# recovery
dat_tidy <- dat_recovered %>% 
  gather(Day, Cases, -c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
  select(`Country/Region`, Day, Cases) %>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  group_by(Day, `Country/Region`) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

names(dat_tidy) <- c("Day", "Country", "Recoveries")
head(dat_tidy)

# get data for the countries
# select countries
#countries <- c("China", "Italy", "Spain", "US", "Germany")

dat_countries <- NULL
for(c in countries) {
  dat_countries <- bind_rows( dat_countries, filter(dat_tidy, Country == c))
}
str(dat_countries)
head(dat_countries)

ggplot(dat_countries, aes(x = Day, y = Recoveries, Country)) +
  geom_step(aes(color = Country), direction = "vh", type = "S") +
  annotate("text", x = as.Date("2020-03-17"), y = 74000, 
           label = paste0("China = ", dat_countries$Recoveries[dim(dat_countries)[1]/3]), 
           fontface = "bold", size = 3) +
  annotate("text", x = as.Date("2020-03-21"), y = 8600, 
           label = paste0("Italy = ", dat_countries$Recoveries[2 * (dim(dat_countries)[1]/3)]), 
           fontface = "bold", size = 3) +
  annotate("text", x = as.Date("2020-03-21"), y = 1500, 
           label = paste0("US = ", dat_countries$Recoveries[dim(dat_countries)[1]]), 
           fontface = "bold", size = 3) +
  ggtitle("Recoveries as of March 22 2020")





### US
US_dat <- dat_dates %>%
  filter(`Country/Region` == "US")

# convert Day from 'chr' to 'Date'
US_dat %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

US_xts <- xts(US_dat$CaseCount, order.by = US_dat$Day)
names(US_xts) <- "Count"
curr_len <- nrow(US_xts)

plot(US_xts, type = "S", 
     main = paste("US Confirmed Cases:", as.numeric(US_xts$Count[curr_len]),
                  "\nAs of:", time(US_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")

### NY
NY_dat <- dat_dates %>%
  filter(`Country/Region` == "US") %>%
  filter(`Province/State` == "New York")

# convert Day from 'chr' to 'Date'
NY_dat %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

NY_xts <- xts(NY_dat$CaseCount, order.by = NY_dat$Day)
names(NY_xts) <- "Count"
curr_len <- nrow(NY_xts)
plot(NY_xts, type = "S", 
     main = paste("NY Confirmed Cases:", as.numeric(NY_xts$Count[curr_len]),
                  "\nAs of:", time(NY_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")


### Italy
IT_dat <- dat_dates %>%
  filter(`Country/Region` == "Italy")

# convert Day from 'chr' to 'Date'
IT_dat %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

IT_xts <- xts(IT_dat$CaseCount, order.by = IT_dat$Day)
names(IT_xts) <- "Count"
curr_len <- nrow(IT_xts)
plot(IT_xts, type = "S", 
     main = paste("Italy Confirmed Cases:", as.numeric(IT_xts$Count[curr_len]),
                  "\nAs of:", time(IT_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")


### triple plots
#par(mfrow = c(3, 1), mar = c(3, 3, 3, 3))
plot(IT_xts, type = "S", 
     main = paste("Italy Confirmed Cases:", as.numeric(IT_xts$Count[curr_len]),
                  "\nNY Confirmed Cases:", as.numeric(NY_xts$Count[curr_len]),
                  "\nUS Confirmed Cases:", as.numeric(US_xts$Count[curr_len]),
                  "                                                     Date:", time(IT_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
lines(NY_xts, type = "S", col = "blue")
lines(US_xts, type = "S", col = "black")

# plot(NY_Ds_xts, type = "S", 
#      main = paste("NY Deaths:", as.numeric(NY_Ds_xts$Count[curr_len])), 
#      col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
# 
# plot(US_Ds_xts, type = "S", 
#      main = paste("US Deaths:", as.numeric(US_Ds_xts$Count[curr_len])), 
#      col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")


### double plots
#par(mfrow = c(3, 1), mar = c(3, 3, 3, 3))
plot(US_xts, type = "S", 
     main = paste("\nNY Confirmed Cases:", as.numeric(NY_xts$Count[curr_len]),
                  "\nUS Confirmed Cases:", as.numeric(US_xts$Count[curr_len]),
                  "                                                     Date:", time(US_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
lines(NY_xts, type = "S", col = "blue")
#lines(US_Ds_xts, type = "S", col = "black")


########  CONFIRMED END ##########
###########  DEATHS  #############

library(tidyr)
library(magrittr)
# load data
dat_Ds <- dat_deaths %>% gather(Day, Cases, -c('Province/State', 'Country/Region', 'Lat', 'Long')) 

### US
US_Ds <- dat_Ds %>%
  filter(`Country/Region` == "US")

# convert Day from 'chr' to 'Date'
US_Ds %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

US_Ds_xts <- xts(US_Ds$CaseCount, order.by = US_Ds$Day)
names(US_Ds_xts) <- "Count"
curr_len <- nrow(US_Ds_xts)
plot(US_Ds_xts, type = "S", 
     main = paste("US Deaths:", as.numeric(US_Ds_xts$Count[curr_len]),
                  "\nAs of:", time(US_Ds_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")

### NY
NY_Ds <- dat_Ds %>%
  filter(`Country/Region` == "US") %>%
  filter(`Province/State` == "New York")

# convert Day from 'chr' to 'Date'
NY_Ds %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases))

NY_Ds_xts <- xts(NY_Ds$CaseCount, order.by = NY_Ds$Day)
names(NY_Ds_xts) <- "Count"
curr_len <- nrow(NY_Ds_xts)
plot(NY_Ds_xts, type = "S", 
     main = paste("NY Deaths:", as.numeric(NY_Ds_xts$Count[curr_len]),
                  "\nAs of:", time(NY_Ds_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")


### Italy
IT_Ds <- dat_Ds %>%
  filter(`Country/Region` == "Italy")

# convert Day from 'chr' to 'Date'
IT_Ds %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases))

IT_Ds_xts <- xts(IT_Ds$CaseCount, order.by = IT_Ds$Day)
names(IT_Ds_xts) <- "Count"
curr_len <- nrow(IT_Ds_xts)
plot(IT_Ds_xts, type = "S", 
     main = paste("Italy Deaths:", as.numeric(IT_Ds_xts$Count[curr_len]),
                  "\nAs of:", time(IT_Ds_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")


### triple plots
#par(mfrow = c(3, 1), mar = c(3, 3, 3, 3))
plot(IT_Ds_xts, type = "S", 
     main = paste("Italy Deaths:", as.numeric(IT_Ds_xts$Count[curr_len]),
                  "\nNY Deaths:", as.numeric(NY_Ds_xts$Count[curr_len]),
                  "\nUS Deaths:", as.numeric(US_Ds_xts$Count[curr_len]),
                  "                                                     Date:", time(IT_Ds_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
lines(NY_Ds_xts, type = "S", col = "blue")
lines(US_Ds_xts, type = "S", col = "black")

# plot(NY_Ds_xts, type = "S", 
#      main = paste("NY Deaths:", as.numeric(NY_Ds_xts$Count[curr_len])), 
#      col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
# 
# plot(US_Ds_xts, type = "S", 
#      main = paste("US Deaths:", as.numeric(US_Ds_xts$Count[curr_len])), 
#      col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")


### double plots
#par(mfrow = c(3, 1), mar = c(3, 3, 3, 3))
plot(US_Ds_xts, type = "S", 
     main = paste("\nNY Deaths:", as.numeric(NY_Ds_xts$Count[curr_len]),
                  "\nUS Deaths:", as.numeric(US_Ds_xts$Count[curr_len]),
                  "                                                     Date:", time(NY_Ds_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
lines(NY_Ds_xts, type = "S", col = "blue")
#lines(US_Ds_xts, type = "S", col = "black")

##########  DEATHS END  ############
##########  RECOVERY  ##############

library(tidyr)
library(magrittr)
# load data
dat_Rs <- dat_recovered %>% gather(Day, Cases, -c('Province/State', 'Country/Region', 'Lat', 'Long')) 

### US
US_Rs <- dat_Rs %>%
  filter(`Country/Region` == "US")

# convert Day from 'chr' to 'Date'
US_Rs %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

US_Rs_xts <- xts(US_Rs$CaseCount, order.by = US_Rs$Day)
names(US_Rs_xts) <- "Count"
curr_len <- nrow(US_Rs_xts)
plot(US_Rs_xts, type = "S", 
     main = paste("US Recovered Cases:", as.numeric(US_Rs_xts$Count[curr_len]),
                  "\nAs of:", time(US_Rs_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")

### NY
NY_Rs <- dat_Rs %>%
  filter(`Country/Region` == "US") %>%
  filter(`Province/State` == "New York")

# convert Day from 'chr' to 'Date'
NY_Rs %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases))

NY_Rs_xts <- xts(NY_Rs$CaseCount, order.by = NY_Rs$Day)
names(NY_Rs_xts) <- "Count"
curr_len <- nrow(NY_Rs_xts)
plot(NY_Rs_xts, type = "S", 
     main = paste("NY Recovered Cases:", as.numeric(NY_Rs_xts$Count[curr_len]),
                  "\nAs of:", time(NY_Rs_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")


### Italy
IT_Rs <- dat_Rs %>%
  filter(`Country/Region` == "Italy")

# convert Day from 'chr' to 'Date'
IT_Rs %<>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases))

IT_Rs_xts <- xts(IT_Rs$CaseCount, order.by = IT_Rs$Day)
names(IT_Rs_xts) <- "Count"
curr_len <- nrow(IT_Rs_xts)
plot(IT_Rs_xts, type = "S", 
     main = paste("Italy Recovered Cases:", as.numeric(IT_Rs_xts$Count[curr_len]),
                  "\nAs of:", time(IT_Rs_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")



### triple plots #####
#par(mfrow = c(3, 1), mar = c(3, 3, 3, 3))
plot(IT_Rs_xts, type = "S", 
     main = paste("Italy Recovery:", as.numeric(IT_Rs_xts$Count[curr_len]),
                  "\nNY Recovery:", as.numeric(NY_Rs_xts$Count[curr_len]),
                  "\nUS Recovery:", as.numeric(US_Rs_xts$Count[curr_len]),
                  "                                                     Date:", time(IT_Ds_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
lines(NY_Rs_xts, type = "S", col = "blue")
lines(US_Rs_xts, type = "S", col = "black")


### double plots #####
#par(mfrow = c(3, 1), mar = c(3, 3, 3, 3))
plot(US_Rs_xts, type = "S", 
     main = paste("\nNY Recovery:", as.numeric(NY_Rs_xts$Count[curr_len]),
                  "\nUS Recovery:", as.numeric(US_Rs_xts$Count[curr_len]),
                  "                                                     Date:", time(NY_Rs_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")
lines(NY_Rs_xts, type = "S", col = "blue")




#########  RECOVERY END ###########
#########  Worldwide ##############

#filter(dat_dates, `Country/Region` == "Canada")

day_dat <- dat_dates %>%
  filter(Day == "1/22/20")

CR_dat <- dat_dates %>%
  group_by('Country/Region')


# convert Day from 'chr' to 'Date'
dat_conf <-
  mutate(dat_dates, Day = as.Date(Day, format = "%m/%d/%y")) %>%
  select(c(Day, Cases)) %>%
  group_by(Day) %>%
  summarise(CaseCount = sum(Cases, na.rm = TRUE))

dat_xts <- xts(dat_conf$CaseCount, order.by = dat_conf$Day)
names(dat_xts) <- "Count"
curr_len <- nrow(dat_xts)
par(mar = c(5, 5, 5, 5))
getOption("scipen")
opt <- options("scipen" = 20)
getOption("scipen")
plot(dat_xts, type = "S", 
     main = paste("Worlwide Confirmed Cases:", as.numeric(dat_xts$Count[curr_len]),
                  "\nAs of:", time(dat_xts[curr_len])), 
     col = "darkgreen", grid.col = "lightgrey", grid.ticks.lty = "dotted")

#ts.plot(dat_xts, main = "Test", type = "h")
#legend("top", legend = c("Test"))
#text(dx, dy, "Test")

plot(stks, legend.loc='top', major.ticks = "years",
     grid.ticks.on = "years", grid.col = "lightgray")

dat_dates %>%
  ggplot(aes(Day, CaseCount)) +
  geom_step() +
  annotate("text", x = as.Date("2020-02-01"), y = 100000, 
           label = paste("Worldwide Confirmed cases:", dat_dates[nrow(dat_dates), 2], "as of", dat_dates[[nrow(dat_dates), ]]))

library(scales)
dat_dates %>%
  ggplot(aes(Day, CaseCount)) +
  geom_point() +
  scale_y_continuous(name = "Number of Cases", labels = comma) +
  annotate("text", x = as.Date("2020-02-01"), y = 100000, 
           label = paste("Worldwide Confirmed cases:", dat_dates[nrow(dat_dates), 2], 
                         "as of", dat_dates[nrow(dat_dates), 1][[1]]))

dat_dates %>%
  ggplot(aes(Day, CaseCount)) +
  geom_path() +
  scale_y_continuous(name = "Number of Cases", labels = comma) +
  annotate("text", x = as.Date("2020-02-01"), y = 100000, 
           label = paste("Worldwide Confirmed cases:", dat_dates[nrow(dat_dates), 2], 
                         "as of", dat_dates[nrow(dat_dates), 1][[1]]))


dxy <- par("usr") 
dx <- dxy[2] - dxy[1]
dx <- 0.15 * dx + dxy[1]
dy <- dxy[4] - dxy[3]
dy <- 0.75 * dy + dxy[3]


### ggplot
# Create the object containing the data and aes layers: dia_plot
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# Add a geom layer with + and geom_point()
dia_plot + geom_point()

# Add the same geom layer, but with aes() inside
dia_plot + geom_point(aes(color = clarity))

### mtcars
# Use lm() to calculate a linear model and save it as carModel
carModel <- lm(mpg ~ wt, data = mtcars)

# Basic plot
mtcars$cyl <- as.factor(mtcars$cyl)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Call abline() with carModel as first argument and set lty to 2
abline(carModel, lty = 2)

# Plot each subset efficiently with lapply
# You don't have to edit this code
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})

# This code will draw the legend of the plot
# You don't have to edit this code
legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")



### ggploting
# Convert cyl to factor (don't need to change)
mtcars$cyl <- as.factor(mtcars$cyl)

# Example from base R (don't need to change)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
abline(lm(mpg ~ wt, data = mtcars), lty = 2)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})
legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")

# Plot 1: add geom_point() to this command to create a scatter plot
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point()  # Fill in using instructions Plot 1

# Plot 2: include the lines of the linear models, per cyl
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() + # Copy from Plot 1
  geom_smooth(method = "lm", se = FALSE)   # Fill in using instructions Plot 2

# Plot 3: include a lm for the entire dataset in its whole
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() + # Copy from Plot 2
  geom_smooth(method = "lm", se = FALSE) +  # Copy from plot 2
  geom_smooth(aes(group = 1), method = "lm", 
              se = FALSE, linetype = 2)   # Fill in using instructions Plot 3

### data layer properly represented
# Load the tidyr package
library(tidyr)

# Fill in the ___ to produce to the correct iris.tidy dataset
iris.tidy <- iris %>%
  gather(key, Value, -Species) %>%
  separate(key, c("Part", "Measure"), "\\.")

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() + 
  geom_point(aes(x = Petal.Length, y = Petal.Width), col = "red")
ggplot(iris.tidy, aes(x = Length, y = Width, col = Part)) +
  geom_point()

### widen data
# Load the tidyr package
library(tidyr)

# Add column with unique ids (don't need to change)
iris$Flower <- 1:nrow(iris)

# Fill in the ___ to produce to the correct iris.wide dataset
iris.wide <- iris %>%
  gather(key, value, -Species, -Flower) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread(Measure, value)

### aes
# All about aesthetics, part 1
# In the video you saw 9 visible aesthetics. Let's apply them to a categorical 
# variable - the cylinders in mtcars, cyl.
# 
# (You'll consider line type when you encounter line plots in the next chapter).
# 
# These are the aesthetics you can consider within aes() in this chapter:
# x, y, color, fill, size, alpha, labels and shape.
# 
# In the following exercise you can assume that the cyl column is categorical. 
# It has already been transformed into a factor for you.

# 1 - Map mpg to x and cyl to y
ggplot(mtcars, aes(mpg, cyl)) +
  geom_point()

# 2 - Reverse: Map cyl to x and mpg to y
ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

# 3 - Map wt to x, mpg to y and cyl to col
ggplot(mtcars, aes(wt, mpg, colour = cyl)) +
  geom_point()

# 4 - Change shape and size of the points in the above plot
ggplot(mtcars, aes(wt, mpg, colour = cyl)) +
  geom_point(shape = 1, size = 4)

### aes 2 -> https://campus.datacamp.com/courses/data-visualization-with-ggplot2-1/chapter-3-aesthetics?ex=3
# am and cyl are factors, wt is numeric
class(mtcars$am)
class(mtcars$cyl)
class(mtcars$wt)

# From the previous exercise
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)

# 1 - Map cyl to fill
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 1, size = 4)

# 2 - Change shape and alpha of the points in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)


### aes 3 --> https://campus.datacamp.com/courses/data-visualization-with-ggplot2-1/chapter-3-aesthetics?ex=4
# Map cyl to size
ggplot(mtcars, aes(wt, mpg, size = cyl)) +
  geom_point()

# Map cyl to alpha
ggplot(mtcars, aes(wt, mpg, alpha = cyl)) +
  geom_point()

# Map cyl to shape 
ggplot(mtcars, aes(wt, mpg, shape = cyl)) +
  geom_point()

# Map cyl to label
ggplot(mtcars, aes(wt, mpg, label = cyl)) +
  geom_text()


### attributes 2 --> https://campus.datacamp.com/courses/data-visualization-with-ggplot2-1/chapter-3-aesthetics?ex=6
#Expand to draw points with alpha 0.5
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(alpha = 0.5)

# Expand to draw points with shape 24 and color yellow
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 24, col = "yellow")

# Expand to draw text with label rownames(mtcars) and color red
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_text(color = "red", label = rownames(mtcars))


# 3 - Map am to col in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl, col = am)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)


### attributes and aes
# Map mpg onto x, qsec onto y and factor(cyl) onto col
ggplot(mtcars, aes(mpg, qsec, col = factor(cyl))) +
  geom_point()

# Add mapping: factor(am) onto shape
ggplot(mtcars, aes(mpg, qsec, col = factor(cyl), shape = factor(am))) +
  geom_point()

# Add mapping: (hp/wt) onto size
ggplot(mtcars, aes(mpg, qsec, col = factor(cyl), shape = factor(am), size = (hp/wt))) +
  geom_point()

cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))

cyl.am + 
  geom_bar(position = "stack")

# Fill - show proportion
cyl.am + 
  geom_bar(position = "fill")  

# Dodging - principles of similarity and proximity
cyl.am +
  geom_bar(position = "dodge") 

# Clean up the axes with scale_ functions
val = c("#E41A1C", "#377EB8")
lab = c("Manual", "Automatic")
cyl.am +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Number") +
  scale_fill_manual("Transmission", 
                    values = val,
                    labels = lab) 

### dummy aesthetic
# 1 - Create jittered plot of mtcars, mpg onto x, 0 onto y
ggplot(mtcars, aes(x = mpg, y = 0)) +
  geom_jitter()

# 2 - Add function to change y axis limits
ggplot(mtcars, aes(x = mpg, y = 0)) +
  geom_jitter() +
  scale_y_continuous(limits = c(-2, 2))


### Overplotting 2 --> https://campus.datacamp.com/courses/data-visualization-with-ggplot2-1/chapter-3-aesthetics?ex=14
# Scatter plot: carat (x), price (y), clarity (color)
ggplot(diamonds, aes(carat, price, col = clarity)) +
  geom_point()


# Adjust for overplotting
ggplot(diamonds, aes(carat, price, col = clarity)) +
  geom_point(alpha = 0.5)


# Scatter plot: clarity (x), carat (y), price (color)
ggplot(diamonds, aes(clarity, carat, col = price)) +
  geom_point(alpha = 0.5)


# Dot plot with jittering
ggplot(diamonds, aes(clarity, carat, col = price)) +
  geom_point(alpha = 0.5, position = "jitter")


### Scatter plots and jittering 2 -->  https://campus.datacamp.com/courses/data-visualization-with-ggplot2-1/chapter-4-geometries?ex=3


# head(dat_dates)
# day_cases <- dat_dates[, c("Day", "Cases")]
# day_cases %>%
#   group_by(Day) %>%
#   mutate(CasesTotal = sum(Cases))
# 
# day_cases %<>%
#   group_by(Day) %>%
#   summarise(CaseCount = sum(Cases))

# day_cases_xts <- xts(day_cases$CaseCount, order.by = day_cases$Day)
# names(day_cases_xts) <- "Count"
# plot.ts(day_cases_xts)
# plot(day_cases_xts)
# 
# plot.ts(test00)
# barplot(test00)


### plotting 2 data frames ##############
# Basic line plot
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line()

# Expand the following command with geom_rect() to draw the recess periods
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess,
            aes( xmin = begin, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_line()


# Multiple time series, part 1
# In the data chapter we discussed how the form of your data affects how you 
# can plot it. Here, you'll explore that topic in the context of multiple time series.
# 
# The dataset you'll use contains the global capture rates of seven salmon species from 1950 - 2010.
# 
# In your workspace, the following dataset is available:
#   
#   fish.species: Each variable (column) is a Salmon Species and each observation (row) is one Year.
# To get a multiple time series plot, however, both Year and Species should be in their own column. 
# You need tidy data: one variable per column. Once you have that you can get the plot shown in the 
# viewer by mapping Year to the x aesthetic and Species to the color aesthetic.
# 
# You'll use the gather() function of the tidyr package, which is already loaded for you.
# 
# Instructions
# 100 XP
# Use gather() to move from fish.species to a tidy data frame, fish.tidy. This data frame should 
# have three columns: Year (int), Species (factor) and Capture (int).
# gather() takes four arguments: the original data frame (fish.species), the name of the key 
# column (Species), the name of the value column (Capture) and the name of the grouping variable, 
# with a minus in front (-Year). They can all be specified as object names (i.e. no "").

# Check the structure as a starting point
#str(fish.species)

# Use gather to go from fish.species to fish.tidy
#fish.tidy <- gather(fish.species, Species, Capture, -Year)

# Recreate the plot shown on the right
# ggplot(fish.tidy, aes(x = Year, y = Capture, color = Species)) +
#   geom_line()

library(AlphaVantageClient)
AlphaVantageClient::setAPIKey("5WDQCT252ZT5CSKB")
example_prices <- fetchSeries(function_nm = "time_series_daily", symbol = "msft")
example_quotes <- fetchSeries(function_nm = "currency_exchange_rate", from_symbol = "EUR", to_symbol = "USD", datatype = "csv")

library(xts)

library(tidyverse)
dat_confirmed <- read_csv("./time_series_19-covid-Confirmed.csv")
dat_deaths <- read_csv("./time_series_19-covid-Deaths.csv")
dat_recovered <- read_csv("./time_series_19-covid-Recovered.csv")
eurusd <- read_csv("./fx_daily_EUR_USD.csv")
