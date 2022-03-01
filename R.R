# Write a line (without the #), command + shift to execute

# You can do math in R
1+1
2*2

# Functions
sin(1)
log(10)

# Comparisons
1 == 1
1 != 1
1 <= 1
1 >= 3

# Variables
x <- 1
y <- 2
z = x + y

# Vectors
1:5
vector <- 1:5
vector2 <- c(1,2,3,4,5)

# Removing variables
# Can remove some variables by rm(variable name)
# Can remove all variables by:
rm(list=ls())

# Search a function by: ?functionname

#See all your variables by ls()

#Packages
install.packages('knitr')
library(knitr)
install.packages('ggplot2')
library(ggplot2)
install.packages('gridExtra')
library(gridExtra)
install.packages('lubridate')
library(lubridate)
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)
install.packages('tidyverse')
library(tidyverse)

installed.packages()

# Loading data
# Don't forget to set your working directory - session, choose directory, ...

gapminder <-read.csv('gapminder.csv')
view(gapminder)
str(gapminder)

# Multiple datatypes
x <- 1
class(x) # numeric
x <- 1.1
class(x) # numeric
x <- '1'
class(x) # character

rm(x)

# Converting datatypes
x <- 1
class(x) # numeric
x <- as.character(x)
class(x) # character
x <- as.integer(x)
class(x) # integer

rm(x)

# View data and columns
gapminder
gapminder$country
unique(gapminder$country)
length(gapminder$country)
length(unique(gapminder$country))
summary(gapminder$country)

# Subsetting
head(gapminder) # returns the first 6 rows
head(gapminder, 3) # returns the first 3 rows
tail(gapminder) # returns the lasst 6 rows
head(gapminder, 3) # returns the lasst 3 rows

gapminder[1,1]
gapminder[1,]
gapminder[1:5, c(1,2,3)]

gapminder[gapminder$country == 'Afghanistan' & gapminder$year == 1952, 'lifeExp']
# This shows us Afghanistan's life expectancy in 1951

gapminder[gapminder$country == 'Afghanistan' & gapminder$year == 1952, c('lifeExp', 'gdpPercap')]

# Create a new file
headgap <- head(gapminder)
# write.table(headgap, file = 'MyFileName.csv', sep = ',', quote = FALSE, row.names = FALSE)
rm(headgap)

# Other functions
min(gapminder$lifeExp)
max(gapminder$lifeExp)
mean(gapminder$lifeExp)
sd(gapminder$lifeExp)

rnorm(1,2,3) # one number, population mean of three, std dev of two

rep(1,4) # repeat 1 four times

seq(1,2,3) # start, stop, by

# Markdowns
# File, new file, R Markdown, add title and author, click yarn ball to knit

# Select()
gapcountry <- select(gapminder, country)
rm(gapcountry)

gapyear_country_lifeexp <- select(gapminder, year, country, lifeExp)
rm(gapyear_country_lifeexp)

# Filter()
afghanistan <- filter(gapminder, country == 'Afghanistan')
rm(afghanistan)

afghanistan1952 <- filter(gapminder, country == 'Afghanistan' & year =='1952')
rm(afghanistan1952)

afghanistan_albania <- filter(gapminder, country == 'Afghanistan' | country == 'Albania')
rm(afghanistan_albania)

# Command + shift + m = %>%
gapeu <- gapminder %>% 
  filter(continent == 'Europe') %>% 
  select(lifeExp, country, year)
rm(gapeu)

# Select is for columns, Filter is for rows

# Mutating data
gapgdp <- gapminder %>% 
  mutate(GDP = gdpPercap * pop)
rm(gapgdp)

yes <- gapminder %>% 
  mutate(yes = 'yes')
rm(yes)

# Ifelse() and count()
gap50 <- gapminder %>% 
  mutate(over50 = ifelse(lifeExp >= 50, 'over 50', 'under 50'))

gap50count <- gap50 %>% 
  filter(over50 == 'under 50') %>% 
  count()

gap50count <- gap50 %>% 
  count(continent, sort = TRUE)

rm(gap50, gap50count)

# Group_by() and summarize()
gdppercap_continent <- gapminder %>% 
  group_by(continent) %>%
  summarize(meanGDPpercap = mean(gdpPercap))

lifeexp_country <- gapminder %>% 
  group_by(country) %>% 
  summarize(avglifeexp = mean(lifeExp))
lifeexp_country <- lifeexp_country %>% 
  filter(avglifeexp == min(avglifeexp) | avglifeexp == max(avglifeexp))

rm(gdppercap_continent, lifeexp_country)

# Tidy data

# Rename()
gapgdp <- gapminder %>% 
  rename(GDPpc = gdpPercap)
rm(gapgdp)

names(gapminder) # retrieve column names

# Add rows (vertical)
newrow <- data_frame(country = 'USA',
                     year = 2222,
                     pop = 1234567,
                     continent = 'Americas',
                     lifeExp = 100,
                     gdpPercap = 200)

gapminder_newrow <- rbind(gapminder, newrow)                     
rm(newrow, gapminder_newrow)                     

# Add columns (horizontal)
set.seed(1)
newcolumn <- rnorm(1704)

gapminder_newcolumn <- cbind(gapminder, newcolumn)

rm(newcolumn, gapminder_newcolumn)

# Separate()
test <- data_frame(x = c(1,2,3), y = c('1-2', '2-3', '3-4'))
test

test2 <- separate(test, col=y, into = c('first', 'second'), sep = '-')
test2

rm(test, test2)

# Missing data

heights <- c(1,2,NA,4,5,NA,7)
mean(heights)
max(heights)
# Returns as NA, do this:
mean(heights, na.rm = TRUE)
max(heights, na.rm = TRUE)

# Removing NA values is a last resort
heights <- na.omit(heights)
heights

is.na(heights)

heights <- c(1,2,NA,4,5,NA,7)
weights <- seq(1,7)
heightsandweights <- cbind(heights, weights)

weights[is.na(heights)]

rm(heights, weights, heightsandweights)

# Multiple comparisons
gap <- gapminder %>% 
  filter(country == 'Canada' | country == 'Mexico' | country == 'United States')
# Is the same thing as...
gap <- gapminder %>% 
  filter(country %in% c('Canada', 'Mexico', 'United States'))

rm(gap)

# Plotting
# ggplot cheatsheet is available at: https://www.rstudio.com/resources/cheatsheets/
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point()

# Save plots by:
  # Exporting under the plots section or...
  # ggsave('MyFileName.pdf') or...
  # pdf('MyFileName.pdf', width = 8, height = 6) ggplot() dev.off()
# Be sure to run all lines together at the same time

# Themes - check out ggthemes package
ggplot(data = gapminder, aes(x = pop, y = lifeExp)) + geom_point() + theme_classic()
ggplot(data = gapminder, aes(x = pop, y = lifeExp)) + geom_point() + theme_minimal()
ggplot(data = gapminder, aes(x = pop, y = lifeExp)) + geom_point() + theme_bw()

# Line plots
g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country))
g <- g + geom_line()
g

# Colors
g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country, color = continent))
g <- g + geom_line()
g

# Add points
g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country, color = continent))
g <- g + geom_line()
g <- g + geom_point(color = 'black')
g
# Changing the order of geom_line() and geom_point() make a visual difference

# Manually select colors
g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country, color = continent))
g <- g + geom_point(color = 'black')
g <- g + geom_line(color = 'darkOliveGreen')
g

g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country, color = continent))
g <- g + geom_line()
g <- g + scale_color_manual(values = c('red', 'pink', 'yellow', 'black', 'green'))
g

# Labels
g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country, color = continent))
g <- g + geom_line()
g <- g + labs(title = 'Life Expectancy over Time', x = 'Year', y = 'Life Expectancy')
g

# Change x and y axes
g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country, color = continent))
g <- g + geom_line()
g <- g + labs(title = 'Life Expectancy over Time', x = 'Year', y = 'Life Expectancy')
g <- g + scale_x_continuous(limits = c(1952,2007), breaks = seq(1952,2007,10), labels = seq(1952,2007,10))
g <- g + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10), labels = seq(0,100,10))
g

# Subsetting and plotting
gapoceania <- gapminder %>% 
  filter(continent == 'Oceania')

g <- ggplot(data = gapoceania, aes(x = year, y = lifeExp))
g <- g + geom_point()
g

g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country))
g <- g + geom_line(color = 'red')
g <- g + geom_line(data = gapoceania, aes(x = year, y = lifeExp, by = country), color = 'black')
g
# Order matters...
g <- ggplot(data = gapminder, aes(x = year, y = lifeExp, by = country))
g <- g + geom_line(data = gapoceania, aes(x = year, y = lifeExp, by = country), color = 'black')
g <- g + geom_line(color = 'red')
g

gapasia <- gapminder %>% 
  filter(continent == 'Asia')
g <- ggplot(data = gapminder, aes(x = year, y = pop))
g <- g + geom_point(color = 'blue')
g <- g + geom_point(data = gapasia, aes(x = year, y = pop), color = 'yellow')
g

rm(gapasia, gapoceania)

# Log axes
g <- ggplot(data = gapminder, aes(x = year, y = pop))
g <- g + geom_point(color = 'blue')
g <- g + scale_y_log10()
g <- g + labs(y = 'log(pop)')
g

# Transparency
g <- ggplot(data = gapminder, aes(x = year, y = pop))
g <- g + geom_point(color = 'blue', alpha = 0.05)
g <- g + scale_y_log10()
g <- g + labs(y = 'log(pop)')
g

# Jitter
g <- ggplot(data = gapminder, aes(x = year, y = pop))
g <- g + geom_jitter(color = 'blue', alpha = 0.05)
g <- g + scale_y_log10()
g <- g + labs(y = 'log(pop)')
g

g <- ggplot(data = gapminder, aes(x = year, y = pop))
g <- g + geom_jitter(color = 'blue', alpha = 0.05, width = 1, height = 0)
g <- g + scale_y_log10()
g <- g + labs(y = 'log(pop)')
g

# Trend line
g <- ggplot(data = gapminder, aes(x = year, y = pop))
g <- g + geom_jitter(color = 'blue', alpha = 0.05, width = 1, height = 0)
g <- g + scale_y_log10()
g <- g + geom_smooth(method = 'lm', size = 2)
g <- g + labs(y = 'log(pop)')
g

# Boxplots
gap2007 <- gapminder %>% 
  filter(year == 2007)
g <- ggplot(data = gap2007, aes(x = continent, y = pop))
g <- g + geom_boxplot()
g <- g + scale_y_log10()
g

rm(gap2007)

# Barplots
meanandsdlifeexpcontinent <- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(MeanLifeExp = mean(lifeExp), SDLifeExp = sd(lifeExp))

g <- ggplot(data = meanandsdlifeexpcontinent, aes(x = continent, y = MeanLifeExp))
g <- g + geom_col()
g <- g + geom_errorbar(aes(ymin = MeanLifeExp - SDLifeExp, ymax = MeanLifeExp + SDLifeExp, width = 0.2))
g

rm(meanandsdlifeexpcontinent)

# Multipanel plots
gapeu <- gapminder %>% 
  filter(continent == 'Europe')

g <- ggplot(data = gapeu, aes(x = year, y = pop))
g <- g + geom_line()
g <- g + facet_wrap(~country)
g

# Axis formatting
g <- ggplot(data = gapeu, aes(x = year, y = pop))
g <- g + geom_line()
g <- g + facet_wrap(~country)
g <- g + theme(axis.text.x = element_text(angle = 45))
g

# That's All For Now!
