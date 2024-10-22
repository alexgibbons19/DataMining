# Alex Gibbons
# March 18, 2024
# CMPT 363 – Data Mining
# Tidyverse code and output
# install.packages("tidyverse")
library("tidyverse")
library("readxl")


mortality_data <- read_excel("C:/Users/Kelly/Downloads/child_mortality_rates.xlsx",sheet=1)
# printing tibble displays first 10 rows of tibble by default
mortality_data
# use print function to print specific num rows
print(mortality_data,n=150)
# to view top and bottom of dataset use head and tail function
head(mortality_data,10) # if n is not specified, 6 is default
tail(mortality_data)

# summary statistics of data set
summary(mortality_data)
# to only get certain columns use select function
select(mortality_data, Year, "01-04 Years")

# to select all BUT certain columns, preceded column name with -
select(mortality_data, -Year)

# combine with summary function to get a summary of column data
summary(select(mortality_data,"01-04 Years"))

# may need to melt data from wide to long
# use pivot_longer function
mortality_long <- pivot_longer(mortality_data,cols=c("01-04 Years","05-09 Years","10-14 Years","15-19 Years"),names_to = "AgeGroup", values_to = "DeathRate")
mortality_long

# use mutate and rename functions to directly modify data within the tibble
mortality_long<-mutate(mortality_long,DeathRate=DeathRate * 100000)
# death rate multiplied by 100,000 to get num people dead

# add calculated column for respective decade
mortality_long <- mutate(mortality_long, Decade = (Year%/%10) *10)

# modify two columns in a single statement
# combine two previous commands
mortality_long <- mutate(mortality_long,DeathRate=DeathRate*100000,Decade=(Year%/%10)*10)

# rename DeathRate column
mortality_long<-rename(mortality_long, DeathsPer100K=DeathRate)

# saving tibble to file
# use saveRDS function
saveRDS(mortality_long,"C:/Users/Kelly/Downloads/mortality_long.rds")
# use readRDS funcition to read tibble from file
readRDS("C:/Users/Kelly/Downloads/mortality_long.rds")

# common functions for calculating summary columns for a tibble:
#   group_by, summarize, n, mean
# pipe operator is also common when summarizing grouped data
#   Ctrl+Shift+M - pipe operator shortcut

# calculate mean num death per 100K for each decade and find num rows each decade
mortality_long %>% group_by(Decade) %>% summarize(MeanDeaths = mean(DeathsPer100K),Count = n())

# visualizing data using ggplot
# ggplot(data,mapping)
# aes(x,y,color,fill)
# geom_line()

ggplot(mortality_long, aes(x=Year,y=DeathsPer100K,color=AgeGroup)) + geom_line()

