#load from file into dataframe
quiz1 <- read.csv('hw1_data.csv')
# get names of columns in dataset
names(quiz1)
# get number of rows in dataset
nrow(quiz1)
#get number of cols in dataset
ncol(quiz1)
#extract first two rows in dataset
head(quiz1,2)
#extract last two rows in dataset
tail(quiz1,2)
# how many missing values for Ozone
nrow(subset(quiz1, is.na(Ozone)))
#mean of Ozone in dataset exclude NAs
# na.rm logical value indicates whether NA values should be stripped
# before computation
mean(quiz1$Ozone, na.rm = TRUE)
# mean of Solar.R column where Ozone > 31 and Temp > 90
# create new dataframe from subset
sr <- subset(quiz1, Ozone> 30 & Temp > 90)
# calculate mean
mean(sr$Solar.R)
# mean of Temp when month =6
t <- subset(quiz1, Month == 6)
mean(t$Temp)
# max of Ozone when month = 5
oz <- subset(quiz1, Month == 5)
max(oz$Ozone, na.rm =TRUE)
# get value of Ozone in the 47th row
quiz1$Ozone[47]
