# Dataset link ###
https://www.kaggle.com/datasets/aiaiaidavid/the-big-dataset-of-ultra-marathon-running?resource=download
# Packages ####
library(readr)
library(tidyverse)

# Import database ####

df <- read_csv("input/UM_RACES.csv")


# Treatment ####

View(df)
# show the first data
glimpse(df)

# data dimension
dim(df)

# types of columns
str(df)

# filter the data with the event distance/length of 50km/50mi and in the USA of 2022
filtered_data <- subset(df,(df$`Event distance/length`== "50km") | (df$`Event distance/length`== "50mi") )
view(filtered_data)

dim(filtered_data)
str(filtered_data)
split_result <- strsplit(filtered_data$`Event name`, "\\(") 
before_bracket <- sapply(split_result, function(x) x[2])
s <- strsplit(before_bracket,"\\)")
b <- sapply(s, function(x) x[1])

filtered_data$state <- b

filtered_data <- subset(filtered_data, filtered_data$state == "USA")

# we removed the h from the athlete performance
q<- strsplit(filtered_data$`Athlete performance`,"\\ ")
p<- sapply(q, function(x) x[1])
filtered_data$`Athlete performance` <- p


# select only the 2022 of data

filtered_data <- subset(filtered_data, filtered_data$`Year of event` == 2020)

filtered_data$athlete_age <- as.integer(2020 - filtered_data$`Athlete year of birth`) 

dim(filtered_data)
str(filtered_data)


# remove this columns ["Athlete club","Athlete country", "Athlete year of birth","Athlete age category"]

new <- filtered_data %>% 
  select(-"Athlete club",-"Athlete country", -"Athlete year of birth",-"Athlete age category")

summary(new)
dim(new)

names(new)

# remove all the NA

nombre_de_na <- sum(is.na(new))

new <- na.omit(new)

# see if their is duplicated values

nb_duplicated <- subset(new,duplicated(new))

# now we gonna fix the type of the recent filtered data

str(new)

glimpse(new)

names(new)


# change columns names 

names(new)[1] <-"year"  
names(new)[2] <-"race_day"
names(new)[3] <- "race_name"
names(new)[4] <- "race_length"
names(new)[5] <- "race_number_of_finishers"
names(new)[6] <- "athlete_performance"
names(new)[7] <- "athlete_gender"
names(new)[8] <- "athlete_average_speed"
names(new)[9] <- "athlete_id"

# now our data is well cleaned and we can proceed to charts and analysis

new %>%
  ggplot(aes(race_length,fill=athlete_gender))+ geom_histogram(stat = "count") +
  labs(title="Distribution of the race_length")+
  theme_bw()

subset(new,new$race_length == "50km") %>%
  ggplot(aes(athlete_average_speed))+ geom_histogram(stat="count",colour="blue")+
  labs(title="Distribution of the average speed in the 50km race length")+
  theme_bw()

subset(new,new$race_length == "50mi") %>%
  ggplot(aes(athlete_average_speed))+ geom_histogram(stat="count",colour="blue")+
  labs(title="Distribution of the average speed in the 50mi race length")+
  theme_bw()

new %>%
  ggplot(aes(race_length,athlete_average_speed,fill=athlete_gender))+
  geom_violin(scale="width",position = "dodge")+
  facet_wrap(~year)+
  theme_minimal()

new %>%
  ggplot(aes(athlete_age,athlete_average_speed,colour=athlete_gender))+
  geom_point(size=3)+
  geom_smooth(method =lm,se=F)+
  theme_minimal()

#find some questions

