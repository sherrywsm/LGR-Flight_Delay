# Q1. Understand the Data (Range, Data type, Visualize the variables, etc.)
setwd("~/Dropbox/EBAC/workshop-data/SB")
delay <- read.csv('Delay_v3.csv')
head(delay)
str(delay)
delay$Flight_DATE <- as.Date(delay$Flight_DATE, format = "%d/%m/%Y")
summary(delay)
library(dplyr)
library(ggplot2)
delay %>%
  group_by(Airline) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(Airline), y = count)) +
  geom_bar(stat = 'identity')
delay %>%
  group_by(Destination) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(Destination), y = count)) +
  geom_bar(stat = 'identity')
delay %>%
  group_by(ORIGIN) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(ORIGIN), y = count)) +
  geom_bar(stat = 'identity')
delay %>%
  group_by(Bad_Weather) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(Bad_Weather), y = count)) +
  geom_bar(stat = 'identity')
delay %>%
  group_by(Flight.Status) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(Flight.Status), y = count)) +
  geom_bar(stat = 'identity')
hist(delay$DEP_TIME)
hist(delay$Distance)
hist(delay$DAY_WEEK)

# Q2. Remove missing values
apply(delay, 2, function(col)sum(is.na(col)))
missing <- delay[rowSums(is.na(delay)) > 0,]
delay_no_missing <- delay[rowSums(is.na(delay)) == 0,]
  
# Q3. Remove duplicated in the dataset
delay_no_dup <- unique(delay_no_missing)

# Q4. Convert the data type
# a.	Convert to factor: Weather, Day_week
delay_no_dup$Bad_Weather <- factor(delay_no_dup$Bad_Weather) 
delay_no_dup$DAY_WEEK <- factor(delay_no_dup$DAY_WEEK)

# b.	Convert Scheduled “DEP_TIME” to Date data type
pacman::p_load(tidyverse, caret, corrplot, caTools, FRACTION, lubridate,IRdisplay)
### temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(delay_no_dup$DEP_TIME))
### delay_no_dup$DEP_TIME <- paste0(temp2, delay_no_dup$DEP_TIME)
### delay_no_dup$DEP_TIME <- format(strptime(delay_no_dup$DEP_TIME, format="%H%M"), format = "%H:%M")
install.packages("datetime")
library(datetime)
delay_no_dup$DEP_TIME <- as.time(delay_no_dup$DEP_TIME)

# Q5. Insights
# 5.1 Which day of the week sees the most delays?
library(dplyr)
library(ggplot2)
delay_no_dup %>%
  group_by(Flight.Status, DAY_WEEK) %>%
  summarize(count_level = n()) %>%
  ggplot(aes(x = as.factor(DAY_WEEK), y = count_level, fill = as.factor(Flight.Status))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = count_level),
            position = position_stack(vjust = .5), vjust = 2)

# 5.2 Which Destination sees the most delay?
delay_no_dup %>%
  group_by(Flight.Status, Destination) %>%
  summarize(count_level = n()) %>%
  ggplot(aes(x = as.factor(Destination), y = count_level, fill = as.factor(Flight.Status))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = count_level),
            position = position_stack(vjust = .5), vjust = 2)
  
# Q6. Predict flights delay by using logistic regression and evaluate model performance
delay_no_dup$Flight.Status <- factor(delay_no_dup$Flight.Status,
                                     levels = c("ontime","delayed"),
                                     labels = c(0,1))

# Model
delay_fit <- glm(Flight.Status ~ . - Flight_NUM, family = binomial, data = delay_no_dup)
summary(delay_fit)

# Likelihood ratio test
attach(delay_fit)
pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
# logistic function outcome
prob <- predict(delay_fit, type = 'response')
prob
# threshold
threshold <- sum(delay_no_dup$Flight.Status == "1")/length(delay_no_dup$Flight.Status)
predict <- ifelse(prob > threshold, 1, 0)
# contigency table
table(delay_no_dup$Flight.Status, predict)
