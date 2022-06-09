#####Problem 1#####
# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:

# FactorialFunction <- function(inputNumber){
#   ???
#     return(Result)
# }
#####Problem 1#####

FactorialFunction <- function(inputNumber){
  if (inputNumber == 0) {
    Result <- 1
  } else {
    
    Result <- inputNumber
    
    while(inputNumber > 1){
      Result <- (inputNumber - 1) * Result
      inputNumber <- inputNumber - 1
    }
  }
  return(Result)
}

FactorialFunction (7)


#####Problem 2#####
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector)
#####Problem 2#####

SDFunction <- function(inputVector){
  Den = sum(inputVector)/length(inputVector)
  Nom = sum((inputVector - Den)^2)
  FinalResult <- sqrt(sum((inputVector - Den)^2)/(length(inputVector)-1))
  return(FinalResult)
}
try <- c(5,9,2,6)
SDFunction(try)
sd(try)


#####Problem 3#####
# Read everything from https://r4ds.had.co.nz/transform.html, 
# in particular chapters 5.6/5.7

#Do all the exercises:
# 5.6.7 Exercises 
# 5.7.1 Exercises
#####Problem 3#####

library(tidyverse)
library(nycflights13)

nycflights13::flights
dd <- flights

NC <- flights %>%
  filter (!is.na(air_time))


#####Problem 4#####
#Find the following:
#4.1 For each carrier what is the most common destination?
#4.2 For each carrier what is the biggest delay?
#4.3 Which are the three plane which have flown the most/least miles?
#4.4 What are the first/last flights for each day in February 2013?
#4.5 Which company flew the most miles in March 2013? Which flew the least?
#4.6 Which month had the most delays over 60 minutes?
#4.7 What is the average time between two consecutive flights?
#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.
#####Problem 4#####
#Upload your homeworks on your own github repo.
#Link to the seminar https://unisofiafaculty.sharepoint.com/:v:/s/AccountingFinanceandDigitalapplicationsSeminargroupI/EfR2uYarKcRFiljWMgRb9U8BL6XsygAzJv_fu7mOCQsYzQ?e=CanPG0

#4.1 For each carrier what is the most common destination?
flights %>% 
  group_by(carrier, dest) %>% 
  count(dest) %>%
  group_by(carrier) %>%
  filter(rank(desc(n)) < 2)

#4.2 For each carrier what is the biggest delay?
flights %>% 
  group_by(carrier) %>% 
  summarise(carrier,delay = arr_delay - dep_delay) %>%
  group_by(carrier) %>%
  filter(rank(desc(delay)) < 2)


#4.3 Which are the three plane which have flown the most/least miles?
flights %>% arrange(distance) 
flights %>% arrange(desc(distance))


#4.4 What are the first/last flights for each day in February 2013?
filter(flights,year==2013,month==12) %>% arrange(dep_time) 
filter(flights,year==2013,month==12) %>% arrange(desc(dep_time))


#4.5 Which company flew the most miles in March 2013? Which flew the least?
filter(flights,month==3,year==2013) %>% arrange(desc(distance))
filter(flights,month==3,year==2013) %>% arrange(air_time)

#4.6 Which month had the most delays over 60 minutes?
flights %>% mutate(delay=arr_delay-dep_delay) %>% filter(delay > 60) %>% 
  group_by(month) %>% summarise(n=n()) %>%
  arrange(desc(n))

