#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.
#####Problem 1#####
library(tidyverse)
library(nycflights13)


coin <- c("Head" = 0, "Tails" = 1)


for (x in 1:1000){
  Money <- 100
  Bet <- 1
  
  while (Money > 0){
    
    cointoss <-sample(coin, 1,replace = TRUE, prob = c(0.3,0.7 ))
    
    if (c(cointoss)==1){
      Money = Money + Bet 
    }
    else {
      Money = Money - Bet
      Bet = min(Money, Bet*2)
    }
    print(Money)
  } 
}
#####Problem 2#####
# Read everything from https://r4ds.had.co.nz/transform.html, up until
# 5.6 Grouped summaries with summarise(). If you want to, you can
# read everything and then https://r4ds.had.co.nz/relational-data.html

#Do all the exercises:
# 5.2.4 Exercises 
#1
delay2 <- filter(flights, arr_delay >= 120 )
destin <- filter(flights, dest == "IAH" | dest == "HOU" )
carri_er <- filter(flights, carrier %in% c("AA", "DL", "UA"))
summer <- filter(flights, month >= 7, month <= 9)
late_notlate <- filter(flights, arr_delay > 120, dep_delay <= 0)
made_up <- filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
midnight <- filter(flights, dep_time <= 600 | dep_time == 2400)


#2
summer2 <- filter(flights, between(month, 7, 9))

#3
MissingInformation <- filter(flights, is.na(dep_time))


#4NA ^ 0
NA | TRUE
FALSE & NA

NaN ^ 0
7 ^ 0
TRUE ^ 0

7 | TRUE
FALSE & TRUE




# 5.3.1 Exercises


arrangeNA <- arrange(flights, desc(is.na(dep_time)), dep_time)

#2 
MostDelayed <- arrange(flights, desc(dep_delay))

#3 
Fastest <- arrange(flights, desc(distance / air_time))

#4
faster <- arrange(flights, desc(distance))



# 5.4.1 Exercises 

#1
select(flights, year, month, day)
select(flights, "year", "month", "day")
select(flights, 1, 2, 3)

#2
select(flights, year, month, year, year) #it does not include them 

#3 
choice <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(choice))

# 5.5.2 Exercises 

#You can also read the official dplyr site.
#https://dplyr.tidyverse.org/index.html
#https://dplyr.tidyverse.org/articles/dplyr.html
#####Problem 2#####

#Copy to the recording https://unisofiafaculty.sharepoint.com/:v:/r/sites/AccountingFinanceandDigitalapplicationsSeminargroupI/Shared%20Documents/General/Recordings/Quantitative%20methods%20in%20finance%202022-20220317_180538-Meeting%20Recording.mp4?csf=1&web=1&e=3m90AT