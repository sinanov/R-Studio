#####Problem 1#####
# Write a loop which iterates over all the numbers from 1 to 10 and prints
# them multiplied by 3
#####Problem 1#####
  
  for(i in 1:10){
    print(i*3)
  }

#####Problem 2#####
# Write a loop which chooses 10 random numbers, one at a time from a normal
# distribution (use rnorm and see the help ?rnorm) and prints the number
# if it is bigger than 1.
#####Problem 2#####

rn=rnorm(10)
for (i in 1:length(rn)){
  if(rn[i]>1){
    print(rn[i])
  }
}

#####Problem 3#####
# What is the probability that out of a group of 6 men and 8 women, if we pick
# 5 people at random, exactly 3 will be men?
# Use a for loop, which simulates the picking.
#####Problem 3#####

Outcome <- 0
for(x in 1:100){
  P <- sample(1:14,5)
  Men <- 0
  for (i in 1:5){
    if(P[i]<7){
      Men <- Men + 1
    
  }
}
if(Men == 3){
  Outcome <- Outcome + 1
  
}
}
probability <- Outcome/x
probability

#####Problem 4#####
# Calculate the price of a european style option with strike price of 120, with an
# expiration date in 100 days.
# The underlying stock has a starting price of 100, which will change every 
# day based on a random number coming from a normal distribution with 
# mean = 0 and standard deviation = 7. This is rnorm(1, mean = 0, sd = 5) 
# The starting price was 100 on day 0.
# On day 1 it is 100 + rnorm(1, mean = 0, sd = 7). 
# On day 2 it is price from day 1 + rnorm(1, mean = 0, sd = 7) etc.
# On day 100 it is price from day 99 + rnorm(1, mean = 0, sd = 7).
# 
# European style options can only be exercised at expiry date. 
# So you must calculate the price of the underlying asset at day 100 and 
# compare it to the strike price. Then calculate the potential profit. 
# Because it is a call option the profit will range from 0 to infinity.
# Make 1000 simulations of the profit and the price of the call option,
# will be equal to the sum of all the profits divided by the number of 
# simulations(in this case 1000).
#####Problem 4#####

Profit <- 0
for(simulations in 1:1000){
  StartingPrice <- 100
  for(days in 1:100){
    StartingPrice <- StartingPrice + rnorm(1, mean = 0, sd=7)
  }
  if(StartingPrice >= 120){
    Profit <- Profit + StartingPrice - 120
  }
}
FinalCase <- Profit/simulations
FinalCase
