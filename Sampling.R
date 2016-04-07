randomSampling <- function(age, fare) {
  randage <- sample(age,100)
  randfare <- sample(fare, 100)
  meanage <- mean(randage)
  meanfare <- mean(randfare)
  sdage <- sd(randage)
  sdfare <- sd(randfare)
  
  print("Random Sampling")
  print(paste("Mean age: ",meanage," Mean Fare: ",meanfare))
  print(paste("SD age: ",sdage," SD Fare: ",sdfare))
}

# Every 5th element , na.rm = TRUE
systematicSampling <- function(age, fare) {
  counter = 0
  systAge <- c()
  systFare <- c()
  # sample(0:11, 1) produces a random number from 1 to 8
  for(i in seq(sample(0:9, 1), nrow(data), 5)){
    systAge <- c(systAge, age[i])
    systFare <- c(systFare, fare[i])
    counter = counter + 1
      if(counter == 100){
        break
      }
  }
  meanage <- mean(systAge)
  meanfare <- mean(systFare)
  sdage <- sd(systAge)
  sdfare <- sd(systFare)
  
  print("Systemic Sampling")
  print(paste("Mean age: ",meanage," Mean Fare: ",meanfare))
  print(paste("SD age: ",sdage," SD Fare: ",sdfare))
}

# Main program goes in here
file = "/Users/ptrollins/Desktop/IS690Q/train.csv"
data <- read.csv(file)
age <- na.omit(data$Age)
fare <- na.omit(data$Fare)

meanage <- mean(age)
meanfare <- mean(fare)
sdage <- sd(age)
sdfare <- sd(fare)

print("All Data")
print(paste("Mean age: ",meanage, " Mean Fare: ", meanfare))
print(paste("SD age: ",sdage, " SD Fare: ", sdfare))

systematicSampling(age,fare)
randomSampling(age,fare)