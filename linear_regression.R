height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
cluster <- rep(2,length(height))
data <- data.frame(height, weight)

#TODO
# Plot height vs. weight
#plot(height,weight)
plot(data, col=cluster)

# TODO
# Create linear regression model using "lm" function
model <- lm("weight ~ height", data)
abline(model)

# Given the heights predict weights
newHeights = c(140, 150, 180, 190)
newData = data.frame(height=newHeights)
# TODO predict weights
prediction <- predict(model, newData)
print(prediction)
