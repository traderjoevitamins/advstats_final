install.packages("forecast")
library(forecast)
## sticky price consumer data 
data <- read.csv("/Users/KenedyDucheine/Documents/academic/fall23/adv stats r/final/CORESTICKM159SFRBATL.csv")

#we did this to be able to compare october's change in sticky price consumer index to the actual data
# november is not available yet 
data <- data[-nrow(data),]

#column two contains changes in scpi 
data <- data.frame(data[,2])

#creating time series starting at first entry date (1967, 1st of dec)
datats <- ts(data, frequency = 12, start = c(1967, 12))
plot.ts(datats) #plot of timeseries 


#gamma = 1 and beta = FALSE produced the most accurate model 
hw_model <- HoltWinters(datats, gamma = 1, beta = FALSE)
plot(hw_model)

comp1 <- hw_model$fitted
comp1 <- comp1[,1]

comp2 <- data[,1]
comp2 <- comp2[13:length(comp2)]

mse = mean((comp2 - comp1)^2)


# predicting using forecast 
predictmodel <- forecast(hw_model, h = 3)
predictmodel
plot(predictmodel)






