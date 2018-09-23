#Import Data
dataset = read.csv("StockPrice.csv")
dataset 

library(caTools)
set.seed(123)

#Split dataset into training set and test set
split = sample.split(dataset$StockPrice, SplitRatio = 2/3)
training_set =  subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Build Regressor Model
regressor =lm(formula = StockPrice ~ Years, dataset)

#Predict
y_pred = predict(regressor, newdata = test_set)




# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$Years, y = training_set$StockPrice),
             colour = 'red') +
  geom_line(aes(x = training_set$Years, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Stock Price vs Years (Training set)') +
  xlab('Years') +
  ylab('Stock Price')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$Years, y = test_set$StockPrice),
             colour = 'red') +
  geom_line(aes(x = training_set$Years, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Stock Price vs Years (Test set)') +
  xlab('Years') +
  ylab('Stock Price')