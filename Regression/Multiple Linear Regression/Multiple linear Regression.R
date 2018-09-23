#Import Dataset
dataset = read.csv('50_Startups.csv')
dataset

#Encoding Categorical Values
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California','Florida'),
                       labels = c(1,2,3))
library(caTools)
set.seed(123)

#Splitting the Dataset
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset,split == FALSE)


#Backward Classification
regressor = lm( formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                data = dataset)
regressor = lm( formula = Profit ~ R.D.Spend + Administration + Marketing.Spend ,
                data = dataset)
regressor = lm( formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = dataset)
regressor = lm( formula = Profit ~ R.D.Spend,
                data = dataset)

#Predict
y_pred = predict(regressor, newdata = test_set)



# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$R.D.Spend, y = training_set$Profit),
             colour = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Profit vs R.D.Spend (Training set)') +
  xlab('R.D.Spend') +
  ylab('Profit')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$R.D.Spend, y = test_set$Profit),
             colour = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Profit vs R.D.Spend (Test set)') +
  xlab('R.D.Spend') +
  ylab('Profit')