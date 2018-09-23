#Import Dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]

library(caTools)
set.seed(123)


#Split dataset into training set and test set
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set =  subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#Build Regressor Model
lin_reg = lm(formula = Salary ~ Level, 
             data = dataset)
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
poly_reg = lm(formula = Salary ~ . ,
              data = dataset)

library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(poly_reg,newdata = dataset)),colour='blue')+ 
  ggtitle('Polynomial Regression') +
  xlab('Level')+
  ylab('Salary')

#Predict
y_pred_lin = predict(lin_reg, data.frame(Level=6.5))
y_pred = predict(poly_reg, data.frame(Level=6.5,Level2=6.5^2,Level3=6.5^3))

