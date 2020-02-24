setwd("C:/Users/Flynn/Desktop/IE500 R/Midterm")
getwd()

#install.packages("readxl")
library(readxl)

#read data
data <-  read_excel("Midterm_ElectricityDemand.xlsx", col_types = "numeric")[-1,]

#### TRAIN AND TEST DATA

set.seed(9909)

train_idx <- sample(x = 1:nrow(data), size = floor(0.80*nrow(data)))

train_data <- data[train_idx,]
test_data <- data[-train_idx,]

#### BEST SUBSET

library(leaps)

datafullsubset <- regsubsets(SALES~., data = data, method = "exhaustive")
summary(datafullsubset)
coef(datafullsubset, 6 )
bestvar <- which.min(summary(datafullsubset)$cp)



cp <- which(summary(datafullsubset)$which[bestvar,] == TRUE)
train_data1 <- train_data[cp]
test_data1 <- test_data[cp]

##### MARS

library(earth)

finalmodel <- earth(SALES~ ., data=train_data1, degree=1 , penalty=3, pmethod="cv", nfold=10, ncross=5)
summary(finalmodel)
print(finalmodel)

train_data$predsales4 <- predict(finalmodel, newdata = (train_data1))
test_data$predsales4 <- predict(finalmodel, newdata = test_data1)

marstest.rmse <- round(sqrt(mean((test_data$predsales4 - test_data$SALES)^2)))
marstest.mse <- round((mean((test_data$predsales4 - test_data$SALES)^2)))

marstrain.rmse <- round(sqrt(mean((train_data$predsales4 - train_data$SALES)^2)))
marstrain.mse <- round((mean((train_data$predsales4 - train_data$SALES)^2)))


plot(finalmodel)

plot(test_data$SALES ~ test_data$predsales4, col=c("red","blue"), xlab= "predicted data", ylab = "actual data" )
