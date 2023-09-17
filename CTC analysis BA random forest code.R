library(readxl)
library(dummy)
library(caret)
library(randomForest)
library(ggplot2)
library(Metrics)
library(lmtest)
library(car)
HR_data <- read_excel("C:/Users/Administrator/Desktop/HR_data.xlsx")
View(HR_data)
cat_data<- data.frame(College=HR_data$College,
                   Role=HR_data$Role,
                   `City type`=HR_data$`City type`)
cat_data<- dummy(cat_data)
View(cat_data)
HR_data <- cbind(cat_data, HR_data[, -c(1:4)])
set.seed(123) 
#Partitioning to training and test set
index <- createDataPartition(HR_data$CTC, p = 0.8, list = FALSE)
train <- HR_data[index, ]
test <- HR_data[-index, ]
View(train)
#For Random Forest
model <- randomForest(train$CTC ~ . , data = train, importance = TRUE, ntree = 1000)
test$Predicted_CTC <- predict(model, newdata = test)
View(test)
#To Calculate root mean square error
# Calculate MAPE
MAPE <- mape(test$CTC, test$Predicted_CTC)
# Calculate RMSE
RMSE <- rmse(test$CTC, test$Predicted_CTC)
MAE <-mean(abs(test$CTC-test$Predicted_CTC))
r_squared <- cor(test$Predicted_CTC, test$CTC)^2
#plot
ggplot(test, aes(x = CTC, y = Predicted_CTC)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "orange") +
  labs(x = "Actual CTC", y = "Predicted", title = "Random Forest Model Performance")
#residual analysis
test$resid_rf<-test$CTC- test$Predicted_CTC
#Trial for new employee
new_test<-read_xlsx("C:/Users/Administrator/Desktop/test2.xlsx")
View(new_test)

new_test$Predicted_CTC <- predict(model, newdata = new_test)

importance(model)
varImpPlot(model)



