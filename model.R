library(caret)
library(gbm)
data_split <- initial_split(data, prop = 0.80, strata = SalePrice)
train_data <- training(data_split)
heldout_data <- testing(data_split)
########################################################################
# Create a linear regression model using the training data
LR_model <- lm(SalePrice ~ ., data = train_data)
LR_predictions <- predict(LR_model, newdata = heldout_data)


LR_rmse <- RMSE(heldout_data$SalePrice, predictions)
paste('RMSE linear regression: ',LR_rmse)

# Calculate the mean squared error
LR_mse <- mean((heldout_data$SalePrice - LR_predictions)^2)

# Print the MSE
paste('MSE linear regression: ',LR_mse)


predict.Test <- predict(model,test_pre)
df.predict.Test <- test[1]
df.predict.Test$SalePrice <- predict.Test

head(df.predict.Test)
# Revert the log transformation using exponential function
df.predict.Test$SalePrice <- exp(df.predict.Test$SalePrice)
head(df.predict.Test)
write.csv(df.predict.Test, file = 'submission.csv', row.names=FALSE)

summary(model)
###################################################################################

#using gradient boosted decent, I did a little hyper parameter testing but it could probably be tuned more
model_gbm = gbm(SalePrice ~ ., data = train_data, distribution = "gaussian", n.trees = 8000, shrinkage = .01, interaction.depth = 2)
summary(model_gbm)  
predictions_gbm <- predict(model_gbm, newdata = heldout_data)
rmse <- RMSE(heldout_data$SalePrice, predictions_gbm)
paste('RMSE gbm: ',rmse)

# Calculate the mean squared error
mse <- mean((heldout_data$SalePrice - predictions_gbm)^2)

# Print the MSE
paste('MSE gbm: ',mse)
            


predict.Test <- predict(model_gbm,test_pre)
df.predict.Test <- test[1]
df.predict.Test$SalePrice <- predict.Test

head(df.predict.Test)
# Revert the log transformation using exponential function
df.predict.Test$SalePrice <- exp(df.predict.Test$SalePrice)
head(df.predict.Test)
write.csv(df.predict.Test, file = 'submission_gbm.csv', row.names=FALSE)                     


##########################################################################
library(randomForest)
model_randomForest <- randomForest(SalePrice ~ ., data = train_data)
predictions_randomForest <- predict(model_randomForest, newdata = heldout_data)
rmse_randomForest <- RMSE(heldout_data$SalePrice, predictions_randomForest)
paste('RMSE randomForest: ',rmse_randomForest)

# Calculate the mean squared error
mse_randomForest <- mean((heldout_data$SalePrice - predictions_randomForest)^2)

# Print the MSE
paste('MSE randomForest: ',mse_randomForest)


predict.Test <- predict(model_randomForest,test_pre)
df.predict.Test <- test[1]
df.predict.Test$SalePrice <- predict.Test

head(df.predict.Test)
# Revert the log transformation using exponential function
df.predict.Test$SalePrice <- exp(df.predict.Test$SalePrice)
head(df.predict.Test)
write.csv(df.predict.Test, file = 'submission_randomForest.csv', row.names=FALSE)                     
#############################################################

library(e1071)
model_svm <- svm(SalePrice ~ ., data = train_data)
predictions_svm <- predict(model_svm, newdata = heldout_data)
rmse_svm <- RMSE(heldout_data$SalePrice, predictions_svm)
paste('RMSE svm: ',rmse_svm)

# Calculate the mean squared error
mse_svm <- mean((heldout_data$SalePrice - predictions_svm)^2)

# Print the MSE
paste('MSE svm: ',mse_svm)

predict.Test <- predict(model_svm,test_pre)
df.predict.Test <- test[1]
df.predict.Test$SalePrice <- predict.Test

head(df.predict.Test)
# Revert the log transformation using exponential function
df.predict.Test$SalePrice <- exp(df.predict.Test$SalePrice)
head(df.predict.Test)
write.csv(df.predict.Test, file = 'submission_svm.csv', row.names=FALSE)

##########################################################
# Define a range of hyperparameter values to search
cost_range <- 10^(-2:2)
gamma_range <- 10^(-2:2)

# Perform a grid search over the hyperparameter values
best_mse <- Inf
for (cost in cost_range) {
  for (gamma in gamma_range) {
    model_svm2 <- svm(SalePrice ~ ., data = train_data, cost = cost, gamma = gamma)
    predictions_svm <- predict(model_svm2, newdata = heldout_data)
    mse <- mean((heldout_data$SalePrice - predictions_svm)^2)
    if (mse < best_mse) {
      best_mse <- mse
      best_cost <- cost
      best_gamma <- gamma
      final_model<-model_svm2
    }
  }
}
# Print the best hyperparameters and the corresponding MSE
print(paste0("Best cost: ", best_cost, ", Best gamma: ", best_gamma))
print(paste0("Best MSE: ", best_mse))

############################################################################

pred<- predict(final_model, newdata = heldout_data)
print(pred)
rmse_svm2 <- RMSE(heldout_data$SalePrice, pred)
paste('RMSE svm using gridsearch: ',rmse_svm2)

# Calculate the mean squared error
mse_svm2 <- mean((heldout_data$SalePrice - pred)^2)

# Print the MSE
paste('MSE svm using gridsearch: ',mse_svm2)
predict.Test <- predict(final_model,test_pre)
df.predict.Test <- test[1]
df.predict.Test$SalePrice <- predict.Test

head(df.predict.Test)
# Revert the log transformation using exponential function
df.predict.Test$SalePrice <- exp(df.predict.Test$SalePrice)
head(df.predict.Test)
write.csv(df.predict.Test, file = 'submission_svm2.csv', row.names=FALSE)
# Calculate the R-squared value
rsq <- cor(pred , heldout_data$SalePrice)^2

# Print the R-squared value
paste('R-squared value of svm using gridsearch: ',rsq)
##########################################################
dict<-list('LR'=as.numeric(LR_rmse),'RF'=as.numeric(rmse_randomForest),'GBM'=as.numeric(rmse),'SVM'=as.numeric(rmse_svm2))
dict
# Convert dictionary to data frame
plot_DF <- data.frame(keys = names(dict), values = unlist(dict))

# Create barplot
barplot(plot_DF$values, names.arg = plot_DF$keys, xlab = "Models", ylab = "RMSE")