library(dplyr)
library(pacman)
library(tidyverse)
library(tidymodels)
library(devtools)
library(data.table)
library(caret)
library(gbm)
#dropping columns with huge number of missing values and unused columns
train_pre<-train %>% select(-GarageYrBlt,-Electrical,-Id,-Alley,-FireplaceQu,-PoolQC,-MiscFeature,-Fence)
View(train_pre)
test_pre<-test %>% select(-GarageYrBlt,-Electrical,-Id,-Alley,-FireplaceQu,-PoolQC,-MiscFeature,-Fence)

sort(colSums(is.na(test_pre)), decreasing = TRUE)

#combining the 2 sq ft variables into a single one
#train
train_pre <- train_pre %>% mutate(SqFt  = X1stFlrSF + X2ndFlrSF)
colnames(train_pre)
train_pre<-train_pre %>% select(-X1stFlrSF,-X2ndFlrSF)
#test
test_pre <- test_pre %>% mutate(SqFt  = X1stFlrSF + X2ndFlrSF)
colnames(test_pre)
test_pre<-test_pre %>% select(-X1stFlrSF,-X2ndFlrSF)
############################################################
#Lot Frontage is highly correlated with Lot area so we use that to fill in missing values
LotArea = append(train_pre[,'LotArea'], test_pre[,'LotArea'])
LotFrontage = append(train[, 'LotFrontage'], test[, 'LotFrontage'])
Lot = data.table(LotArea, LotFrontage)
Frontage_model = lm(LotFrontage ~ LotArea, data = Lot)
# Identify the missing values
missing <- is.na(train_pre$LotFrontage)
missing_test <- is.na(test_pre$LotFrontage)
# Predict the missing values using the fitted model
train_pre$LotFrontage[missing] <- predict(Frontage_model, newdata = train_pre[missing, ])
test_pre$LotFrontage[missing_test] <- predict(Frontage_model, newdata = test_pre[missing_test, ])
############################################################
fill_with_none= c("GarageQual", "GarageCond", "GarageFinish", "GarageType","BsmtCond", "BsmtQual", "BsmtFinType1", "BsmtFinType2", "BsmtExposure", "MasVnrType", "MSZoning", "Utilities", "KitchenQual")
#These are the missing values that are actually "None"
for (column in fill_with_none) {
  train_pre[[column]][is.na(train_pre[[column]])] <- "None"
  test_pre[[column]][is.na(test_pre[[column]])] <- "None"
}
############################################################
fill_with_mode=c("MasVnrType", "MSZoning", "Utilities", "Functional", "Exterior1st", "Exterior2nd", "SaleType")
#for missing categorical data we use the most common value
Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
for (column in fill_with_mode) {
  non_missing <- !is.na(train_pre[[column]])
  mode_val <- Mode(train_pre[[column]][non_missing])
  train_pre[[column]][is.na(train_pre[[column]])] <- mode_val
  test_pre[[column]][is.na(test_pre[[column]])] <- mode_val
}
############################################################
#for missing discrete data we use the median value
filling_with_median = c("BsmtFullBath", "BsmtHalfBath", "GarageCars")
for (column in filling_with_median) {
  median_val <- median(train_pre[[column]], na.rm = TRUE)
  train_pre[[column]][is.na(train_pre[[column]])] <- median_val
  test_pre[[column]][is.na(test_pre[[column]])] <- median_val
}
#############################################################
#for missing continuous data we use the mean value
filling_with_mean = c("MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "GarageArea")
for (column in filling_with_mean) {
  mean_val <- mean(train[[column]], na.rm = TRUE)
  train_pre[[column]][is.na(train_pre[[column]])] <- mean_val
  test_pre[[column]][is.na(test_pre[[column]])] <- mean_val
}
#####################################################################
sort(colSums(is.na(train_pre)), decreasing = TRUE)

# Apply the natural logarithm function to the 'SalePrice' column to normalize 
train_pre$SalePrice <- log(train_pre$SalePrice)


#################################################################
#convert factors that are ratings to a numeric value
rating_col = c("GarageQual", "GarageCond", "BsmtQual", "BsmtCond", "KitchenQual", "ExterQual", "ExterCond")
scores = c("None", "Po", "Fa", "TA", "Gd", "Ex")

for (column in rating_col) {
  train_pre[[column]] <- factor(train_pre[[column]], levels = scores)
  train_pre[[column]] <- as.integer(train_pre[[column]])
  test_pre[[column]] <- factor(test_pre[[column]], levels = scores)
  test_pre[[column]] <- as.integer(test_pre[[column]])
}
unique(train$ExterQual)
unique(train_pre$ExterQual)

#############################################
library(plyr)
train_pre$LotShape<-as.integer(revalue(train_pre$LotShape, c('IR3' = 0, 'IR2' = 1, 'IR1' = 2, 'Reg' = 3)))
test_pre$LotShape<-as.integer(revalue(test_pre$LotShape, c('IR3' = 0, 'IR2' = 1, 'IR1' = 2, 'Reg' = 3)))
table(train_pre$LotShape)
#garage
Garage_F <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)
train_pre$GarageFinish<-as.integer(revalue(train_pre$GarageFinish, Garage_F))
test_pre$GarageFinish<-as.integer(revalue(test_pre$GarageFinish, Garage_F))
table(train_pre$GarageFinish)
#basement
Bsmt_Q <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
train_pre$BsmtExposure <- as.integer(revalue(train_pre$BsmtExposure, Bsmt_Q))
test_pre$BsmtExposure <- as.integer(revalue(test_pre$BsmtExposure, Bsmt_Q))
table(train_pre$BsmtExposure)

Bsmt_T <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
train_pre$BsmtFinType1<-as.integer(revalue(train_pre$BsmtFinType1, Bsmt_T))
test_pre$BsmtFinType1<-as.integer(revalue(test_pre$BsmtFinType1, Bsmt_T))
table(train_pre$BsmtFinType1)

train_pre$BsmtFinType2<-as.integer(revalue(train_pre$BsmtFinType2, Bsmt_T))
test_pre$BsmtFinType2<-as.integer(revalue(test_pre$BsmtFinType2, Bsmt_T))
table(train_pre$BsmtFinType2)
######################################
# Street
train_pre$Street[ train_pre$Street == 'Grvl' ] <- 0
train_pre$Street[ train_pre$Street == 'Gravel' ] <- 0
train_pre$Street[ train_pre$Street == 'Pave' ] <- 1
train_pre$Street[ train_pre$Street == 'Paved' ] <- 1
train_pre$Street<-as.integer(train_pre$Street)

test_pre$Street[ test_pre$Street == 'Grvl' ] <- 0
test_pre$Street[ test_pre$Street == 'Gravel' ] <- 0
test_pre$Street[ test_pre$Street == 'Pave' ] <- 1
test_pre$Street[ test_pre$Street == 'Paved' ] <- 1
test_pre$Street<-as.integer(test_pre$Street)

########################################

#######################################
train_pre %>% 
  skimr::skim()
#####################################
str(train_pre$YrSold)
str(train_pre$MoSold)
train_pre$YrSold <- as.numeric(as.factor(train_pre$YrSold))
train_pre$MoSold <- as.numeric(as.factor(train_pre$MoSold))


