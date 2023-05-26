library(dplyr)
library(pacman)
library(tidyverse)
library(tidymodels)
library(devtools)
train<-read.csv('train.csv')
test<-read.csv('test.csv')
View(test)
View(train)
sprintf("The database has a total of %s columns and %s rows.", dim(train)[2], dim(train)[1])
sprintf("The test data has a total of %s columns and %s rows.", dim(test)[2], dim(test)[1])
str(train)
summary(train)
tidymodels_prefer()
p_load(plotly)

train %>% 
  skimr::skim()

sort(colSums(is.na(train)), decreasing = TRUE)
sum(is.na(test))
#percentage of data missing in train
sum(is.na(train)) / (nrow(train) *ncol(train))

hist(train$SalePrice)
# price distribution
ggplot( train, aes( x = SalePrice)) + 
  geom_histogram( binwidth = 10000, color = "black", fill="white") +
  geom_vline( xintercept = mean( train$SalePrice ), color = 'red' ) +
  ggtitle("Price distribution") + 
  xlab("Sale Price") + ylab("Count") + 
  theme( title = element_text(size = 16),
         axis.title.y = element_text(size = 11),
         axis.text.y = element_text(size = 11),
         axis.title.x = element_text(size = 11),
         axis.text.x = element_text(size = 11),
         plot.margin = margin(t = 5, r = 15, b = 5, l = 15),
         plot.caption = element_text(size = 10),
         aspect.ratio = 0.8)


# Create a scatter plot with a trend line
cor_coef <- cor(train$LotFrontage, train$LotArea)
plot(train$LotFrontage, train$LotArea, main = "Scatter plot with trend line", xlab = "LotFrontage", ylab = "LotArea")
abline(lm(train$LotArea ~ train$LotFrontage), col = "red")
text(x = 1, y = 9, labels = paste("Correlation Coefficient =", round(cor_coef, 2)))


# Identify the columns with null values
null_cols <- colnames(train)[colSums(is.na(train)) > 0]

# Print the names of the columns with null values
print(null_cols)
