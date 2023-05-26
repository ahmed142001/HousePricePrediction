#Corelation with sales price
library(corrplot)
numeric <- which(sapply(train_pre, is.numeric))
all_numeric <- train_pre[, numeric]
cor_numeric <- cor(all_numeric, use="pairwise.complete.obs")

sorted <- as.matrix(sort(cor_numeric[,'SalePrice'], decreasing = TRUE))

CorHigh <- names(which(apply(sorted, 1, function(x) abs(x)>0.4)))

cor_numeric <- cor_numeric[CorHigh, CorHigh]

options(repr.plot.width=50,repr.plot.height=30)
corrplot.mixed(cor_numeric, tl.col="black", tl.pos = "lt")

print(CorHigh)
#################################
#final training data
data<-subset(train_pre,select = CorHigh)
summary(data)
#################################
#Effects of ground living area on the Price of a House
ggplot(data = data, mapping = aes(x=GrLivArea, y=SalePrice))+ geom_point(colour="blue")+
  geom_smooth(colour="black", method = lm, se=F, formula='y ~ x') +
  labs(title = "Effects of ground living area on the Price of a House",
       subtitle = "The selling price of a house increases as the ground living area widens",
       x ="ground living area", y = "Sales Price" )
##################################
#Effects of Overall Quality on the Price of a House
ggplot(data = data, mapping = aes(x=OverallQual, y=SalePrice))+ geom_point(colour="blue")+
  geom_smooth(colour="black", method = lm, se=F, formula='y ~ x') +
  labs(title = "Effects of Overall Quality on the Price of a House",
       subtitle = "The selling price of a house increases as the over quality",
       x ="Overall Quality", y = "Sales Price" )
##################################
#Effects of the square feet on the Price of a House
ggplot(data = data, mapping = aes(x=SqFt, y=SalePrice))+ geom_point(colour="blue")+
  geom_smooth(colour="black", method = lm, se=F, formula='y ~ x') +
  labs(title = "Effects of the square feet on the Price of a House",
       subtitle = "The selling price of a house increases as the square feet increase",
       x ="square feet", y = "Sales Price" )
##################################
#Effects of the YearBuilt on the Price of a House
ggplot(data = data, mapping = aes(x=YearBuilt, y=SalePrice))+ geom_point(colour="blue")+
  geom_smooth(colour="black", method = lm, se=F, formula='y ~ x') +
  labs(title = "Effects of the YearBuilt on the Price of a House",
       subtitle = "The selling price of a house increases as the YearBuilt increase",
       x ="YearBuilt", y = "Sales Price" )
######################################
#visualizing best model predictions
# Create a data frame with your predicted values and actual values
Visualize_df <- data.frame(predicted = pred, actual = heldout_data$SalePrice)
# Create a scatterplot
ggplot(Visualize_df, aes(x = predicted, y = actual)) +
  geom_point() +
  labs(title ="Best Model predictions",x = "Predicted Values", y = "Actual Values")
########################################
library(ggplot2)
ggplot(heldout_data,aes(x=pred,y=SalePrice))+
  geom_point(size=.01 , color = "steelblue")+
  geom_smooth(method = "loess", color="darkred")+
  labs(x="Predicted", y="Actual")+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
