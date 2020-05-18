##installing packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("lmtest")
install.packages("olsrr")
install.packages("faraway")
install.packages("usdm")
install.packages("fpp2")
install.packages("car")
install.packages("leaps")
install.packages("glmnet")
install.packages("MASS")
install.packages("robustbase")
install.packages("mgcv")
install.packages("caret")
install.packages("extraoperators")

##loading packages
library(dplyr)
library(ggplot2)
library(GGally)
library(lmtest)
library(olsrr)
library(faraway)
library(usdm)
library(fpp2)
library(car)
library(leaps)
library(glmnet)
library(caret)
library(MASS)
library(robustbase)
library(mgcv)
library(quantreg)
library(extraoperators)

##loading data
airbnb <- read.csv("AB_NYC_2019.csv")
#viewing data
str(airbnb)
View(airbnb)

##removing duplicate or un-useful columns: id,name,host_id,host_name,last_review,reviews_per_month 
airbnb2 <- airbnb[,-c(1,2,3,4,13,14)]
#viewing changes
str(airbnb2)
View(airbnb2)


##normalizing non-factor variables
#normalize function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
#normalizing non-factor columns
airbnb2_norm <- airbnb2
airbnb2_norm[c(3:4,6:10)] <- as.data.frame(lapply(airbnb2_norm[c(3:4,6:10)], normalize))
#viewing normalized data
View(airbnb2_norm)
str(airbnb2_norm)

##visualizing data
#price distribution
ggplot(airbnb2, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "blue") + 
  geom_density(alpha = 0.2, fill = "blue") +
  ggtitle("Distribution of price") +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(airbnb$price), 2), size = 2, linetype = 3)
#number of observations per nyc burrough
airbnb2 %>% group_by(neighbourhood_group) %>% tally() %>% 
  ggplot(aes(x = reorder(neighbourhood_group, n), n)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Number of Rentals by Burrough") +
  geom_text(aes(x = neighbourhood_group, y = 1, label = paste0(n),
                colour = ifelse(neighbourhood_group %in%
                                  c("Manhattan", "Brooklyn", 
                                    "Queens"), '1', '2')),
            hjust=-1.5, vjust=.5, size = 4, 
            fontface = 'bold') +
  coord_flip() +
  scale_color_manual(values=c("white","black"), guide = F)


##Basic linear regressions (all variables included)
#lin reg with non_normalized data
lin_reg_basic <- lm(price ~., data = airbnb2)
summary(lin_reg_basic)
#lin reg with normalized data
lin_reg_basic_norm <- lm(price ~., data = airbnb2_norm)
summary(lin_reg_basic_norm)
#Checking normality of residuals
#plotting residuals 
par(mfrow = c(2,2))
plot(lin_reg_basic)
#plotting histogram of residuals
hist(lin_reg_basic$residuals)
#running shapiro wilk test
shapiro.test(lin_reg_basic$residuals)
#running brusch pagan test
bptest(lin_reg_basic)
#plotting cook's distance
ols_plot_cooksd_chart(lin_reg_basic)
str(airbnb2)
#creating x 
X <- airbnb2[,c(1:5,7:10)]
#creating y 
y <- airbnb2$price
#predicting
predict_basic_lm <- predict(lin_reg_basic, X)
#comparing prediction to actual
round(postResample(predict_basic_lm, y),3)

#plot with bubbles showing amplification of influence
influencePlot(lin_reg_basic, id.method="noteworthy",
              main="Outlier Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")
#plot showing outliers outside of range
plot(dffits(lin_reg_basic), 
     ylab = "Standardized dfFits", xlab = "Index", 
     main = paste("Standardized DfFits, \n critical value = .15",
                  round(0.1,3)))  
abline(h = 0.10, col = "red", lty = 2)
abline(h = -0.10, col = "red", lty = 2)
#creating dataframe 
airbnb2_cook <- airbnb2
airbnb2_cook$cd <- cooks.distance(lin_reg_basic)
airbnb2_cook$dffits <- dffits(lin_reg_basic)
outliers <- subset(airbnb2_cook, dffits %gele% c(-.10, .10))
nrow(outliers)
outliers2 <- subset(airbnb2_cook, cd < .0000000001)
nrow(outliers2)
#lin reg without outliers
lm_no_outliers <- lm(price~neighbourhood_group + neighbourhood + latitude+ longitude + room_type + minimum_nights + number_of_reviews, data=outliers2)
summary(lm_no_outliers)
#lin reg without neighbourhood
lm_no_outliers2 <- lm(price~neighbourhood_group + latitude + longitude + room_type + minimum_nights + number_of_reviews, data=outliers)
summary(lm_no_outliers2)
#lin reg putting back neighbourhood group but without rentals in Queens
noqueens <- outliers[-c(outliers$neighbourhood_group == "Queens"),]
View(noqueens)
lm_no_outliers3 <- lm(price~neighbourhood_group + neighbourhood + latitude + longitude + room_type + minimum_nights + number_of_reviews, data=noqueens)
summary(lm_no_outliers3)
#plotting residuals 
par(mfrow = c(2,2))
plot(lm_no_outliers)
#plotting histogram of residuals
hist(lm_no_outliers$residuals)
#running shapiro wilk test
shapiro.test(lm_no_outliers$residuals)
#running brusch pagan test
bptest(lm_no_outliers)

#other lin regs selecting certain variables
lin_reg_2 <- lm(price ~ neighbourhood_group + room_type + number_of_reviews + calculated_host_listings_count + availability_365, data = airbnb2)

lin_reg_3 <- lm(price ~ neighbourhood + room_type + neighbourhood_group*room_type, data = airbnb2)


lin_reg_4 <- lm(price ~., data = airbnb2_numeric)
summary(lin_reg_4)

##Running VIF on numeric
#creating data frame with just numeric and interger variables
airbnb2_numeric <- airbnb2[c(3:4,6:10)]
str(airbnb2_numeric)
View(airbnb2_numeric)
#making int variables all numeric
airbnb2_numeric[3:7] <- lapply(airbnb2_numeric[3:7], as.numeric)
#removing dependent variable price
airbnb2_numeric_minus_price <- airbnb2_numeric[,-c(3)]
str(airbnb2_numeric_minus_price)
#running VIF
vifstep(airbnb2_numeric_minus_price[,c(1:6)], th=5)

##seeing correlation of numeric variables(incl. price) w/ each other
cor(airbnb2_numeric)

##powertransforming variables
#just price
p1 <- powerTransform(price ~., data = airbnb2_numeric, family = "yjPower")
p1$lambda
p1$roundlam
summary(p1)
#cant use lm with log y so Using alternative method to run lin reg with transformed visits when lambda not 0 or 1
#Adding column for power transfored visits to initial data set
#copying data set
airbnb3_numeric <- airbnb2_numeric
#adding new column for power transformed price
airbnb3_numeric$price_trans <- (airbnb2_numeric$price^p1$lambda - 1)/p1$lambda
str(airbnb3_numeric)
#taking out non-transformed price variable
airbnb4_numberic <- airbnb3_numeric[,-c(3)]
str(airbnb4_numberic)
#creating linear regression with transformed visits and ind. variables from q3
lm_powertrans_price <- lm(price_trans ~., data = airbnb4_numberic)

##Model Matrix
#taking out neghbourhood variables because it takes too long to run model matrix with it
airbnb2_sans <- airbnb2[,c(1,4,5,6,9,10)]
str(airbnb2_sans)
View(airbnb2_sans)
#creating model matrix
airbnb_matrix <- model.matrix(price ~.*., data = airbnb2_sans)
#removing intercept
airbnb_matrix_nointercept <- airbnb_matrix[, -1]
#vifstepping model matrix
vif <- vifstep(airbnb_matrix_nointercept, th=5)
#exclude vifstep excluded variables and form new data fram
vif_matrix_x <- exclude(airbnb_matrix_nointercept, vif)
vif_matrix_full <- data.frame(cbind(vif_matrix_x, price = airbnb2$price))
#lin reg with vifstep model matrix
linreg_vifmatrix <- lm( price ~., data = vif_matrix_full)
#seeing results of vifstep model matrix
summary(linreg_vifmatrix)
#setting up AIC
result_vifmatrix <- ols_step_both_aic(linreg_vifmatrix)
#running AIC
result_vifmatrix
#reggression with AIC variables
lm_aic <- lm(price ~ neighbourhood_groupManhattan.availability_365 + room_typePrivate.room.availability_365 +neighbourhood_groupBrooklyn.room_typePrivate.room+neighbourhood_groupManhattan.room_typePrivate.room+longitude+room_typeShared.room.availability_365+neighbourhood_groupBrooklyn.availability_365+neighbourhood_groupBrooklyn+neighbourhood_groupQueens.room_typePrivate.room+neighbourhood_groupQueens.availability_365+neighbourhood_groupManhattan.room_typeShared.room+neighbourhood_groupManhattan.calculated_host_listings_count+neighbourhood_groupBrooklyn.room_typeShared.room+neighbourhood_groupStaten.Island.room_typePrivate.room+neighbourhood_groupStaten.Island.availability_365+neighbourhood_groupQueens.calculated_host_listings_count+room_typeShared.room.calculated_host_listings_count, data = vif_matrix_full)
#viewing results of aic
summary(lm_aic)

#creating x 
X <- vif_matrix_x
#creating y 
y <- airbnb2$price
#creating lasso model
lasso <- glmnet(X, y, alpha = 1, lambda = 0.1)
#predicting lasso
predict_lasso <- predict(lasso, X)
#comparing prediction to actual price from data set
round(postResample(predict_lasso, y),3) 

