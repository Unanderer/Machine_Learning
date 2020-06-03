
#############################################################################
#
#   RMSLE
#   02/16/2019
#   Hunter Martin
#
#############################################################################

# removed for privacy reasons
setwd( "PATH/TO/DATA" )



install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("xtable")
install.packages("rio")
install.packages("caTools")
install.packages("glmnet")
install.packages("mlbench")
install.packages("grid")


library(tidyverse)
library(PerformanceAnalytics)
library(gdata)
library(stringr)
library(scales)
library(graphics)
library(caret)
library(knitr)
library(Hmisc)
library(corrplot)
library(xtable)
library(rio)
library(dplyr)
library(MASS)
library(caTools)
library(glmnet)
library(Metrics)
library(mlbench)
library(ggplot2)



#xlsx converted to .csv
#Read in data
house <- read.csv("HousePricesAbridged.csv", header = TRUE)

#Feature Engineering
Remodeled <- c(house$YrSold - house$YearRemodAdd)
house <- cbind(house, Remodeled)
house$Remodeled <- ifelse(Remodeled > 0, 1, 0)

summary(house$Remodeled)



#Check for missing values
colSums(is.na(house))

#Columns with missing values:
#LotFrontage, BsmtFullBath, BsmtHalfBath, KitchenQual,GaregeCars, SalePrice


#Impute missing BsmtFull and BsmtHalf baths to 0 since there is no other indication they exist
house$BsmtFullBath[which(is.na(house$BsmtFullBath))] <- 0
house$BsmtHalfBath[which(is.na(house$BsmtHalfBath))] <- 0

#Recheck those two NA's
colSums(is.na(house))
#BsmtFull and BsmtHalf NA's are imputed successfully


# There is 1 NA in GarageCars. I input a 2 based on houses with similar features
house$GarageCars[is.na(house$GarageCars)] <- 2
summary(house)
colSums(is.na(house))

#After recheck, there are no NA's for GarageCars

#The one NA in KitchenQual will be assigned based on the overall quality metrics: TA
which(is.na(house$KitchenQual))
house[1556, 20] <- "TA"

#After recheck, there are no NA's for KitchenQual

#Impute mean LotFrontage data for 486 values of NA

# There are 486 NAs  in LotFrontage variable
#Impute with the maen
NA_Lot_Frontage <- which(is.na(house$LotFrontage))
house$LotFrontage[NA_Lot_Frontage] <-
  mean(house$LotFrontage, na.rm = TRUE)

colSums(is.na(house))

#After recheck, there are no NA's for LotFrontage


#I need to input values for two missing Functional metrics. By looking at OverallQual, and OverallCond, I'm imputing "Mod"
# for row 2217 and "Maj1" for row 2474
which(is.na(house$Functional))

house[2217, 22] <- "Mod"
house[2474, 22] <- "Maj1"

colSums(is.na(house))


#After recheck, there are no NA's for Functional




#########
#Find which columns are factors

names(Filter(is.factor, house))

#[1] "HouseStyle"  "ExterQual"   "CentralAir"  "KitchenQual"
#[5] "Functional"  "PavedDrive"

#Set levels and convert to numeric

house$HouseStyle <-
  factor(
    house$HouseStyle,
    levels = c(
      "1Story",
      "1.5Unf",
      "SFoyer",
      "SLvl",
      "1.5Fin",
      "2Story",
      "2.5Unf",
      "2.5Fin"
    )
  )
levels(house$HouseStyle)
house$HouseStyle <- as.numeric(house$HouseStyle)

p1 <- ggplot(house, aes(HouseStyle)) + geom_bar(stat = "count")

house$ExterQual <-
  factor(house$ExterQual, levels = c("Fa", "TA", "Gd", "Ex"))
house$ExterQual <- as.numeric(house$ExterQual)

p2 <- ggplot(house, aes(ExterQual)) + geom_bar(stat = "count")

levels(house$CentralAir)
contrasts(as.factor(house$CentralAir))
#The regression will properly use No as 0 and Yes as 1, no level changes needed
house$CentralAir <- as.numeric(house$CentralAir)
p3 <- ggplot(house, aes(CentralAir)) + geom_bar(stat = "count")

house$KitchenQual <-
  factor(house$KitchenQual, levels = c("Fa", "TA", "Gd", "Ex"))
levels(house$KitchenQual)
house$KitchenQual <- as.numeric(house$KitchenQual)
p4 <- ggplot(house, aes(KitchenQual)) + geom_bar(stat = "count")

levels(house$Functional)
house$Functional <-
  factor(house$Functional,
         levels = c("Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"))
house$Functional <- as.numeric(house$Functional)
p5 <- ggplot(house, aes(Functional)) + geom_bar(stat = "count")

levels(house$PavedDrive)
#Levels were already ranked least to best
house$PavedDrive <- as.numeric(house$PavedDrive)
p6 <- ggplot(house, aes(PavedDrive)) + geom_bar(stat = "count")

names(Filter(is.factor, house))


##Plots

multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <-
          as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }
multiplot(p1, p2, p3, p4, p5, p6, cols = 2)



#No factors remain

str(house)




############  Histograms and Summary Stats

#Looking for skewed data here in need of transformation



h1 <- hist(house$LotFrontage)
g1 <- ggplot(house, aes(LotFrontage, SalePrice)) + geom_point()
summary(house$LotFrontage)
sd(house$LotFrontage)

h2 <- hist(house$LotArea)
g2 <- ggplot(house, aes(LotArea, SalePrice)) + geom_point()
summary(house$LotArea)
sd(house_data$LotArea)

h3 <- hist(house$YearBuilt)
g3 <- ggplot(house, aes(YearBuilt, SalePrice)) + geom_point()
summary(house$YearBuilt)
sd(house$YearBuilt)

h4 <- hist(house$YearRemodAdd)
g4 <- ggplot(house, aes(YearRemodAdd, SalePrice)) + geom_point()
summary(house$YearRemodAdd)
sd(house$YearRemodAdd)

h5 <- hist(house$X1stFlrSF)
g5 <- ggplot(house, aes(X1stFlrSF, SalePrice)) + geom_point()
summary(house$X1stFlrSF)
sd(house$X1stFlrSF)

h6 <- hist(house$X2ndFlrSF)
g6 <- ggplot(house, aes(X2ndFlrSF, SalePrice)) + geom_point()
summary(house$X2ndFlrSF)
sd(house$X2ndFlrSF)

h7 <- hist(house$GrLivArea)
g7 <- ggplot(house, aes(GrLivArea, SalePrice)) + geom_point()
summary(house$GrLivArea)
sd(house$GrLivArea)

multiplot(g1, g2, g3, g4, g5, g6, g7, cols = 2)

par(mfrow = c(2, 4))
hist(house$LotFrontage)
hist(house$LotArea)
hist(house$YearBuilt)
hist(house$YearRemodAdd)
hist(house$X1stFlrSF)
hist(house$X2ndFlrSF)
hist(house$GrLivArea)


########### TRANSFORMING VARIABLES

#Based on this analysis, will transform LotFrontage, LotArea, First and Second floor square footage


house$LotFrontage <- log(house$LotFrontage)
house$LotArea <- log(house$LotArea)
house$X1stFlrSF <- log(house$X1stFlrSF)
house$X2ndFlrSF <- log(house$X2ndFlrSF)
#Log on the 2nd floor SF resulted in -Inf when the value was zero
#Must revert to zeros
house$X2ndFlrSF[which(house$X2ndFlrSF == -Inf)] <- 0


#Split data into training and test set, with the test set comprised of
#those observations missing SalePrice. Validation sets are subsets of train  and testfor validation


set.seed(123)

#Initial train and test set
train <- house[1:1460,]
test <- house[1461:2919,]

# Make a validated train and test set
validated_train <- train[1:730, ]
validated_test <- train[731:1460, ]


#Looking at distribution of target variable (SalePrice)
ggplot(train, aes(x = SalePrice)) +
  geom_histogram(fill = "#E495A5", col = "black", bins = 55) +
  scale_x_continuous(name = "Sale Price",
                     labels = dollar,
                     limits = c(0, 800000))



glimpse(train)
summary(train)


###########   Feature Selection


model <- lm(formula = SalePrice ~ . - Id, data = train)
summary(model)
#R^2= 0.8187
step <-
  stepAIC(model, direction = "backward", na.action = na.remove)
summary(step)
step$anova
#R^2= 0.8189, better

step1 <-
  stepAIC(model, direction = "forward", na.action = na.remove)
step1$anova
summary(step1)
#R^2= 0.8187 same

step2 <- stepAIC(model, direction = "both", na.action = na.remove)
step2$anova
summary(step2)
#R^2= 0.8189

#Since forward did not add anything, the results for both are the same as for backward



#Explore corelation
################################
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use = "pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above] ^ 2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Transform the cor.prob output to a 4 column matrix

flattenSquareMatrix <- function(m) {
  if ((class(m) != "matrix") |
      (nrow(m) != ncol(m)))
    stop("Must be a square matrix.")
  if (!identical(rownames(m), colnames(m)))
    stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(
    i = rownames(m)[row(m)[ut]],
    j = rownames(m)[col(m)[ut]],
    cor = t(m)[ut],
    p = m[ut]
  )
}


#create correlation matrix, only numeric, then flattened matrix for plotting
cor_prep <- cor.prob(train[sapply(train, is.numeric)])

matrix <- flattenSquareMatrix(cor_prep)


#subset matrix to only include highly correlated variables
matrix_HighCor <- subset(matrix, cor >= 0.5 | cor < -0.5)

#call table

kable(matrix_HighCor)







######### RF Importance



library(randomForest)
importance <-
  randomForest(
    SalePrice ~ LotArea + HouseStyle + OverallQual + OverallCond +
      YearBuilt + ExterQual + CentralAir + GrLivArea + BsmtFullBath +
      FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual +
      TotRmsAbvGrd + Functional + Fireplaces + GarageCars,
    data = train,
    ntree = 1000,
    keep.forest = FALSE,
    importance = TRUE
  )
varImpPlot(importance, scale = T)
####Importance visual points to GrLivArea, OverallQual, GarageCars, ExterQual, BsmtFullBath, and LotArea as the six most important variables to predicting SalePrice


####################   Regressions




#ols is directly copied from the backwards stepwise feature selection
ols <- lm(formula = SalePrice ~ . - Id, data = validated_train)

summary(ols)

ols_fitted <- predict(ols, newdata = validated_test)
ols_error <- rmsle(validated_test$SalePrice, ols_fitted)
ols_error
# RMSLE is 0.1684



# Write results to a csv
#test$SalePrice <- predict(ols, test)
#OLS_backward_final <- test[, c(1,27)]
#write_csv(OLS_backward_final, "ols_final.csv")



###########      Polynomial


#After playing with the polynomial, an exponent of 3 gave the best.
polyReg <-
  lm(
    SalePrice ~ poly(LotArea, 3, raw = TRUE) + HouseStyle + OverallQual + OverallCond +
      YearBuilt + ExterQual + CentralAir + GrLivArea + BsmtFullBath +
      FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual +
      TotRmsAbvGrd + Functional + Fireplaces + GarageCars,
    data = validated_train
  )

poly_fitted <- predict(polyReg, newdata = validated_test)
poly_error <- rmsle(validated_test$SalePrice, poly_fitted)
poly_error  # RMSLE is 0.1719


# Write results to a csv
#test$SalePrice <- predict(polyReg, test)
#poly_final <- test[, c(1,27)]
#write_csv(poly_final, "poly_final.csv")




######    Ridge, LASSO, Elastic Net



##First test using all features
glm.cv.ridge <-
  cv.glmnet(data.matrix(validated_train),
            validated_train$SalePrice,
            alpha = 0)
glm.cv.lasso <-
  cv.glmnet(data.matrix(validated_train),
            validated_train$SalePrice,
            alpha = 1)
glm.cv.net   <-
  cv.glmnet(data.matrix(validated_train),
            validated_train$SalePrice,
            alpha = 0.06)
#Trial and error on the value of alpha for the elastic net model gave me the best results


#Now find Lambda that minimizes error (Optimization)


penalty.ridge <- glm.cv.ridge$lambda.min
penalty.lasso <- glm.cv.lasso$lambda.min
penalty.net   <- glm.cv.net$lambda.min

#Train models using new lambdas

glm.ridge <-
  glmnet(
    x = data.matrix(validated_train),
    y = validated_train$SalePrice,
    alpha = 0,
    lambda = penalty.ridge
  )
glm.lasso <-
  glmnet(
    x = data.matrix(validated_train),
    y = validated_train$SalePrice,
    alpha = 1,
    lambda = penalty.lasso
  )
glm.net   <-
  glmnet(
    x = data.matrix(validated_train),
    y = validated_train$SalePrice,
    alpha = 0.06,
    lambda = penalty.net
  )


#Predict sale price on train set using RMSE

y_pred.ridge <-
  as.numeric(predict(glm.ridge, data.matrix(validated_train)))
y_pred.lasso <-
  as.numeric(predict(glm.lasso, data.matrix(validated_train)))
y_pred.net   <-
  as.numeric(predict(glm.net,   data.matrix(validated_train)))


rmsle(validated_train$SalePrice, y_pred.ridge)
rmsle(validated_train$SalePrice, y_pred.lasso)
rmsle(validated_train$SalePrice, y_pred.net)

#Testing RMSLE for test set
y_pred_valid.ridge <-
  as.numeric(predict(glm.ridge, data.matrix(validated_test)))
y_pred_valid.lasso <-
  as.numeric(predict(glm.lasso, data.matrix(validated_test)))
y_pred_valid.net   <-
  as.numeric(predict(glm.net,   data.matrix(validated_test)))

rmsle(validated_test$SalePrice, y_pred_valid.ridge)
rmsle(validated_test$SalePrice, y_pred_valid.lasso)
rmsle(validated_test$SalePrice, y_pred_valid.net)


#####
# rmsle(validated_test$SalePrice,y_pred_valid.ridge)
#[1] 0.06909702
#> rmsle(validated_test$SalePrice,y_pred_valid.lasso)
#[1] 0.01621857
#> rmsle(validated_test$SalePrice,y_pred_valid.net)
#[1] 0.01629591
#Therefore the elastic net regression produces the best results

#What are the variables and coefficients of the different models?
ridge_coeffs <- coef(glm.cv.ridge, s = "lambda.min")
ridge_coeffs_data <-
  data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

#lasso_coeffs <- coef(glm.cv.lasso, s = "lambda.min")
#lasso_coeffs_data <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

#net_coeffs <- coef(glm.cv.net, s = "lambda.min")
#net_coeffs_data <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
# They are all the same

#For submission

test$SalePrice <- as.numeric(predict(glm.net, data.matrix(test)))
net_final <- test[, c(1, 27)]
write.csv(net_final, "net_final.csv")



################################  Plotly

#Interactive visualization of regression, provides opportunity
#to better understand hyperplanes

install.packages("plotly")
library(plotly)
library(reshape2)
y <- validated_test$SalePrice
x1 <- validated_train$GrLivArea
x2 <- validated_train$LotArea

df <- data.frame(x1, x2, y)



### Estimation of the regression plane
mod <- lm(y ~ x1 + x2, data = df, na.action =
            na.omit)


cf.mod <- coef(mod)

### Calculate z on a grid of x-y values
x1.seq <- seq(min(x1), max(x1), length.out = 231)
x2.seq <- seq(min(x2), max(x2), length.out = 231)
z.mtx <- t(outer(x1.seq, x2.seq, function(x1, x2)
  cf.mod[1] + cf.mod[2] * x1 + cf.mod[3] * x2))


#### Draw the plane with "plot_ly" and add points with "add_trace"


# Draw plane with plotly surface plot
p <-
  plot_ly(
    x =  ~ x1.seq,
    y =  ~ x2.seq,
    z =  ~ z.mtx,
    colors = c("#f5cb11", "#b31d83"),
    type = "surface"
  ) %>%
  add_trace(
    data = df,
    x = x1,
    y = x2,
    z = y,
    mode = "markers",
    type = "scatter3d",
    marker = list(
      color = "black",
      opacity = 0.7,
      symbol = 105
    )
  ) %>%
  layout(
    scene = list(
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = 1),
      xaxis = list(title = "GrLivArea", range = c(334, 4676)),
      yaxis =
        list(title = "LotArea", range = c(7, 12)),
      zaxis = list(title = "SalePrice", range = c(34000, 755000))
    )
  )

p
