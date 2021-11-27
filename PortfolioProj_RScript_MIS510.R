setwd("C:\\Users\\Ryan\\Documents\\Grad School\\MIS510-DataMining&Visualization")

GermanCredit.df <- read.csv("GermanCredit.csv")

head(GermanCredit.df)

GermanCredit.df <- GermanCredit.df[,-1]  #drop object column

head(GermanCredit.df)

GermanCredit.df[1]

#partition data

set.seed(1) #set seed for model
train.index <- sample(c(1:dim(GermanCredit.df)[1]), dim(GermanCredit.df)[1]*0.6)
train.index
train.df <- GermanCredit.df[train.index, ]
valid.df <- GermanCredit.df[-train.index, ]

head(train.df) #show first 6 rows of the training partition
head(valid.df) #show first 6 rows of the validation partition

##LOGISTIC REGRESSION MODEL

logit.reg <- glm(RESPONSE ~ ., data = train.df, family = "binomial") #create logistic regression model
options(scipen=999) #show values in non-scientific notation
summary(logit.reg) #display model results

library(gains)
pred <- predict(logit.reg, valid.df, type = "response") #create prediction values based on the validation partition

pred #display prediction values



gain <- gains(as.numeric(valid.df$RESPONSE), pred, groups=length(pred)) #store gains values for gains chart

valid.df$RESPONSE <- as.numeric(as.character(valid.df$RESPONSE)) #convert RESPONSE variable for graphing

#plot gains chart for the RESPONSE variable logistic regression model
plot(c(0,gain$cume.pct.of.total*sum(valid.df$RESPONSE))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$RESPONSE))~c(0, dim(valid.df)[1]), lty=2)

# install.packages("caret")
library(caret)

# install.packages("e1071")
library(e1071)

#create confusion matrix for Response Variable and predicted values
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid.df$RESPONSE))

##SELECTED VARIABLE LOGISTIC REGRESSION
SelectedGermanCredit.df <- GermanCredit.df[ ,c(1,2,3,11,13,23,29,30,31)] #Select statistically significant variables.
head(SelectedGermanCredit.df) #display first six rows of selected variables

#partition data with selected variables
set.seed(1) #set seed for model
train.index2 <- sample(c(1:dim(SelectedGermanCredit.df)[1]), dim(SelectedGermanCredit.df)[1]*0.6)
train.df2 <- SelectedGermanCredit.df[train.index2, ]
valid.df2 <- SelectedGermanCredit.df[-train.index2, ]

head(train.df2) #show first 6 rows of the training partition
head(valid.df2) #show first 6 rows of the validation partition


logit.reg2 <- glm(RESPONSE ~ ., data = train.df2, family = "binomial")
options(scipen=999)
summary(logit.reg2)

library(gains)
pred2 <- predict(logit.reg2, valid.df2, type = "response")

pred2
gain2 <- gains(as.numeric(valid.df2$RESPONSE), pred2, groups=length(pred2))

valid.df2$RESPONSE <- as.numeric(as.character(valid.df2$RESPONSE))

plot(c(0,gain2$cume.pct.of.total*sum(valid.df2$RESPONSE))~
       c(0,gain2$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df2$RESPONSE))~c(0, dim(valid.df2)[1]), lty=2)

# install.packages("caret")
library(caret)

# install.packages("e1071")
library(e1071)

confusionMatrix(as.factor(ifelse(pred2 > 0.5, 1, 0)), as.factor(valid.df2$RESPONSE))

##CLASSIFICATION TREE

library(rpart)
#install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
#install.packages("rpart.plot")
library(rpart.plot)


#regression tree
default.ct <- rpart(RESPONSE ~ ., data = train.df, method = "class")
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)