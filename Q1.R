install.packages(ggplot2)
install.packages(class)
library(class)
library(ggplot2)
library(MASS)
library(leaps)
c_data <- read.csv("cereal.csv")
head(c_data)
summary(c_data)
dim(c_data)
new_data <- c_data[-c(1,2,3)]
head(new_data)
##Dividing the data into test and training##
set.seed(248)
indi = sample(length(new_data[,1]), 2/3*length(new_data[,1]))
train = new_data[indi, ]
test = new_data[-indi, ]
summary(train)
summary(test)
##Part(a)##
boxplot(new_data$rating~new_data$sugars, main="Boxplot of rating against sugars", xlab="sugars", ylab="rating")
model_train<-lm(rating~., data = train)
summary(model_train)
model_test<-lm(rating~., data = test)
summary(model_test)
##train error##
class(train[,12])
##train[ ,12] <- as.numeric(train[ ,12])
y_train <- round(predict(model_train, data = train))
y_true  <- train$rating
data.frame(y_train,y_true)
error_train <- (1/length(y_train))*sum((y_train-y_true)^2)
error_train
##test error##
y_test <- round(predict(model_test, data = test))
y_true  <- test$rating
data.frame(y_test,y_true)
error_test <- (1/length(y_test))*sum((y_test-y_true)^2)
error_test
##plot(new_data$sugars,new_data$rating, main="Ratings vs Sugars (with regression line)", xlab= "Sugars Content", ylab="Cereal Rating", col="blue", pch=18)
##abline(model$coef, lwd=2, col="dark blue")
##part-b## Forward Subset selection##
?regsubsets
regfit.fwd <- regsubsets(rating~., data = train, nbest = 1, nvmax = 12, method = "forward", really.big = TRUE)
my_sum <- summary(regfit.fwd)
my_sum
summary(my_sum)
##Calculating MSE##
test.matx = model.matrix(rating ~., data=test)
val.f.error = rep(NA,12)

for (i in 1:12){
  coefie = coef(regfit.fwd, id=i)
  predic = test.matx[,names(coefie)]%*%coefie
  val.f.error[i] = mean((test$rating-predic)^2)
}
val.f.error
##Visualize the best model##model with 9 variables is the best model##
which(my_sum$cp == min(my_sum$cp))
which(my_sum$bic == min(my_sum$bic))
x11()
plot(regfit.fwd, scale = "r2")
x11()
plot(regfit.fwd, scale = "adjr2")
x11()
plot(regfit.fwd, scale = "Cp")
x11()
plot(regfit.fwd, scale = "bic")
##part-c## Exhaustive subset selection##
regfit.full <- regsubsets(rating~., data = train, nbest = 1, nvmax = 12, method = "exhaustive", really.big = TRUE)
my_sum_full <- summary(regfit.full)
my_sum_full
summary(my_sum_full)
##Calculating MSE##
test.matx = model.matrix(rating ~., data=test)
val.e.error = rep(NA,12)

for (i in 1:12){
  coefie = coef(regfit.full, id=i)
  pred = test.matx[,names(coefie)]%*%coefie
  val.e.error[i] = mean((test$rating-pred)^2)
}
val.e.error
##Visualize the best model##model with 9 variables is the best model##
which(my_sum$cp == min(my_sum$cp))
which(my_sum$bic == min(my_sum$bic))
x11()
plot(regfit.fwd, scale = "r2")
x11()
plot(regfit.fwd, scale = "adjr2")
x11()
plot(regfit.fwd, scale = "Cp")
x11()
plot(regfit.fwd, scale = "bic")
# Examine the best 7 (or whatever) variable models
summary(regfit.full)$outmat[7,]
summary(regfit.fwd)$outmat[7,]
##Look at the regression models determined by the different methods
coef(regfit.full, 5)
coef(regfit.fwd, 5)
