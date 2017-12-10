# ISYE6414 Project
# setwd('D:\\Academics\\Georgia Tech\\Fall\\ISyE 6414 Regression Analysis\\Project\\Video Dataset')
library(MASS)
library(car)

#Reading the data from file
data <- read.table("transcoding_measurement.tsv", sep='\t', header=TRUE)

# Remove first column
data <- data[,-1]

#Removing the frames column as i + p + b = frame
data <- data[ ,-which(names(data) == "frames")]

#Removing the size column as i_size + b_size + p_size = size
data <- data[ ,-which(names(data) == "size")]

#Removing the b_size column as its empty
data <- data[ ,-which(names(data) == "b_size")]

# Subset data by either factor or numeric
data_num <- data[,sapply(data,is.numeric)]

# Understanding correlations between variables
round(cor(data_num),2)

# Correlation Matrix

library(corrplot)
par(mfrow=c(1,1))
X = model.matrix(lm(utime ~ ., data = data))[,-1]
X = cbind(data$utime, X)
corrplot(cor(X), tl.cex = 1)

#Strong correlation seen in height and width variables
plot(data$height,data$width)
plot(data$o_height,data$o_width)

#Removing the width column as its highly correlated with height
data <- data[ ,-which(names(data) == "width")]

#Removing the width column as its highly correlated with height
data <- data[ ,-which(names(data) == "o_width")]

# Subset data by either factor or numeric
data_fac <- data[,sapply(data,is.factor)]
data_num <- data[,sapply(data,is.numeric)]
colnames(data_fac)
colnames(data_num)

# Frequency/Histogram Plot
par(mfrow=c(4,4))
for (col in colnames(data_fac)) {
  plot(data_fac[,col], xlab=col, ylab="Frequency/Count", main=paste("Frequency of" , col)  )
}
for ( col in colnames(data_num)) {
  hist(data[,col], xlab=col, ylab="Frequency/Count", main=paste("Histogram of" , col))
}

data_res <- data_num[,14]
data_num <- data_num[,-14]

# Scatter/box Plot for numeric variables
par(mfrow=c(4,4))
for (col in colnames(data_fac)) {
  boxplot(data_res~data_fac[,col], xlab=col, ylab="Transcoding Time in sec")
}

for (col in colnames(data_num)) {
  plot(data_res~data_num[,col], xlab=col, ylab="Transcoding Time in sec")
}

#Attaching the data to R environment
attach(data)

#Fitting the model
model <- lm(log(utime)~.,data=data)
summary(model)

#Extracting specific parameter values
coef(summary(model))["duration","Pr(>|t|)"]
coef(summary(model))["duration","Estimate"]

#MLR Assumption validation
res = model$res
par(mfrow = c(4,4))

#Verifying Constant variance and Linearity
plot(utime, res, xlab = "utime", ylab = "Residuals", pch = 19, main = 'Residual vs utime')
abline(h = 0, col="red")

plot(height, res, xlab = "height", ylab = "Residuals", pch = 19, main = 'Residual vs height')
abline(h = 0,col="red")

plot(bitrate, res, xlab = "bitrate", ylab = "Residuals", pch = 19, main = 'Residual vs bitrate')
abline(h = 0,col="red")

plot(framerate, res, xlab = "framerate", ylab = "Residuals", pch = 19, main = 'Residual vs framerate')
abline(h = 0,col="red")

plot(i, res, xlab = "i", ylab = "Residuals", pch = 19, main = 'Residual vs i')
abline(h = 0,col="red")

plot(p, res, xlab = "p", ylab = "Residuals", pch = 19, main = 'Residual vs p')
abline(h = 0,col="red")

plot(b, res, xlab = "b", ylab = "Residuals", pch = 19, main = 'Residual vs b')
abline(h = 0,col="red")

plot(i_size, res, xlab = "i_size", ylab = "Residuals", pch = 19, main = 'Residual vs i_size')
abline(h = 0,col="red")

plot(p_size, res, xlab = "p_size", ylab = "Residuals", pch = 19, main = 'Residual vs p_size')
abline(h = 0,col="red")

plot(o_height, res, xlab = "o_height", ylab = "Residuals", pch = 19, main = 'Residual vs o_height')
abline(h = 0,col="red")

plot(o_bitrate, res, xlab = "o_bitrate", ylab = "Residuals", pch = 19, main = 'Residual vs o_bitrate')
abline(h = 0,col="red")

plot(o_framerate, res, xlab = "o_framerate", ylab = "Residuals", pch = 19, main = 'Residual vs o_framerate')
abline(h = 0,col="red")

plot(umem, res, xlab = "umem", ylab = "Residuals", pch = 19, main = 'Residual vs umem')
abline(h = 0,col="red")

#Verifying Normality of residuals
hist(res, xlab="Residuals", main= "Histogram of Residuals")
qqPlot(residuals(model),lwd=1, main = 'Normality of residuals')

#Identifying influencing datapoints
cookdist <- cooks.distance(model)
plot(cookdist, main = "Cook's Distance")
detach(data)


#Removing outlying points in Cook's dist and reverifying regression model
HighCDP <- which(cookdist >= 0.05)

#Printing high influential points
data[HighCDP,]

####Remove Outliers by Cook's Distance#######
#Fitting the model again with the influential points removed
set.seed(999)
# Split the dataset (with outliers removed) by 90% (Train/Validation) and 10% (Final Test)
newdata_ind <- sample(1:nrow(data[-HighCDP,]), floor(0.9*nrow(data[-HighCDP,])))
newdata<- (data[-HighCDP,])[newdata_ind,]
newdata_final_test <- (data[-HighCDP,])[-newdata_ind,]

model_woHighCDP <- lm(log(utime)~.,data=newdata)
summary(model_woHighCDP)

attach(newdata)

#MLR Assumption validation
res = model_woHighCDP$res
par(mfrow = c(4,4))

#Verifying Constant variance and Linearity
plot(utime, res, xlab = "utime", ylab = "Residuals", pch = 19, main = 'Residual vs utime')
abline(h = 0,col="red")

plot(height, res, xlab = "height", ylab = "Residuals", pch = 19, main = 'Residual vs height')
abline(h = 0,col="red")

plot(bitrate, res, xlab = "bitrate", ylab = "Residuals", pch = 19, main = 'Residual vs bitrate')
abline(h = 0,col="red")

plot(framerate, res, xlab = "framerate", ylab = "Residuals", pch = 19, main = 'Residual vs framerate')
abline(h = 0,col="red")

plot(i, res, xlab = "i", ylab = "Residuals", pch = 19, main = 'Residual vs i')
abline(h = 0,col="red")

plot(p, res, xlab = "p", ylab = "Residuals", pch = 19, main = 'Residual vs p')
abline(h = 0,col="red")

plot(b, res, xlab = "b", ylab = "Residuals", pch = 19, main = 'Residual vs b')
abline(h = 0,col="red")

plot(i_size, res, xlab = "i_size", ylab = "Residuals", pch = 19, main = 'Residual vs i_size')
abline(h = 0,col="red")

plot(p_size, res, xlab = "p_size", ylab = "Residuals", pch = 19, main = 'Residual vs p_size')
abline(h = 0,col="red")

plot(o_height, res, xlab = "o_height", ylab = "Residuals", pch = 19, main = 'Residual vs o_height')
abline(h = 0,col="red")

plot(o_bitrate, res, xlab = "o_bitrate", ylab = "Residuals", pch = 19, main = 'Residual vs o_bitrate')
abline(h = 0,col="red")

plot(o_framerate, res, xlab = "o_framerate", ylab = "Residuals", pch = 19, main = 'Residual vs o_framerate')
abline(h = 0,col="red")

plot(umem, res, xlab = "umem", ylab = "Residuals", pch = 19, main = 'Residual vs umem')
abline(h = 0,col="red")

#Verifying Normality of residuals
hist(res, xlab="Residuals", main= "Histogram of Residuals")
qqPlot(residuals(model_woHighCDP),lwd=1, main = 'Normality of residuals')

#Verifying Independence of residuals
plot(res, xlab = 'Sequence',ylab = 'Residuals', main = 'Residuals Independence')

#Identifying influencing datapoints
cookdist <- cooks.distance(model_woHighCDP)
plot(cookdist, main = "Cook's Distance")

detach(newdata)

####Train models with different subset of data & Derive Coef#######
#Number of iterations for running the data
num_iter = 20

#Sequence of dataset sizes as fraction of total data
size_vec = c(0.4, 0.8, 0.95, 1.0)

#Initialize list of lists for parameters
parameter_vals = list()

#Iterating for different sample sizes
for (size_index in seq(length(size_vec))){
  
  #Intialize all variables to null
  duration_vec <- codech264_vec <- codecmpeg4_vec <- codecvp8_vec <- height_vec <- bitrate_vec <- NULL
  framerate_vec <- i_vec <- p_vec <- b_vec <- i_size_vec <- p_size_vec <- o_codech264_vec <- NULL
  o_codecmpeg4_vec <- o_codecvp8_vec <- o_bitrate_vec <- o_framerate_vec <-  o_height_vec <- NULL
  umem_vec <- NULL
  
  #Calculate the fraction size %
  size_i = size_vec[size_index]
    
  #%age size of the sample size
  smp_size_i <- floor(size_i * nrow(newdata))
  
  #Collecting model coefficient values for multiple runs for fixed fractional sample size
  for (cnt in seq(num_iter)){
    ## set the seed to make your partition reproductible
    train_ind <- sample(seq_len(nrow(newdata)), size = smp_size_i)
    train <- newdata[train_ind, ]
    
    #Fit the model for the iteration
    model_stab <- lm(log(utime)~.,data=train)
    
    #Extracting specific parameter values
    duration_vec <- c(duration_vec, coef(summary(model_stab))["duration","Estimate"])
    codech264_vec <- c(codech264_vec, coef(summary(model_stab))["codech264","Estimate"])
    codecmpeg4_vec <- c(codecmpeg4_vec, coef(summary(model_stab))["codecmpeg4","Estimate"])
    codecvp8_vec <- c(codecvp8_vec, coef(summary(model_stab))["codecvp8","Estimate"])
    height_vec <- c(height_vec, coef(summary(model_stab))["height","Estimate"])
    bitrate_vec <- c(bitrate_vec, coef(summary(model_stab))["bitrate","Estimate"])
    framerate_vec <- c(framerate_vec, coef(summary(model_stab))["framerate","Estimate"])
    i_vec <- c(i_vec, coef(summary(model_stab))["i","Estimate"])
    p_vec <- c(p_vec, coef(summary(model_stab))["p","Estimate"])
    b_vec <- c(b_vec, coef(summary(model_stab))["b","Estimate"])
    i_size_vec <- c(i_size_vec, coef(summary(model_stab))["i_size","Estimate"])
    p_size_vec <- c(p_size_vec, coef(summary(model_stab))["p_size","Estimate"])
    o_codech264_vec <- c(o_codech264_vec, coef(summary(model_stab))["o_codech264","Estimate"])
    o_codecmpeg4_vec <- c(o_codecmpeg4_vec, coef(summary(model_stab))["o_codecmpeg4","Estimate"])
    o_codecvp8_vec <- c(o_codecvp8_vec, coef(summary(model_stab))["o_codecvp8","Estimate"])
    o_bitrate_vec <- c(o_bitrate_vec, coef(summary(model_stab))["o_bitrate","Estimate"])
    o_framerate_vec <- c(o_framerate_vec, coef(summary(model_stab))["o_framerate","Estimate"])
    o_height_vec <- c(o_height_vec, coef(summary(model_stab))["o_height","Estimate"])
    umem_vec <- c(umem_vec, coef(summary(model_stab))["umem","Estimate"])
  }
  parameter_vals_sizei = list(duration_vec, codech264_vec,codecmpeg4_vec,codecvp8_vec, height_vec, 
                              bitrate_vec, framerate_vec, i_vec, p_vec, b_vec, i_size_vec, p_size_vec, 
                              o_codech264_vec,o_codecmpeg4_vec, o_codecvp8_vec, o_bitrate_vec, 
                              o_framerate_vec, o_height_vec, umem_vec)
  parameter_vals[size_index] = list(parameter_vals_sizei)
}


#Plotting the graph for 1st variable
vars = c('duration', 'codech264','codecmpeg4','codecvp8', 'height,bitrate', 'framerate', 'i', 
         'p', 'b', 'i_size', 'p_size', 'o_codech264','o_codecmpeg4', 'o_codecvp8', 'o_bitrate', 
         'o_framerate', 'o_height', 'umem')

#Plotting first 9 variables
n_model_vars = 9
par(mfrow = c(3,3))
for (var_index in seq(n_model_vars)){
  plot(parameter_vals[[1]][[var_index]],col='black', ylab=vars[var_index])
  points(points(parameter_vals[[2]][[var_index]],col='blue'))
  points(points(parameter_vals[[3]][[var_index]],col='red'))
  points(points(parameter_vals[[4]][[var_index]],col='green'))
  legend("bottomright",legend=c('0.4', '0.8', '0.95', '1.0'), pch=1, col=c("black","blue","red","green"))
}

mtext('Stability of coefficient values vs Sample sizes', side = 3, line = -2, outer = TRUE)

#Plotting 10-18 variables
n_model_vars = 9 
par(mfrow = c(3,3))
for (var_index in seq(10, 10+n_model_vars-1)){
  plot(parameter_vals[[1]][[var_index]],col='black', ylab=vars[var_index])
  points(points(parameter_vals[[2]][[var_index]],col='blue'))
  points(points(parameter_vals[[3]][[var_index]],col='red'))
  points(points(parameter_vals[[4]][[var_index]],col='green'))
  legend("bottomright",legend=c('0.4', '0.8', '0.95', '1.0'), pch=1, col=c("black","blue","red","green"))
}
mtext('Stability of coefficient values vs Sample sizes', side = 3, line = -2, outer = TRUE)


#Plotting last variable
n_model_vars = 1 
par(mfrow = c(3,3))
for (var_index in seq(19, 19+n_model_vars-1)){
  plot(parameter_vals[[1]][[var_index]],col='black', ylab=vars[var_index])
  points(points(parameter_vals[[2]][[var_index]],col='blue'))
  points(points(parameter_vals[[3]][[var_index]],col='red'))
  points(points(parameter_vals[[4]][[var_index]],col='green'))
  legend("bottomright",legend=c('0.4', '0.8', '0.95', '1.0'), pch=1, col=c("black","blue","red","green"))
}
mtext('Stability of coefficient values vs Sample sizes', side = 3, line = -2, outer = TRUE)

####Variable Selection#######
#STEPWISE REGRESSION
lowermodel = lm(log(utime)~umem, data = newdata)
#upper = model_woHighCDP
#lower = lowermodel
stepwisemodelforward = step(lowermodel, scope=list(lower=lowermodel,upper=model_woHighCDP), direction='forward')
stepwisemodelback = step(model_woHighCDP, scope=list(lower=lowermodel,upper=model_woHighCDP), direction='backward')

#LASSO REGRESSION
par(mfrow = c(1,1))
library(glmnet)
Y = cbind(data$utime)
#X = cbind(duration, codec, height, bitrate, framerate, i, p, i_size, p_size, o_codec, o_bitrate,
#          o_framerate, o_height, umem)
X = model.matrix(model_woHighCDP)[,-1]
lasmodel = cv.glmnet(X, newdata[,c('utime')], alpha = 1, nfolds=10)
lasmodellam = glmnet(X,newdata[,c('utime')], alpha = 1, nlambda = 100)
coef(lasmodellam, s=lasmodel$lambda.min)
plot(lasmodellam,xvar='lambda',lwd=2)
abline(v=log(lasmodel$lambda.min),col='black',lty=2,lwd=2)


###ELASTIC NET###
X = model.matrix(model_woHighCDP)[,-1]
modelcv = cv.glmnet(X,newdata[,c('utime')], alpha =.5, nfolds = 10)
modelcvlam = glmnet(X,newdata[,c('utime')], alpha =.5, nlambda = 100)
coef(modelcvlam, s=modelcv$lambda.min)


###RIDGE REGRESSION###
X = model.matrix(model_woHighCDP)[,-1]
modelcv = cv.glmnet(X,newdata[,c('utime')], alpha = 0, nfolds = 10)
modelcvlam = glmnet(X,newdata[,c('utime')], alpha = 0, nlambda = 100)
coef(modelcvlam, s=modelcv$lambda.min)

#Add or drop all possible single terms to model
drop1(model_woHighCDP)

#Choose model by AIC, default direction is both
stepAIC(model_woHighCDP)


####Training & Validatation between Models using 90% of the original dataset#######

# Define Accuracy Metrics 
# Initiate Mean Square Error
mse_full<- mse_step_forward <- mse_step_backward <- mse_step_both <- mse_lasso <- mse_ridge <- mse_elastic <- NULL

#set.seed(999)
# Number of iterations for running the data
num_iter = 20

for (cnt in seq(num_iter)){
  newdata_train_ind <- sample(1:nrow(newdata), floor(0.8*nrow(newdata)))
  newdata_train <- newdata[newdata_train_ind,]
  newdata_test <- newdata[-newdata_train_ind,]
  dim(newdata_train)
  dim(newdata_test)
  X_test <- newdata_test[,-16]
  y_test <- newdata_test[,16]
  
  # Prediction on Test Set
  # Full Model
  full_model <- lm(log(utime)~.,data=newdata_train)
  y_predict_full <- predict(full_model, X_test)
  y_predict_full <- exp(y_predict_full)
  mse_full <- cbind(mse_full,mean((y_predict_full-y_test)^2))
  
  # Stepwise Regression Forward
  min_model <- lm(log(utime)~umem, data = newdata_train)
  step_forward_model <- step(min_model, scope=list(lower=min_model,upper=full_model), direction='forward')
  y_predict_step_forward <- predict(step_forward_model, X_test)
  y_predict_step_forward <- exp(y_predict_step_forward)
  mse_step_forward <- cbind(mse_step_forward,mean((y_predict_step_forward-y_test)^2))
  
  # Stepwise Regression Backward
  step_back_model <- step(full_model, scope=list(lower=min_model,upper=full_model), direction='backward')
  y_predict_step_backward <- predict(step_back_model, X_test)
  y_predict_step_backward <- exp(y_predict_step_backward)
  mse_step_backward <- cbind(mse_step_backward,mean((y_predict_step_backward-y_test)^2))
  
  # Stepwise Regression Both
  step_both_model <- stepAIC(full_model)
  y_predict_step_both <- predict(step_both_model, X_test)
  y_predict_step_both <- exp(y_predict_step_both)
  mse_step_both <- cbind(mse_step_both,mean((y_predict_step_both-y_test)^2))
  
  # Convert to matrix form for glmnet
  X_train <- model.matrix(full_model)[,-1]
  X_test <- (model.matrix(log(utime)~.,data=newdata_test))[,-1]
  y_train <- log(newdata_train[,c('utime')])
  
  # Lasso Regression
  lasso_model_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds=10)
  lasso_model <- glmnet(X_train, y_train, alpha = lasso_model_cv$lambda.min)
  #coef(lasso_model, s=lasso_model_cv$lambda.min)
  y_predict_lasso <- predict(lasso_model, newx = X_test, s= lasso_model_cv$lambda.min, type="response")
  y_predict_lasso <- exp(y_predict_lasso)
  mse_lasso <- cbind(mse_lasso, mean((y_predict_lasso-y_test)^2))
  
  # Ridge Regression
  ridge_model_cv <- cv.glmnet(X_train, y_train, alpha = 0, nfolds=10)
  ridge_model <- glmnet(X_train, y_train, alpha = ridge_model_cv$lambda.min)
  y_predict_ridge <- predict(ridge_model, newx = X_test, s= ridge_model_cv$lambda.min, type="response")
  y_predict_ridge <- exp(y_predict_ridge)
  mse_ridge <- cbind(mse_ridge, mean((y_predict_ridge-y_test)^2))
  
  # Elastic Net Regression
  elastic_model_cv <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds=10)
  elastic_model <- glmnet(X_train, y_train, alpha = elastic_model_cv$lambda.min)
  y_predict_elastic <- predict(elastic_model, newx = X_test, s= elastic_model_cv$lambda.min, type="response")
  y_predict_elastic <- exp(y_predict_elastic)
  mse_elastic <- cbind(mse_elastic, mean((y_predict_elastic-y_test)^2))
}
model_names <- c("Full Model", "Stepwise Forward", "Step Backward", "Step Both", "Lasso", "Ridge", "Elastic Net")
list_mse <- list(mse_full,mse_step_forward,mse_step_backward,mse_step_both,mse_lasso,mse_ridge,mse_elastic)

par(mfrow = c(2,4))
for (i in seq(length(model_names))) {
  plot(seq(num_iter), unlist(list_mse[i]), main = model_names[i], xlab="Iteration", ylab="MSE")
  abline(h=mean(unlist(list_mse[i])), col="red")
  #print("MSE of %s",model_names[i])
  cat("MSE Summary of", model_names[i], "After",num_iter, "Runs :\n")
  print(summary(unlist(list_mse[i])))
}

####K-fold Cross Validation#######

# Define Accuracy Metrics 
# Initiate Mean Square Error
mse_full<- mse_step_forward <- mse_step_backward <- mse_step_both <- mse_lasso <- mse_ridge <- mse_elastic <- NULL

set.seed(999)
# Number of iterations for running the data
k_fold = 10
newdata_cross <- newdata[sample(nrow(newdata)),]
folds <-cut(seq(1,nrow(newdata_cross)), breaks=k_fold, labels=FALSE)

for (cnt in seq(k_fold)){
  newdata_test_ind <- which(folds==cnt, arr.ind = TRUE)
  newdata_test <- newdata_cross[newdata_test_ind,]
  newdata_train <- newdata_cross[-newdata_test_ind,]
  head(newdata_train)
  
  dim(newdata_train)
  dim(newdata_test)
  X_test <- newdata_test[,-16]
  y_test <- newdata_test[,16]
  
  # Prediction on Test Set
  # Full Model
  full_model <- lm(log(utime)~.,data=newdata_train)
  y_predict_full <- predict(full_model, X_test)
  y_predict_full <- exp(y_predict_full)
  mse_full <- cbind(mse_full,mean((y_predict_full-y_test)^2))
  
  # Stepwise Regression Forward
  min_model <- lm(log(utime)~umem, data = newdata_train)
  step_forward_model <- step(min_model, scope=list(lower=min_model,upper=full_model), direction='forward')
  y_predict_step_forward <- predict(step_forward_model, X_test)
  y_predict_step_forward <- exp(y_predict_step_forward)
  mse_step_forward <- cbind(mse_step_forward,mean((y_predict_step_forward-y_test)^2))
  
  # Stepwise Regression Backward
  step_back_model <- step(full_model, scope=list(lower=min_model,upper=full_model), direction='backward')
  y_predict_step_backward <- predict(step_back_model, X_test)
  y_predict_step_backward <- exp(y_predict_step_backward)
  mse_step_backward <- cbind(mse_step_backward,mean((y_predict_step_backward-y_test)^2))
  
  # Stepwise Regression Both
  step_both_model <- stepAIC(full_model)
  y_predict_step_both <- predict(step_both_model, X_test)
  y_predict_step_both <- exp(y_predict_step_both)
  mse_step_both <- cbind(mse_step_both,mean((y_predict_step_both-y_test)^2))
  
  # Convert to matrix form for glmnet
  X_train <- model.matrix(full_model)[,-1]
  X_test <- (model.matrix(log(utime)~.,data=newdata_test))[,-1]
  y_train <- log(newdata_train[,c('utime')])
  
  # Lasso Regression
  lasso_model_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds=10)
  lasso_model <- glmnet(X_train, y_train, alpha = lasso_model_cv$lambda.min)
  #coef(lasso_model, s=lasso_model_cv$lambda.min)
  y_predict_lasso <- predict(lasso_model, newx = X_test, s= lasso_model_cv$lambda.min, type="response")
  y_predict_lasso <- exp(y_predict_lasso)
  mse_lasso <- cbind(mse_lasso, mean((y_predict_lasso-y_test)^2))
  
  # Ridge Regression
  ridge_model_cv <- cv.glmnet(X_train, y_train, alpha = 0, nfolds=10)
  ridge_model <- glmnet(X_train, y_train, alpha = ridge_model_cv$lambda.min)
  y_predict_ridge <- predict(ridge_model, newx = X_test, s= ridge_model_cv$lambda.min, type="response")
  y_predict_ridge <- exp(y_predict_ridge)
  mse_ridge <- cbind(mse_ridge, mean((y_predict_ridge-y_test)^2))
  
  # Elastic Net Regression
  elastic_model_cv <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds=10)
  elastic_model <- glmnet(X_train, y_train, alpha = elastic_model_cv$lambda.min)
  y_predict_elastic <- predict(elastic_model, newx = X_test, s= elastic_model_cv$lambda.min, type="response")
  y_predict_elastic <- exp(y_predict_elastic)
  mse_elastic <- cbind(mse_elastic, mean((y_predict_elastic-y_test)^2))
}

model_names <- c("Full Model", "Stepwise Forward", "Step Backward", "Step Both", "Lasso", "Ridge", "Elastic Net")
list_mse <- list(mse_full,mse_step_forward,mse_step_backward,mse_step_both,mse_lasso,mse_ridge,mse_elastic)

par(mfrow = c(2,4))
for (i in seq(length(model_names))) {
  plot(seq(k_fold), unlist(list_mse[i]), main = model_names[i], xlab="Iteration", ylab="MSE")
  abline(h=mean(unlist(list_mse[i])), col="red")
  #print("MSE of %s",model_names[i])
  cat("MSE Summary of", model_names[i], "With",k_fold, "Fold Validation :\n")
  print(summary(unlist(list_mse[i])))
}


# Test on model selected using 10% of the original dataset
X_test_final <- newdata_final_test[,-18]
y_test_final <- newdata_final_test[,18]
full_model_final <- lm(log(utime)~.,data=newdata)
y_predict_full_final <- predict(full_model_final, X_test_final)
y_predict_full_final <- exp(y_predict_full_final)
mse_full_final <- mean((y_predict_full_final-y_test_final)^2)

X_train_final <- model.matrix(full_model_final)[,-1]
X_test_final <- (model.matrix(log(utime)~.,data=newdata_final_test))[,-1]
y_train_final <- log(newdata[,c('utime')])

# Elastic Net Regression
elastic_model_cv_final <- cv.glmnet(X_train_final, y_train_final, alpha = 0.5, nfolds=10)
elastic_model_final <- glmnet(X_train_final, y_train_final, alpha = elastic_model_cv_final$lambda.min)
y_predict_elastic_final <- predict(elastic_model_final, newx = X_test_final, s= elastic_model_cv_final$lambda.min, type="response")
y_predict_elastic_final <- exp(y_predict_elastic_final)
mse_elastic_final <- mean((y_predict_elastic_final-y_test_final)^2)

