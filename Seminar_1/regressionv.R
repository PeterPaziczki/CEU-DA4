################################################################
#
# R Seminar 1, Zsuzsa Holler, Ăgoston Reguly
#
# Airbnb London 2017 march 05 data
#
# Main aim: Prediction models
#
# source: inside airbnb
# workfile 4
################################################################
#
# uses   airbnb_hackney_workfile_adj.csv
#
################################################################

# Clear environment
rm( list = ls())
#import libraries
library(data.table)
# install.packages("caret")
library(caret)

################################
## Some basic functions 2 use ##
################################

# Means Squared Error for log models
mse_log <- function (pred, y,corr){
  (mean((exp(y) - exp(pred) * exp(corr^2/2))^2, na.rm=T )) # corr is variance here
} # RMSE and log correction merged into one function

# Means Squared Error for simple models (level model)
mse_lev <- function(pred,y)
{
  (mean((pred-y)^2, na.rm=T))
}

#############
# Load data #
#############

# Set path
path <- "/Users/User/Documents/R_projects/CEU-DA4/Seminar_1"
setwd(path)
getwd()

# Used area
area <- "hackney"
data <- fread(paste0("airbnb_",area,"_workfile_adj.csv"),stringsAsFactors = T)
# Remove missing data, that has no score rating
data <- data[!is.na(n_review_scores_rating)]
# Change Infinite values with NaNs
for (j in 1:ncol(data) ) set(data, which(is.infinite(data[[j]])), j, NA)

#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since") 
basic_log <- c("ln_accommodates", "ln_beds", "f_property_type", "f_room_type","ln_days_since") 

# Factorized variables
basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type") 
reviews <- c("f_number_of_reviews","n_review_scores_rating") 
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since2")
poly_log <- c("ln_accommodates2","ln_days_since2","ln_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  names(data)[grep("^d_.*",names(data))] 

# Factor values
X1  <- c("f_room_type*f_property_type",  "f_number_of_reviews*f_property_type") 
# Interactions of factors and dummies
X2  <- c("d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type") 
# Other interactions
X3  <- c(paste0("(f_property_type + f_room_type + f_cancellation_policy + f_bed_type) * (",
                paste(names(data)[19:67],collapse=" + "),")"))

# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + ")) 
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))

# Create models in logs, models: 1-8
modellog1 <- " ~ ln_accommodates"
modellog2 <- paste0(" ~ ",paste(basic_log,collapse = " + "))
modellog3 <- paste0(" ~ ",paste(c(basic_log, basic_add),collapse = " + ")) 
modellog4 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log),collapse = " + "))
modellog5 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1),collapse = " + "))
modellog6 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2),collapse = " + "))
modellog7 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities),collapse = " + "))
modellog8 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities,X3),collapse = " + "))


#################################
# Create test and train samples #
#################################

# create test and train samples (90% of observations in train sample)
smp_size <- floor(0.9 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180122)

# create ids: 
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
train_ids <- sample(seq_len(nrow(data)), size = smp_size)

# create a new variable for train/test sample
data$train <- 0
data$train[train_ids] <- 1
# Create train and test sample variables
data_train <- data[train==1,]
data_test <- data[train==0,]

####################################
#         Compare models           #
####################################

# Create list to save model results
model_results <- list()

# For each level and logs
for (type in c("lev","log")) {
  # for each model
  for (i in ( 1 : 8 ) ) {
    
    # Get the proper model names
    model_name <- paste0("model",type,i)
    # Get the proper target variable
    yvar <- ifelse(type=="lev","price","ln_price")
    # Get the depedent variables
    xvars <- eval(parse(text = model_name))
    # Create the appropriate formula
    formula <- formula(paste0(yvar,xvars))
    # Estimate on the training data
    model <- lm(formula,data = data_train)
    # Predict on the training sample (in-sample)
    prediction_train <- predict(model, newdata = data_train)
    # Predict on the testing sample (out-of--sample)
    prediction_test <- predict(model, newdata = data_test)
    
    # Estimate the appropriate Criteria
    if (type=="lev") {
      mse_train <- mse_lev(prediction_train, data_train[,mget(yvar)])
      mse_test <- mse_lev(prediction_test, data_test[,mget(yvar)])
    } else {
      rmselog <- mse_lev(prediction_train, data_train[,mget(yvar)])**(1/2)
      mse_train <- mse_log(prediction_train, data_train[,mget(yvar)],rmselog)
      mse_test <- mse_log(prediction_test, data_test[,mget(yvar)],rmselog)
    }
    # Bayesian Criteria
    BIC <- BIC(model) # it does not use the concept of training and test sample, it is just a feature value of this model,
    ## it penalizes the number of variables and observations
    # Save into model results
    model_results[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model=model,
                                        prediction_train = prediction_train,prediction_test = prediction_test,
                                        mse_train = mse_train,mse_test = mse_test,BIC = BIC)
  }
}
## warning - rank-deficient: there are some dummy variables that are linearly dependent from each other, but it is gonna be a big issue now

## Example for levels:
vals <- matrix(rep(NaN,3*8),nrow=3,ncol=8)
for ( modelNum in 1 : 8 ){
  for ( crit in c("mse_train","mse_test","BIC") ){
    if ( modelNum == 1 ) dt <- model_results$modellev1
    if ( modelNum == 2 ) dt <- model_results$modellev2
    if ( modelNum == 3 ) dt <- model_results$modellev3
    if ( modelNum == 4 ) dt <- model_results$modellev4
    if ( modelNum == 5 ) dt <- model_results$modellev5
    if ( modelNum == 6 ) dt <- model_results$modellev6
    if ( modelNum == 7 ) dt <- model_results$modellev7
    if ( modelNum == 8 ) dt <- model_results$modellev8
    if ( crit == "mse_train" ) i <- 1 
    vals[i,modelNum] <- dt$mse_train
    if ( crit == "mse_test" ) i <- 2
    vals[i,modelNum] <- dt$mse_test
    if ( crit == "BIC" ) i <- 3
    vals[i,modelNum] <- dt$BIC
  }
}
vals

##############################
#      cross validation      #
##############################

## K/N = 10
n_folds=10
# Create the folds
folds_i <- sample(rep(1:n_folds, length.out = nrow(data) ))
# Create results
model_results_cv <- list()

for (type in c("lev","log")) {
  for (i in (1:8)){
    model_name <- paste0("model",type,i)
    
    yvar <- ifelse(type=="lev","price","ln_price")
    xvars <- eval(parse(text = model_name))
    formula <- formula(paste0(yvar,xvars))
    
    # Initialize values
    rmse_train <- c()
    rmse_train<- c()
    BIC<- c()
    
    # Do the k-fold estimation
    for (k in 1:n_folds) {
      test_i <- which(folds_i == k)
      # Train sample: all except test_i
      data_train <- data[-test_i, ]
      # Test sample
      data_test <- data[test_i, ]
      # Estimation and prediction
      model <- lm(formula,data = data_train)
      prediction_train <- predict(model, newdata = data_train)
      prediction_test <- predict(model, newdata = data_test)
      
      # Criteria evaluation
      if (type=="lev") {
        mse_train[k] <- mse_lev(prediction_train, data_train[,mget(yvar)])
        mse_test[k] <- mse_lev(prediction_test, data_test[,mget(yvar)])
      } else {
        rmselog <- mse_lev(prediction_train, data_train[,mget(yvar)])**(1/2)
        mse_train[k] <- mse_log(prediction_train, data_train[,mget(yvar)],rmselog)
        mse_test[k] <- mse_log(prediction_test, data_test[,mget(yvar)],rmselog)
      }
      
      BIC[k] <- BIC(model)
    }
    
    model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model=model,
                                           prediction_train = prediction_train,prediction_test = prediction_test,
                                           mse_train = mse_train,mse_test = mse_test,BIC = BIC)
  }
}

## Example for levels:
vals_CV <- matrix(rep(NaN,3*8),nrow=3,ncol=8)
for ( modelNum in 1 : 8 ){
  for ( crit in c("mse_train","mse_test","BIC") ){
    if ( modelNum == 1 ) dt <- model_results_cv$modellev1
    if ( modelNum == 2 ) dt <- model_results_cv$modellev2
    if ( modelNum == 3 ) dt <- model_results_cv$modellev3
    if ( modelNum == 4 ) dt <- model_results_cv$modellev4
    if ( modelNum == 5 ) dt <- model_results_cv$modellev5
    if ( modelNum == 6 ) dt <- model_results_cv$modellev6
    if ( modelNum == 7 ) dt <- model_results_cv$modellev7
    if ( modelNum == 8 ) dt <- model_results_cv$modellev8
    if ( crit == "mse_train" ) i <- 1 
    vals_CV[i,modelNum] <- mean( dt$mse_train )
    if ( crit == "mse_test" ) i <- 2
    vals_CV[i,modelNum] <- mean( dt$mse_test )
    if ( crit == "BIC" ) i <- 3
    vals_CV[i,modelNum] <- mean( dt$BIC )
  }
}
vals_CV




#################################
#           LASSO               #
#################################

# Package
#install.packages("glmnet")
library(glmnet)

## LASSO Model 1) for ln price:
# Convert training data to matrix format, use the broadest data set
formula <- formula(paste0("ln_price",modellog8))
# Create matrix
x <- model.matrix(formula,data)
# Call of LASSO function
# alpha = 1 gives lasso penalty
# find the best lambda from our list via cross-validation
lasso1 <- cv.glmnet( x , data[ ( match( rownames( x ) ,rownames( data ) ) ) , ln_price ], alpha = 1)
# Optimal lambda parameter
bestlam1 <- lasso1$lambda.min
# Prediction
lasso1.pred <- predict(lasso1, newx = x, s=bestlam1)
# MSE value
corr <- mse_lev(lasso1.pred,data[(match(rownames(x),rownames(data))),ln_price])
lasso1.mse <- mse_log(lasso1.pred,data[(match(rownames(x),rownames(data))),ln_price],corr)

## LASSO Model 2) for price levels:
formula <- formula(paste0("price",modellev8))
x <- model.matrix(formula,data)
lasso2 <- cv.glmnet(x, data[(match(rownames(x),rownames(data))),price], alpha = 1)
bestlam2 <- lasso2$lambda.min
lasso2.pred <- predict(lasso2, newx = x, s=bestlam2)
lasso2.mse <- mse_lev(lasso2.pred,data[(match(rownames(x),rownames(data))),price])

###################################################
# FIGURES FOR FITTED VS ACTUAL OOUTCOME VARIABLES #
###################################################  

# Choose model 7  
m <-7
# Get test sample logicals
test_i <- which(folds_i == 5)
data_train <- data[-test_i, ]
# Test sample
data_test <- data[test_i, ]
# Target variable
Ylev <- data_test[,price]
meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE= meanY -2* sdY 
meanY_p2SE= meanY +2* sdY 
Y5p = quantile(Ylev,0.05)
Y95p = quantile(Ylev,0.95)

# Logged target variable
Ylog <- data_test[,ln_price]

## Model 1) modellev7 on price
formula <- formula(paste0("price",modellev7))
modellev <- lm(formula,data = data_train)
# Predicted values
predictionlev_test <- predict(modellev, newdata = data_test)

## Model 2) modellog7 on log price
formula <- formula(paste0("ln_price",modellog7))
modellog <- lm(formula,data = data_train)
predictionlog_test <- predict(modellog, newdata = data_test)
rmselog <- mse_lev(predict(modellog, newdata = data_train),data_train[,ln_price])**(1/2)
# Predicted values
predictionlog_test2 <- exp(predictionlog_test) * exp((rmselog)^2/2)

# Create data frame with the real and predicted values
d <- data.frame( ylev=Ylev , ylog=Ylog , 
                 predlev=predictionlev_test , predlog=predictionlog_test2)
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot the Level and Log Prediction less than 400
ggplot(data = d[(d$ylev<400),], aes(x=ylev, y=predlev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_point(aes(x=ylev, y=predlog), size=2, colour="blue",shape=4)+
  geom_smooth(method = "lm", se = F, colour="orange") +
  geom_smooth(aes(x=ylev, y=predlog),method = "lm", se = F, colour="blue") +
  theme_bw()
#ggsave("log_vs_lin_all.png")

# Plot the Level and Log Prediction within 0.5% and 95%
ggplot(data = d[(d$ylev>Y5p) & (d$ylev<Y95p),], aes(x=ylev, y=predlev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_point(aes(x=ylev, y=predlog), size=2, colour="blue",shape=4)+
  geom_smooth(method = "lm", se = F, colour="orange") +
  geom_smooth(aes(x=ylev, y=predlog),method = "lm", se = F, colour="blue") +
  theme_bw()
#ggsave("log_vs_lin_95.png")

# Plot the log aginst the 45 line
ggplot(data = d[(d$ylev>Y5p) & (d$ylev<Y95p),], aes(x=ylev, y=predlev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x=ylev,y=ylev),method = "lm", se = F, color="black") +
  theme_bw()
#ggsave("log_vs_45.png")

# Level prediction against the errors
ggplot(data = d, aes(x=ylev, y=elev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#ggsave("F14_preerr1.png")

