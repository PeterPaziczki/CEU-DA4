# Install and attach necessary packages
library(data.table)
library(glmnet) # lasso
library(rpart) #trees
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(randomForest)
library(gbm)

#define MSE function
mse_lev <- function(pred,y)
{
  (mean((pred-y)^2, na.rm=T))
}


############################################LOAD DATA

# Set working directory
setwd("/home/zsuzsa/Documents/DA4")

# Used area
area <- "hackney"
data <- fread(paste0("airbnb_",area,"_workfile_adj.csv"),stringsAsFactors = T)
# Remove missing data, that has no score rating
data <- data[!is.na(n_review_scores_rating)]
# Change Infinite values with NaNs
for (j in 1:ncol(data) ) set(data, which(is.infinite(data[[j]])), j, NA)

#################################
# Create test and train samples #
#################################

set.seed(2801)
# create test and train samples (80% of observations in train sample)
smp_size <- floor(0.8 * nrow(data))

# create ids: 
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
train_ids <- sample(seq_len(nrow(data)), size = smp_size)

# create a new variable for train/test sample
data$train <- 0
data$train[train_ids] <- 1
# Create train and test sample variables
d_train <- data[train==1,]
d_test <- data[train==0,]
  
#############################################define models

basic_lev  <- c("n_accommodates", "n_beds",  "n_days_since", "f_property_type", "f_room_type") 
# Factorized variables
basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type") 
reviews <- c("f_number_of_reviews","n_review_scores_rating") 
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")
# Factor values
X1  <- c("f_room_type*f_property_type",  "f_number_of_reviews*f_property_type") 
# Interactions of factors and dummies
X2  <- c("d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type") 
# Dummy variables: Extras -> collect all options and create dummies
amenities <-  names(data)[grep("^d_.*",names(data))] 

# models for illustration purposes
model0<- formula(paste0("price ~ ",paste(c(basic_lev[1]),collapse = " + ")) )

# these are sparse models for ols
model_lin1<-  formula(paste0("price ~ ",paste(c(basic_lev, basic_add),collapse = " + ")) )
model_lin2<- formula(paste0("price ~ ",paste(c(basic_lev, basic_add, reviews, amenities[1:37]),collapse = " + ")) )
model_allvar<-formula(paste0("price ~ ",paste(c(basic_lev, basic_add, reviews, amenities),collapse = " + ")) )
model_allvar2<-formula(paste0("price ~ ",paste(c(basic_lev, basic_add, reviews, poly_lev, X1, X2, amenities),collapse = " + ")) )

#####################
# OLS
#####################

n_folds=10
# Create the folds
folds_i <- sample(rep(1:n_folds, length.out = nrow(d_train) ))

#cross validation of regressions
cv_reg <- function(formula,n_folds,folds_i,data )
{
    rmse_test<- c()
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
    
    rmse_test[k] <- mse_lev(prediction_test, data_test[,"price"])
  }  
  return(rmse_test)
}

rmse_ols1 <- mean(cv_reg(model_lin2,n_folds,folds_i,d_train ))**(1/2)
rmse_ols2 <- mean(cv_reg(model_allvar,n_folds,folds_i,d_train ))**(1/2)
rmse_ols3 <- mean(cv_reg(model_allvar2,n_folds,folds_i,d_train ))**(1/2)

#test rmse for best regression model
model_ols <- lm(model_allvar2,data = d_train)
prediction_test_ols <- predict(model_ols, newdata = d_test)
rmse_ols_test <- mse_lev(prediction_test_ols, d_test[,"price"])**(1/2)

#####################
# LASSO
#####################

# Call of LASSO function
x <- model.matrix(model_allvar,d_train)
# alpha = 1 gives lasso penalty
# find the best lambda from our list via cross-validation
lasso2 <- cv.glmnet( x , d_train[ ( match( rownames( x ) ,rownames( d_train ) ) ) , price ], alpha = 1)
# Optimal lambda parameter
bestlam2 <- lasso2$lambda.min
rmse_lasso <- lasso2$cvm[lasso2$lambda == bestlam2]**(1/2)
rmse_lasso

#test rmse for best lasso
lasso_best <- glmnet( x , d_train[ ( match( rownames( x ) ,rownames( d_train ) ) ) , price ], alpha = 1, lambda = bestlam2)
newx <- model.matrix(model_allvar,d_test)
prediction_test_lasso <- predict(lasso_best, newx = newx)
rmse_lasso_test <- mse_lev(prediction_test_lasso, d_test[,"price"])**(1/2)

#####################
#rpart - Trees
#####################

# we do some trees for illustration purposes
# single predictor

md3 <- rpart(model0,data=d_train, 
             method = "anova",
             control=rpart.control(minsplit=100, cp=0.001))
summary(md3)
printcp(md3)
plotcp(md3)

# simple plot tree
plot(md3, uniform=TRUE, main="Tree for basic model")
text(md3, use.n=TRUE, all=TRUE, cex=1)


# nice  plots
png(filename = "fa5a.png")
prp(md3,varlen=5, main = "Airbnb Hackney, Large tree")	
dev.off()
# my fav plot -->exported as png
png(filename = "fa1b.png")
fancyRpartPlot(md3, main = "Airbnb Hackney, Large tree")	
dev.off()


# smaller tree
md3s <- rpart(model0,data=d_train, 
              method = "anova",
              control=rpart.control(minsplit=100,minbucket=50,cp=0.1))
summary(md3s)

# nice  plots
png(filename = "fa2a.png")
prp(md3s,varlen=5, main = "Airbnb Hackney prices, small tree")	
dev.off()
# my fav plot -->exported as png
png(filename = "fa2b.png")
fancyRpartPlot(md3s, main = "Airbnb Hackney, small tree")	
dev.off()

##############################################x
### Look at models to compare
##############################################x


# tree to compare
md3 <- rpart(model_allvar,data=d_train, 
             method = "anova",
             control=rpart.control(minbucket=10,cp=0.00001))
summary(md3)

price_pred <- predict(md3, d_test)
rmse_cart <- sqrt(mean((price_pred-d_test$price)^2,na.rm=T))
rmse_cart

printcp(md3)
plotcp(md3)
# my fav plot -->exported as png
png(filename = "fa4b.png")
fancyRpartPlot(md3, main = "Airbnb Hackney, small multivar tree")	
dev.off()

# prune the tree 
pfit <-prune(md3, cp=0.01 )
summary(pfit)

# getting rmse
price_pred <- predict(pfit, d_test)
rmse_cartpr <- sqrt(mean((price_pred-d_test$price)^2,na.rm=T))
rmse_cartpr

printcp(pfit)
plotcp(pfit)
png(filename = "f_prune.png")
prp(pfit,varlen=5, main = "Airbnb Hackney prices, pruned tree")	
dev.off()



#####################
# Random Forest
#####################


#mtry <- sqrt(ncol(x)) - here it's 7
md <- randomForest(model_allvar, data=d_train, na.action = na.omit, ntree=300,  importance = TRUE, mtry=7)
p_pred <- predict(md, d_test)
rmse_RF <- sqrt(mean((p_pred-d_test$price)^2,na.rm=T))
rmse_RF


md
plot(md)
varImpPlot(md, type= 1)

#   plots
png(filename = "airbnb_rf_varimp.png")
varImpPlot(md, type = 1)
dev.off()


#y value in partial plot is really just the mean marginal response
png(filename = "airbnb_marginals.png")
op <- par(mfrow=c(2,2))
partialPlot(md, as.data.frame(d_train), n_accommodates)
partialPlot(md, as.data.frame(d_train), f_room_type)
partialPlot(md, as.data.frame(d_train), n_review_scores_rating )
partialPlot(md, as.data.frame(d_train), f_property_type)
dev.off()


#####################
# GBM Gaussian - squared loss fn
#####################

md2 <- gbm(model_allvar , data=d_train,
           distribution = "gaussian",n.trees = 200, interaction.depth = 10, shrinkage = 0.001)
price_pred <- predict(md2, d_test, n.trees = 200)
rmse_gbm <- sqrt(mean((price_pred-d_test$price)^2,na.rm=T))
rmse_gbm


#####################
# look at test rmse of selected models
#####################

print(paste("OLS (all vars + interacts)  RMSE : ", round(rmse_ols_test, digits=2)))
print(paste("OLS - LASSO  RMSE : ", round(rmse_lasso_test, digits=2)))
print(paste("CART (cp=0.0001) RMSE : ", round(rmse_cart, digits=2)))
print(paste("CART (pruned) RMSE : ", round(rmse_cartpr, digits=2)))
print(paste("RF (tree:300, cty=7)  RMSE : ", round(rmse_RF, digits=2)))
print(paste("gbm  (200, 10, 0.001) RMSE : ", round(rmse_gbm, digits=2)))



