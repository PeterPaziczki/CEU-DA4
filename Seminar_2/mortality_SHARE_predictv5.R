# whether dies within 7 years 
# deceased = "Deceased by 2013"
# smokes = "Smoked in 2007"

#install.packages("DescTools")

# Clear environment
rm( list = ls())
#import libraries
library(data.table)
library(DescTools)
library(pROC)
library(randomForest)
library(ggplot2)

#######################DATA LOAD

path <- "/home/zsuzsa/Documents/DA4"
setwd(path)
data <- fread("mortality_oldage_eu.csv",stringsAsFactors = T)

######################CLEAN DATA

data <- data[(age>=50) & (age<80)]
data <- data[(!is.na(smokes)) & (!is.na(ever_smoked)) & (!is.na(deceased)) &
               (!is.na(female)) & (!is.na(eduyears_mod)) & (!is.na(income10g)) ]  

data[,agegroup:=cut(age, seq(50,80,5), right = F,include.lowest = T )]
data[,agegi:=cut(age, seq(50,80,1), right = F,include.lowest = T )]
data[,agesq:=age^2]
data[,.(min_age = min(age) , max_age = max(age), n=.N ), by=agegroup]

data[eduyears_mod==10.5,eduyears_mod:=10 ]
data[eduyears_mod==17.5,eduyears_mod:=17 ]
data <- data[eduyears_mod!=0]

data[,f_eduyears_mod:=factor(eduyears_mod)]
data[,f_income10g:=factor(income10g)]
# for RF we will need as factor
data[,f_deceased:=factor(deceased)]
class(data$f_deceased)

Y = "deceased"

#####################################################################
# CALIBRATION EXAMPLE
# AGE ONLY

logit <- glm(deceased ~ age, family = "binomial", data=data)
logit
data$p <- predict(logit, data, type='response')

ggplot(data = data, aes(x=age , y=p)) +
  geom_line(size=1, colour="darkgreen", linetype=2 ) +
  geom_smooth(aes(x=age , y=deceased), method="loess", colour="orange", se=F)+
  ylab("Probability of dying within six years") +
  theme_bw()
#ggsave("logit_lpoly_age.png")


# calibration for all specific values
data[,.(mean = mean(deceased)), by=p>0.5]

#calibration for 10 groups of p, equal # obs
data[,pcat:=cut(p,10)]
cal10 <- data[,.(mean_y = mean(deceased), mean_p = mean(p) ), by=pcat]

#calibration for 10 groups of p, equal width
data[,pcat:=cut(p, seq(0,1,0.02), right = F,include.lowest = T )]
cal10_2 <- data[,.(mean_y = mean(deceased), mean_p = mean(p), min = min(p) , max = max(p), n=.N ), by=pcat]

ggplot(data = cal10, aes(x=mean_p, y=mean_y)) +
  geom_line(aes(x=mean_p,y=mean_p), size=1, colour="orange",  linetype=2 )+
  geom_line(size=2, colour="darkgreen")+
  xlab("Mean predicted probability") +
  ylab("") +
  theme_bw()
#ggsave("calibration.png")

##############################################################################
# BRIER SCORES

logit2 <- glm(deceased ~ 1, family = "binomial", data=data)
data$p2 <- predict(logit2, data, type='response')

BrierScore(logit)
BrierScore(logit2)


#############################################################################
# ROC CURVE

roc(data$deceased , data$p, plot=T)

#################################Separate a test set for final evaluation

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

#############################################################################

# FOUR MODELS

m1 <- get(Y) ~ age 
m2 <- get(Y) ~ smokes +ever_smoked + female + age + eduyears_mod + income10g
m3 <- get(Y) ~ smokes*age + ever_smoked*age + female*age + f_eduyears_mod*age + f_income10g*age
m4 <- get(Y) ~ smokes*agegi + ever_smoked*agegi + female*agegi + eduyears_mod*agegi + f_income10g*agegi

###########################################################################
## IN-SAMPLE

in_sample_brier <- list()
in_sample_auc <- list()

for (i in 1:4){
  name <- paste("logit",2+i,sep="")
  logit <- glm(get(paste("m",i,sep='')), family = "binomial", data=d_train)
  d_train[, (paste('p',2+i, sep=''))] <- predict(logit, d_train, type='response')
  #print(summary(logit))
  in_sample_brier[[paste0("m",i)]] <- BrierScore(logit) 
  in_sample_auc[[paste0("m",i)]] <- roc(d_train[,get(Y)] , d_train[,get(paste('p',2+i, sep=''))] , plot=F)$auc
  assign(name,logit)
}

################################################################
## 5-FOLD CROSS-VALIDATION

set.seed(34167)

n_folds=5
# Create the folds
folds_i <- sample(rep(1:n_folds, length.out = nrow(d_train) ))

cv_results<-list()

for (i in 1:4){
  BRIER <- 0
  ROCAREA <- 0
  for (t in 1:n_folds){
    test_i <- which(folds_i == t)
    # Train sample: all except test_i
    data_train <- d_train[-test_i, ]
    # Test sample
    data_test <- d_train[test_i, ]
    logit <- glm(get(paste("m",i,sep='')), family = "binomial", data=data_train)
    p <- predict(logit, newdata=data_test , type='response')
    BRIER=BRIER + BrierScore(data_test[,get(Y)],p)
    ROCAREA=ROCAREA+ roc(data_test[,get(Y)] , p, plot=F)$auc
  }
  cv_results[[paste("model ",i,sep='')]] <- c("BRIER_SCORE"= BRIER/5 , "ROC_AREA"=ROCAREA/5)
}

cv_results

####################################RANDOM FOREST

mrf <- f_deceased ~ smokes +ever_smoked + female + age + eduyears_mod + income10g + agegi + f_eduyears_mod + f_income10g

md <- randomForest(mrf, data = d_train, ntree = 200, mtry=3)
print(md)
varImpPlot(md, type = 2)


# look at two conf matrix
phat <- predict(md, data_test, type = "prob")[,"1"]
table(ifelse(phat>0.05,1,0), data_test$f_deceased)
table(ifelse(phat>0.10,1,0), data_test$f_deceased)


################################################

# for comparison, train-test logits

logit_best <- glm(m2, family = "binomial", data=d_train)
p <- predict(logit_best, newdata=d_test , type='response')
BrierScore(d_test[,get(Y)],p)
plot.roc(d_test[,get(Y)] , p, print.auc = TRUE)

model.test.prob <- predict(md, data_test, type = "prob",norm.votes = TRUE)
plot.roc(predictor = model.test.prob[,2],  d_test$f_deceased, print.auc = TRUE)
