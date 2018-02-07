###########################
#
# R Seminar 1, Zsuzsa Holler, Ágoston Reguly
#
# Craigslist: Predicting prices of the cars
#
# Main aim: Feature engineering - Descriptives -
#           Regression with log correction
#
# workfile 1
###########################

### IMPORT PACKAGES
#import libraries
#install.packages("data.table")
#install.packages("lmtest")
#install.packages("ggplot2")
#install.packages("sandwich")

# Clear environment
rm( list=ls())
# Load libraries
library(data.table)
library(lmtest)
library(ggplot2)
library(sandwich)

##### IMPORT DATA

# load in data - add your personal path
path <- "/Users/User/Documents/R_projects/CEU-DA4/Seminar_1"
setwd(path)
getwd()

# Get data from folder
data <- fread(file = "craiglist_Chicago_first200_clean.csv" , stringsAsFactors = F )
# Names
names(data) <- gsub("[^[:alnum:]]", "",names(data)) 
# Ensure Integer indexing
data[,ID:=.I]

############################
######  SELECT SAMPLE ######

table(data$fuel)
# Drop missing values and non-gas
data <- data[(fuel=="gas") | is.na(fuel)] # hybrid cars probably have other price structure ...
table(data$titlestatus)
# Drop non cleans and missing
data <- data[(titlestatus=="clean") | is.na(titlestatus)] # only using clean and non-missing data
table(data$condition)
# Drop new, fair and missing values
data <- data[(!condition %in% c("new","fair")) | is.na(condition)]
table(data$type)
# Drop coupe and missing
data <- data[(!type=="coupe") | is.na(type)] # we are only interested in sedans

####################################
###### FEATURE ENGINEERING #########

## Prices
# Create new character variable
data[,pricestr:=price]
data[,price:=as.numeric(gsub("\\$", "",pricestr))] # saving the price into strings
# Drop missing values
data <- data[!is.na(price)]
# Create log-prices (Be careful with price = 0 -> lnprice = -Inf)
data[,lnprice:=log(price)] # zero prices have to be dropped or corrected ...

## Age
# Create year variable
data[,year:=as.numeric(substr(name,1,4))] # it gives us the age of cars
table(data$year, useNA = "always" ) # the early years of 2000s are overrepresented, but we will control for them
# Create age variable
data[,age:=2018-year]

#odometer: miles
# Rescale odometer
data[,odometer:=as.numeric(odometer)/10000] # just scaling it not to have high beta values
# Remove remotely used cars, and missing variables
data <- data[(!((odometer<1) & (age>=3))) | is.na(odometer)  | is.na(age)]

#count missing odometer
sum(is.na(data$odometer)) # 48 missing values

#mean fill for missing odometer (by age)
#(for some ages all are missing, these are replaced by grouping age inti 2y)
data[,odometer:=ifelse(is.na(odometer), mean(odometer, na.rm = T),odometer) ,by = age]
data[,odometer:=ifelse(is.na(odometer), mean(odometer, na.rm = T),odometer) ,by = floor((age+1)/2) ]

data[,lnodometer:=log(odometer)] # log of distance

################################################
# get more detailed type info from text fields #

# LE, XLE, SE and HYBRID and create dummy variables
# grepl: pattern matching and replacement
data[, `:=`(LE = as.numeric((grepl(" le| LE",title) | grepl(" le| LE",name))),
            XLE = as.numeric((grepl(" xle| XLE",title) | grepl(" xle| XLE",name) )),
            SE = as.numeric((grepl(" se| SE",title) | grepl(" se| SE",name))),
            Hybrid = as.numeric((grepl(" Hybrid| hybrid| HYBRID",title) | grepl(" Hybrid| hybrid| HYBRID",name))) )]
## creating dummy categorical variables

# Dropping Hybrid values (different model)
data <- data[!Hybrid==1]

# Variable for condition  (Logicals)
data[,`:=`(cond_excellent = (condition=="excellent") ,
           cond_good = (condition=="good")  ,
           cond_likenew = (condition=="like new")  )]
## doing the same as above

# If missing replace it with FALSE
data[is.na(cond_excellent),cond_excellent := FALSE ]
data[is.na(cond_good),cond_good := FALSE ]
data[is.na(cond_likenew),cond_likenew := FALSE ]
## treating missing values as false values

# Cylinders -> TRUE if 6 cylinders otherwise FALSE
data[,cylind6 := cylinders=="6 cylinders" ]
data[is.na(cylind6),cylind6 := FALSE ]

###################
##  DESCRIPTIVES ##

#price histograms
qplot(data$price, geom="histogram", binwidth=1300, fill=I("lightblue"), col=I("white"))+theme_bw()
#ggsave("F13_h_price.png")

qplot(data$lnprice, geom="histogram", binwidth=0.25, fill=I("lightblue"), col=I("white"))+theme_bw()
#ggsave("F13_h_lnprice.png")
## it has a normal like distribution

data[,.(mean_price = mean(price) ,  n=.N ),by = condition]
data[,.(mean_price = mean(price) ,  n=.N ),by = drive]


###################################
# SIMPLE REGRESSION: PRICE ON AGE #
# lowess: price, lnprice          #

# Poly for age on price 
ggplot(data = data, aes(x=age , y=price)) +
  geom_point(size=1.5, colour="orange", shape=4) +
  ylim(0,20000)+
  xlim(0,25)+
  geom_smooth(method="loess", colour="darkgreen", se=F)+
  theme_bw()
#ggsave("Ch13_p_age_lowess.png")
## there is a highly non-linear relation between price and age

# Poly for age on lnprice 
ggplot(data = data, aes(x=age , y=lnprice)) +
  geom_point(size=1.5, colour="orange", shape=4) +
  ylim(6,10)+
  xlim(0,25)+
  geom_smooth(method="loess", colour="darkgreen", se=F)+
  theme_bw()
#ggsave("Ch13_lnp_age_lowess.png")
## this relation is more linear-ish

# Create RMSE function
rmse <- function(pred,y,df)
{
  sqrt(sum((pred-y)^2)/(df))
}
## symmetric loss function
## if there is a high value, it would be extremely penalized, I don't want to ruin my model because of outliers

############################
### (Linear) Regressions ###

###
# Model 1) price:  with quadratic term
# New variable: square of age
data[,agesq:=age^2]
# First regression model
reg1 <- lm(price ~ age + agesq,data=data)
# Test coefficient's significance
coeftest(reg1, vcov=sandwich) # they are significant; it is robust because of heteroscedasticity
# Estimate teh RMSE value
rmse1 <- rmse(predict(reg1),data$price,reg1$df.residual)
# Save the predicted values
data <- data[,phat_age_quad:=predict(reg1)] # data gives back the predicted values

###
# Model 2: Polynomial regression
reg2 <- loess(price ~ age, data=data)
# Save values
data <- data[,phat_age_lowess:=predict(reg2)]

# Compare the two model's prediction
ggplot(data = data, aes(x=age , y=price)) +
  geom_line(aes(y=phat_age_lowess),colour="darkgreen" ) +
  geom_line(aes(y=phat_age_quad),colour="orange",  linetype=2) +
  theme_bw()
#ggsave("Ch13_p_age_quad_vs_lowess.png")
## the polinomial is close to loess


###
# Model 3: Linear regression on lnprice
reg3 <- lm(lnprice ~ age,data=data)
# Test values
coeftest(reg3, vcov=sandwich)
# RMSE
rmse3 <- rmse(predict(reg3),data$lnprice,reg3$df.residual)
# save the residuals
data <- data[,e:=resid(reg3)]
# Check normality of residuals
ggplot(data = data, aes(x=e)) +
  geom_histogram(aes(y=..density..), binwidth=0.15, fill=I("lightblue"), col=I("white")) +
  stat_function(fun=dnorm,color="green",args=list(mean=mean(data$e), sd=sd(data$e))) +
  theme_bw()
#ggsave("Ch13_lnp_age_resid.png")
## we are checking the normality of the residuals, because it has something to do with correction.
## jensen inequality - when using logs the expected value of the log variable is not equal to the log of the expected variable
## we are checking that the thing above is valid or not


###############################
### MULTIVARIATE REGRESSION ###

###
# Model 4): age and odometer, their squares and other features on price
data[,odometersq:=odometer^2]
reg4 <- lm(price ~ age+agesq+odometer+odometersq+LE+XLE+SE+cond_likenew+cond_excellent+cond_good+cylind6,data=data)
coeftest(reg4, vcov=sandwich)
## two variables are not significant but the other are
rmse4 <-rmse(predict(reg4),data$price,reg4$df.residual)

###
# Model 5): age and odometer, their squares and other features on lnprice
reg5 <- lm(lnprice ~ age+odometer+LE+XLE+SE+cond_likenew+cond_excellent+cond_good+cylind6,data=data)
coeftest(reg5, vcov=sandwich)
## more or less the same results
rmse5 <-  rmse(predict(reg5),data$lnprice,reg5$df.residual)
data <- data[,e_reg5:=resid(reg5)]
# Check for normality
ggplot(data = data, aes(x=e_reg5)) +
  geom_histogram(aes(y=..density..), binwidth=0.15, fill=I("lightblue"), col=I("white")) +
  stat_function(fun=dnorm,color="green",args=list(mean=mean(data$e_reg5), sd=sd(data$e_reg5))) +
  theme_bw()
#ggsave("Ch13_lnp_multireg_resid.png")
## looks much better but still not the best fit

###########################
### PREDICTIONS SUMMARY ###


#generate new observation with features our car
dt1 <- data.table(age=10, agesq=10^2,odometer=12,odometersq=12^2,LE=1,XLE=0,SE=0,cond_likenew=F,
                  cond_excellent=T,cond_good=F,cylind6=F) # adding a new observation and trying to predict a price
# Add to dataset
data <- rbind(data,dt1, fill=T)

###
# Predict the price w Model 1 (price w age, age^2)
z1 <- predict(reg1, newdata = data[dim(data)[1],], se.fit=TRUE)
# standard error of the point prediction
p1<- z1[[1]]
p1
p1_pse <- z1[[2]]
p1_pse
p1_PIlow <- p1 - 2*p1_pse
p1_PIlow # lower bound - if wanted to sell the car quickly, could go with this price
p1_PIhigh <- p1 + 2*p1_pse
p1_PIhigh # higher bound - if not in a rush, I could go with this price

###
# Predict the price w Model 3 (lnprice w age)
lnz1 <- predict(reg3, newdata = data[dim(data)[1],], se.fit=TRUE)
lnz1
lnp1<- lnz1[[1]]
lnp1
lnp1_pse <- lnz1[[2]]
lnp1_pse

# Correction Function
corEval <- function( Ey , rmse ){
  val <- exp( Ey ) * exp( rmse ^ 2 / 2 )
} # we are not working with the pedicted standard error but the model error, that is why we are using rmse

lnp1_c <- corEval( lnp1 , rmse3 )
lnp1_c
lnp1_PIlow <- corEval( lnp1 - 2*lnp1_pse , rmse3 )
lnp1_PIlow
lnp1_PIhigh <- corEval( lnp1 + 2*lnp1_pse , rmse3 )
lnp1_PIhigh

###
# Predict the price w Model 4 (age and odometer, their squares and other features on price)
z2 <- predict(reg4, newdata = data[dim(data)[1],], se.fit=TRUE)
p2<- z2[[1]]
p2_pse <- z2[[2]]
p2_PIlow <- p2 - 2*p2_pse
p2_PIhigh <- p2 + 2*p2_pse

###
# Predict the lnprice w Model 5 (age and odometer, their squares and other features on lnprice)
lnz2 <- predict(reg5, newdata = data[dim(data)[1],], se.fit=TRUE)
lnp2<- lnz2[[1]]
lnp2_pse <- lnz2[[2]]
# Corrected values
lnp2_c <- corEval( lnp2 , rmse5 )
lnp2_PIlow <- corEval( lnp2 - 2*lnp2_pse , rmse5 )
lnp2_PIhigh <- corEval( lnp2 + 2*lnp2_pse , rmse5 )

### Comparisons
#sum1 <- matrix( c( p1 , lnp1_c , p2 , lnp2_c ,
 #                  p1_PIlow , lnp1_PIlow , p2_PIlow , lnp2_PIlow ,
 #                  p1_PIhigh , lnp1_PIhigh , p2_PIhigh , lnp2_PIhigh ) , nrow = 3 ,ncol = 4)
c(p1 , lnp1_c , p2 , lnp2_c)
c(p1_PIlow , lnp1_PIlow , p2_PIlow , lnp2_PIlow)
c(p1_PIhigh , lnp1_PIhigh , p2_PIhigh , lnp2_PIhigh)

#sum1