################################################################
#
# R Seminar 1, Zsuzsa Holler, Ăgoston Reguly
#
# Airbnb London 2017 march 05 data
#
# Main aim: Descriptive statistics, Simple regressions
#
# source: inside airbnb
# workfile 3
################################################################
#
#  uses   airbnb_london_workfile.csv
#  saves  airbnb_hackney_workfile_adj.csv
#
################################################################

# Clear environment
rm( list = ls())
#import libraries
library(data.table)
library(ggplot2)

# Set path
path <- "/Users/User/Documents/R_projects/CEU-DA4/Seminar_1"
setwd(path)
getwd()

# Load the data
data <- fread(file = "airbnb_london_workfile.csv" , stringsAsFactors = F )

# work with Hackney only
data <- data[neighbourhood_cleansed =="Hackney"]
# Export the hackey data
#fwrite(data,"airbnb_hackney_workfile.csv")


#####################
### look at price ###
#####################

summary(data$price)
# Create logs
data[,ln_price:=log(price)] # check for zeros
# Plot
qplot(data$price, geom="histogram", binwidth=50)+theme_bw()
qplot(data$ln_price, geom="histogram", binwidth=0.25)+theme_bw()
# Remove extreme values from prices
data <- data[!price>1000]

# Much neater histograms
qplot(data$ln_price, geom="histogram", binwidth=0.15, fill=I("lightblue"), col=I("white"))+theme_bw()
#ggsave("F14_h_lnprice.png")
## it is closer to normal

qplot(data$price, geom="histogram", binwidth=25, fill=I("lightblue"), col=I("white"))+theme_bw()
#ggsave("F14_h_price.png")


################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution
data[,.(mean_price = mean(price) ,  min_price= min(price) ,max_price = max(price),  n=.N ),by = n_accommodates]

ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=2, colour="orange")+
  ylim(0,800)+
  xlim(0,15)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour="navy", se=FALSE)+
  theme_bw()
#ggsave("F14_s_n_accommodates.png")
## the higher number of people accomodated, the higher the price

# Squares and further values to create
data[, `:=`(n_accommodates2=n_accommodates**2, ln_accommodates=log(n_accommodates) ,
            ln_accommodates2=log(n_accommodates)**2) ]
# Regression 1: ln price and num of accomodates and squares
lm(ln_price ~ n_accommodates + n_accommodates2, data=data) # some kind of negative effect as number of accomodates increases
## let's see what would happen if we dropped the square ...

# Regression 2: ln price and log num of accomodates
lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accomodates
lm(ln_price ~ n_accommodates, data=data)


# lowess with scatterplot: price is lower than 800, num of acc.
ggplot(data = data[data$price<=800], aes(x=n_accommodates, y=price)) +
  geom_point(size=1.5, colour="orange", shape=4) +
  ylim(0,800)+
  xlim(0,18)+
  geom_smooth(method="loess", colour="darkgreen", se=F)+
  labs(x="Number of people accomodated",y="Daily price (USD)")+
  theme_bw()
#ggsave("F14_l_n_accommodates.png")
## there is some non-linearities

# lowess with scatterplot: log-price is lower than 800, log-num of acc.
ggplot(data = data[data$price<=800], aes(x=ln_accommodates, y=ln_price)) +
  geom_point(size=1.5, colour="orange", shape=4) +
  ylim(1,7)+
  xlim(0,3)+
  geom_smooth(method="loess", colour="darkgreen", se=F)+
  labs(x="Log number of people accomodated",y="Log daily price")+
  theme_bw()
#ggsave("F14_l_ln_accommodates.png")
# maybe best is to have log people -> better linear approximation, but different interpretation!!
## maybe we can say the using linears will be okay
## what are the warnings? when working with logs that are close to zero, there could be problems, but now everything is okay ...

## Beds
data[,.(mean_price = mean(price) ,  min_price= min(price) ,max_price = max(price),  n=.N ),by = n_beds]
# maybe best is to have log beds
data[,ln_beds:=log(n_beds)] # taking the log to have better interpretation


## bathrooms
qplot(data$n_bathrooms, geom="histogram", binwidth=0.5, fill=I("lightblue"), col=I("white"))+theme_bw()
#ggsave("F14_h_n_bathrooms.png")
## there is a large number of accommodations with one bathroom

# Pool accomodations with 0,1,2,10 bathrooms
data[,f_bathroom:=cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F)]
data[,.(mean_price = mean(price) ,  n=.N ),by = f_bathroom]
## creating factors to group accommodations based on the number of bathrooms

## Number of reviews
qplot(data[data$n_number_of_reviews<100]$n_number_of_reviews,
      geom="histogram", binwidth=5, fill=I("lightblue"), col=I("white"))+ 
  labs(x="Number of reviews")+
  theme_bw()
# ggsave("F14_h_n_number_of_reviews.png")

# number of reviews: use logs as well
data[,ln_number_of_reviews:=log(n_number_of_reviews+1)]
qplot(data$ln_number_of_reviews,
      geom="histogram", binwidth=0.5, fill=I("lightblue"), col=I("white"))+ 
  theme_bw()
#ggsave("F14_h_ln_number_of_reviews.png")
## it is still exponential ... but not that high

# Pool num of reviews to 3 categories: none, 1-51 and >51
data[,f_number_of_reviews:=cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F)]
data[,.(median_price = median(price) ,mean_price = mean(price) ,  n=.N ),by = f_number_of_reviews]
## grouping / pooling the reviews ... they are factors already ... median and mean are close to each other

# Regression 1: log-price and number of reviews
lm(ln_price ~ f_number_of_reviews, data=data) # might be a relevant variable ...
# Regression 2: log-price and log number of reviews
lm(ln_price ~ ln_number_of_reviews, data=data) # there seems to be a not so strong connection

## Time since
# Create variables, measuring the time since: squared, cubic, logs
data[,`:=`(ln_days_since= log(n_days_since),ln_days_since2 = log(n_days_since)**2
           ,ln_days_since3 = log(n_days_since)**3 , n_days_since2=n_days_since**2, n_days_since3=n_days_since**3)]
# Check the effect
ggplot(data = data[(data$price<=800) & (ln_days_since>2)], aes(x=ln_days_since , y=ln_price)) +
  geom_point(size=1.5, colour="orange", shape=4) +
  ylim(1,7)+
  xlim(2,7)+
  geom_smooth(method="loess", colour="darkgreen", se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bw()
#ggsave("F14_l_ln_days_ince.png")
## there might be some pattern

#-Inf values
#lm(ln_price ~ ln_days_since + ln_days_since2 + ln_days_since3, data=data)

## review score effect
ggplot(data = data[(data$price<=800) & (n_review_scores_rating>=60)], aes(x=n_review_scores_rating , y=ln_price)) +
  geom_point(size=1.5, colour="orange", shape=4) +
  ylim(1,7)+
  xlim(60,100)+
  geom_smooth(method="loess", colour="darkgreen", se=F)+
  labs(x="Review score",y="Log daily price")+
  theme_bw()
#ggsave("F14_l_n_number_of_reviews.png")
## it can be meaningful and useful to not only look at the full population but subsamples too

# Create log of review scores
data[,ln_review_scores_rating :=log(n_review_scores_rating )]
# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data) 
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data) 
#leave as is

## minimum nights
lm(ln_price ~ n_minimum_nights,data=data) # it is almost zero, let's pool the nights and see if it is more meaningful that way

# Pool and categorize the number of minimum nights: 1,2,3, 3+
data[,f_minimum_nights:=cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F)]

lm(ln_price ~ f_minimum_nights,data=data)
## comparing to one night stay if minimum stay is two nights, then the percentage in price with one extra night is going to be larger by 24%

###########################
## look at categoricals  ##
###########################

categoricals <- c("f_property_type", "f_room_type", "f_cancellation_policy", "f_bed_type") # defines already defined as factors

for (var in categoricals) {
  print(data[,.(mean_price = mean(price) ,  n=.N ),by = mget(var)])
}

#####################################

# fwrite(data,"airbnb_hackney_workfile_adj.csv")

