################################################################
#
# R Seminar 1, Zsuzsa Holler, Ăgoston Reguly
#
# Airbnb London 2017 march 05 data
#
# Main aim: Feature engineering 
#
# source: inside airbnb
# workfile 2
################################################################
#
# uses   airbnb_london_cleaned.csv
#  saves  airbnb_london_workfile.csv
#
################################################################

# Clear environment
rm( list = ls())
#import libraries
library(data.table)

# Set path
path <- "/Users/User/Documents/R_projects/CEU-DA4/Seminar_1"
setwd(path)
getwd()

# Import data
data <- fread(file = "airbnb_london_cleaned.csv" , stringsAsFactors = F )

## Factor Variables

#Factor Propery types
table(data$property_type)
types <- c("Apartment", "Townhouse", "House") # only interested in these three
# Merge values to f_property_type
data <- data[property_type %in% types]
data[property_type == "Townhouse",property_type:="House"]
data[,f_property_type:=factor(property_type)]
# saving as a factor

# Factor room types
table(data$room_type)
data[,f_room_type:=factor(room_type)]

# Factor Cancellation Policy
table(data$cancellation_policy)
data[(cancellation_policy=="super_strict_30") | (cancellation_policy=="super_strict_60"),cancellation_policy:="strict"]
data[,f_cancellation_policy:=factor(cancellation_policy)]
# just saving it into a factor

# Factor bed types
table(data$bed_type)
data[(bed_type=="Futon") | (bed_type=="Pull-out Sofa") | (bed_type=="Airbed"),bed_type:="Couch" ] # merging a few types and namint the couch
data[,f_bed_type:=factor(bed_type)]

data[,f_neighbourhood_cleansed:=factor(neighbourhood_cleansed)] # also factor

## Create Numerical variables
# Daily USD price
data[,usd_price_day:=price]
data[,price:=as.numeric(gsub(",","",usd_price_day))]
data[,p_host_response_rate:=as.numeric(host_response_rate)]
names(data)[names(data) == "cleaning_fee"] = "usd_cleaning_fee"
# saving price as string to have a back up variable

# Accomodates and Bathrooms to numeric
table(data$accommodates)
table(data$bathrooms)
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews","guests_included",
                "reviews_per_month","extra_people","minimum_nights","beds")
data[, (paste0("n_",numericals)) := lapply(.SD, as.numeric), .SDcols=numericals] # inserting letter n before numerical variables
## it is a tool to able to distinguish between variables

#create days since first review
data[,n_days_since := as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") - as.Date(first_review ,format="%Y-%m-%d"))]

# Create dummy vars
dummies <- names(data)[seq(73,122)]
data[, (paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)) )) := .SD , .SDcols=dummies]

# Create Data File as dummies, numerics, factors, percentages, usd
data <- data[,mget(c(names(data)[grep("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*",names(data))], "price",
                     "neighbourhood_cleansed","neighbourhood_cleansed","cancellation_policy","room_type","property_type"))]

# fwrite(data,"airbnb_london_workfile.csv")

