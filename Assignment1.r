# Alex Gibbons and Lauren Kawecki
# Final Exercise
# Airbnb dataset
# 4/29/2024
library("tidyverse")
# Support Vector Machine Classifier
# download dataset from web
airbnb <- read.csv(file="C:/Users/Kelly/Desktop/MCSP24/CMPT363 - Data Mining/projectAirbnbDataset.csv",header=TRUE)

# Removed unneeded cols
airbnb <- airbnb %>% select(-c(
  "id","access","amenities","availability_30", "availability_60", 
  "availability_90", "availability_365","host_listings_count","square_feet", 
  "monthly_price", "host_response_time","is_business_travel_ready", 
  "host_has_profile_pic","require_guest_profile_picture","description",
  "host_about","host_location","host_neighbourhood","house_rules",
  "interaction","latitude","longitude","neighborhood_overview",
  "neighbourhood","notes","space","transit","X.randomControl.",
  "host_response_rate","host_since","host_verifications","weekly_price",
  "host_acceptance_rate","zipcode","state","property_type","market","city"
))
names(airbnb) <- c(
  "highBookingRate", "accommodates", "bathrooms", 
  "bedType", "bedrooms", "beds", "cancellationPolicy", 
  "cleaningFee", "extraPeople", "guestsIncluded", 
  "hostIdentityVerified", "hostIsSuperhost", "instantBookable", 
  "isLocationExact", "maximumNights", "minimumNights", 
  "price", "requireGuestPhoneVerification", 
  "requiresLicense", "reviewScoresAccuracy", "reviewScoresCheckin", 
  "reviewScoresCleanliness", "reviewScoresCommunication", 
  "reviewScoresLocation", "reviewScoresRating", "reviewScoresValue", 
  "roomType", "securityDeposit"
)
df <- airbnb

# cleaning data
df$cleaningFee <- as.numeric(gsub("\\$", "", df$cleaningFee))
df$extraPeople <- as.numeric(gsub("\\$", "", df$extraPeople))
df$securityDeposit <- as.numeric(gsub("\\$", "", df$securityDeposit))
df$price <- as.numeric(gsub("\\$", "", df$price))

# replacing na values
meanPrice <- mean(df$price, na.rm = TRUE)
meanCleaningFee <- mean(df$cleaningFee, na.rm = TRUE)
meanExtraPeople <- mean(df$extraPeople, na.rm = TRUE)
meanSecurityDeposit <- mean(df$securityDeposit, na.rm = TRUE)
meanAccuracy <- mean(df$reviewScoresAccuracy, na.rm = TRUE)
meanCheckIn <- mean(df$reviewScoresCheckin, na.rm = TRUE)
meanCleanliness <- mean(df$reviewScoresCleanliness, na.rm = TRUE)
meanCommunication <- mean(df$reviewScoresCommunication, na.rm = TRUE)
meanLocation <- mean(df$reviewScoresLocation, na.rm = TRUE)
meanRating <- mean(df$reviewScoresRating, na.rm = TRUE)
meanValue <- mean(df$reviewScoresValue, na.rm = TRUE)

df$price[is.na(df$price)] <- meanPrice
df$cleaningFee[is.na(df$cleaningFee)] <- meanCleaningFee
df$extraPeople[is.na(df$extraPeople)] <- meanExtraPeople
df$securityDeposit[is.na(df$securityDeposit)] <- meanSecurityDeposit
df$hostIsSuperhost[is.na(df$hostIsSuperhost)] <- FALSE
df$reviewScoresAccuracy[is.na(df$reviewScoresAccuracy)] <- meanAccuracy
df$reviewScoresCheckin[is.na(df$reviewScoresCheckin)] <- meanCheckIn
df$reviewScoresCleanliness[is.na(df$reviewScoresCleanliness)] <- meanCleanliness
df$reviewScoresCommunication[is.na(df$reviewScoresCommunication)] <- meanCommunication
df$reviewScoresLocation[is.na(df$reviewScoresLocation)] <- meanLocation
df$reviewScoresRating[is.na(df$reviewScoresRating)] <- meanRating
df$reviewScoresValue[is.na(df$reviewScoresValue)] <- meanValue

df$highBookingRate <- factor(df$highBookingRate, levels=c(0,1), labels=c("low","high"))

train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$highBookingRate)
table(df.validate$highBookingRate)


# SVM
library("e1071")
svm.model <- svm(highBookingRate~.,data=df.train)
svm.pred <- predict(svm.model, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$highBookingRate,svm.pred,dnn=c("Actual","Predicted"))
print(svm.perf)
tn <- svm.perf[1,1]
fp <- svm.perf[1,2]
fn <- svm.perf[2,1]
tp <- svm.perf[2,2]

accuracy <- (tp+tn)/(tp+tn+fp+fn)
error.rate <- (fp+fn)/(tp+tn+fp+fn)
sensitivity <- tp/(tp+fn)
specificity <- tn/(tn+fp)
precision <- tp/(tp+fp)
recall <- tp/(tp+fn)
f.measure <- (2*precision*recall)/(precision+recall)

print(paste("Accuracy: ",accuracy))
print(paste("Error Rate: ",error.rate))
print(paste("Sensitivity: ",sensitivity))
print(paste("Specificity: ",specificity))
print(paste("Precision: ",precision))
print(paste("Recall: ",recall))
print(paste("F-measure: ",f.measure))
