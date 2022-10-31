### ALEX KIKOS ###
### FEEDING AMERICA EASTERN WISCONSIN ###
### AGENCY OPTIMIZER ###
### FALL 2022 ###

rm(list=ls())

library(readxl)
library(tidyverse)
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(forecast)

###DATA IMPORTS AND MANAGEMENT###
###DATA IMPORTS AND MANAGEMENT###
###DATA IMPORTS AND MANAGEMENT###

data<-read_excel("C:/Users/Alex/Documents/Feeding America/FAEW_AgencyOptimizer2022/class_EverySite.xlsx")
data <- data[,c(1:7, 38:39)]
surveyData <- read_excel("C:/Users/Alex/Documents/Feeding America/FAEW_AgencyOptimizer2022/class_2017_Cleaned_CombinedSurveyData.xlsx")
food_dist <- read_excel("C:/Users/Alex/Documents/Feeding America/FAEW_AgencyOptimizer2022/class_2017_2021_AgencyFoodReceived.xlsx")
surveyScores <- read_excel("C:/Users/Alex/Documents/Feeding America/FAEW_AgencyOptimizer2022/class_2017_Combined_Survey_Scores.xlsx")
surveyScores <- surveyScores[,1:2]
#Q2 data from following year represents totals from entire prvious year (according to FAEW team)
mpin17 <- read.csv("C:/Users/Alex/Documents/Feeding America/FAEW_AgencyOptimizer2022/class_cleaned_MPIN_201802.csv")
mpin21 <- read.csv("C:/Users/Alex/Documents/Feeding America/FAEW_AgencyOptimizer2022/class_cleaned_MPIN_202202.csv")
#total_foodinsecure17 = sum(mpin17$Food.Insecure.Persons)
#total_foodinsecure21 = sum(mpin21$Food.Insecure.Persons)

#two options to convert lat/long vals into radians for distance calcs
# data <- data %>%
#   mutate("RadLat" = Latitude / (180/pi) )
#   
# data <- data %>%
#   mutate("RadLong" = Longitude / (180/pi) )
data <- data %>%
  mutate("RadLat" = Latitude / 57.29577951)
data <- data %>%
  mutate("RadLong" = Longitude / 57.29577951 )

data <- filter(data, Latitude > 1 & Longitude != 0)
data <- data[!duplicated(data[10:11]),]
#data <- data[!duplicated(data[1:2]),]

#loops through all agencies and calculates the closest agency, total number within 5, 10 & 25 miles
for (x in 1:nrow(data)) {
  vals <- c()
  currLat = data$RadLat[x]
  currLong = data$RadLong[x]
  currAgency = data$AgencyRef[x]
  d <- 0
  currD <- 0
  for (y in 1:nrow(data)) {
    newAgency = data$AgencyRef[y]
    if (currAgency != newAgency) {
      newLat = data$RadLat[y]
      newLong = data$RadLong[y]
      newAgency = data$AgencyRef[y]
      
      #formula to find distance between two (converted) geo locations per https://www.geeksforgeeks.org/program-distance-two-points-earth/
      #Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 - long1)]
      currD <- 3963.0 * acos((sin(currLat) * sin(newLat)) + cos(currLat) * cos(newLat) * cos(currLong-newLong)) 
      vals <- c(vals, currD)
    }
  }
  df <- data.frame(vals)
  df <- round(df, 2)
  closestAgency = min(df)
  within5 = sum(df <= 5.0)
  within10 = sum(df <= 10.0)
  within25 = sum(df <= 25.0)
  data$nearestAgency[x] <- closestAgency
  data$numwithin5[x] <- within5
  data$numwithin10[x] <- within10
  data$numwithin25[x] <- within25
}

#renames pantry size columns to make easier to use/read
data$`Pantry Size`[data$`Pantry Size` == "SMALL <200"] <- "Small"
data$`Pantry Size`[data$`Pantry Size` == "MEDIUM >200 <500"] <- "Medium"
data$`Pantry Size`[data$`Pantry Size` == "LARGE >500"] <- "Large"
data$`Pantry Size`[data$`Pantry Size` == "**NONE**"] <- "NA/Special"
data$`Pantry Size`[data$`Pantry Size` == "SPECIAL PROGRAMS"] <- "NA/Special"

#merge the datasets all into agencyData2
agencyData <- merge(x=surveyData, y=food_dist, by.x="Agency Reference Number", by.y="Agency Ref")
agencyData2 <- merge(x=agencyData, y=data, by.x="Agency Reference Number", by.y="AgencyRef")


#INITIAL CLASSIFICATION of agencies based on survey score with ranges provided by FAEW (Carey)
#needed b/c not all locations were given partner type label
surveyScores <- surveyScores %>% 
  mutate(partnerType = case_when(Score >= .90 ~ 'Visionary',
                                 Score < .90 & Score >= .80 ~ 'Humanitarian',
                                 Score < .80 & Score >= .70 ~ 'Community',
                                 Score < .70 ~ 'Program')
  )
agencyData2 <- merge(x=agencyData2, y=surveyScores, by.x="Respondent ID", by.y="Respondent ID")
agencyData2 <- agencyData2[,c(1:142,152:157)]
agencyData2 <- (agencyData2[complete.cases(agencyData2), ])

total_received17 = sum(agencyData2$`2017 Pounds Received`)

#uses Percentile Ranks to create new "scores" and makes new thresholds for each type of partner
#allows for more balance in each type of group, rather than just 1 or 2 at the top, allows for better understanding of what makes
#top groups the best
agencyData2 <- agencyData2 %>% 
  mutate(score_rank = percent_rank(agencyData2$Score))

agencyData2 <- agencyData2 %>% 
  mutate(partnerType = case_when(score_rank >= .75 ~ 'Visionary',
                                 score_rank < .75 & score_rank >= .50 ~ 'Humanitarian',
                                 score_rank < .50 & score_rank >= .25 ~ 'Community',
                                 score_rank < .25 ~ 'Program')
  )

# #loops through and finds ratio of each agency's 2017 lbs food received to food insecure persons in their county
# for (x in 1:nrow(agencyData2)){
#   currCounty = agencyData2$`County Code`[x]
#   currPounds = agencyData2$`2017 Pounds Received`[x]
#   fip <- mpin17[which(grepl(currCounty, mpin17$County)),3]
#   agencyData2$pounds2fip[x] <- currPounds/fip
# }

#loops through and finds ratio of each agency's 2021 lbs food received to food insecure persons in their county
for (x in 1:nrow(agencyData2)){
  currCounty = agencyData2$`County Code`[x]
  currPounds = agencyData2$`2021 Pounds Received`[x]
  fip <- mpin21[which(grepl(currCounty, mpin21$County)),3]
  agencyData2$pounds2fip[x] <- currPounds/fip
}

#selects only NUMERIC values and outcome variable 'partnertype' to utilize simpler df
numData_raw <- agencyData2[sapply(agencyData2, is.numeric) | names(agencyData2)=='partnerType']
#removes the response ID, monthly amount served and the score vals
numData_raw <- numData_raw[,c(3:79,81,83)]
#moves partnerType to front for easier accessibility moving forward
numData_raw <- numData_raw %>% relocate(partnerType)
numData <- numData_raw[,]



### SUPERVISED DATA MANAGEMENT ###
### SUPERVISED DATA MANAGEMENT ###
### SUPERVISED DATA MANAGEMENT ###


## CLASSIFICATION TREE ##
## CLASSIFICATION TREE ##
## CLASSIFICATION TREE ##

classData <- agencyData2[,c(4:138,143:146,148)]
classData <- classData %>% relocate(partnerType)

#factors all non-numeric values
classData[sapply(classData, is.character)] <- lapply(classData[sapply(classData, is.character)],
                                       as.factor)
summary(classData$partnerType)


#train on more obs, higher than 70%
#instability within the data/predictive nature of the models

set.seed(1)
myIndex <- createDataPartition(classData$partnerType, p=0.7, list=FALSE)
trainSet <- classData[myIndex,]
validationSet <- classData[-myIndex,]


set.seed(1)
full_tree <- rpart(partnerType ~ ., 
                   data = trainSet, 
                   method = "class", 
                   cp = 0, 
                   minsplit = 2, 
                   minbucket = 1)
prp(full_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)


# par(cex = .5)
# plot(full_tree)
# text(full_tree)
# summary(full_tree)

printcp(full_tree)

#cp value is slightly higher than the cp value listed above for the min xerror val
pruned_tree <- prune(full_tree, cp = 0.0985916)
prp(pruned_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)

predicted_class <- predict(pruned_tree, validationSet, type = "class")
predicted_prob <- predict(pruned_tree, validationSet, type= 'prob')
head(predicted_prob)

##MODEL PERFORMANCE INDEPENDENT OF CUTOFF
validationSet$partnerType <- unclass(validationSet$partnerType)
gains_table <- gains(validationSet$partnerType, predicted_prob[,4])
gains_table
##the output shows possible target class probabilities in the pruned tree
##under 'Mean Model Score'

##LIFT CHART
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$partnerType)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', 
     ylab = "Cumulative", 
     type = "l")
lines(c(0, sum(validationSet$partnerType))~c(0, dim(validationSet)[1]), 
      col="red", 
      lty=2)

##DECILE-WISE LIFT CHART
barplot(gains_table$mean.resp/mean(validationSet$partnerType), 
        names.arg=gains_table$depth, 
        xlab="Percentile", 
        ylab="Lift", 
        ylim=c(0, 3.0), 
        main="Classification Tree Decile-Wise Lift Chart")

##RECEIVER OPERATOR CURVE
roc_object <- roc(validationSet$partnerType, predicted_prob[,4])
plot.roc(roc_object)
legend("bottomright", legend="Classification Tree (AUC = 0.5778)", col="black", lty=1,lwd=2, cex=1, text.font=1, bg='white')
auc(roc_object)
#Area under the curve: 0.5778 aka model correctly classifies visionary partners 57.78% of the time (given all input values)
#seed = 1
#70/30 split .5778 auc - 3 nodes
#80/20 split .5833 auc - 11 nodes
#90/10 split .5000 auc - 2 nodes





## REGRESSION TREE ##
## REGRESSION TREE ##
## REGRESSION TREE ##
#predict ratio of pounds of food received to food-insecure-persons in their county (PFIP in the report)


###TREE 1 - using ALL input variable ###

#2021 lbs of food
regData <- agencyData2[,c(4:138,143:146,150)]
regData <- regData %>% relocate(pounds2fip)

#factor all character values first then turn into numerics for regression tree (unclass)
regData[sapply(regData, is.character)] <- lapply(regData[sapply(regData, is.character)],
                                                     as.factor)
regData[sapply(regData, is.factor)] <- lapply(regData[sapply(regData, is.factor)],
                                                 unclass)

set.seed(2)
myIndex <- createDataPartition(regData$pounds2fip, p=0.7, list=FALSE)
trainSet <- regData[myIndex,]
validationSet <- regData[-myIndex,]

##FULL TREE
set.seed(2)
full_tree <- rpart(pounds2fip ~ ., data = trainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)

#min xerror is 2.7738, cp5
pruned_tree <- prune(full_tree, cp = 1.5249e-02) 
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

##now predict the average balances of the observations in the validation set
predicted_value <- predict(pruned_tree, validationSet)
##PERFORMANCE EVALUATION
accuracy(predicted_value, validationSet$pounds2fip)

#     ME    RMSE    MAE       MPE     MAPE
# -20.98211 116.709 46.195 -5578.276 5613.357




###TREE 2 - using only YES/NO (1/0) values from initial survey, rather than multiple choice responses

#2021 lbs of food
regData2 <- agencyData2[,c(4:138,143:146,150)]
regData2 <- regData2 %>% relocate(pounds2fip)

#filters to only use numerics (1 & 0 values (yes/no questions) and a couple others)
regData2 <- regData2[sapply(regData2, is.numeric)]


##partition the data set
##recall that a large portion of data is sometimes warranted
set.seed(1)
myIndex <- createDataPartition(regData2$pounds2fip, p=0.7, list=FALSE)
trainSet <- regData2[myIndex,]
validationSet <- regData2[-myIndex,]

##FULL TREE
set.seed(1)
full_tree <- rpart(pounds2fip ~ ., data = trainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)

#cp11 smallest other than cp2 (too close to 1)
pruned_tree <- prune(full_tree, cp = 1.5580e-03) 
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

##now predict the average balances of the observations in the validation set
predicted_value <- predict(pruned_tree, validationSet)
##PERFORMANCE EVALUATION
accuracy(predicted_value, validationSet$pounds2fip)
# 
# ME        RMSE      MAE       MPE     MAPE
# 11.66853 85.55034 34.67481 -1331.513 1383.922
# 
