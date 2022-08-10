#Group Project
#### What Do We Want to Test For----
# Can we accurately predict which platform for movies
# Can we accurately predict which platform for series
# Can we make a recommendation based on demographics
# Bar Graph comparing each platform amounts
# % of NA Vs Ranked IMDB
# % of NA Vs Ranked Rotten.Tomatoes
# Distribution of Genres (may have to limit to most popular genres)
# Distribution of Ages
# Distribution of Languages (may have to limit to Most Common + All Others)
# 
# Are there qualifying criteria to enter a particular platform
# This may be more of an intuition question or pull from tree structures

#### What Are Some Of The Anticipated Challenges ----
# Have to be careful in generalizing, not all movies that fall into these categories are streamed
# Movies Data has considerably more information, it may result in overfitting
# Series Data is far more consolidated, it may not give us enough info for prediction models to be good

#### Import Data ----
setwd("C:/Users/robli/Desktop/R Group Project")
Movies <- read.csv("Movies.csv")
#Series <- read.csv("tv_shows.csv")

#### Libraries ----
#install.packages("tidyr")   REMOVE # FOR INITIAL INSTALL OF PACKAGE
#install.packages("stringr")
#install.packages("e1071")
library(tidyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(adabag)
library(caret)
library(e1071)
#### LIST OF SHARED VARIABLE NAMES ----
# Movies: Dataframe containg 16744 movies across multiple streaming platforms
#$Title:              Name of Movie
#$Year:               Year that Movie was released
#$Age:                Target Age Group and Higher 
#$IMDB:               IMDB Score
#$Rotten.Tomatoes:    Rotten Tomatoes Score
#$Netflix:            Indication of Netflix Availability
#$Hulu:               Indication of Hulu Availability 
#$Prime.Video:        Indication of Prime Availability
#$Disney.:            Indication of Disney+ Availability
#$Directors:          Who the Director(s) Was              *Multiple in some Cases
#$NGenres:            Classifications of Genres            *Multiple in some Cases
#$Country:            Location                              *?*? Need to clarify  
#$Language:           Language                              *?*? Need to clarify
#$Runtime:            Total Length of Run Time
#Added columns for Data Manipulation
#NumLanguages         Number of Languages 
#NumDirectors         Number of Directors
#NumGenres            Number of Genres
#NumCountries         Number of Countries
#English              Available in English
#Spanish              Available in Spanish
#Mandarin             Available in Mandarin
#Cantonese            Available in Cantonese
#Tagalog              Available in Tagalog
#Vietnamese           Available in Vietnamese
#Arabic               Available in Arabic
#French               Available in French
#Korean               Available in Korean
#Russian              Available in Russian
#German               Available in German
#Added Prediction List
#Predict.PP.NFX.UB     Prediction of UnBalanced Netflix Postpruned Tree
#Predict.PP.NFX.Bal    Prediction of Balanced Netflix Postpruned Tree
#Predict.Bag.NFX.UB    Prediction of UnBalanced Netflix Bagged Tree
#Predict.Bag.NFX.Bal   Prediction of Balanced Netflix Bagged Tree
#Predict.Boost.NFX.UB  Prediction of UnBalanced Netflix Boosted Tree
#Predict.Boost.NFX.Bal Prediction of Balanced Netflix Boosted Tree
#Predict.Log.NFX.UB    Prediction of UnBalanced Netflix Through Logistical Regression 
#Predict.Log.NFX.Bal   Prediction of Balanced Netflix Through Logistical Regression 
#Predict.NB.NFX.UB     Prediction of UnBalanced Netflix Through NAive Bayes 
#Predict.NB.NFX.Bal    Prediction of Balanced Netflix Through Naive Bayes

#### CONVERSION OF Movies DATA ----
#Duplicate Origianl Data
MoviesOriginal<- Movies     #Duplicate of Origianl Data

#Convert Columns to specific data types

#Convert columns to Factors  
Movies$Age         <- as.factor(Movies$Age)
Movies$Netflix     <- as.factor(Movies$Netflix)
Movies$Prime.Video <- as.factor(Movies$Prime.Video)
Movies$Disney.     <- as.factor(Movies$Disney.)
Movies$Hulu        <- as.factor(Movies$Hulu)

#Convert Rotten.Tomatoes from  string% to an intenger or NA for unrated
Movies$Rotten.Tomatoes = as.integer(gsub("%", "", Movies$Rotten.Tomatoes))

#Create a count of the # of languages, directors, genres
Movies$NumLanguages  <- str_count(Movies$Language,",") + 1
Movies$NumDirectors  <- str_count(Movies$Directors,",") + 1
Movies$NumGenres     <- str_count(Movies$Genres,",") + 1
Movies$NumCountries  <- str_count(Movies$Country,",") + 1
#Create indication for 10 most popular languages in the USA and group all others
Movies$English       <- as.integer(str_detect(Movies$Language, "English"))
Movies$Spanish       <- as.integer(str_detect(Movies$Language, "Spanish"))
Movies$Mandarin      <- as.integer(str_detect(Movies$Language, "Mandarin"))
Movies$Cantonese     <- as.integer(str_detect(Movies$Language, "Cantonese"))
Movies$Tagalog       <- as.integer(str_detect(Movies$Language, "Tagalog"))
Movies$Vietnamese    <- as.integer(str_detect(Movies$Language, "Vietnamese"))
Movies$Arabic        <- as.integer(str_detect(Movies$Language, "Arabic"))
Movies$French        <- as.integer(str_detect(Movies$Language, "French"))
Movies$Korean        <- as.integer(str_detect(Movies$Language, "Korean"))
Movies$Russian       <- as.integer(str_detect(Movies$Language, "Russian"))
Movies$German        <- as.integer(str_detect(Movies$Language, "German"))
Movies$Other.Language<- (Movies$NumLanguages - Movies$English - Movies$Russian
                         -Movies$Cantonese    - Movies$Tagalog - Movies$French
                         -Movies$Vietnamese   - Movies$Spanish - Movies$Arabic 
                         -Movies$Mandarin     - Movies$Korean  - Movies$German)

#Create subsets for control value designation and Balanced Predictions Samples
KnownNflx   <- Movies[Movies$Netflix == 1,]
KnownPrime  <- Movies[Movies$Prime.Video == 1,]
KnownHulu   <- Movies[Movies$Hulu == 1,]
KnownDisney <- Movies[Movies$Disney. == 1,]
NotNflx     <- Movies[Movies$Netflix == 0,]
NotPrime    <- Movies[Movies$Prime.Video == 0,]
NotHulu     <- Movies[Movies$Hulu == 0,]
NotDisney   <- Movies[Movies$Disney. == 0,]

#Added Prediction List  DONT THINK WE ARE GOING TO USE THESE
#Movies$Predict.PP.NFX.UB    <- 0 #Prediction of UnBalanced Netflix Postpruned Tree
#Movies$Predict.PP.NFX.Bal   <- 0 #Prediction of Balanced Netflix Postpruned Tree
#Movies$Predict.Bag.NFX.UB   <- 0 #Prediction of UnBalanced Netflix Bagged Tree
#Movies$Predict.Bag.NFX.Bal  <- 0 #Prediction of Balanced Netflix Bagged Tree
#Movies$Predict.Boost.NFX.UB <- 0 #Prediction of UnBalanced Netflix Boosted Tree
#Movies$Predict.Boost.NFX.Bal<- 0 # Prediction of Balanced Netflix Boosted Tree
#Movies$Predict.Log.NFX.UB   <- 0 # Prediction of UnBalanced Netflix Through Logistical Regression 
#Movies$Predict.Log.NFX.Bal  <- 0 # Prediction of Balanced Netflix Through Logistical Regression 
#Movies$Predict.NB.NFX.UB    <- 0 # Prediction of UnBalanced Netflix Through NAive Bayes 
#Movies$Predict.NB.NFX.Bal   <- 0 # Prediction of Balanced Netflix Through Naive Bayes
#Movies$Predict.Ensemble.NFX.UB  <- 0 # Prediction of UnBalanced Netflix Through Ensemble 
#Movies$Predict.Ensemble.Bal <- 0 # Prediction of Balanced Netflix Through Ensemble

#Create control values for tree variables
set.seed        (8675309) # for reproducible results
XValue        <- 10
MinSplitValue <- 300
CPValue       <- 0
MFinalBag     <- 10
MFinalBoost   <- 40


#### CREATE A POST-PRUNED DECISION TREE ----
#CODE FUNCTIONALITY DESCRIPTION
# 1) Create a decision tree without knowledge of streaming platforms
# 2) Post-prune tree based on best CP value


#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
####  AMAZON PRIME PREDICTION SECTION STARTS HERE ####
# 1) Create a decision tree without knowledge of streaming platforms
# 2) Post-prune tree based on best CP value


#Create control values for tree variables
set.seed        (8675309) # for reproducible results
XValue        <- 10
MinSplitValue <- 300
CPValue       <- 0
MFinalBag     <- 10
MFinalBoost   <- 40

# Split the adjusted data into testing and training data sets
# We will first randomly select 2/3 of the rows
Sample.PRM.UB <- sample(1:nrow(Movies),nrow(Movies)*(2/3)) 
Train.PRM.UB <- Movies[Sample.PRM.UB,]   #2/3 of the Data 
Test.PRM.UB <- Movies[-Sample.PRM.UB,]   #the other 1/3 of the Data
Ensemble.UB <- Test.PRM.UB               #initial creation of Ensemble approach

#Set Test Variables for the trees, can create multiple versions of Variable tests using this technique
Test.PRM.Variables <- (Prime.Video ~ Year + Age + IMDb + Rotten.Tomatoes + Type + NumGenres + NumCountries + NumLanguages + Runtime + English + Spanish + Mandarin + Cantonese + Tagalog + Vietnamese + Arabic + French + Korean + Russian + German)

# Full Grown Decision Tree for Prime.Video
Fit.PRM.UB <- rpart(Test.PRM.Variables,
                    data = Train.PRM.UB,
                    method = "class",
                    control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                    parms= list(split="gini"))
#Fit.PRM.UB
#nrow(Fit.PRM.UB$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Prime.Video using the unpruned decision tree and compare it to the actual Prime.Video value
Pred.PRM.UB <- predict(Fit.PRM.UB, Test.PRM.UB, type="class")
Actual.PRM.UB <- Test.PRM.UB$Prime.Video

CM.PRM.UB <- table(Pred.PRM.UB, Actual.PRM.UB)
Pre.CM.PRM.UBtrain <- confusionMatrix(CM.PRM.UB, positive = '1')
#Pre.CM.PRM.UBtrain

# POST-PRUNE The decision tree using the best CP
BestCPFit.PRM.UB <- Fit.PRM.UB$cptable[which.min(Fit.PRM.UB$cptable[,"xerror"]),"CP"]
Post.Fit.PRM.UB <- prune.rpart(Fit.PRM.UB, cp=BestCPFit.PRM.UB)
#nrow(Post.Fit.PRM.UB$frame) #Perspective for how much smaller the post-pruned decision tree is

# Compute the confusion matrices of the post-pruned predictions
Post.CM.PRM.UBtrain <- confusionMatrix(table(predict(Post.Fit.PRM.UB, Train.PRM.UB, type="class"),
                                             Train.PRM.UB$Prime.Video), positive='1')
Post.CM.PRM.UBtest <-  confusionMatrix(table(predict(Post.Fit.PRM.UB, Test.PRM.UB, type="class"),
                                             Test.PRM.UB$Prime.Video), positive='1')
#Post.CM.PRM.UBtrain
#Post.CM.PRM.UBtest

#Store the prediction in the testing data set
Ensemble.UB$Predict.PP.PRM.UB    <- predict(Post.Fit.PRM.UB, Test.PRM.UB, type="class") #Prediction of UnBalanced Prime.Video Postpruned Tree

#Store the predicted value for the training data set to be used in an ensemble, this ensemble approach was created after the other ensemble variable approach
Ensemble2.UB<-Train.PRM.UB 
Ensemble2.UB$Predict.PP.PRM.UB <- as.factor(predict(Post.Fit.PRM.UB,Train.PRM.UB, type = "class"))


# Create Decision tree for Prime.Video using Bagging
Bag.Fit.PRM.UB <- bagging(Test.PRM.Variables,
                          data = Train.PRM.UB,
                          mfinal = MFinalBag)
Pred.Bag.PRM.UB <- predict(Bag.Fit.PRM.UB, Test.PRM.UB, type = "class")

# Create Confusion Matrix for test prediction
CM.Bag.PRM.UB <- confusionMatrix(as.factor(Pred.Bag.PRM.UB$class), Test.PRM.UB$Prime.Video,positive='1')
#CM.Bag.PRM.UB

Ensemble.UB$Predict.Bag.PRM.UB   <- as.factor(Pred.Bag.PRM.UB$class) #Prediction of UnBalanced Prime.Video Bagged Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.UB.P1<-predict(Bag.Fit.PRM.UB,Train.PRM.UB, type = "class")
Ensemble2.UB$Predict.Bag.PRM.UB<-as.factor(Ensemble2.PRM.UB.P1$class)

# Create Decision tree for Prime.Video using Boosting
Boost.Fit.PRM.UB <- boosting(Test.PRM.Variables,
                             data = Train.PRM.UB,
                             mfinal = MFinalBoost)
Pred.Boost.PRM.UB  <- predict(Boost.Fit.PRM.UB, Test.PRM.UB, type = "class")

# Create Confusion Matrix for test prediction
CM.Boost.PRM.UB <- confusionMatrix(as.factor(Pred.Boost.PRM.UB $class), Test.PRM.UB$Prime.Video,positive='1')
#CM.Boost.PRM.UB
Ensemble.UB$Predict.Boost.PRM.UB <- as.factor(Pred.Boost.PRM.UB$class) #Prediction of UnBalanced Prime.Video Boosted Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.UB.P2<-predict(Boost.Fit.PRM.UB,Train.PRM.UB, type = "class")
Ensemble2.UB$Predict.Boost.PRM.UB<-as.factor(Ensemble2.PRM.UB.P2$class)

#***********************************************************************************************************************************  
#### CREATE A BALANCED TEST DATA DECISION TREE ----  
#This section does the same thing as the decision tree above except it uses a smaller balanced training set 
#to compensate for the smaller proportions of Netflix, Hulu, and Disney+ content and heavy proportion of Prime Video.

# Split the adjusted data into testing and training data sets
##Create a balanced training set by taking 2/3rds of the KnownPrime (2373) and the same # of NotPrime
##This Should contain 4746 observations of 50/50 data
##pull sample from all KnownPrime containing but using 2/3 of the size of NotPrime because this is over proportioned data
Sample1.PRM.Bal <- sample(1:nrow(KnownPrime),nrow(NotPrime)*(2/3)) ##Note these are different from the later samples
##pull sample from all NotPrime containing the same number as the Sample1
Sample2.PRM.Bal <- sample(1:nrow(NotPrime), nrow(NotPrime)*(2/3))  


Train.PRM.Bal<- rbind(NotPrime[Sample2.PRM.Bal,], KnownPrime[Sample1.PRM.Bal,])   #2/3 of the balanced data 
Test.PRM.Bal <- rbind(NotPrime[-Sample2.PRM.Bal,] , KnownPrime[-Sample1.PRM.Bal,])  #the rest of the data

Ensemble.Bal <- Test.PRM.Bal

# Full Grown Decision Tree for balanced Prime.Video data
Fit.PRM.Bal <- rpart(Test.PRM.Variables,
                     data = Train.PRM.Bal,
                     method = "class",
                     control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                     parms= list(split="gini"))
#Fit.PRM.Bal
#nrow(Fit.PRM.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Prime.Video using the unpruned decision tree and compare it to the actual Prime.Video value
Pred.PRM.Bal <- predict(Fit.PRM.Bal, Test.PRM.Bal, type="class")
Actual.PRM.Bal <- Test.PRM.Bal$Prime.Video

CM.PRM.Bal <- table(Pred.PRM.Bal, Actual.PRM.Bal)
#addmargins(CM.PRM.Bal)
Post.CM.PRM.Bal<-confusionMatrix(CM.PRM.Bal,positive = '1')
#Post.CM.PRM.Bal

# POST-PRUNE The decision tree using the best CP
BestCPFit.PRM.Bal <- Fit.PRM.Bal$cptable[which.min(Fit.PRM.Bal$cptable[,"xerror"]),"CP"]
Post.Fit.PRM.Bal <- prune.rpart(Fit.PRM.Bal, cp=BestCPFit.PRM.Bal)
#nrow(Post.Fit.PRM.Bal$frame) #Perspective for how much smaller the post-pruned decision tree is

# Compute the confusion matrices of the post-pruned predictions
Post.CM.PRM.Baltrain <- confusionMatrix(table(predict(Post.Fit.PRM.Bal, Train.PRM.Bal, type="class"),
                                              Train.PRM.Bal$Prime.Video), positive='1')

Post.CM.PRM.Baltest <-  confusionMatrix(table(predict(Post.Fit.PRM.Bal, Test.PRM.Bal, type="class"),
                                              Test.PRM.Bal$Prime.Video), positive='1')
#Post.CM.PRM.Baltrain
#Post.CM.PRM.Baltest

Ensemble.Bal$Predict.PP.PRM.Bal    <- predict(Post.Fit.PRM.Bal, Test.PRM.Bal, type="class") #Prediction of Balanced Prime.Video Postpruned Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.Bal<-Train.PRM.Bal 
Ensemble2.Bal$Predict.PP.PRM.Bal <- as.factor(predict(Post.Fit.PRM.Bal,Train.PRM.Bal, type = "class"))

# Create Decision tree for Prime.Video using Bagging
Bag.Fit.PRM.Bal <- bagging(Test.PRM.Variables,
                           data = Train.PRM.Bal,
                           mfinal = MFinalBag)
Pred.Bag.PRM.Bal <- predict(Bag.Fit.PRM.Bal, Test.PRM.Bal, type = "class")
# Create Confusion Matrix for test prediction
CM.Bag.PRM.Bal <- confusionMatrix(as.factor(Pred.Bag.PRM.Bal$class), Test.PRM.Bal$Prime.Video,positive='1')
#CM.Bag.PRM.Bal

Ensemble.Bal$Predict.Bag.PRM.Bal   <- as.factor(Pred.Bag.PRM.Bal$class) #Prediction of Balanced Prime.Video Bagged Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.Bal.P1<-predict(Bag.Fit.PRM.Bal,Train.PRM.Bal, type = "class")
Ensemble2.Bal$Predict.Bag.PRM.Bal<-as.factor(Ensemble2.PRM.Bal.P1$class)

# Create Decision tree for Prime.Video using Boosting
Boost.Fit.PRM.Bal <- boosting(Test.PRM.Variables,
                              data = Train.PRM.Bal,
                              mfinal = MFinalBoost)
Pred.Boost.PRM.Bal <- predict(Boost.Fit.PRM.Bal, Test.PRM.Bal, type = "class")
# Create Confusion Matrix for test prediction
CM.Boost.PRM.Bal <- confusionMatrix(as.factor(Pred.Boost.PRM.Bal$class), Test.PRM.Bal$Prime.Video,positive='1')
#CM.Boost.PRM.Bal

Ensemble.Bal$Predict.Boost.PRM.Bal <- as.factor(Pred.Boost.PRM.Bal$class) #Prediction of Balanced Prime.Video Boosted Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.Bal.P2<-predict(Boost.Fit.PRM.Bal,Train.PRM.Bal, type = "class")
Ensemble2.Bal$Predict.Boost.PRM.Bal<-as.factor(Ensemble2.PRM.Bal.P2$class)

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#*#* These 3 lines of code will come into play much later in the file (around line 1425) and will be signified as an outside the box  #*#*#*#*#*#
#*#* appoach.  We will use this balanced boosted predictor to create new movie set called Predicted.PRM with a prime prediction       #*#*#*#*#*#
Predicted.PRM <- Movies
PRM.Predict <- predict(Boost.Fit.PRM.Bal, Predicted.PRM, type = "class")
Predicted.PRM$PRM.Predict <- as.factor(PRM.Predict$class)
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#***********************************************************************************************************************************  
#### CREATE A PREDICITION USING LOGISTIC REGRESSION ----  
Logit.Train.PRM.UB <- Train.PRM.UB
Logit.PRM.UB <- glm(Test.PRM.Variables,
                    data = Logit.Train.PRM.UB,
                    family = "binomial")
#summary(Logit.PRM.UB)
Logit.Test.PRM.UB <- Test.PRM.UB
#summary(Logit.Test.PRM.UB)

Logit.Test.PRM.UB$Reg.Predict <- predict(Logit.PRM.UB, Logit.Test.PRM.UB, type = "response")

Logit.Test.PRM.UB$PRM.Predict <- ifelse(Logit.Test.PRM.UB$Reg.Predict > 0.5, 1 ,0)
CM.Logit.PRM.UB <- confusionMatrix(as.factor(Logit.Test.PRM.UB$PRM.Predict), Logit.Test.PRM.UB$Prime.Video,positive='1')
#CM.Logit.PRM.UB

Ensemble.UB$Predict.Log.PRM.UB <- as.factor(Logit.Test.PRM.UB$PRM.Predict)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.UB.P3<-predict(Logit.PRM.UB, Logit.Train.PRM.UB, type = "response")
Ensemble2.PRM.UB.P3$class <- ifelse(Ensemble2.PRM.UB.P3 > 0.5, 1 ,0)
Ensemble2.UB$Predict.Log.PRM.UB<-as.factor(Ensemble2.PRM.UB.P3$class)
#********************************************

Logit.Train.PRM.Bal <- Train.PRM.Bal
Logit.PRM.Bal <- glm(Test.PRM.Variables,
                     data = Logit.Train.PRM.Bal,
                     family = "binomial")
#summary(Logit.PRM.Bal)
Logit.Test.PRM.Bal <- Test.PRM.Bal
#summary(Logit.Test.PRM.Bal)

Logit.Test.PRM.Bal$Reg.Predict <- predict(Logit.PRM.Bal, Logit.Test.PRM.Bal, type = "response")
Logit.Test.PRM.Bal$PRM.Predict <- ifelse(Logit.Test.PRM.Bal$Reg.Predict > 0.5, 1 ,0)

CM.Logit.PRM.Bal <- confusionMatrix(as.factor(Logit.Test.PRM.Bal$PRM.Predict), Logit.Test.PRM.Bal$Prime.Video,positive='1')
#CM.Logit.PRM.Bal

Ensemble.Bal$Predict.Log.PRM.Bal <- as.factor(Logit.Test.PRM.Bal$PRM.Predict)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.Bal.P3<-predict(Logit.PRM.Bal, Logit.Train.PRM.Bal, type = "response")
Ensemble2.PRM.Bal.P3$class <- ifelse(Ensemble2.PRM.Bal.P3 > 0.5, 1 ,0)
Ensemble2.Bal$Predict.Log.PRM.Bal<-as.factor(Ensemble2.PRM.Bal.P3$class)

#***********************************************************************************************************************************  
#### CREATE A PREDICITION USING NAIVE BAYES CLASSIFIER----  
NBayes.Train.PRM.UB <- Train.PRM.UB
NBayes.Test.PRM.UB <- Test.PRM.UB

# Run Naive Bayes Prediction for Raw and Class Probability
NBayes.PRM.UB <- naiveBayes(Test.PRM.Variables,
                            data = NBayes.Train.PRM.UB,
                            na.action=na.pass)
NBayes.Test.PRM.UB.Predict.Raw <- predict(NBayes.PRM.UB, NBayes.Test.PRM.UB, type = "raw")
NBayes.Test.PRM.UB.Predict.Class <- predict(NBayes.PRM.UB, NBayes.Test.PRM.UB, type = "class")


#Generate Confusion Matrix 
CM.NBayes.PRM.UB <- confusionMatrix(NBayes.Test.PRM.UB.Predict.Class, NBayes.Test.PRM.UB$Prime.Video, positive="1")
#CM.NBayes.PRM.UB

Ensemble.UB$Predict.NB.PRM.UB <- as.factor(NBayes.Test.PRM.UB.Predict.Class)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.UB.P4<-predict(NBayes.PRM.UB, NBayes.Train.PRM.UB, type = "class")
Ensemble2.UB$Predict.NB.PRM.UB<-Ensemble2.PRM.UB.P4

#***********************************************
#Run with Balanced Training Set
NBayes.Train.PRM.Bal <- Train.PRM.Bal
NBayes.Test.PRM.Bal <- Test.PRM.Bal

# Run Naive Bayes Prediction for Raw and Class Probability
NBayes.PRM.Bal <- naiveBayes(Test.PRM.Variables,
                             data = NBayes.Train.PRM.Bal,
                             na.action=na.pass)
NBayes.Test.PRM.Bal.Predict.Raw <- predict(NBayes.PRM.Bal, NBayes.Test.PRM.Bal, type = "raw")
NBayes.Test.PRM.Bal.Predict.Class <- predict(NBayes.PRM.Bal, NBayes.Test.PRM.Bal, type = "class")


#Generate Confusion Matrix 
CM.NBayes.PRM.Bal <- confusionMatrix(NBayes.Test.PRM.Bal.Predict.Class, NBayes.Test.PRM.Bal$Prime.Video, positive="1")
#CM.NBayes.PRM.Bal

Ensemble.Bal$Predict.NB.PRM.Bal <- as.factor(NBayes.Test.PRM.Bal.Predict.Class)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.PRM.Bal.P4<-predict(NBayes.PRM.Bal, NBayes.Train.PRM.Bal, type = "class")
Ensemble2.Bal$Predict.NB.PRM.Bal<-Ensemble2.PRM.Bal.P4

#***********************************************************************************************************************************  
#### USE ALL OF YOUR CLASSIFIERS TO CREATE AN ENSEMBLE CLASSIFIER---- 

Ensemble.UB$Predict.Ensemble.PRM.UB <- as.factor(round(((
  as.integer(Ensemble.UB$Predict.PP.PRM.UB)+
    as.integer(Ensemble.UB$Predict.Bag.PRM.UB)+
    as.integer(Ensemble.UB$Predict.Boost.PRM.UB)+
    as.integer(Ensemble.UB$Predict.Log.PRM.UB)+
    as.integer(Ensemble.UB$Predict.NB.PRM.UB))/10),0))   #Divide by 10 since as.integer converts 1 to 2

CM.Ensemble.UB <- confusionMatrix(Ensemble.UB$Predict.Ensemble.PRM.UB, Ensemble.UB$Prime.Video, positive ="1")
#CM.Ensemble.UB


Ensemble.Bal$Predict.Ensemble.PRM.Bal <- as.factor(round(((
  as.integer(Ensemble.Bal$Predict.PP.PRM.Bal)+
    as.integer(Ensemble.Bal$Predict.Bag.PRM.Bal)+
    as.integer(Ensemble.Bal$Predict.Boost.PRM.Bal)+
    as.integer(Ensemble.Bal$Predict.Log.PRM.Bal)+
    as.integer(Ensemble.Bal$Predict.NB.PRM.Bal))/10),0))   #Divide by 10 since as.integer converts 1 to 2

CM.Ensemble.Bal <- confusionMatrix(Ensemble.Bal$Predict.Ensemble.PRM.Bal, Ensemble.Bal$Prime.Video, positive ="1")
#CM.Ensemble.Bal


#********************************************************************************************************************
####Decision Tree Ensemble----
#Unbalanced Version
Ensemble2.DT.PRM.UB <- rpart(Prime.Video ~ Predict.PP.PRM.UB+Predict.Bag.PRM.UB+Predict.Boost.PRM.UB+Predict.Log.PRM.UB+Predict.NB.PRM.UB,
                             data = Ensemble2.UB,
                             method = "class",
                             control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                             parms= list(split="gini"))

#nrow(Ensemble2.DT.PRM.UB$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Prime.Video using the unpruned decision tree on the different predicted methods and compare it to the actual Prime.Video value
Ensemble2.Predict.DT.PRM.UB <-predict(Ensemble2.DT.PRM.UB, Ensemble.UB, type="class")
Ensemble2.Actual.PRM.UB <- Test.PRM.UB$Prime.Video


CM.Ensemble2.DT.PRM.UB <- confusionMatrix(Ensemble2.Predict.DT.PRM.UB, Ensemble2.Actual.PRM.UB,positive="1")
#CM.Ensemble2.DT.PRM.UB #decision tree ensemble 
#CM.Ensemble.UB #for comparison to the average ensemble
#Balanced Version
Ensemble2.DT.PRM.Bal <- rpart(Prime.Video ~ Predict.PP.PRM.Bal+Predict.Bag.PRM.Bal+Predict.Boost.PRM.Bal+Predict.Log.PRM.Bal+Predict.NB.PRM.Bal,
                              data = Ensemble2.Bal,
                              method = "class",
                              control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                              parms= list(split="gini"))

#nrow(Ensemble2.DT.PRM.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Prime.Video using the unpruned decision tree on the different predicted methods and compare it to the actual Prime.Video value
Ensemble2.Predict.DT.PRM.Bal <-predict(Ensemble2.DT.PRM.Bal, Ensemble.Bal, type="class")
Ensemble2.Actual.PRM.Bal <- Test.PRM.Bal$Prime.Video


CM.Ensemble2.DT.PRM.Bal <- confusionMatrix(Ensemble2.Predict.DT.PRM.Bal, Ensemble2.Actual.PRM.Bal,positive="1")
#CM.Ensemble2.DT.PRM.Bal #decision tree ensemble 
#CM.Ensemble.Bal #for comparison to the average ensemble

#********************************************************************************************************************
####Bagged Decision Tree Ensemble----
#UnBalanced Version
Ensemble2.Bagged.PRM.UB <- bagging(Prime.Video ~ Predict.PP.PRM.UB+Predict.Bag.PRM.UB+Predict.Boost.PRM.UB+Predict.Log.PRM.UB+Predict.NB.PRM.UB,
                                   data = Ensemble2.UB,
                                   mfinal = MFinalBag)

Ensemble2.Predict.Bag.PRM.UB <- predict(Ensemble2.Bagged.PRM.UB, Ensemble.UB, type = "class")

CM.Ensemble2.Bag.PRM.UB <- confusionMatrix(as.factor(Ensemble2.Predict.Bag.PRM.UB$class), Ensemble2.Actual.PRM.UB,positive="1")
#CM.Ensemble2.Bag.PRM.UB 

#Balanced Version
Ensemble2.Bagged.PRM.Bal <- bagging(Prime.Video ~ Predict.PP.PRM.Bal+Predict.Bag.PRM.Bal+Predict.Boost.PRM.Bal+Predict.Log.PRM.Bal+Predict.NB.PRM.Bal,
                                    data = Ensemble2.Bal,
                                    mfinal = MFinalBag)

Ensemble2.Predict.Bag.PRM.Bal <- predict(Ensemble2.Bagged.PRM.Bal, Ensemble.Bal, type = "class")

CM.Ensemble2.Bag.PRM.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Bag.PRM.Bal$class), Ensemble2.Actual.PRM.Bal,positive="1")
#CM.Ensemble2.Bag.PRM.Bal 

#********************************************************************************************************************
####Boosted Decision Tree Ensemble----
#UnBalanced Version
Ensemble2.Boost.PRM.UB <- boosting(Prime.Video ~ Predict.PP.PRM.UB+Predict.Bag.PRM.UB+Predict.Boost.PRM.UB+Predict.Log.PRM.UB+Predict.NB.PRM.UB,
                                   data = Ensemble2.UB,
                                   mfinal = MFinalBoost)
Ensemble2.Predict.Boost.PRM.UB <- predict(Ensemble2.Boost.PRM.UB, Ensemble.UB, type = "class")

CM.Ensemble2.Boost.PRM.UB <- confusionMatrix(as.factor(Ensemble2.Predict.Boost.PRM.UB$class), Ensemble2.Actual.PRM.UB,positive="1")
#CM.Ensemble2.Boost.PRM.UB 

#Balanced Version
Ensemble2.Boost.PRM.Bal <- boosting(Prime.Video ~ Predict.PP.PRM.Bal+Predict.Bag.PRM.Bal+Predict.Boost.PRM.Bal+Predict.Log.PRM.Bal+Predict.NB.PRM.Bal,
                                    data = Ensemble2.Bal,
                                    mfinal = MFinalBoost)
Ensemble2.Predict.Boost.PRM.Bal <- predict(Ensemble2.Boost.PRM.Bal, Ensemble.Bal, type = "class")

CM.Ensemble2.Boost.PRM.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Boost.PRM.Bal$class), Ensemble2.Actual.PRM.Bal,positive="1")
#CM.Ensemble2.Boost.PRM.Bal 

#********************************************************************************************************************
####Logistic Regression Ensemble----
#UnBalanced Version
Ensemble2.Logit.PRM.UB <- glm(Prime.Video ~ Predict.PP.PRM.UB+Predict.Bag.PRM.UB+Predict.Boost.PRM.UB+Predict.Log.PRM.UB+Predict.NB.PRM.UB,
                              data = Ensemble2.UB,
                              family = "binomial")

Ensemble2.Predict.Reg.Logit.PRM.UB <-predict(Ensemble2.Logit.PRM.UB, Ensemble.UB, type="response")
Ensemble2.Predict.Logit.PRM.UB <- ifelse(Ensemble2.Predict.Reg.Logit.PRM.UB >.5,1,0)

CM.Ensemble2.Logit.PRM.UB <- confusionMatrix(as.factor(Ensemble2.Predict.Logit.PRM.UB), Ensemble2.Actual.PRM.UB,positive="1")
#CM.Ensemble2.Logit.PRM.UB  

#Balanced Version
Ensemble2.Logit.PRM.Bal <- glm(Prime.Video ~ Predict.PP.PRM.Bal+Predict.Bag.PRM.Bal+Predict.Boost.PRM.Bal+Predict.Log.PRM.Bal+Predict.NB.PRM.Bal,
                               data = Ensemble2.Bal,
                               family = "binomial")

Ensemble2.Predict.Reg.Logit.PRM.Bal <-predict(Ensemble2.Logit.PRM.Bal, Ensemble.Bal, type="response")
Ensemble2.Predict.Logit.PRM.Bal <- ifelse(Ensemble2.Predict.Reg.Logit.PRM.Bal >.5,1,0)

CM.Ensemble2.Logit.PRM.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Logit.PRM.Bal), Ensemble2.Actual.PRM.Bal,positive="1")
#CM.Ensemble2.Logit.PRM.Bal  

#********************************************************************************************************************
####Naive Bayes Ensemble----
#UnBalanced
Ensemble2.NB.PRM.UB <- naiveBayes(Prime.Video ~ Predict.PP.PRM.UB+Predict.Bag.PRM.UB+Predict.Boost.PRM.UB+Predict.Log.PRM.UB+Predict.NB.PRM.UB,
                                  data = Ensemble2.UB)

Ensemble2.NB.RAW.PRM.UB<-predict(Ensemble2.NB.PRM.UB, Ensemble.UB, type="raw")
Ensemble2.NB.PRM.UB<-predict(Ensemble2.NB.PRM.UB, Ensemble.UB, type="class")

CM.Ensemble2.NB.PRM.UB <- confusionMatrix(Ensemble2.NB.PRM.UB, Ensemble2.Actual.PRM.UB,positive="1")
#CM.Ensemble2.NB.PRM.UB 

#Balanced Version
Ensemble2.NB.PRM.Bal <- naiveBayes(Prime.Video ~ Predict.PP.PRM.Bal+Predict.Bag.PRM.Bal+Predict.Boost.PRM.Bal+Predict.Log.PRM.Bal+Predict.NB.PRM.Bal,
                                   data = Ensemble2.Bal)

Ensemble2.NB.RAW.PRM.Bal<-predict(Ensemble2.NB.PRM.Bal, Ensemble.Bal, type="raw")
Ensemble2.NB.PRM.Bal<-predict(Ensemble2.NB.PRM.Bal, Ensemble.Bal, type="class")

CM.Ensemble2.NB.PRM.Bal <- confusionMatrix(Ensemble2.NB.PRM.Bal, Ensemble2.Actual.PRM.Bal,positive="1")
#CM.Ensemble2.NB.PRM.Bal 


#### SHOW ALL CONFUSION MATRIX FOR PRM ####
Pre.CM.PRM.UBtrain
Post.CM.PRM.UBtrain
Post.CM.PRM.UBtest
CM.Bag.PRM.UB
CM.Boost.PRM.UB
Post.CM.PRM.Bal
Post.CM.PRM.Baltrain
Post.CM.PRM.Baltest
CM.Bag.PRM.Bal
CM.Boost.PRM.Bal
CM.Logit.PRM.UB
CM.Logit.PRM.Bal
CM.NBayes.PRM.UB
CM.NBayes.PRM.Bal
CM.Ensemble.UB
CM.Ensemble.Bal
CM.Ensemble2.DT.PRM.UB
CM.Ensemble2.DT.PRM.Bal
CM.Ensemble2.Bag.PRM.UB 
CM.Ensemble2.Bag.PRM.Bal
CM.Ensemble2.Boost.PRM.UB 
CM.Ensemble2.Boost.PRM.Bal
CM.Ensemble2.Logit.PRM.UB 
CM.Ensemble2.Logit.PRM.Bal 
CM.Ensemble2.NB.PRM.UB 
CM.Ensemble2.NB.PRM.Bal 


AllCM.PRM <- matrix(NA,nrow=23,ncol=7)
colnames(AllCM.PRM)=c('Confusion Matrix','Accuracy','Sensitivity','Specificity','Balanced Accuracy','Pos Pred Value','Neg Pred Value')
AllCM.PRM[1,]   <- c("Post.CM.PRM.UBtest",Post.CM.PRM.UBtest$overall[1],Post.CM.PRM.UBtest$byClass[c(1,2,11,3,4)])
AllCM.PRM[2,]   <- c("CM.Bag.PRM.UB",CM.Bag.PRM.UB$overall[1],CM.Bag.PRM.UB$byClass[c(1,2,11,3,4)])
AllCM.PRM[3,]   <- c("CM.Boost.PRM.UB",CM.Boost.PRM.UB$overall[1],CM.Boost.PRM.UB$byClass[c(1,2,11,3,4)])
AllCM.PRM[4,]   <- c("Post.CM.PRM.Bal",Post.CM.PRM.Bal$overall[1],Post.CM.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[5,]   <- c("Post.CM.PRM.Baltest",Post.CM.PRM.Baltest$overall[1],Post.CM.PRM.Baltest$byClass[c(1,2,11,3,4)])
AllCM.PRM[6,]   <- c("CM.Bag.PRM.Bal",CM.Bag.PRM.Bal$overall[1],CM.Bag.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[7,]   <- c("CM.Boost.PRM.Bal",CM.Boost.PRM.Bal$overall[1],CM.Boost.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[8,]   <- c("CM.Logit.PRM.UB",CM.Logit.PRM.UB$overall[1],CM.Logit.PRM.UB$byClass[c(1,2,11,3,4)])
AllCM.PRM[9,]   <- c("CM.Logit.PRM.Bal",CM.Logit.PRM.Bal$overall[1],CM.Logit.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[10,]   <- c("CM.NBayes.PRM.UB",CM.NBayes.PRM.UB$overall[1],CM.NBayes.PRM.UB$byClass[c(1,2,11,3,4)])
AllCM.PRM[11,]   <- c("CM.NBayes.PRM.Bal",CM.NBayes.PRM.Bal$overall[1],CM.NBayes.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[12,]   <- c("CM.Ensemble.UB",CM.Ensemble.UB$overall[1],CM.Ensemble.UB$byClass[c(1,2,11,3,4)])
AllCM.PRM[13,]   <- c("CM.Ensemble.Bal",CM.Ensemble.Bal$overall[1],CM.Ensemble.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[14,]   <- c("CM.Ensemble2.DT.PRM.UB",CM.Ensemble2.DT.PRM.UB$overall[1],CM.Ensemble2.DT.PRM.UB$byClass[c(1,2,11,3,4)])
AllCM.PRM[15,]   <- c("CM.Ensemble2.DT.PRM.Bal",CM.Ensemble2.DT.PRM.Bal$overall[1],CM.Ensemble2.DT.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[16,]   <- c("CM.Ensemble2.Bag.PRM.UB ",CM.Ensemble2.Bag.PRM.UB $overall[1],CM.Ensemble2.Bag.PRM.UB $byClass[c(1,2,11,3,4)])
AllCM.PRM[17,]   <- c("CM.Ensemble2.Bag.PRM.Bal",CM.Ensemble2.Bag.PRM.Bal$overall[1],CM.Ensemble2.Bag.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[18,]   <- c("CM.Ensemble2.Boost.PRM.UB ",CM.Ensemble2.Boost.PRM.UB $overall[1],CM.Ensemble2.Boost.PRM.UB $byClass[c(1,2,11,3,4)])
AllCM.PRM[19,]   <- c("CM.Ensemble2.Boost.PRM.Bal",CM.Ensemble2.Boost.PRM.Bal$overall[1],CM.Ensemble2.Boost.PRM.Bal$byClass[c(1,2,11,3,4)])
AllCM.PRM[20,]   <- c("CM.Ensemble2.Logit.PRM.UB ",CM.Ensemble2.Logit.PRM.UB $overall[1],CM.Ensemble2.Logit.PRM.UB $byClass[c(1,2,11,3,4)])
AllCM.PRM[21,]   <- c("CM.Ensemble2.Logit.PRM.Bal ",CM.Ensemble2.Logit.PRM.Bal $overall[1],CM.Ensemble2.Logit.PRM.Bal $byClass[c(1,2,11,3,4)])
AllCM.PRM[22,]   <- c("CM.Ensemble2.NB.PRM.UB ",CM.Ensemble2.NB.PRM.UB $overall[1],CM.Ensemble2.NB.PRM.UB $byClass[c(1,2,11,3,4)])
AllCM.PRM[23,]   <- c("CM.Ensemble2.NB.PRM.Bal ",CM.Ensemble2.NB.PRM.Bal $overall[1],CM.Ensemble2.NB.PRM.Bal $byClass[c(1,2,11,3,4)])

Test.PRM.Variables
AllCM.PRM


#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#WRITE CODE HERE
# Split the adjusted data into testing and training data sets
# We will first randomly select 2/3 of the rows
#Test.NFX.Variables <- (Netflix ~ Year + Age + IMDb + Rotten.Tomatoes + Type + NumGenres + NumCountries + NumLanguages + Runtime + English + Spanish + Mandarin + Cantonese + Tagalog + Vietnamese + Arabic + French + Korean + Russian + German)
Test.NFX.Variables <- (Netflix ~ Year + Age + IMDb + Type + NumCountries + NumLanguages + Runtime + English + Spanish + Mandarin + Cantonese + Tagalog + Vietnamese + Arabic + French + Korean + Russian + German)



#***********************************************************************************************************************************  
#### CREATE A BALANCED TEST DATA DECISION TREE ----  
#This section does the same thing as the decision tree above except it uses a smaller balanced training set 
#to compensate for the smaller proportions of Netflix, Hulu, and Disney+ content.

#NEW VARIABLES USED IN THIS SECTION


#WRITE CODE HERE
# Split the adjusted data into testing and training data sets
# We will first randomly select 2/3 of the rows

##KnownNflx contains the 3560 Netflix samples
##NotNFlx contains the 13184 samples that are not Netflix
##Create a balanced training set by taking 2/3rds of the KnownNvlx (2373) and the same # of NotNflx
##This Should contain 4746 observations of 50/50 data
##pull sample from all KnownNflx containing 2/3 of the samples
Sample1.NFX.Bal <- sample(1:nrow(KnownNflx),nrow(KnownNflx)*(2/3)) 
##pull sample from all NotNflx containing the same number as the Sample1
Sample2.NFX.Bal <- sample(1:nrow(NotNflx), nrow(KnownNflx)*(2/3))  


Train.NFX.Bal<- rbind(NotNflx[Sample2.NFX.Bal,], KnownNflx[Sample1.NFX.Bal,])   #2/3 of the balanced data 
Test.NFX.Bal <- rbind(NotNflx[-Sample2.NFX.Bal,] , KnownNflx[-Sample1.NFX.Bal,])  #the rest of the data

Ensemble.Bal <- Test.NFX.Bal

#*#*#*#  Need to figure out how to make the test data not include the 

# Full Grown Decision Tree for Netflix
#Fit.NFX.Bal <- rpart(Netflix ~ Title + Year + Age + IMDb + Rotten.Tomatoes + Hulu + Prime.Video + Disney. + Type + Directors + Genres + Country + Language + Runtime, #Original Data Labels
#Fit.NFX.Bal <- rpart(Netflix ~ Year + Age + IMDb + Rotten.Tomatoes + Hulu + Prime.Video + Disney. + Type + NumDirectors + NumGenres + NumCountries + NumLanguages + Runtime , #Converted Data, Just English and NumLanguages
Fit.NFX.Bal <- rpart(Test.NFX.Variables,
                     data = Train.NFX.Bal,
                     method = "class",
                     control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                     parms= list(split="gini"))
#Fit.NFX.Bal
#nrow(Fit.NFX.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Netflix using the unpruned decision tree and compare it to the actual Netflix value
Pred.NFX.Bal <- predict(Fit.NFX.Bal, Test.NFX.Bal, type="class")
Actual.NFX.Bal <- Test.NFX.Bal$Netflix

CM.NFX.Bal <- table(Pred.NFX.Bal, Actual.NFX.Bal)
#addmargins(CM.NFX.Bal)
Post.CM.NFX.Bal<-confusionMatrix(CM.NFX.Bal,positive = '1')
#Post.CM.NFX.Bal

# POST-PRUNE The decision tree using the best CP
BestCPFit.NFX.Bal <- Fit.NFX.Bal$cptable[which.min(Fit.NFX.Bal$cptable[,"xerror"]),"CP"]
Post.Fit.NFX.Bal <- prune.rpart(Fit.NFX.Bal, cp=BestCPFit.NFX.Bal)
#nrow(Post.Fit.NFX.Bal$frame) #Perspective for how much smaller the post-pruned decision tree is

# Plot the Post- Pruned Tree - NOTE. same as pre-pruned tree fit.small2 
#prp(Post.Fit.NFX.Bal, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
#    main="Post-prune Tree with best cp")  
#rpart.plot(Post.Fit.NFX.Bal, type = 1, extra = 1, main="Classification Tree for Netflix Prediction w/ Post-Prune")
#rpart.plot(Post.Fit.NFX.Bal, type = 1, extra = 4, main="Classification Tree (%) for Netflix Prediction w/ Post-Prune")

# Compute the confusion matrices of the post-pruned predictions
Post.CM.NFX.Baltrain <- confusionMatrix(table(predict(Post.Fit.NFX.Bal, Train.NFX.Bal, type="class"),
                                              Train.NFX.Bal$Netflix), positive='1')

Post.CM.NFX.Baltest <-  confusionMatrix(table(predict(Post.Fit.NFX.Bal, Test.NFX.Bal, type="class"),
                                              Test.NFX.Bal$Netflix), positive='1')
#Post.CM.NFX.Baltrain
#Post.CM.NFX.Baltest

Ensemble.Bal$Predict.PP.NFX.Bal    <- predict(Post.Fit.NFX.Bal, Test.NFX.Bal, type="class") #Prediction of Balanced Netflix Postpruned Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.Bal<-Train.NFX.Bal 
Ensemble2.Bal$Predict.PP.NFX.Bal <- as.factor(predict(Post.Fit.NFX.Bal,Train.NFX.Bal, type = "class"))

# Create Decision tree for Netflix using Bagging
#Bag.Fit.NFX.Bal <- bagging(Netflix ~ Year + Age + IMDb + Rotten.Tomatoes + Type + NumGenres + NumCountries + NumLanguages + Runtime + English + Spanish + Mandarin + Cantonese + Tagalog + Vietnamese + Arabic + French + Korean + Russian + German,   #Converted Data, Top 10 Languages in America          
Bag.Fit.NFX.Bal <- bagging(Test.NFX.Variables,
                           data = Train.NFX.Bal,
                           mfinal = MFinalBag)
Pred.Bag.NFX.Bal <- predict(Bag.Fit.NFX.Bal, Test.NFX.Bal, type = "class")
# Create Confusion Matrix for test prediction
CM.Bag.NFX.Bal <- confusionMatrix(as.factor(Pred.Bag.NFX.Bal$class), Test.NFX.Bal$Netflix,positive='1')
#CM.Bag.NFX.Bal

Ensemble.Bal$Predict.Bag.NFX.Bal   <- as.factor(Pred.Bag.NFX.Bal$class) #Prediction of Balanced Netflix Bagged Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.NFX.Bal.P1<-predict(Bag.Fit.NFX.Bal,Train.NFX.Bal, type = "class")
Ensemble2.Bal$Predict.Bag.NFX.Bal<-as.factor(Ensemble2.NFX.Bal.P1$class)

# Create Decision tree for Netflix using Boosting
Boost.Fit.NFX.Bal <- boosting(Test.NFX.Variables,  
                              data = Train.NFX.Bal,
                              mfinal = MFinalBoost)
Pred.Boost.NFX.Bal <- predict(Boost.Fit.NFX.Bal, Test.NFX.Bal, type = "class")
# Create Confusion Matrix for test prediction
CM.Boost.NFX.Bal <- confusionMatrix(as.factor(Pred.Boost.NFX.Bal$class), Test.NFX.Bal$Netflix,positive='1')
#CM.Boost.NFX.Bal

Ensemble.Bal$Predict.Boost.NFX.Bal <- as.factor(Pred.Boost.NFX.Bal$class) #Prediction of Balanced Netflix Boosted Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.NFX.Bal.P2<-predict(Boost.Fit.NFX.Bal,Train.NFX.Bal, type = "class")
Ensemble2.Bal$Predict.Boost.NFX.Bal<-as.factor(Ensemble2.NFX.Bal.P2$class)

#***********************************************************************************************************************************  
#### CREATE A PREDICITION USING LOGISTIC REGRESSION ----  
#********************************************

Logit.Train.NFX.Bal <- Train.NFX.Bal
#Logit.NFX.Bal <- glm(Netflix ~ Year + Age + IMDb + Rotten.Tomatoes + Type + NumGenres + NumCountries + NumLanguages + Runtime + English + Spanish + Mandarin + Cantonese + Tagalog + Vietnamese + Arabic + French + Korean + Russian + German,   #Converted Data, Top 10 Languages in America 
#Logit.NFX.Bal <- glm(Netflix ~ Year + Age + IMDb + Type + NumGenres + NumCountries + NumLanguages + English + Spanish + Mandarin + Cantonese + Tagalog + Vietnamese + Arabic + French + Korean + Russian + German,                          
Logit.NFX.Bal <- glm(Test.NFX.Variables,
                    data = Logit.Train.NFX.Bal,
                    family = "binomial")
#summary(Logit.NFX.Bal)
Logit.Test.NFX.Bal <- Test.NFX.Bal
#summary(Logit.Test.NFX.Bal)

Logit.Test.NFX.Bal$Reg.Predict <- predict(Logit.NFX.Bal, Logit.Test.NFX.Bal, type = "response")

#CAN WE PLAY WITH THE FIRST OF THE THREE NUMBERS BELOW TO SEE WHAT VALUE GIVES US THE BEST RESULTS?
Logit.Test.NFX.Bal$NFX.Predict <- ifelse(Logit.Test.NFX.Bal$Reg.Predict > 0.5, 1 ,0)
CM.Logit.NFX.Bal <- confusionMatrix(as.factor(Logit.Test.NFX.Bal$NFX.Predict), Logit.Test.NFX.Bal$Netflix,positive='1')
#CM.Logit.NFX.Bal

Ensemble.Bal$Predict.Log.NFX.Bal <- as.factor(Logit.Test.NFX.Bal$NFX.Predict)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.NFX.Bal.P3<-predict(Logit.NFX.Bal, Logit.Train.NFX.Bal, type = "response")
Ensemble2.NFX.Bal.P3$class <- ifelse(Ensemble2.NFX.Bal.P3 > 0.5, 1 ,0)
Ensemble2.Bal$Predict.Log.NFX.Bal<-as.factor(Ensemble2.NFX.Bal.P3$class)

#***********************************************************************************************************************************
#### CREATE A PREDICITION USING NAIVE BAYES CLASSIFIER----  
#***********************************************
#Run with Balanced Training Set

NBayes.Train.NFX.Bal <- Train.NFX.Bal
NBayes.Test.NFX.Bal <- Test.NFX.Bal

# Run Naive Bayes Prediction for Raw and Class Probability
NBayes.NFX.Bal <- naiveBayes(Test.NFX.Variables,  
                            data = NBayes.Train.NFX.Bal,
                            na.action=na.pass)
NBayes.Test.NFX.Bal.Predict.Raw <- predict(NBayes.NFX.Bal, NBayes.Test.NFX.Bal, type = "raw")
NBayes.Test.NFX.Bal.Predict.Class <- predict(NBayes.NFX.Bal, NBayes.Test.NFX.Bal, type = "class")


#Generate Confusion Matrix 
CM.NBayes.NFX.Bal <- confusionMatrix(NBayes.Test.NFX.Bal.Predict.Class, NBayes.Test.NFX.Bal$Netflix, positive="1")
#CM.NBayes.NFX.Bal

Ensemble.Bal$Predict.NB.NFX.Bal <- as.factor(NBayes.Test.NFX.Bal.Predict.Class)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.NFX.Bal.P4<-predict(NBayes.NFX.Bal, NBayes.Train.NFX.Bal, type = "class")
Ensemble2.Bal$Predict.NB.NFX.Bal<-Ensemble2.NFX.Bal.P4

#***********************************************************************************************************************************  
#### USE ALL OF YOUR CLASSIFIERS TO CREATE AN ENSEMBLE CLASSIFIER---- 

Ensemble.Bal$Predict.Ensemble.NFX.Bal <- as.factor(round(((
    as.integer(Ensemble.Bal$Predict.PP.NFX.Bal)+
    as.integer(Ensemble.Bal$Predict.Bag.NFX.Bal)+
    as.integer(Ensemble.Bal$Predict.Boost.NFX.Bal)+
    as.integer(Ensemble.Bal$Predict.Log.NFX.Bal)+
    as.integer(Ensemble.Bal$Predict.NB.NFX.Bal))/10),0))   #Divide by 10 since as.integer converts 1 to 2

CM.Ensemble.Bal <- confusionMatrix(Ensemble.Bal$Predict.Ensemble.NFX.Bal, Ensemble.Bal$Netflix, positive ="1")
#CM.Ensemble.Bal



#********************************************************************************************************************
####Decision Tree Ensemble----
#Balanced Version
Ensemble2.DT.NFX.Bal <- rpart(Netflix ~ Predict.PP.NFX.Bal+Predict.Bag.NFX.Bal+Predict.Boost.NFX.Bal+Predict.Log.NFX.Bal+Predict.NB.NFX.Bal,
                             data = Ensemble2.Bal,
                             method = "class",
                             control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                             parms= list(split="gini"))

#nrow(Ensemble2.DT.NFX.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Netflix using the unpruned decision tree on the different predicted methods and compare it to the actual Netflix value
Ensemble2.Predict.DT.NFX.Bal <-predict(Ensemble2.DT.NFX.Bal, Ensemble.Bal, type="class")
Ensemble2.Actual.NFX.Bal <- Test.NFX.Bal$Netflix


CM.Ensemble2.DT.NFX.Bal <- confusionMatrix(Ensemble2.Predict.DT.NFX.Bal, Ensemble2.Actual.NFX.Bal,positive="1")
#CM.Ensemble2.DT.NFX.Bal #decision tree ensemble 
#CM.Ensemble.Bal #for comparison to the average ensemble

#********************************************************************************************************************
####Bagged Decision Tree Ensemble----
#Balanced Version
Ensemble2.Bagged.NFX.Bal <- bagging(Netflix ~ Predict.PP.NFX.Bal+Predict.Bag.NFX.Bal+Predict.Boost.NFX.Bal+Predict.Log.NFX.Bal+Predict.NB.NFX.Bal,
                                   data = Ensemble2.Bal,
                                   mfinal = MFinalBag)

Ensemble2.Predict.Bag.NFX.Bal <- predict(Ensemble2.Bagged.NFX.Bal, Ensemble.Bal, type = "class")

CM.Ensemble2.Bag.NFX.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Bag.NFX.Bal$class), Ensemble2.Actual.NFX.Bal,positive="1")
#CM.Ensemble2.Bag.NFX.Bal 

#********************************************************************************************************************
####Boosted Decision Tree Ensemble----
#Balanced Version
Ensemble2.Boost.NFX.Bal <- boosting(Netflix ~ Predict.PP.NFX.Bal+Predict.Bag.NFX.Bal+Predict.Boost.NFX.Bal+Predict.Log.NFX.Bal+Predict.NB.NFX.Bal,
                                   data = Ensemble2.Bal,
                                   mfinal = MFinalBoost)
Ensemble2.Predict.Boost.NFX.Bal <- predict(Ensemble2.Boost.NFX.Bal, Ensemble.Bal, type = "class")

CM.Ensemble2.Boost.NFX.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Boost.NFX.Bal$class), Ensemble2.Actual.NFX.Bal,positive="1")
#CM.Ensemble2.Boost.NFX.Bal 

#********************************************************************************************************************
####Logistic Regression Ensemble----
#Balanced Version
Ensemble2.Logit.NFX.Bal <- glm(Netflix ~ Predict.PP.NFX.Bal+Predict.Bag.NFX.Bal+Predict.Boost.NFX.Bal+Predict.Log.NFX.Bal+Predict.NB.NFX.Bal,
                              data = Ensemble2.Bal,
                              family = "binomial")

Ensemble2.Predict.Reg.Logit.NFX.Bal <-predict(Ensemble2.Logit.NFX.Bal, Ensemble.Bal, type="response")
Ensemble2.Predict.Logit.NFX.Bal <- ifelse(Ensemble2.Predict.Reg.Logit.NFX.Bal >.5,1,0)

CM.Ensemble2.Logit.NFX.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Logit.NFX.Bal), Ensemble2.Actual.NFX.Bal,positive="1")
#CM.Ensemble2.Logit.NFX.Bal  

#********************************************************************************************************************
####Naive Bayes Ensemble----
#Balanced Version
Ensemble2.NB.NFX.Bal <- naiveBayes(Netflix ~ Predict.PP.NFX.Bal+Predict.Bag.NFX.Bal+Predict.Boost.NFX.Bal+Predict.Log.NFX.Bal+Predict.NB.NFX.Bal,
                                  data = Ensemble2.Bal)

Ensemble2.NB.RAW.NFX.Bal<-predict(Ensemble2.NB.NFX.Bal, Ensemble.Bal, type="raw")
Ensemble2.NB.NFX.Bal<-predict(Ensemble2.NB.NFX.Bal, Ensemble.Bal, type="class")

CM.Ensemble2.NB.NFX.Bal <- confusionMatrix(Ensemble2.NB.NFX.Bal, Ensemble2.Actual.NFX.Bal,positive="1")
#CM.Ensemble2.NB.NFX.Bal 


#### SHOW ALL CONFUSION MATRIX FOR NFX ####
Post.CM.NFX.Bal
Post.CM.NFX.Baltrain
Post.CM.NFX.Baltest
CM.Bag.NFX.Bal
CM.Boost.NFX.Bal
CM.Logit.NFX.Bal
CM.NBayes.NFX.Bal
CM.Ensemble.Bal
CM.Ensemble2.DT.NFX.Bal
CM.Ensemble2.Bag.NFX.Bal
CM.Ensemble2.Boost.NFX.Bal
CM.Ensemble2.Logit.NFX.Bal 
CM.Ensemble2.NB.NFX.Bal 


AllCM.NFX <- matrix(NA,nrow=12,ncol=7)
colnames(AllCM.NFX)=c('Confusion Matrix','Accuracy','Sensitivity','Specificity','Balanced Accuracy','Pos Pred Value', 'Neg Pred Value')
AllCM.NFX[1,]   <- c("Post.CM.NFX.Bal",Post.CM.NFX.Bal$overall[1],Post.CM.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[2,]   <- c("Post.CM.NFX.Baltest",Post.CM.NFX.Baltest$overall[1],Post.CM.NFX.Baltest$byClass[c(1,2,11,3,4)])
AllCM.NFX[3,]   <- c("CM.Bag.NFX.Bal",CM.Bag.NFX.Bal$overall[1],CM.Bag.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[4,]   <- c("CM.Boost.NFX.Bal",CM.Boost.NFX.Bal$overall[1],CM.Boost.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[5,]   <- c("CM.Logit.NFX.Bal",CM.Logit.NFX.Bal$overall[1],CM.Logit.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[6,]   <- c("CM.NBayes.NFX.Bal",CM.NBayes.NFX.Bal$overall[1],CM.NBayes.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[7,]   <- c("CM.Ensemble.Bal",CM.Ensemble.Bal$overall[1],CM.Ensemble.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[8,]   <- c("CM.Ensemble2.DT.NFX.Bal",CM.Ensemble2.DT.NFX.Bal$overall[1],CM.Ensemble2.DT.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[9,]   <- c("CM.Ensemble2.Bag.NFX.Bal",CM.Ensemble2.Bag.NFX.Bal$overall[1],CM.Ensemble2.Bag.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[10,]   <- c("CM.Ensemble2.Boost.NFX.Bal",CM.Ensemble2.Boost.NFX.Bal$overall[1],CM.Ensemble2.Boost.NFX.Bal$byClass[c(1,2,11,3,4)])
AllCM.NFX[11,]   <- c("CM.Ensemble2.Logit.NFX.Bal ",CM.Ensemble2.Logit.NFX.Bal $overall[1],CM.Ensemble2.Logit.NFX.Bal $byClass[c(1,2,11,3,4)])
AllCM.NFX[12,]   <- c("CM.Ensemble2.NB.NFX.Bal ",CM.Ensemble2.NB.NFX.Bal $overall[1],CM.Ensemble2.NB.NFX.Bal $byClass[c(1,2,11,3,4)])

Test.NFX.Variables
AllCM.NFX


#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
####  Different approach to Starts here HULU PREDICTION SECTION STARTS HERE ####
#WRITE CODE HERE
set.seed        (8675309) # for reproducible results
XValue        <- 10
MinSplitValue <- 10
CPValue       <- 0
MFinalBag     <- 2
MFinalBoost   <- 40

Test.HLU.Variables <- (Hulu ~ Year + Age + IMDb + Rotten.Tomatoes + Type + NumGenres + NumCountries + NumLanguages + Runtime + English + Spanish + Mandarin + Cantonese + Tagalog + Vietnamese + Arabic + French + Korean + Russian + German)
#Test.HLU.Variables <- (Hulu ~ Year + Age + IMDb + Rotten.Tomatoes + Type + NumGenres + NumLanguages + Runtime + English )
#Test.HLU.Variables <- (Hulu ~ Year + Age + Type + NumGenres + NumLanguages + Runtime + English )

#***********************************************************************************************************************************  
#### CREATE A BALANCED TEST DATA DECISION TREE ----  

Sample1.HLU.Bal <- sample(1:nrow(KnownHulu),nrow(KnownHulu)*(2/3)) 
Sample2.HLU.Bal <- sample(1:nrow(NotHulu), nrow(KnownHulu)*(2/3))  

Train.HLU.Bal<- rbind(NotHulu[Sample2.HLU.Bal,], KnownHulu[Sample1.HLU.Bal,])   #2/3 of the balanced data 
Test.HLU.Bal <- rbind(NotHulu[-Sample2.HLU.Bal,] , KnownHulu[-Sample1.HLU.Bal,])  #the rest of the data

Ensemble.Bal <- Test.HLU.Bal

Fit.HLU.Bal <- rpart(Test.HLU.Variables,
                     data = Train.HLU.Bal,
                     method = "class",
                     control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                     parms= list(split="gini"))

#nrow(Fit.HLU.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Hulu using the unpruned decision tree and compare it to the actual Hulu value
Pred.HLU.Bal <- predict(Fit.HLU.Bal, Test.HLU.Bal, type="class")
Actual.HLU.Bal <- Test.HLU.Bal$Hulu

CM.HLU.Bal <- table(Pred.HLU.Bal, Actual.HLU.Bal)
#addmargins(CM.HLU.Bal)
Post.CM.HLU.Bal<-confusionMatrix(CM.HLU.Bal,positive = '1')
#Post.CM.HLU.Bal

# POST-PRUNE The decision tree using the best CP
BestCPFit.HLU.Bal <- Fit.HLU.Bal$cptable[which.min(Fit.HLU.Bal$cptable[,"xerror"]),"CP"]
Post.Fit.HLU.Bal <- prune.rpart(Fit.HLU.Bal, cp=BestCPFit.HLU.Bal)
#nrow(Post.Fit.HLU.Bal$frame) #Perspective for how much smaller the post-pruned decision tree is

# Compute the confusion matrices of the post-pruned predictions
Post.CM.HLU.Baltrain <- confusionMatrix(table(predict(Post.Fit.HLU.Bal, Train.HLU.Bal, type="class"),
                                              Train.HLU.Bal$Hulu), positive='1')

Post.CM.HLU.Baltest <-  confusionMatrix(table(predict(Post.Fit.HLU.Bal, Test.HLU.Bal, type="class"),
                                              Test.HLU.Bal$Hulu), positive='1')
#Post.CM.HLU.Baltrain
#Post.CM.HLU.Baltest

Ensemble.Bal$Predict.PP.HLU.Bal    <- predict(Post.Fit.HLU.Bal, Test.HLU.Bal, type="class") #Prediction of Balanced Hulu Postpruned Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.Bal<-Train.HLU.Bal 
Ensemble2.Bal$Predict.PP.HLU.Bal <- as.factor(predict(Post.Fit.HLU.Bal,Train.HLU.Bal, type = "class"))

#*#*#*#*#*#
# Create Decision tree for Hulu using Bagging
Bag.Fit.HLU.Bal <- bagging(Test.HLU.Variables,
                           data = Train.HLU.Bal,
                           mfinal = MFinalBag)
Pred.Bag.HLU.Bal <- predict(Bag.Fit.HLU.Bal, Test.HLU.Bal, type = "class")
# Create Confusion Matrix for test prediction
CM.Bag.HLU.Bal <- confusionMatrix(as.factor(Pred.Bag.HLU.Bal$class), Test.HLU.Bal$Hulu,positive='1')
#CM.Bag.HLU.Bal

Ensemble.Bal$Predict.Bag.HLU.Bal   <- as.factor(Pred.Bag.HLU.Bal$class) #Prediction of Balanced Hulu Bagged Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.HLU.Bal.P1<-predict(Bag.Fit.HLU.Bal,Train.HLU.Bal, type = "class")
Ensemble2.Bal$Predict.Bag.HLU.Bal<-as.factor(Ensemble2.HLU.Bal.P1$class)

# Create Decision tree for Hulu using Boosting
Boost.Fit.HLU.Bal <- boosting(Test.HLU.Variables,
                              data = Train.HLU.Bal,
                              mfinal = MFinalBoost)
Pred.Boost.HLU.Bal <- predict(Boost.Fit.HLU.Bal, Test.HLU.Bal, type = "class")

# Create Confusion Matrix for test prediction
CM.Boost.HLU.Bal <- confusionMatrix(as.factor(Pred.Boost.HLU.Bal$class), Test.HLU.Bal$Hulu,positive='1')
#CM.Boost.HLU.Bal

Ensemble.Bal$Predict.Boost.HLU.Bal <- as.factor(Pred.Boost.HLU.Bal$class) #Prediction of Balanced Hulu Boosted Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.HLU.Bal.P2<-predict(Boost.Fit.HLU.Bal,Train.HLU.Bal, type = "class")
Ensemble2.Bal$Predict.Boost.HLU.Bal<-as.factor(Ensemble2.HLU.Bal.P2$class)
#*#*#*#*#*#


#***********************************************************************************************************************************  
#### CREATE A PREDICITION USING LOGISTIC REGRESSION ----  

Logit.Train.HLU.Bal <- Train.HLU.Bal

Logit.HLU.Bal <- glm(Test.HLU.Variables,
                     data = Logit.Train.HLU.Bal,
                     family = "binomial")
#summary(Logit.HLU.Bal)
Logit.Test.HLU.Bal <- Test.HLU.Bal
#summary(Logit.Test.HLU.Bal)

Logit.Test.HLU.Bal$Reg.Predict <- predict(Logit.HLU.Bal, Logit.Test.HLU.Bal, type = "response")
Logit.Test.HLU.Bal$HLU.Predict <- ifelse(Logit.Test.HLU.Bal$Reg.Predict > 0.5, 1 ,0)

CM.Logit.HLU.Bal <- confusionMatrix(as.factor(Logit.Test.HLU.Bal$HLU.Predict), Logit.Test.HLU.Bal$Hulu,positive='1')
#CM.Logit.HLU.Bal

Ensemble.Bal$Predict.Log.HLU.Bal <- as.factor(Logit.Test.HLU.Bal$HLU.Predict)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.HLU.Bal.P3<-predict(Logit.HLU.Bal, Logit.Train.HLU.Bal, type = "response")
Ensemble2.HLU.Bal.P3$class <- ifelse(Ensemble2.HLU.Bal.P3 > 0.5, 1 ,0)
Ensemble2.Bal$Predict.Log.HLU.Bal<-as.factor(Ensemble2.HLU.Bal.P3$class)

#***********************************************************************************************************************************  
#### CREATE A PREDICITION USING NAIVE BAYES CLASSIFIER----  
#***********************************************
#Run with Balanced Training Set

NBayes.Train.HLU.Bal <- Train.HLU.Bal
NBayes.Test.HLU.Bal <- Test.HLU.Bal

NBayes.HLU.Bal <- naiveBayes(Test.HLU.Variables,
                             data = NBayes.Train.HLU.Bal,
                             na.action=na.pass)
NBayes.Test.HLU.Bal.Predict.Raw <- predict(NBayes.HLU.Bal, NBayes.Test.HLU.Bal, type = "raw")
NBayes.Test.HLU.Bal.Predict.Class <- predict(NBayes.HLU.Bal, NBayes.Test.HLU.Bal, type = "class")


#Generate Confusion Matrix 
CM.NBayes.HLU.Bal <- confusionMatrix(NBayes.Test.HLU.Bal.Predict.Class, NBayes.Test.HLU.Bal$Hulu, positive="1")
#CM.NBayes.HLU.Bal

Ensemble.Bal$Predict.NB.HLU.Bal <- as.factor(NBayes.Test.HLU.Bal.Predict.Class)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.HLU.Bal.P4<-predict(NBayes.HLU.Bal, NBayes.Train.HLU.Bal, type = "class")
Ensemble2.Bal$Predict.NB.HLU.Bal<-Ensemble2.HLU.Bal.P4

#***********************************************************************************************************************************  
#### USE ALL OF YOUR CLASSIFIERS TO CREATE AN ENSEMBLE CLASSIFIER---- 

Ensemble.Bal$Predict.Ensemble.HLU.Bal <- as.factor(round(((
  as.integer(Ensemble.Bal$Predict.PP.HLU.Bal)+
    as.integer(Ensemble.Bal$Predict.Log.HLU.Bal)+
    as.integer(Ensemble.Bal$Predict.NB.HLU.Bal))/10),0))   #Divide by 10 since as.integer converts 1 to 2

CM.Ensemble.Bal <- confusionMatrix(Ensemble.Bal$Predict.Ensemble.HLU.Bal, Ensemble.Bal$Hulu, positive ="1")
#CM.Ensemble.Bal



#********************************************************************************************************************
####Decision Tree Ensemble----

#Balanced Version
Ensemble2.DT.HLU.Bal <- rpart(Hulu ~ Predict.PP.HLU.Bal+Predict.Log.HLU.Bal+Predict.NB.HLU.Bal,
                              data = Ensemble2.Bal,
                              method = "class",
                              control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                              parms= list(split="gini"))

#nrow(Ensemble2.DT.HLU.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Hulu using the unpruned decision tree on the different predicted methods and compare it to the actual Hulu value
Ensemble2.Predict.DT.HLU.Bal <-predict(Ensemble2.DT.HLU.Bal, Ensemble.Bal, type="class")
Ensemble2.Actual.HLU.Bal <- Test.HLU.Bal$Hulu


CM.Ensemble2.DT.HLU.Bal <- confusionMatrix(Ensemble2.Predict.DT.HLU.Bal, Ensemble2.Actual.HLU.Bal,positive="1")
#CM.Ensemble2.DT.HLU.Bal #decision tree ensemble 
#CM.Ensemble.Bal #for comparison to the average ensemble

#********************************************************************************************************************


#********************************************************************************************************************
####Logistic Regression Ensemble----

#Balanced Version
Ensemble2.Logit.HLU.Bal <- glm(Hulu ~ Predict.PP.HLU.Bal+Predict.Log.HLU.Bal+Predict.NB.HLU.Bal,
                               data = Ensemble2.Bal,
                               family = "binomial")

Ensemble2.Predict.Reg.Logit.HLU.Bal <-predict(Ensemble2.Logit.HLU.Bal, Ensemble.Bal, type="response")
Ensemble2.Predict.Logit.HLU.Bal <- ifelse(Ensemble2.Predict.Reg.Logit.HLU.Bal >.5,1,0)

CM.Ensemble2.Logit.HLU.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Logit.HLU.Bal), Ensemble2.Actual.HLU.Bal,positive="1")
#CM.Ensemble2.Logit.HLU.Bal  

#********************************************************************************************************************
####Naive Bayes Ensemble----

#Balanced Version
Ensemble2.NB.HLU.Bal <- naiveBayes(Hulu ~ Predict.PP.HLU.Bal+Predict.Log.HLU.Bal+Predict.NB.HLU.Bal,
                                   data = Ensemble2.Bal)

Ensemble2.NB.RAW.HLU.Bal<-predict(Ensemble2.NB.HLU.Bal, Ensemble.Bal, type="raw")
Ensemble2.NB.HLU.Bal<-predict(Ensemble2.NB.HLU.Bal, Ensemble.Bal, type="class")

CM.Ensemble2.NB.HLU.Bal <- confusionMatrix(Ensemble2.NB.HLU.Bal, Ensemble2.Actual.HLU.Bal,positive="1")
#CM.Ensemble2.NB.HLU.Bal 


#### SHOW ALL CONFUSION MATRIX FOR HLU ####

Post.CM.HLU.Bal
Post.CM.HLU.Baltrain
Post.CM.HLU.Baltest
CM.Logit.HLU.Bal
CM.NBayes.HLU.Bal
CM.Ensemble.Bal
CM.Ensemble2.DT.HLU.Bal
CM.Ensemble2.Logit.HLU.Bal 
CM.Ensemble2.NB.HLU.Bal 


AllCM.HLU <- matrix(NA,nrow=10,ncol=7)
colnames(AllCM.HLU)=c('Confusion Matrix','Accuracy','Sensitivity','Specificity','Balanced Accuracy','Pos Pred Value', 'Neg Pred Value')
AllCM.HLU[1,]   <- c("Post.CM.HLU.Bal",Post.CM.HLU.Bal$overall[1],Post.CM.HLU.Bal$byClass[c(1,2,11,3,4)])
AllCM.HLU[2,]   <- c("Post.CM.HLU.Baltest",Post.CM.HLU.Baltest$overall[1],Post.CM.HLU.Baltest$byClass[c(1,2,11,3,4)])
AllCM.HLU[3,]   <- c("CM.Bag.HLU.Bal",CM.Bag.HLU.Bal$overall[1],CM.Bag.HLU.Bal$byClass[c(1,2,11,3,4)])
AllCM.HLU[4,]   <- c("CM.Boost.HLU.Bal",CM.Boost.HLU.Bal$overall[1],CM.Boost.HLU.Bal$byClass[c(1,2,11,3,4)])
AllCM.HLU[5,]   <- c("CM.Logit.HLU.Bal",CM.Logit.HLU.Bal$overall[1],CM.Logit.HLU.Bal$byClass[c(1,2,11,3,4)])
AllCM.HLU[6,]   <- c("CM.NBayes.HLU.Bal",CM.NBayes.HLU.Bal$overall[1],CM.NBayes.HLU.Bal$byClass[c(1,2,11,3,4)])
AllCM.HLU[7,]   <- c("CM.Ensemble.Bal",CM.Ensemble.Bal$overall[1],CM.Ensemble.Bal$byClass[c(1,2,11,3,4)])
AllCM.HLU[8,]   <- c("CM.Ensemble2.DT.HLU.Bal",CM.Ensemble2.DT.HLU.Bal$overall[1],CM.Ensemble2.DT.HLU.Bal$byClass[c(1,2,11,3,4)])
AllCM.HLU[9,]   <- c("CM.Ensemble2.Logit.HLU.Bal ",CM.Ensemble2.Logit.HLU.Bal $overall[1],CM.Ensemble2.Logit.HLU.Bal $byClass[c(1,2,11,3,4)])
AllCM.HLU[10,]   <- c("CM.Ensemble2.NB.HLU.Bal ",CM.Ensemble2.NB.HLU.Bal $overall[1],CM.Ensemble2.NB.HLU.Bal $byClass[c(1,2,11,3,4)])

Test.HLU.Variables
AllCM.HLU




#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
#******************************************************************
####  Different approach to Starts here Disney PREDICTION SECTION STARTS HERE ####
#WRITE CODE HERE
set.seed        (8675309) # for reproducible results
XValue        <- 10
MinSplitValue <- 10
CPValue       <- 0
MFinalBag     <- 2
MFinalBoost   <- 40


Test.DSY.Variables <- (Disney. ~ Year + Age + IMDb + Rotten.Tomatoes + Type + NumGenres + NumLanguages + Runtime + English )

#***********************************************************************************************************************************  
#### CREATE A BALANCED TEST DATA DECISION TREE ----  

Sample1.DSY.Bal <- sample(1:nrow(KnownDisney),nrow(KnownDisney)*(2/3)) 
Sample2.DSY.Bal <- sample(1:nrow(NotDisney), nrow(KnownDisney)*(2/3))  


Train.DSY.Bal<- rbind(NotDisney[Sample2.DSY.Bal,], KnownDisney[Sample1.DSY.Bal,])   #2/3 of the balanced data 
Test.DSY.Bal <- rbind(NotDisney[-Sample2.DSY.Bal,] , KnownDisney[-Sample1.DSY.Bal,])  #the rest of the data

Ensemble.Bal <- Test.DSY.Bal

Fit.DSY.Bal <- rpart(Test.DSY.Variables,
                     data = Train.DSY.Bal,
                     method = "class",
                     control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                     parms= list(split="gini"))
#Fit.DSY.Bal
#nrow(Fit.DSY.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Hulu using the unpruned decision tree and compare it to the actual Hulu value
Pred.DSY.Bal <- predict(Fit.DSY.Bal, Test.DSY.Bal, type="class")
Actual.DSY.Bal <- Test.DSY.Bal$Disney.

CM.DSY.Bal <- table(Pred.DSY.Bal, Actual.DSY.Bal)
#addmargins(CM.DSY.Bal)
Post.CM.DSY.Bal<-confusionMatrix(CM.DSY.Bal,positive = '1')
#Post.CM.DSY.Bal

# POST-PRUNE The decision tree using the best CP
BestCPFit.DSY.Bal <- Fit.DSY.Bal$cptable[which.min(Fit.DSY.Bal$cptable[,"xerror"]),"CP"]
Post.Fit.DSY.Bal <- prune.rpart(Fit.DSY.Bal, cp=BestCPFit.DSY.Bal)
#nrow(Post.Fit.DSY.Bal$frame) #Perspective for how much smaller the post-pruned decision tree is

# Compute the confusion matrices of the post-pruned predictions
Post.CM.DSY.Baltrain <- confusionMatrix(table(predict(Post.Fit.DSY.Bal, Train.DSY.Bal, type="class"),
                                              Train.DSY.Bal$Disney.), positive='1')

Post.CM.DSY.Baltest <-  confusionMatrix(table(predict(Post.Fit.DSY.Bal, Test.DSY.Bal, type="class"),
                                              Test.DSY.Bal$Disney.), positive='1')
#Post.CM.DSY.Baltrain
#Post.CM.DSY.Baltest

Ensemble.Bal$Predict.PP.DSY.Bal    <- predict(Post.Fit.DSY.Bal, Test.DSY.Bal, type="class") #Prediction of Balanced Hulu Postpruned Tree

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.Bal<-Train.DSY.Bal 
Ensemble2.Bal$Predict.PP.DSY.Bal <- as.factor(predict(Post.Fit.DSY.Bal,Train.DSY.Bal, type = "class"))


#***********************************************************************************************************************************  
#### CREATE A PREDICITION USING LOGISTIC REGRESSION ----  

Logit.Train.DSY.Bal <- Train.DSY.Bal

Logit.DSY.Bal <- glm(Test.DSY.Variables,
                     data = Logit.Train.DSY.Bal,
                     family = "binomial")
#summary(Logit.DSY.Bal)

Logit.Test.DSY.Bal <- Test.DSY.Bal
#summary(Logit.Test.DSY.Bal)

Logit.Test.DSY.Bal$Reg.Predict <- predict(Logit.DSY.Bal, Logit.Test.DSY.Bal, type = "response")
Logit.Test.DSY.Bal$DSY.Predict <- ifelse(Logit.Test.DSY.Bal$Reg.Predict > 0.5, 1 ,0)

CM.Logit.DSY.Bal <- confusionMatrix(as.factor(Logit.Test.DSY.Bal$DSY.Predict), Logit.Test.DSY.Bal$Disney.,positive='1')
#CM.Logit.DSY.Bal

Ensemble.Bal$Predict.Log.DSY.Bal <- as.factor(Logit.Test.DSY.Bal$DSY.Predict)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.DSY.Bal.P3<-predict(Logit.DSY.Bal, Logit.Train.DSY.Bal, type = "response")
Ensemble2.DSY.Bal.P3$class <- ifelse(Ensemble2.DSY.Bal.P3 > 0.5, 1 ,0)
Ensemble2.Bal$Predict.Log.DSY.Bal<-as.factor(Ensemble2.DSY.Bal.P3$class)

#***********************************************************************************************************************************  
#### CREATE A PREDICITION USING NAIVE BAYES CLASSIFIER----  
#***********************************************
#Run with Balanced Training Set
NBayes.Train.DSY.Bal <- Train.DSY.Bal
NBayes.Test.DSY.Bal <- Test.DSY.Bal

# Run Naive Bayes Prediction for Raw and Class Probability
NBayes.DSY.Bal <- naiveBayes(Test.DSY.Variables,
                             data = NBayes.Train.DSY.Bal,
                             na.action=na.pass)
NBayes.Test.DSY.Bal.Predict.Raw <- predict(NBayes.DSY.Bal, NBayes.Test.DSY.Bal, type = "raw")
NBayes.Test.DSY.Bal.Predict.Class <- predict(NBayes.DSY.Bal, NBayes.Test.DSY.Bal, type = "class")


#Generate Confusion Matrix 
CM.NBayes.DSY.Bal <- confusionMatrix(NBayes.Test.DSY.Bal.Predict.Class, NBayes.Test.DSY.Bal$Disney., positive="1")
#CM.NBayes.DSY.Bal

Ensemble.Bal$Predict.NB.DSY.Bal <- as.factor(NBayes.Test.DSY.Bal.Predict.Class)

#Store the predicted value for the training data set to be used in an ensemble
Ensemble2.DSY.Bal.P4<-predict(NBayes.DSY.Bal, NBayes.Train.DSY.Bal, type = "class")
Ensemble2.Bal$Predict.NB.DSY.Bal<-Ensemble2.DSY.Bal.P4

#***********************************************************************************************************************************  
#### USE ALL OF YOUR CLASSIFIERS TO CREATE AN ENSEMBLE CLASSIFIER---- 

Ensemble.Bal$Predict.Ensemble.DSY.Bal <- as.factor(round(((
  as.integer(Ensemble.Bal$Predict.PP.DSY.Bal)+
    as.integer(Ensemble.Bal$Predict.Log.DSY.Bal)+
    as.integer(Ensemble.Bal$Predict.NB.DSY.Bal))/10),0))   #Divide by 10 since as.integer converts 1 to 2

CM.Ensemble.Bal <- confusionMatrix(Ensemble.Bal$Predict.Ensemble.DSY.Bal, Ensemble.Bal$Disney., positive ="1")
#CM.Ensemble.Bal

#********************************************************************************************************************
####Decision Tree Ensemble----

#Balanced Version
Ensemble2.DT.DSY.Bal <- rpart(Disney. ~ Predict.PP.DSY.Bal+Predict.Log.DSY.Bal+Predict.NB.DSY.Bal,
                              data = Ensemble2.Bal,
                              method = "class",
                              control=rpart.control(xval=XValue, minsplit=MinSplitValue, cp=CPValue),
                              parms= list(split="gini"))

#nrow(Ensemble2.DT.DSY.Bal$frame) #Perspective for how big the unpruned decision tree is


# Create the prediction for Hulu using the unpruned decision tree on the different predicted methods and compare it to the actual Hulu value
Ensemble2.Predict.DT.DSY.Bal <-predict(Ensemble2.DT.DSY.Bal, Ensemble.Bal, type="class")
Ensemble2.Actual.DSY.Bal <- Test.DSY.Bal$Disney.


CM.Ensemble2.DT.DSY.Bal <- confusionMatrix(Ensemble2.Predict.DT.DSY.Bal, Ensemble2.Actual.DSY.Bal,positive="1")
#CM.Ensemble2.DT.DSY.Bal #decision tree ensemble 
#CM.Ensemble.Bal #for comparison to the average ensemble

#********************************************************************************************************************


#********************************************************************************************************************
####Logistic Regression Ensemble----

#Balanced Version
Ensemble2.Logit.DSY.Bal <- glm(Disney. ~ Predict.PP.DSY.Bal+Predict.Log.DSY.Bal+Predict.NB.DSY.Bal,
                               data = Ensemble2.Bal,
                               family = "binomial")

Ensemble2.Predict.Reg.Logit.DSY.Bal <-predict(Ensemble2.Logit.DSY.Bal, Ensemble.Bal, type="response")
Ensemble2.Predict.Logit.DSY.Bal <- ifelse(Ensemble2.Predict.Reg.Logit.DSY.Bal >.5,1,0)

CM.Ensemble2.Logit.DSY.Bal <- confusionMatrix(as.factor(Ensemble2.Predict.Logit.DSY.Bal), Ensemble2.Actual.DSY.Bal,positive="1")
#CM.Ensemble2.Logit.DSY.Bal  

#********************************************************************************************************************
####Naive Bayes Ensemble----

#Balanced Version
Ensemble2.NB.DSY.Bal <- naiveBayes(Disney. ~ Predict.PP.DSY.Bal+Predict.Log.DSY.Bal+Predict.NB.DSY.Bal,
                                   data = Ensemble2.Bal)

Ensemble2.NB.RAW.DSY.Bal<-predict(Ensemble2.NB.DSY.Bal, Ensemble.Bal, type="raw")
Ensemble2.NB.DSY.Bal<-predict(Ensemble2.NB.DSY.Bal, Ensemble.Bal, type="class")

CM.Ensemble2.NB.DSY.Bal <- confusionMatrix(Ensemble2.NB.DSY.Bal, Ensemble2.Actual.DSY.Bal,positive="1")
#CM.Ensemble2.NB.DSY.Bal 


#### SHOW ALL CONFUSION MATRIX FOR DSY ####

Post.CM.DSY.Bal
Post.CM.DSY.Baltrain
Post.CM.DSY.Baltest
CM.Logit.DSY.Bal
CM.NBayes.DSY.Bal
CM.Ensemble.Bal
CM.Ensemble2.DT.DSY.Bal
CM.Ensemble2.Logit.DSY.Bal 
CM.Ensemble2.NB.DSY.Bal 


AllCM.DSY <- matrix(NA,nrow=8,ncol=7)
colnames(AllCM.DSY)=c('Confusion Matrix','Accuracy','Sensitivity','Specificity','Balanced Accuracy','Pos Pred Value', 'Neg Pred Value')

AllCM.DSY[1,]   <- c("Post.CM.DSY.Bal",Post.CM.DSY.Bal$overall[1],Post.CM.DSY.Bal$byClass[c(1,2,11,3,4)])
AllCM.DSY[2,]   <- c("Post.CM.DSY.Baltest",Post.CM.DSY.Baltest$overall[1],Post.CM.DSY.Baltest$byClass[c(1,2,11,3,4)])
AllCM.DSY[3,]   <- c("CM.Logit.DSY.Bal",CM.Logit.DSY.Bal$overall[1],CM.Logit.DSY.Bal$byClass[c(1,2,11,3,4)])
AllCM.DSY[4,]   <- c("CM.NBayes.DSY.Bal",CM.NBayes.DSY.Bal$overall[1],CM.NBayes.DSY.Bal$byClass[c(1,2,11,3,4)])
AllCM.DSY[5,]   <- c("CM.Ensemble.Bal",CM.Ensemble.Bal$overall[1],CM.Ensemble.Bal$byClass[c(1,2,11,3,4)])
AllCM.DSY[6,]   <- c("CM.Ensemble2.DT.DSY.Bal",CM.Ensemble2.DT.DSY.Bal$overall[1],CM.Ensemble2.DT.DSY.Bal$byClass[c(1,2,11,3,4)])
AllCM.DSY[7,]   <- c("CM.Ensemble2.Logit.DSY.Bal ",CM.Ensemble2.Logit.DSY.Bal $overall[1],CM.Ensemble2.Logit.DSY.Bal $byClass[c(1,2,11,3,4)])
AllCM.DSY[8,]   <- c("CM.Ensemble2.NB.DSY.Bal ",CM.Ensemble2.NB.DSY.Bal $overall[1],CM.Ensemble2.NB.DSY.Bal $byClass[c(1,2,11,3,4)])

Test.DSY.Variables
AllCM.DSY






#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    THIS IS WHERE THE OUTSIDE THE BOX THINKING OCCURS    #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##                                                         #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    WE CONSIDER THE ABILITY TO UTILIZE OUR PREDICTIONS   #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    TO SEE IF WE CAN IMPROVE THE QUALITY OF OUR POOR     #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    CLASSIFICATIONS FOR NETFLIX, HULU, AND DISNEY        #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##                                                         #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    THE RULE WAS SIMPLE: IF THE MOVIE HAD PREVIOUSLY     #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    BEEN  PREDICTED, THEN A NEW PREDICTION WOULD BE      #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    IGNORED.  ORDER GOES PRIME THEN NETFLIX THEN         #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    DISNEY THEN HULU.  HULU DOES NOT IMPROVE WITH        #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    THIS TECHNIQUE BUT THE POS PRED VALUE FOR NETFLIX    #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    AND DISNEY BOTH IMPROVE SIGNIFICANTLY.               #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##                                                         #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    WE CHOOSE TO UTILIZE THE LOGIT FUNCTION FOR THIS     #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##    NEW TECHNIQUE BUT OTHERS COULD BE USED AS WELL.      #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##                                                         #*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#


#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
PDNotNFX <- Predicted.PRM[Predicted.PRM$Netflix == 0,]
PDKnownNFX <- Predicted.PRM[Predicted.PRM$Netflix == 1,]
PDSample1.NFX.Bal <- sample(1:nrow(PDKnownNFX),nrow(PDKnownNFX)*(2/3)) 
PDSample2.NFX.Bal <- sample(1:nrow(PDNotNFX), nrow(PDKnownNFX)*(2/3))  


PDTrain.NFX.Bal<- rbind(PDNotNFX[PDSample2.NFX.Bal,], PDKnownNFX[PDSample1.NFX.Bal,])   #2/3 of the balanced data 
PDTest.NFX.Bal <- rbind(PDNotNFX[-PDSample2.NFX.Bal,] , PDKnownNFX[-PDSample1.NFX.Bal,])  #the rest of the data

confusionMatrix(as.factor(Predicted.PRM$PRM.Predict), 
                Predicted.PRM$Prime.Video, positive ='1')

#*#*#*#*#*# USING PRIME Prediction and NFX Logit Prediction#*#*#*#*#*##*#*#*#*#*##*#*#
#Predicted.PRM$NFX.PredictLogit=0
Predicted.NFX <- predict(Logit.NFX.Bal, Predicted.PRM, type = "response")
Predicted.PRM$NFX.PredictLogit <- ifelse(Predicted.NFX>.4,1,0)
Predicted.PRM$NFX.PredictLogit[is.na(Predicted.PRM$NFX.PredictLogit)] = 0
Predicted.PRM$NFX.Predict <-ifelse (Predicted.PRM$PRM.Predict==1,0,Predicted.PRM$NFX.PredictLogit)

#Show Prime Prediction
confusionMatrix(as.factor(Predicted.PRM$NFX.Predict), 
                Predicted.PRM$Netflix,positive='1')
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#

#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#

NotDisney <- Predicted.PRM[Predicted.PRM$Disney. == 0,]
KnownDisney <- Predicted.PRM[Predicted.PRM$Disney. == 1,]
Sample1.DSY.Bal <- sample(1:nrow(KnownDisney),nrow(KnownDisney)*(2/3)) 
Sample2.DSY.Bal <- sample(1:nrow(NotDisney), nrow(KnownDisney)*(2/3))  


Train.DSY.Bal<- rbind(NotDisney[Sample2.DSY.Bal,], KnownDisney[Sample1.DSY.Bal,])   #2/3 of the balanced data 
Test.DSY.Bal <- rbind(NotDisney[-Sample2.DSY.Bal,] , KnownDisney[-Sample1.DSY.Bal,])  #the rest of the data

#*#*#*#*#*# USING PRIME Prediction and Disney Logit Prediction#*#*#*#*#*##*#*#*#*#*##*#*#
#Predicted.PRM$DSY.PredictLogit=0
Predicted.DSY <- predict(Logit.DSY.Bal, Predicted.PRM, type = "response")
Predicted.PRM$DSY.PredictLogit <- ifelse(Predicted.DSY>.4,1,0)
Predicted.PRM$DSY.PredictLogit[is.na(Predicted.PRM$DSY.PredictLogit)] = 0
Predicted.PRM$DSY.Predict <-ifelse (Predicted.PRM$PRM.Predict==1 ,0,Predicted.PRM$DSY.PredictLogit)
Predicted.PRM$DSY.Predict2 <-ifelse ((Predicted.PRM$PRM.Predict==1 | Predicted.PRM$NFX.Predict==1),0,Predicted.PRM$DSY.PredictLogit)


confusionMatrix(as.factor(Predicted.PRM$DSY.Predict), 
                Predicted.PRM$Disney.,positive='1')
confusionMatrix(as.factor(Predicted.PRM$DSY.Predict2), 
                Predicted.PRM$Disney.,positive='1')
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#

PDNotHLU <- Predicted.PRM[Predicted.PRM$Hulu == 0,]
PDKnownHLU <- Predicted.PRM[Predicted.PRM$Hulu == 1,]
PDSample1.HLU.Bal <- sample(1:nrow(PDKnownHLU),nrow(PDKnownHLU)*(2/3)) 
PDSample2.HLU.Bal <- sample(1:nrow(PDNotHLU), nrow(PDKnownHLU)*(2/3))  


PDTrain.HLU.Bal<- rbind(PDNotHLU[PDSample2.HLU.Bal,], PDKnownHLU[PDSample1.HLU.Bal,])   #2/3 of the balanced data 
PDTest.HLU.Bal <- rbind(PDNotHLU[-PDSample2.HLU.Bal,] , PDKnownHLU[-PDSample1.HLU.Bal,])  #the rest of the data

#*#*#*#*#*# USING PRIME Prediction and HLU Logit Prediction#*#*#*#*#*##*#*#*#*#*##*#*#
#Predicted.PRM$HLU.PredictLogit=0
Predicted.HLU <- predict(Logit.HLU.Bal, Predicted.PRM, type = "response")
Predicted.PRM$HLU.PredictLogit <- ifelse(Predicted.HLU>.4,1,0)
Predicted.PRM$HLU.PredictLogit[is.na(Predicted.PRM$HLU.PredictLogit)] = 0
Predicted.PRM$HLU.Predict <-ifelse (Predicted.PRM$PRM.Predict==1,0,Predicted.PRM$HLU.PredictLogit)
Predicted.PRM$HLU.Predict2 <-ifelse ((Predicted.PRM$PRM.Predict==1|Predicted.PRM$NFX.Predict==1),0,Predicted.PRM$HLU.PredictLogit)
Predicted.PRM$HLU.Predict3 <-ifelse ((Predicted.PRM$PRM.Predict==1|Predicted.PRM$NFX.Predict==1|Predicted.PRM$DSY.Predict==1),0,Predicted.PRM$HLU.PredictLogit)

confusionMatrix(as.factor(Predicted.PRM$HLU.Predict), 
                Predicted.PRM$Hulu,positive='1')
confusionMatrix(as.factor(Predicted.PRM$HLU.Predict2), 
                Predicted.PRM$Hulu,positive='1')
confusionMatrix(as.factor(Predicted.PRM$HLU.Predict3), 
                Predicted.PRM$Hulu,positive='1')
#*#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#v#*#*#*#*#*##*#*#*#*#*##*#*#*#*#*##*#*#*#*#*#
#Number of Movies that had no prediction made 
Predicted.PRM$NoPrediction <-ifelse ((Predicted.PRM$PRM.Predict==1|Predicted.PRM$NFX.Predict==1|Predicted.PRM$DSY.Predict==1|Predicted.PRM$HLU.Predict2==1),0,1)
sum(Predicted.PRM$NoPrediction)

#To get a feel for which platforms we were missing in the NoPredictions subset
NoPredictions <- Predicted.PRM[Predicted.PRM$NoPrediction==1,]
nrow(NoPredictions)
confusionMatrix(as.factor(NoPredictions$NoPrediction), 
                NoPredictions$Prime.Video,positive='1')
confusionMatrix(as.factor(NoPredictions$NoPrediction), 
                NoPredictions$Netflix,positive='1')
confusionMatrix(as.factor(NoPredictions$NoPrediction), 
                NoPredictions$Disney.,positive='1')
confusionMatrix(as.factor(NoPredictions$NoPrediction),
                NoPredictions$Hulu,positive='1')












#### Import Data ----
rm(list=ls())
cat("\014")

Movies <- read.csv("Movies.csv")


#### Libraries ----
#install.packages("tidyr")   REMOVE # FOR INITIAL INSTALL OF PACKAGE
#install.packages("stringr")
#install.packages("e1071")
library(tidyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(adabag)
library(caret)
library(e1071)


#### CONVERSION OF Movies DATA ----
#Duplicate Origianl Data

#Convert Columns to specific data types

#Convert columns to Factors  
Movies$Age         <- as.factor(Movies$Age)
Movies$Netflix     <- as.factor(Movies$Netflix)
Movies$Prime.Video <- as.factor(Movies$Prime.Video)
Movies$Disney.     <- as.factor(Movies$Disney.)
Movies$Hulu        <- as.factor(Movies$Hulu)

#Convert Rotten.Tomatoes from  string% to an intenger or NA for unrated
Movies$RTLength <- nchar(Movies$Rotten.Tomatoes)  #string length for Rotten Tomatoes % 
#Truncate % or store "NA" if no rating
for (i in 1:nrow(Movies)){
  if (Movies$RTLength[i] >0){
    Movies$Rotten.Tomatoes[i]<-substr(Movies$Rotten.Tomatoes[i],start = 1,stop = (Movies$RTLength[i]-1))}
  else {
    Movies$Rotten.Tomatoes[i] <- "NA"  }
}
#Convert to an integer
Movies$Rotten.Tomatoes <- as.integer(Movies$Rotten.Tomatoes)

#Create a count of the # of languages, directors, genres
Movies$NumLanguages  <- str_count(Movies$Language,",") + 1
Movies$NumDirectors  <- str_count(Movies$Directors,",") + 1
Movies$NumGenres     <- str_count(Movies$Genres,",") + 1
Movies$NumCountries  <- str_count(Movies$Country,",") + 1
#Create indication for 10 most popular languages in the USA and group all others
Movies$English       <- as.integer(str_detect(Movies$Language, "English"))
Movies$Spanish       <- as.integer(str_detect(Movies$Language, "Spanish"))
Movies$Mandarin      <- as.integer(str_detect(Movies$Language, "Mandarin"))
Movies$Cantonese     <- as.integer(str_detect(Movies$Language, "Cantonese"))
Movies$Tagalog       <- as.integer(str_detect(Movies$Language, "Tagalog"))
Movies$Vietnamese    <- as.integer(str_detect(Movies$Language, "Vietnamese"))
Movies$Arabic        <- as.integer(str_detect(Movies$Language, "Arabic"))
Movies$French        <- as.integer(str_detect(Movies$Language, "French"))
Movies$Korean        <- as.integer(str_detect(Movies$Language, "Korean"))
Movies$Russian       <- as.integer(str_detect(Movies$Language, "Russian"))
Movies$German        <- as.integer(str_detect(Movies$Language, "German"))
Movies$Other.Language<- (Movies$NumLanguages - Movies$English - Movies$Russian
                         -Movies$Cantonese    - Movies$Tagalog - Movies$French
                         -Movies$Vietnamese   - Movies$Spanish - Movies$Arabic 
                         -Movies$Mandarin     - Movies$Korean  - Movies$German)

#Create subsets for control value designation
KnownNflx   <- Movies[Movies$Netflix == 1,]
KnownPrime  <- Movies[Movies$Prime.Video == 1,]
KnownHulu   <- Movies[Movies$Hulu == 1,]
KnownDisney <- Movies[Movies$Disney. == 1,]
NotNflx   <- Movies[Movies$Netflix == 0,]
NotPrime  <- Movies[Movies$Prime.Video == 0,]
NotHulu   <- Movies[Movies$Hulu == 0,]
NotDisney <- Movies[Movies$Disney. == 0,]





#### Part two ----

# Shows that a majority of the data is unrated
a <- ggplot(Movies,aes(Age))
a + geom_histogram(binwidth = 5, stat = "count",color = "black", fill ="darkred")+ labs(title = "Most Movies are Unrated")
a + geom_density(kernel = "gaussian", fill = "darkred")

gIMDb <- ggplot(Movies, aes(Age, IMDb))
gRotten <- ggplot(Movies, aes(Age, Rotten.Tomatoes))

gIMDb + geom_jitter()+ labs(title = "Movies IMDb Distribution by Age")
gRotten + geom_jitter()+ labs(title = "Movies Rotten Tomatoes Distribution by Age")

#compared ratings given by rotten tomatoes to the ones given by IMDB and how in line they are with each other with ratings
Movies$z <- with(Movies, sqrt(IMDb^2 + Rotten.Tomatoes^2)) 
i1 <- ggplot(Movies, aes(IMDb, Rotten.Tomatoes))
i1 + geom_raster(aes(fill = z), hjust=0.5,vjust=0.5, interpolate=FALSE)+ labs(title = "IMDb and Rotten Tomatoes Relationship")
Movies$x <- with(Movies, Age) 
i2 <- ggplot(Movies, aes(IMDb, Rotten.Tomatoes))
i2 + geom_raster(aes(fill = x), hjust=0.5,vjust=0.5, interpolate=FALSE) + labs(title = "Age Groupings and Ratings")
vn <- ggplot(Movies, aes(Age, fill = Netflix))
vn + geom_bar(position = "stack", color = "red") + labs(title = "Overall Netflix Content in Age Brackets")
vn <- ggplot(Movies, aes(Age, fill = Hulu))
vn + geom_bar(position = "stack", color = "green") + labs(title = "Overall Hulu Content in Age Brackets")
vn <- ggplot(Movies, aes(Age, fill = Prime.Video))
vn + geom_bar(position = "stack", color = "yellow") + labs(title = "Overall Prime Content in Age Brackets")
vn <- ggplot(Movies, aes(Age, fill = Disney.))
vn + geom_bar(position = "stack", color = "lightblue") + labs(title = "Overall Disney Content in Age Brackets")
#ratings is tied to runtime and shows similarities between movies and series alike
Movies$Runtime <- as.integer(Movies$Runtime)
MoviesTest <- Movies[Movies$Runtime < 350,]
kn1 <- ggplot(MoviesTest,aes(Runtime, IMDb))
kn1 + geom_jitter()
kn1 + geom_smooth(model = lm) + labs(title = "IMDb Rating by Runtime")
kn1 <- ggplot(MoviesTest,aes(IMDb, Runtime))
kn1 + geom_jitter()
kn1 + geom_smooth(model = lm) + labs(title = "IMDb Rating by Runtime")
kn1 <- ggplot(MoviesTest,aes(Rotten.Tomatoes, Runtime))
kn1 + geom_jitter()
kn1 + geom_smooth(model = lm) + labs(title = "Rotten Tomatoes Rating by Runtime")
kn1 <- ggplot(MoviesTest,aes(Runtime, Rotten.Tomatoes))
kn1 + geom_jitter()
kn1 + geom_smooth(model = lm) + labs(title = "Rotten Tomatoes Rating by Runtime")

# Part 2: End User Recommendations by Genres ----
# clear current workspace and console
rm(list=ls())
cat("\014")

# load in the data file into data.frame
Movies <- read.csv("Movies.csv")


#Load Appropriate Libraries
library(tidyverse) 
library(tidyr)
library(stringr)
library(data.table) #kim added for mac
library(adabag) 
library(caret) 
library(e1071)
library(readr) 
library(dplyr)
library(ggplot2) 
library(sjmisc)

#Change Disney. to Disney
Movies <- Movies %>% rename(Disney = Disney.)

#Add genre columns to Movies df
Movies$Action       <-as.numeric(str_detect(Movies$Genre, "Action"))
Movies$Adventure       <- as.numeric(str_detect(Movies$Genre, "Adventure"))
Movies$SciFi       <- as.numeric(str_detect(Movies$Genre, "Sci-Fi"))
Movies$Thriller       <- as.numeric(str_detect(Movies$Genre, "Thriller"))
Movies$Comedy       <- as.numeric(str_detect(Movies$Genre, "Comedy"))
Movies$Western       <- as.numeric(str_detect(Movies$Genre, "Western"))
Movies$Family       <- as.numeric(str_detect(Movies$Genre, "Family"))
Movies$Animation       <- as.numeric(str_detect(Movies$Genre, "Animation"))
Movies$Drama       <- as.numeric(str_detect(Movies$Genre, "Drama"))
Movies$War       <- as.numeric(str_detect(Movies$Genre, "War"))
Movies$Biography       <- as.numeric(str_detect(Movies$Genre, "Biography"))
Movies$Crime       <- as.numeric(str_detect(Movies$Genre, "Crime"))
Movies$Fantasy       <- as.numeric(str_detect(Movies$Genre, "Fantasy"))
Movies$Romance       <- as.numeric(str_detect(Movies$Genre, "Romance"))
Movies$Mystery       <- as.numeric(str_detect(Movies$Genre, "Mystery"))
Movies$Sport       <- as.numeric(str_detect(Movies$Genre, "Sport"))
Movies$Documentary       <- as.numeric(str_detect(Movies$Genre, "Documentary"))
Movies$Musical       <- as.numeric(str_detect(Movies$Genre, "Musical"))

# Calculate sums of each genre in each platform. ----
# Create dfs with data from each individual platform. 
# Pipe Genre data from each new df. Create a col_count using the sjmisc package.
YesNetflix <- Movies %>% filter(Netflix == 1)
NetflixGenres <- YesNetflix %>% select(Action, Adventure, SciFi, Thriller, Comedy, Western, Family, Animation, Drama, War, Biography, Crime, Fantasy, Romance, Mystery, Sport, Documentary, Musical)
#NetflixGenres
NetflixGenreSums <- col_count(NetflixGenres, count=1, append = FALSE)
NetflixGenreSums

YesPrimeVideo <- Movies %>% filter(Prime.Video == 1)
PrimeGenres <- YesPrimeVideo %>% select(Action, Adventure, SciFi, Thriller, Comedy, Western, Family, Animation, Drama, War, Biography, Crime, Fantasy, Romance, Mystery, Sport, Documentary, Musical)
#PrimeGenres
PrimeGenreSums <- col_count(PrimeGenres, count=1, append = FALSE)
PrimeGenreSums

YesHulu <- Movies %>% filter(Hulu == 1)
HuluGenres <- YesHulu %>% select(Action, Adventure, SciFi, Thriller, Comedy, Western, Family, Animation, Drama, War, Biography, Crime, Fantasy, Romance, Mystery, Sport, Documentary, Musical)
#HuluGenres
HuluGenreSums <- col_count(HuluGenres, count=1, append = FALSE)
HuluGenreSums

YesDisney <- Movies %>% filter(Disney == 1)
DisneyGenres <- YesDisney%>% select(Action, Adventure, SciFi, Thriller, Comedy, Western, Family, Animation, Drama, War, Biography, Crime, Fantasy, Romance, Mystery, Sport, Documentary, Musical)
#DisneyGenres
DisneyGenreSums <- col_count(DisneyGenres, count=1, append = FALSE)
DisneyGenreSums

# Bind the col_counts together to create a pivot table.
rbind(NetflixGenreSums, PrimeGenreSums, HuluGenreSums, DisneyGenreSums) #FINALLY!
Platforms_Genres <- bind_rows(NetflixGenreSums, PrimeGenreSums, HuluGenreSums, DisneyGenreSums)
row.names(Platforms_Genres) <- c("Netflix", "Prime", "Hulu", "Disney")
#Platforms_Genres

# Add column with row names (also removes rownames)
PG <- dplyr::as_tibble(Platforms_Genres, rownames = "Platforms")
PG

#A: Plot action movies by platform
act <- ggplot(PG, aes(Platforms, Action))
act + geom_bar(stat='identity')

#B: plot adventure movies by platform
adv <- ggplot(PG, aes(Platforms, Adventure))
adv + geom_bar(stat='identity')

#C: plot thriller movies by platform
adv <- ggplot(PG, aes(Platforms, Adventure))
adv + geom_bar(stat='identity')

#A-C are nice, but we would like to see the genre data compared side-by-side
# Use barplot function to create a clustered bar graph. ----
# Add genre names for the x axis labels.
# Add colors for the platforms. Adjust dimensions.
barplot(as.matrix(Platforms_Genres), main = "Number of Movies in Each Genre by Platform",
        ylab = "Number of Movies",
        names.arg = c("Action", "Adventure", "SciFi", "Thriller", "Comedy", "Western", "Family", "Animation", "Drama", "War", "Biography", "Crime", "Fantasy", "Romance", "Mystery", "Sport", "Documentary", "Musical"),
        beside = TRUE,
        col = c("firebrick1", "gold", "chartreuse1", "darkblue"),
        legend.text = rownames(Platforms_Genres), las=2, cex.lab=1.2, mgp= c(3,1,0), cex.axis=1.0, cex.main = 1.5)

# Now we want to see what the proportion of genres are by platform. ----

# Here I transposed the Platform_Genres table to get the platforms on the x axis
mx <- t(as.matrix(Platforms_Genres))
mx

#proportion tables
prop.table((mx))  #calculate proportion of genres per platform against all date
prop.table(mx, 1) #calculate proportion of movies from each genre
prop.table(mx, 2) #calculate proportion of genres on each platform

# Use barplot function to create a graphic showing the proportions of genres on each platform
p <-barplot(prop.table(mx, margin=2), main = "Proportions of Genres in Each Platform",
            xlab = "Platforms",
            ylab = "Proportions",
            col = c("aquamarine1", "coral1", "darkblue", "red2", "darkorchid", "darkseagreen1", "yellow", "darkslategray1",
                    "darkgoldenrod1", "blue1", "deeppink2", "darkslategray", "darkorange1", "violetred3", "chocolate2", "cornflowerblue",
                    "pink", "seagreen1"),
            legend.text = c("Action", "Adventure", "SciFi", "Thriller", "Comedy", "Western", "Family", "Animation", "Drama", "War", "Biography", 
                            "Crime", "Fantasy", "Romance", "Mystery", "Sport", "Documentary", "Musical"),
            args.legend = list(bty="n", x= "right", ncol=1, text.font = 1), xlim=c(0,7))
p

# Part 2: End User Recommendations by IMDb Score ----

# First, change platforms in Movies df to factor
Movies$Netflix <- as.factor(Movies$Netflix)
Movies$Hulu <- as.factor(Movies$Hulu)
Movies$Prime.Video <-as.factor(Movies$Prime.Video)
Movies$Disney <- as.factor(Movies$Disney)

# Next, create a histogram of distribution of all IMDb scores from data set
g <- ggplot(Movies, aes(IMDb))
g + geom_histogram(binwidth=.5)

# histogram of IMDb Data with Platform data
g + geom_histogram(aes(fill=Netflix), binwidth=.5)
g + geom_histogram(aes(fill=Hulu), binwidth=.5)
g + geom_histogram(aes(fill=Prime.Video), binwidth=.5)
g + geom_histogram(aes(fill=Disney), binwidth=.5)


#Language visualization section
rm(list=ls())
cat("\014")

Movies <- read.csv("Movies.csv")
library(tidyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(adabag) 
library(caret) 
library(e1071)
library(readr)
library(dplyr) 
library(sjmisc)

Movies <- Movies %>% rename(Disney = Disney.)
Movies$NumLanguages  <- str_count(Movies$Language,",") + 1
Movies$English       <- as.integer(str_detect(Movies$Language, "English"))
Movies$Spanish       <- as.integer(str_detect(Movies$Language, "Spanish"))
Movies$Mandarin      <- as.integer(str_detect(Movies$Language, "Mandarin"))
Movies$Cantonese     <- as.integer(str_detect(Movies$Language, "Cantonese"))
Movies$Tagalog       <- as.integer(str_detect(Movies$Language, "Tagalog"))
Movies$Vietnamese    <- as.integer(str_detect(Movies$Language, "Vietnamese"))
Movies$Arabic        <- as.integer(str_detect(Movies$Language, "Arabic"))
Movies$French        <- as.integer(str_detect(Movies$Language, "French"))
Movies$Korean        <- as.integer(str_detect(Movies$Language, "Korean"))
Movies$Russian       <- as.integer(str_detect(Movies$Language, "Russian"))
Movies$German        <- as.integer(str_detect(Movies$Language, "German"))

Movies$Other.Language3<- (Movies$NumLanguages - Movies$English - Movies$Spanish 
                          -Movies$Mandarin)

#Movies$Rotten.Tomatoes = as.integer(gsub("%", "", Movies$Rotten.Tomatoes))

## THI CODE DROPS NA'S FROM ENTIRE DATASET. Unsure for now if this is a good idea
# drop_na(Movies)

Movies$Age <- as.factor(Movies$Age)
Movies$Netflix <- as.factor(Movies$Netflix)
Movies$Hulu <- as.factor(Movies$Hulu)
Movies$Prime.Video <-as.factor(Movies$Prime.Video)
Movies$Disney <- as.factor(Movies$Disney)
Movies$IMDb <- as.factor(Movies$IMDb)

Movies$Rotten.Tomatoes = as.integer(gsub("%", "", Movies$Rotten.Tomatoes))


Movies$English <- as.integer(str_detect(Movies$Language, "English"))


##use this as basis for visualization, creates clear categories and data frames for each platform

EnglishMovies<- Movies %>% filter(English == 1)
EnglishNetflix <- EnglishMovies %>% filter (Netflix == 1)
EnglishNetflix2 <- EnglishNetflix %>% select(Netflix)
EnglishNetflixSum <- col_count(EnglishNetflix2, count=1, append = FALSE)
EnglishHulu <- EnglishMovies %>% filter (Hulu == 1)
EnglishHulu2 <- EnglishHulu %>% select(Hulu)
EnglishHuluSum <- col_count(EnglishHulu2, count=1, append = FALSE)
EnglishPrime <- EnglishMovies %>% filter (Prime.Video == 1)
EnglishPrime2 <- EnglishPrime %>% select(Prime.Video)
EnglishPrimeSum <- col_count(EnglishPrime2, count=1, append = FALSE)
EnglishDisney <- EnglishMovies %>% filter (Disney == 1)
EnglishDisney2 <- EnglishDisney %>% select(Disney)
EnglishDisneySum <- col_count(EnglishDisney2, count=1, append = FALSE)

EnglishStreamingProviders <- bind_rows(EnglishNetflixSum, EnglishHuluSum,EnglishPrimeSum,EnglishDisneySum)

EnglishStreamingProviders

barplot(as.matrix(EnglishStreamingProviders), main = "English Language by Platform",
        xlab = "Providers",
        ylab = "Number of Movies",
        names.arg = c("Netflix", "Hulu", "Prime", "Disney"),col = c("firebrick1", "chartreuse1", "gold", "darkblue"),
        beside = TRUE, ylim = NULL, xlim = NULL,
        legend = rownames(EnglishStreamingProviders))

OtherLanguageMovies<- Movies %>% filter(Other.Language3 == 1)
OtherLanguageNetflix <- OtherLanguageMovies %>% filter (Netflix == 1)
OtherLanguageNetflix2 <- OtherLanguageNetflix %>% select(Netflix)
OtherLanguageNetflixSum <- col_count(OtherLanguageNetflix2, count=1, append = FALSE)
OtherLanguageHulu <- OtherLanguageMovies %>% filter (Hulu == 1)
OtherLanguageHulu2 <- OtherLanguageHulu %>% select(Hulu)
OtherLanguageHuluSum <- col_count(OtherLanguageHulu2, count=1, append = FALSE)
OtherLanguagePrime <- OtherLanguageMovies %>% filter (Prime.Video == 1)
OtherLanguagePrime2 <- OtherLanguagePrime %>% select(Prime.Video)
OtherLanguagePrimeSum <- col_count(OtherLanguagePrime2, count=1, append = FALSE)
OtherLanguageDisney <- OtherLanguageMovies %>% filter (Disney == 1)
OtherLanguageDisney2 <- OtherLanguageDisney %>% select(Disney)
OtherLanguageDisneySum <- col_count(OtherLanguageDisney2, count=1, append = FALSE)

OtherLanguageStreamingProviders <- bind_rows(OtherLanguageNetflixSum, 
                                             OtherLanguageHuluSum,OtherLanguagePrimeSum,OtherLanguageDisneySum)

barplot(as.matrix(OtherLanguageStreamingProviders), main = "Non-English languages by Platform",
        xlab = "Providers",
        ylab = "Number of Movies",
        names.arg = c("Netflix", "Hulu", "Prime", "Disney"), col = c("firebrick1", "chartreuse1", "gold", "darkblue"),
        beside = TRUE, ylim= NULL, xlim = NULL,
        legend = rownames(EnglishStreamingProviders))
