# Classification Tree
library(dplyr)
library(readr)
library(date)
library(lubridate)
library(caret)
library(forecast)
library(rpart)
library(rpart.plot)
# install.packages("tidyverse")
library(tidyverse)
library(e1071)
library(ROSE)
options(scipen = 999) #Removes scientific notation
options(max.print = 1000000)

#Converting raw data without ohe, where variables are characters, into factors.
merli_cleaneddata_withoutohe = read_csv("merli_cleaneddata_withoutohe.csv") %>%
  mutate_if(is.character, as.factor)

#Split to Train/Test data according to 60-40 split.
set.seed(1)
merli.index <- sample(nrow(merli_cleaneddata_withoutohe), nrow(merli_cleaneddata_withoutohe)*0.6)
merli.train = merli_cleaneddata_withoutohe[merli.index,]
merli.test = merli_cleaneddata_withoutohe[-merli.index,]

table(merli.train$default)

#     0      1 
#105958  29337 
#Observed high rows of no defaults, compared to those that defaulted. Would have low sensitivity as a result.

# Renaming columns so that ROSE could work.
test.merli.train = rename(merli.train,
                          yr_newloan = "1yr_newloan",
                          yr_def_loan= "1yr_def_loan",
                          ageatdec2020 = "ageat31dec2020")

test.merli.test = rename(merli.test,
                         yr_newloan = "1yr_newloan",
                         yr_def_loan= "1yr_def_loan",
                         ageatdec2020 = "ageat31dec2020")

# Data Balancing by using ROSE
oversample.merli.train = ROSE(default~., data = test.merli.train)$data
table(oversample.merli.train$default)
#    0     1 
#67746 67549 

rpart.tree4 <- rpart(default ~., data = oversample.merli.train, method = "class",
                     control = rpart.control(minsplit=1, cp=0))
cptest.tree4 = data.frame(printcp(rpart.tree4)) %>%
  mutate(rel.error_xstd = rel.error + xstd) 
# Best CP 0.00011103051, nsplit 394. Row 57.

rpart.tree5 = rpart(default ~., data = oversample.merli.train, method = "class",
                    control = rpart.control(minsplit=3000, cp=0.00011103051))
prp(rpart.tree5, type = 1, extra = 1, split.font = 1, varlen = -10)
rpart.tree5.pred = predict(rpart.tree5, test.merli.test, type="class")
confusionMatrix(table(rpart.tree5.pred, test.merli.test$default),
                positive = "1")
#rpart.rules(rpart.tree5)
rules = data.frame(rpart.rules(rpart.tree5))
#write_csv(rules, path = 'rules.csv')

#when minsplit = 3000
#rpart.tree5.pred     
#     0     1
#0 25876  4350
#1 44692 15280
#Accuracy : 0.4563 
#Sensitivity : 0.7784             
#Specificity : 0.3667             
#Pos Pred Value : 0.2548             
#Neg Pred Value : 0.8561   

table(test.merli.test$default)
#0     1 
#70568 19630 
# Low pos pred value likely because test data contains very low observations where default = 1.

# Import 2020 data
merli_cleaned_new = read_csv("merli_new_cleanedwithoutohe.csv") %>%
  mutate_if(is.character, as.factor) %>%
  rename(yr_newloan = "1yr_newloan",
         yr_def_loan= "1yr_def_loan",
         ageatdec2020 = "ageat31dec2020")

pred.default.class = predict(rpart.tree5, merli_cleaned_new, type="class")
pred.default.prob = predict(rpart.tree5, merli_cleaned_new, type="prob")
pred.default.combined = cbind(merli_cleaned_new,
                              pred.default.class,
                              pred.default.prob)

table(pred.default.combined$pred.default.class)
#    0     1 
#34181 74768

#View(pred.default.combined)
#write_csv(pred.default.combined, "pred.default.combined.csv")

#Provide useful statistical summaries of findings
nrow(filter(pred.default.combined, pred.default.class == "1"))
nrow(pred.default.combined)

# Percentage of defaults
nrow(filter(pred.default.combined, pred.default.class == "1"))/nrow(pred.default.combined)
# 0.6862661
# 68.6% of total loans are probable to default

# Number of rows of defaults where probability > x%
nrow(filter(pred.default.combined, pred.default.combined[,18] >= 0.5))
nrow(filter(pred.default.combined, pred.default.combined[,18] >= 0.6))
nrow(filter(pred.default.combined, pred.default.combined[,18] >= 0.7))

# % Composition
nrow(filter(pred.default.combined, pred.default.combined[,18] >= 0.5))/nrow(pred.default.combined)
nrow(filter(pred.default.combined, pred.default.combined[,18] >= 0.6))/nrow(pred.default.combined)
nrow(filter(pred.default.combined, pred.default.combined[,18] >= 0.7))/nrow(pred.default.combined)

default_data = filter(pred.default.combined, pred.default.class == "1")
total_loan_loss = sum(default_data$amt_loan)
# $12,273,725,290 is the total loans loss to defaulting

# Loan loss amount based on probability of default
sum(filter(pred.default.combined, pred.default.combined[,18] >= 0.6)$amt_loan) #60%: 10736187484
sum(filter(pred.default.combined, pred.default.combined[,18] >= 0.7)$amt_loan) #70%: 1548988234

# Attempting to use C5 to see if it's a better substitute model
library(C50)
merli.c5 = C5.0(as.factor(default)~., data = oversample.merli.train, trials = 5)
summary(merli.c5)
merli.c5.pred = predict(merli.c5, test.merli.test, type="class")
confusionMatrix(table(merli.c5.pred, merli.test$default),
                positive = "1")
#Sensitivity : 0.7617             
#Specificity : 0.3866             
#Pos Pred Value : 0.2568             
#Neg Pred Value : 0.8537       

#Results from C5 worst than recursive partition. Therefore do not rely.