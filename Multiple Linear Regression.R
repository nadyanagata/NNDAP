#Regression on other Target Variables such as : amt_loan and ltv_ratio

library(dplyr)
library(readr)
library(date)
library(lubridate)
library(caret)
library(forecast)

# Importing historical dataset and filtered by non-defaults only, for regression on target variables.
merli_cleanedwodist = read_csv("merlicleaneddata.csv")%>%
        filter(default == "0")%>% select(-default)

options(scipen=999)

#Regression on Target variable : amt_loan

#Removing the N/A variables due to OHE issues - Curse of Dimensionality
merli.sel.loan <- merli_cleanedwodist [-c(15,21)]

#Testing which train-test would be best for MLR
#Data partition : 60-40 
set.seed(1)
merli.loan.index <- sample(nrow(merli.sel.loan), 0.6*nrow(merli.sel.loan))

train.loan <- merli.sel.loan[merli.loan.index,]
test.loan <- merli.sel.loan[-merli.loan.index,]

#Regression on amt_loan (60-40)
merli.loan.mlm <- lm(amt_loan~.,data=train.loan)
summary(merli.loan.mlm)

merli.loan.predict <- predict(merli.loan.mlm,test.loan)
accuracy(merli.loan.predict,test.loan$amt_loan)

#Accuracy Measure
#ME     RMSE      MAE         MPE   MAPE
#Test set 74.90112 6521.729 4098.255 -0.01404173 2.8443

#Data partition : 70-30 ---> BEST MODEL 
set.seed(1)
merli.loan.index2 <- sample(nrow(merli.sel.loan), 0.7*nrow(merli.sel.loan))

train.loan2 <- merli.sel.loan[merli.loan.index2,]
test.loan2 <- merli.sel.loan[-merli.loan.index2,]

#Regression on amt_loan (70-30)
merli.loan.mlm2 <- lm(amt_loan~.,data=train.loan2)
summary(merli.loan.mlm2)

merli.loan.predict2 <- predict(merli.loan.mlm2,test.loan2)
merli.loan.results2 <- data.frame ("Actual" = test.loan2$amt_loan,
                                  "Predicted" = merli.loan.predict2,
                                  "Residuals" = test.loan2$amt_loan - merli.loan.predict2)
accuracy(merli.loan.predict2,test.loan2$amt_loan)

#Accuracy Measure
#               ME     RMSE      MAE          MPE     MAPE
#Test set 32.79374 6402.005 4068.074 -0.009527713 2.830387

#Data partition : 50-50
set.seed(1)
merli.loan.index3 <- sample(nrow(merli.sel.loan), 0.5*nrow(merli.sel.loan))

train.loan3 <- merli.sel.loan[merli.loan.index3,]
test.loan3 <- merli.sel.loan[-merli.loan.index3,]

#Regression on amt_loan (50-50)
merli.loan.mlm3 <- lm(amt_loan~.,data=train.loan3)
summary(merli.loan.mlm3)

merli.loan.predict3 <- predict(merli.loan.mlm3,test.loan3)
accuracy(merli.loan.predict3,test.loan3$amt_loan)

#               ME     RMSE      MAE         MPE     MAPE
#Test set 79.03101 6477.371 4089.211 -0.01657813 2.836729

# RMSE on 70-30 is the best. Therefore use 70-30 split for regression.

#Subset search algorithms on best regression, data partition : 70-30
#Backward selection
merli.loan.bwe <- step(merli.loan.mlm2, direction = "backward")
merli.loan.bwe.pred <- predict (merli.loan.bwe,test.loan2)
accuracy(merli.loan.bwe.pred,test.loan2$amt_loan)

#Accuracy Measure
#                ME     RMSE     MAE          MPE     MAPE
#Test set 32.75224 6401.755 4068.13 -0.009515248 2.830612


#Forward selection 
merli.loan.fwd <- step(merli.loan.mlm2, direction = "forward")
merli.loan.fwd.pred <- predict (merli.loan.fwd,test.loan2)
accuracy(merli.loan.fwd.pred,test.loan2$amt_loan)

#Accuracy Measure 
#                ME     RMSE      MAE          MPE     MAPE
#Test set 32.79374 6402.005 4068.074 -0.009527713 2.830387


#Stepwise regression 
merli.loan.both <- step(merli.loan.mlm2, direction = "both")
summary(merli.loan.both)
merli.loan.both.pred <- predict (merli.loan.both,test.loan2)

accuracy(merli.loan.both.pred,test.loan2$amt_loan)

#Accuracy Measure
#               ME     RMSE     MAE          MPE     MAPE
#Test set 32.75224 6401.755 4068.13 -0.009515248 2.830612

#Stepwise regression when direction = "both" yields the best prediction model since lowest RMSE.

#Step:  AIC=2175169
#amt_loan ~ veh_cost + ltv_ratio + ttl_loanacc + def_loanacc + 
#  app_loanamt + dis_loanamt + call_ins + emp_statFulltime.Employee + 
#  cbs_ratingA + cbs_ratingC + cbs_ratingD + cbs_ratingE

#Post-CB loans when fitted into new predicted model for amt_loan
merli.new <- read_csv ("merli_new_cleaneddata.csv")
merli.new.loan.pred<- predict(merli.loan.both,merli.new)

accuracy(merli.new.loan.pred,merli.new$amt_loan)
#Accuracy Measure
#               ME     RMSE      MAE       MPE     MAPE
#Test set 1225.999 6413.702 4058.395 0.6935328 2.644348
merli.new.updated<- mutate(merli.new, "Predicted amt_loan"= merli.new.loan.pred)

merli.new.results <- data.frame("Actual"= merli.new$amt_loan,
                                "Predicted"=merli.new.loan.pred,
                               "Residuals"=merli.new.loan.pred-merli.new$amt_loan)

#write_csv(merli.new.results,path="merli.new.amt_loan.csv")
summary(merli.new.results)

#Target variables : ltv_ratio
#Removing the NA variables
merli.sel.ratio <- merli_cleanedwodist [-c(15,21)]

#Data partition: 60-40
set.seed(1)
merli.ratio.index <- sample(nrow(merli.sel.ratio), 0.6*nrow(merli.sel.ratio))

train.ratio <- merli.sel.ratio[merli.ratio.index,]
test.ratio <- merli.sel.ratio[-merli.ratio.index,]

#Regression on ltv_ratio (60-40)
merli.ratio.mlm <- lm(ltv_ratio~.,data=train.ratio)
summary(merli.ratio.mlm)

merli.ratio.predict <- predict(merli.ratio.mlm,test.ratio)


accuracy(merli.ratio.predict,test.ratio$ltv_ratio)

#Accuracy Measure
#               ME     RMSE      MAE        MPE   MAPE
#Test set -0.0264384 2.695382 1.824693 -0.2117625 3.0494

#Data partition : 70-30 --> BEST MODEL
set.seed(1)
merli.ratio.index2 <- sample(nrow(merli.sel.ratio), 0.7*nrow(merli.sel.ratio))

train.ratio2 <- merli.sel.ratio[merli.ratio.index2,]
test.ratio2 <- merli.sel.ratio[-merli.ratio.index2,]

#Regression on ltv_ratio (70-30)
merli.ratio.mlm2 <- lm(ltv_ratio~.,data=train.ratio2)
summary(merli.ratio.mlm2)

merli.ratio.predict2 <- predict(merli.ratio.mlm2,test.ratio2)
merli.ratio.results2 <- data.frame ("Actual" = test.ratio2$ltv_ratio,
                                    "Predicted" = merli.ratio.predict2,
                                    "Residuals" = test.ratio2$ltv_ratio - merli.ratio.predict2)
accuracy(merli.ratio.predict2,test.ratio2$ltv_ratio)

#Accuracy Measure
#                  ME     RMSE      MAE        MPE   MAPE
#Test set -0.01077241 2.660811 1.81027 -0.1929089 3.0377

#Data partitioning : 50-50
set.seed(1)
merli.ratio.index3 <- sample(nrow(merli.sel.ratio), 0.5*nrow(merli.sel.ratio))

train.ratio3 <- merli.sel.ratio[merli.ratio.index3,]
test.ratio3 <- merli.sel.ratio[-merli.ratio.index3,]

#Regression on ltv_ratio (50-50)
merli.ratio.mlm3 <- lm(ltv_ratio~.,data=train.ratio3)
summary(merli.ratio.mlm3)

merli.ratio.predict3 <- predict(merli.ratio.mlm3,test.ratio3)

accuracy(merli.ratio.predict3,test.ratio3$ltv_ratio)
  
#                   ME     RMSE      MAE        MPE     MAPE
#Test set -0.02945112 2.677444 1.821984 -0.2143406 3.046748

# RMSE on 70-30 is the best. Therefore use 70-30 split for regression.

#Subset selection algorithms on Best Model with data partition 70-30
#Backward search
merli.ratio.bwe <- step(merli.ratio.mlm2, direction = "backward")
merli.ratio.bwe.pred <- predict (merli.ratio.bwe,test.ratio2)
accuracy(merli.ratio.bwe.pred,test.ratio2$ltv_ratio)

#Accuracy Measure
#                  ME     RMSE      MAE        MPE     MAPE
#Test set -0.01093662 2.660407 1.813779 -0.1931365 3.037342

#Step:  AIC=248958.7
#ltv_ratio ~ amt_loan + veh_cost + ttl_loanacc + def_loanacc + 
#  dis_loanamt + `1yr_newloan` + call_ins + ageat31dec2020 + 
#  emp_statFulltime.Employee + cbs_ratingA + cbs_ratingB + cbs_ratingC + 
#  cbs_ratingD + cbs_ratingE

#Forward search
merli.ratio.fwd <- step(merli.ratio.mlm2, direction = "forward")
merli.ratio.fwd.pred <- predict (merli.ratio.fwd,test.ratio2)
accuracy(merli.ratio.fwd.pred,test.ratio2$ltv_ratio)

#               ME     RMSE      MAE        MPE   MAPE
#Test set -0.01077241 2.660811 1.814027 -0.1929089 3.0377

#Both direction
merli.ratio.both <- step(merli.ratio.mlm2, direction = "both")
merli.ratio.both.pred <- predict (merli.ratio.both,test.ratio2)
summary(merli.ratio.both)
accuracy(merli.ratio.both.pred,test.ratio2$ltv_ratio)

#Accuracy Measure
#                   ME     RMSE      MAE        MPE     MAPE
#Test set -0.01093662 2.660407 1.813779 -0.1931365 3.037342

#Stepwise regression when direction = "both" yields the best prediction model since lowest RMSE.

#Step:  AIC=248958.7
#ltv_ratio ~ amt_loan + veh_cost + ttl_loanacc + def_loanacc + 
#  dis_loanamt + `1yr_newloan` + call_ins + ageat31dec2020 + 
#  emp_statFulltime.Employee + cbs_ratingA + cbs_ratingB + cbs_ratingC + 
#  cbs_ratingD + cbs_ratingE

#Post-CB loans when fitted into new predicted model for ltv_ratio
merli.new.ratio.pred<- predict(merli.ratio.both,merli.new)

merli.new.updated<- mutate(merli.new, "Predicted amt_loan"= merli.new.loan.pred,
                           "Predicted ltv_ratio"= merli.new.loan.pred)
accuracy(merli.new.ratio.pred, merli.new$ltv_ratio)

#Accuracy Measure
#               ME    RMSE      MAE        MPE     MAPE
#Test set -0.3863678 2.57833 1.723315 -0.7005227 2.847537

merli.new.results2 <- data.frame("Actual"= merli.new$ltv_ratio,
                                "Predicted"=merli.new.ratio.pred,
                                "Residuals"=merli.new.ratio.pred-merli.new$ltv_ratio)

#write_csv (merli.new.updated,path="merli.new.updated.csv")
#write_csv(merli.new.results2,path="merli.new.ltv_ratio.csv")

summary(merli.new.results2)

#Correlation plot to identify correlation between default and the two target variables separately.
merli_cleaned <- read_csv("merlicleaneddata.csv")
library(corrplot)
corrplot(cor(merli_cleaned), type = "upper")
cor(merli_cleaned)

#Correlation between default and call_ins : 0.043843845

#Correlation between default and amt_loan : 0.07928577

# Identify number of call ins where default = 1
merli_historical_defaultonly = read_csv("merli_cleaneddata_withoutohe.csv") %>%
  filter(default == "1")
table(merli_historical_defaultonly$call_ins)
#0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    18    19 
#41105  5307  1456   504   231   127    90    55    43    14    11     7     4     3     2     4     2     2 

# Shows that high % of those who default did not call in, or only small number of call ins.
# However correlation is weakly positive. Should've been negative if call ins increasing would decrease
# the probability of default.