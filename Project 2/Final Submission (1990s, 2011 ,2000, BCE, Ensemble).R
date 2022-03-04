# GROUP 6 Project Code


library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(yardstick)
library(glmnet)
library(coefplot)
library(ggplot2)
library(stringr)
library(quanteda)
library(tidyverse)

options(scipen=999)
# Reading Fundamentals Annual our group extracted from WRDS
Fundamentals <- read_csv("Fundamentals Annual 2005-2010.csv", 
                         col_types = cols(datadate = col_date(format ="%Y%m%d")))

Fundamentals <-Fundamentals %>%
  mutate(year = year(datadate)) %>%
  select(-c(fyear))

unique(Fundamentals$gsector)

#replace all NA values with 0
Fundamentals[is.na(Fundamentals)]<- 0

GICS <- read.csv("GICS compiled.csv")

# Assigning GICS Industry based on Gsector
Fundamentals_GICS <- Fundamentals %>% 
  filter(year != 2010) %>% 
  left_join(GICS, by = "gsector")

# Converting some of the variables as numeric or characters. 
Fundamentals_GICS$gvkey = as.numeric(Fundamentals_GICS$gvkey)
Fundamentals_GICS$gsector = as.character(Fundamentals_GICS$gsector)
Fundamentals_GICS$gicdesc = as.character(Fundamentals_GICS$gicdesc)
Fundamentals_GICS$indfmt = as.character(Fundamentals_GICS$indfmt)
Fundamentals_GICS$gicdesc [is.na(Fundamentals_GICS$gicdesc)] <- 0
unique(Fundamentals_GICS$gsector)

# Train data set
train <- read.csv("Restate_int_train.csv")
train_GICS <- left_join(train, Fundamentals_GICS, by = c("gvkey","year"))

# These are the 10 duplicated observations
duplicated <- train_GICS[duplicated(train_GICS[c('gvkey', 'year')]),] 

# By running the code below, remove 10 extra observations due to duplicate gvkey & year
train_GICS <- train_GICS %>% distinct(gvkey, year, .keep_all = TRUE)

# To remove observation without any matching data from WRDS. 22 observations removed
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

train_GICS <- delete.na(train_GICS, 31)

# Prediction Model 1 (1990s)
train_GICS_Model1 <- train_GICS %>%  group_by(gvkey) %>%
  mutate(ni_revt = ni/revt) %>% # net profit margin
  mutate(roa = ni/at) %>% # return on assets
  mutate(log_lt = log(lt)) %>% # log of liabilities
  mutate(lt_seq = lt/seq) %>% # total liabilities/total equity
  mutate(lt_at = lt/at) %>% # total liabilities/total assets
  mutate(quick = (act-invt)/lct) %>% # quick ratio 
  mutate(wcap_at = wcap/at) %>% # working capital ratio
  mutate(invt_revt = invt/revt) %>% # inventory/revenue
  mutate(invt_at = invt/at) %>% # inventory/assets
  mutate(ni_ppent = ni/ppent) %>% # net income/ppe
  mutate(rect_revt = rect/revt) %>% # A/R/revenue
  mutate(gp_at = gp/at) %>% # gross profit/total assets
  mutate(revt_m_gp = revt - gp) %>% # revenue - gross profit
  mutate(ch_at = ch/at) %>% # cash/total assets
  mutate(log_at = log(at)) %>% # log of assets
  mutate(ppent_at = ppent/at) %>% # ppe / total assets
  mutate(revt_chg = (revt/lag(revt))-1) %>% # change in revenue
  mutate(AR_chg = (rect/lag(rect))) %>% # change in A/R
  mutate(AR_chg_binary = ifelse(AR_chg > "1.1", 1,0)) %>% # scaled change in A/R
  mutate(GP_chg = (gp/lag(gp))) %>% # change in gross profit
  mutate(GP_chg_binary = ifelse(GP_chg > "1.1", 1,0)) %>% ungroup() # scaled change in gross profit

# Replace the NA and Inf figures again with 0
is.na(train_GICS_Model1) <- sapply(train_GICS_Model1, is.infinite)
train_GICS_Model1[is.na(train_GICS_Model1)] <- 0

# Setting the equation for glm as formula
equation_1990 <- as.formula("Restate_Int ~ ebit + ni_revt + roa + log_lt + lt_seq + lt_at + quick + wcap_at + 
                   invt_revt + invt_at + ni_ppent + rect_revt + gp_at + revt_m_gp + ch_at + log_at + 
                   ppent_at + wcap + revt_chg + AR_chg + AR_chg_binary + GP_chg + GP_chg_binary")

fit_1990s <- glm(equation_1990, data = train_GICS_Model1,family = "binomial")

summary(fit_1990s)


# Evaluate out-of-sample ROC AUC using yardstick library
testing <- read.csv("Restate_int_test.csv")

# Assigning GICS Industry based on Gsector
Fundamentals_testing <- Fundamentals %>% 
  filter(year == 2010) %>% 
  left_join(GICS, by = "gsector")

Fundamentals_testing$gvkey <- as.numeric(Fundamentals_testing$gvkey)
testing_GICS<- left_join(testing, Fundamentals_testing, by = c("gvkey","year"))

# By running the code below, remove 5 extra observations due to duplicate gvkey & year
testing_GICS <- testing_GICS %>% distinct(gvkey, year, .keep_all = TRUE) %>% mutate(Restate_Int = 0) 

# To drop datadate variable before assigning 0 to NA values
testing_GICS <- testing_GICS  %>% select(-c(datadate,indfmt))

testing_GICS_Model1 <- testing_GICS %>% 
  mutate(ni_revt = ni/revt) %>% # net profit margin
  mutate(roa = ni/at) %>% # return on assets
  mutate(log_lt = log(lt)) %>% # log of liabilities
  mutate(lt_seq = lt/seq) %>% # total liabilities/total equity
  mutate(lt_at = lt/at) %>% # total liabilities/total assets
  mutate(quick = (act-invt)/lct) %>% # quick ratio 
  mutate(wcap_at = wcap/at) %>% # working capital ratio
  mutate(invt_revt = invt/revt) %>% # inventory/revenue
  mutate(invt_at = invt/at) %>% # inventory/assets
  mutate(ni_ppent = ni/ppent) %>% # net income/ppe
  mutate(rect_revt = rect/revt) %>% # A/R/revenue
  mutate(gp_at = gp/at) %>% # gross profit/total assets
  mutate(revt_m_gp = revt - gp) %>% # revenue - gross profit
  mutate(ch_at = ch/at) %>% # cash/total assets
  mutate(log_at = log(at)) %>% # log of assets
  mutate(ppent_at = ppent/at) %>% # ppe / total assets
  mutate(revt_chg = (revt/lag(revt))-1) %>% # change in revenue
  mutate(AR_chg = (rect/lag(rect))) %>% # change in A/R
  mutate(AR_chg_binary = ifelse(AR_chg > "1.1", 1,0)) %>% # scaled change in A/R
  mutate(GP_chg = (gp/lag(gp))) %>% # change in gross profit
  mutate(GP_chg_binary = ifelse(GP_chg > "1.1", 1,0)) # scaled change in gross profit

# Replace the NA and Inf figures again with 0 for testing dataset
is.na(testing_GICS_Model1) <- sapply(testing_GICS_Model1, is.infinite)
testing_GICS_Model1[is.na(testing_GICS_Model1)] <- 0

pred1 <- predict(fit_1990s, testing_GICS_Model1, type = "response")
prediction <- data.frame(pred1)

# To upload to kaggle
kaggle_fit_1990s <- mutate(testing_GICS_Model1, Restate_Int = pred1)
kaggle_fit_1990s <- kaggle_fit_1990s %>% select(gvkey, Restate_Int)
write_csv(kaggle_fit_1990s, "Restate_Fit_1990s.csv")

# LASSO for 1990s Model
x_model1 <- model.matrix(equation_1990, data= train_GICS_Model1)[,-1]
y_model1 <- model.frame(equation_1990, data= train_GICS_Model1)[,"Restate_Int"]

set.seed(1)

cvfit1 <- cv.glmnet(x= x_model1  ,  
                    y = y_model1,
                    family = "binomial" ,
                    alpha = 1 ,
                    type.measure = "auc" )
plot(cvfit1)
coef(cvfit1, s = 'lambda.min') 
coefplot(cvfit1, lambda='lambda.min', sort='magnitude')

lasso_model1 <- model.matrix(equation_1990, data=testing_GICS_Model1)[,-1]
lasso_pred1 <- predict(cvfit1,lasso_model1, type="response", s = 'lambda.min')
lasso_pred1 <- data.frame(lasso_pred1)

lasso_test1 <- data.frame('gvkey' = testing_GICS_Model1$gvkey, "Restate_Int" = lasso_pred1)
lasso_test1 <- lasso_test1 %>% rename(Restate_Int = "lambda.min")
write.csv(lasso_test1 , "Restate_Fit_1990s_LASSO.csv", row.names = FALSE)



# 2011s Model
train_GICS_Model2 <- train_GICS %>% group_by(gvkey)%>%
  mutate(log_at = log(at)) %>% # log of assets
  mutate(lag_ni = lag(ni))%>% # lagged net income
  mutate(chg_cashmargin = ((1-(cogs+(invt-lag(invt))))/(sale-(rect-lag(rect))))-
           ((1-(cogs+(lag(invt)-lag(invt,2))))/(lag(sale)-(lag(rect)-lag(rect,2))))/
           ((1-(cogs+(lag(invt)-lag(invt,2))))/(lag(sale)-(lag(rect)-lag(rect,2)))))%>% # change in cash margin
  mutate(ppent_at = ppent/at) %>% # ppe to total assets
  mutate(nco = (at-act-ivao)-(lt-lct-dltt)) %>% # for calculation purposes
  mutate(financing = (ivst+ivao)-(dltt+pstk+dlc)) %>% # for calculation purposes
  mutate(wc = (act-che)- (lct-dlc)) %>% # for calculation purposes
  mutate(rsst_acc = ((wc-lag(wc))+(nco-lag(nco))+(financing-lag(financing)))/((at+lag(at))/2)) %>% # rsst accruals
  mutate(chg_rect = (rect-lag(rect))/((at+lag(at))/2)) %>% # change in receivables
  mutate(chg_inv = (invt-lag(invt))/(at+lag(at))/2) %>% # change in inventory
  mutate(chg_roa = (ib / ((at+lag(at))/2)) - (lag(ib) /((lag(at)+lag(at,2))/2))) %>% # change in roa
  mutate(oplease_dum = ifelse(mrc1>0|mrc2>0|mrc3>0|mrc4>0|mrc5>0,1,0)) %>% # indicator for operating lease
  mutate (issuance = ifelse(sstk>0|dltis>0,1,0)) %>% # indicator for stock/bond issuance
  mutate (soft_assets = (at - ppent - che)/((at+lag(at))/2))%>% # percentage of soft asset
  mutate (pct_chg_cashsales = ((sale-(rect-lag(rect)))-(lag(sale)-(lag(rect)-lag(rect,2))))/(lag(sale)-(lag(rect)-lag(rect,2))))%>% # percentage change of cash sales
  mutate (ch_emp = ((emp/lag(emp))-1)/((at/lag(at))-1)) %>% # percentage change in no of employees
  mutate (ch_backlog = ((ob/lag(ob))-1)/((sale/lag(sale))-1))%>% # percentage change in order of backlog
  mutate (bm = ceq/(csho*prcc_f)) %>% # book-to-market
  mutate (big_n_aud = ifelse(au == 4|au == 5|au == 6|au == 7,1,0)) %>% # indicator for big n auditors
  mutate (mid_n_aud = ifelse(au == 11|au == 17|au == 21,1,0)) %>%  # indicator for mid-size auditors
  mutate(cffin= fincf/((at+lag(at))/2)) %>% # total financing raised
  mutate (exfin = ifelse(((oancf-((capx+lag(capx,2)+lag(capx,3))/3))/act)< -0.5,1,0))%>% # ex ante financing need
  mutate (leverage = dltt/at) %>% # leverage
  mutate(mgr = ifelse(aqp>0,1,0))%>% # indicator for merger
  mutate(restruct = ifelse(rcp>0,1,0)) %>% # indicator for restructuring
  mutate(wc_acc = ((act - lag(act)) - (che - lag(che))-(lct - lag(lct))-(dlc-lag(dlc))-(txp-lag(txp))-dp)/((at+lag(at))/2))%>% # wc accruals
  ungroup()

# Replace the NA and Inf figures again with 0
is.na(train_GICS_Model2) <- sapply(train_GICS_Model2, is.infinite)
train_GICS_Model2[is.na(train_GICS_Model2)] <- 0

equation_2011 <- as.formula("Restate_Int ~ log_at + lag_ni + ppent_at + chg_cashmargin + rsst_acc + chg_rect + chg_inv +
                   chg_roa + oplease_dum + issuance + soft_assets + pct_chg_cashsales+ ch_emp + ch_backlog + bm + big_n_aud + 
                   mid_n_aud +cffin + exfin +leverage+ mgr + restruct + wc_acc")

fit_2011s <- glm(equation_2011, family = binomial, data = train_GICS_Model2)

summary(fit_2011s)

# Evaluate out-of-sample ROC AUC using yardstick library
testing_GICS_Model2 <- testing_GICS %>%
  mutate(log_at = log(at)) %>% # log of assets
  mutate(lag_ni = lag(ni))%>% # lagged net income
  mutate(chg_cashmargin = ((1-(cogs+(invt-lag(invt))))/(sale-(rect-lag(rect))))-
           ((1-(cogs+(lag(invt)-lag(invt,2))))/(lag(sale)-(lag(rect)-lag(rect,2))))/
           ((1-(cogs+(lag(invt)-lag(invt,2))))/(lag(sale)-(lag(rect)-lag(rect,2)))))%>% # change in cash margin
  mutate(ppent_at = ppent/at) %>% # ppe to total assets
  mutate(nco = (at-act-ivao)-(lt-lct-dltt)) %>% # for calculation purposes
  mutate(financing = (ivst+ivao)-(dltt+pstk+dlc)) %>% # for calculation purposes
  mutate(wc = (act-che)- (lct-dlc)) %>% # for calculation purposes
  mutate(rsst_acc = ((wc-lag(wc))+(nco-lag(nco))+(financing-lag(financing)))/((at+lag(at))/2)) %>% # rsst accruals
  mutate(chg_rect = (rect-lag(rect))/((at+lag(at))/2)) %>% # change in receivables
  mutate(chg_inv = (invt-lag(invt))/(at+lag(at))/2) %>% # change in inventory
  mutate(chg_roa = (ib / ((at+lag(at))/2)) - (lag(ib) /((lag(at)+lag(at,2))/2))) %>% # change in roa
  mutate(oplease_dum = ifelse(mrc1>0|mrc2>0|mrc3>0|mrc4>0|mrc5>0,1,0)) %>% # indicator for operating lease
  mutate (issuance = ifelse(sstk>0|dltis>0,1,0)) %>% # indicator for stock/bond issuance
  mutate (soft_assets = (at - ppent - che)/((at+lag(at))/2))%>% # percentage of soft asset
  mutate (pct_chg_cashsales = ((sale-(rect-lag(rect)))-(lag(sale)-(lag(rect)-lag(rect,2))))/(lag(sale)-(lag(rect)-lag(rect,2))))%>% # percentage change of cash sales
  mutate (ch_emp = ((emp/lag(emp))-1)/((at/lag(at))-1)) %>% # percentage change in no of employees
  mutate (ch_backlog = ((ob/lag(ob))-1)/((sale/lag(sale))-1))%>% # percentage change in order of backlog
  mutate (bm = ceq/(csho*prcc_f)) %>% # book-to-market
  mutate (big_n_aud = ifelse(au == 4|au == 5|au == 6|au == 7,1,0)) %>% # indicator for big n auditors
  mutate (mid_n_aud = ifelse(au == 11|au == 17|au == 21,1,0)) %>% # indicator for mid-size auditors
  mutate(cffin= fincf/((at+lag(at))/2)) %>% # total financing raised
  mutate (exfin = ifelse(((oancf-((capx+lag(capx,2)+lag(capx,3))/3))/act)< -0.5,1,0))%>% # ex ante financing need
  mutate (leverage = dltt/at) %>% # leverage
  mutate(mgr = ifelse(aqp>0,1,0))%>% # indicator for merger
  mutate(restruct = ifelse(rcp>0,1,0)) %>% # indicator for restructuring
  mutate(wc_acc = ((act - lag(act)) - (che - lag(che))-(lct - lag(lct))-(dlc-lag(dlc))-(txp-lag(txp))-dp)/((at+lag(at))/2)) # wc accruals

is.na(testing_GICS_Model2) <- sapply(testing_GICS_Model2, is.infinite)
testing_GICS_Model2[is.na(testing_GICS_Model2)] <- 0

pred2 <- predict(fit_2011s, testing_GICS_Model2, type = "response")
prediction2 <- data.frame(pred2)

# To upload to kaggle
kaggle_fit_2011s <- mutate(testing_GICS_Model2, Restate_Int = pred2)
kaggle_fit_2011s <- kaggle_fit_2011s %>% select(gvkey, Restate_Int)
write_csv(kaggle_fit_2011s, "Restate_Fit_2011s.csv")

# LASSO for 2011s Model
x_model2 <- model.matrix(equation_2011, data= train_GICS_Model2)[,-1]
y_model2 <- model.frame(equation_2011, data= train_GICS_Model2)[,"Restate_Int"]

set.seed(1)

cvfit2 <- cv.glmnet(x= x_model2  ,  
                    y = y_model2,
                    family = "binomial" ,
                    alpha = 1 ,
                    type.measure = "auc" )
plot(cvfit2)
coef(cvfit2, s = 'lambda.min') 
coefplot(cvfit2, lambda='lambda.min', sort='magnitude')

lasso_model2 <- model.matrix(equation_2011, data=testing_GICS_Model2)[,-1]
lasso_pred2 <- predict(cvfit2,lasso_model2, type="response", s = 'lambda.min')
lasso_pred2 <- data.frame(lasso_pred2)

lasso_test2 <- data.frame('gvkey' = testing_GICS_Model2$gvkey, "Restate_Int" = lasso_pred2)
lasso_test2 <- lasso_test2 %>% rename(Restate_Int = "lambda.min")
write.csv(lasso_test2 , "Restate_Fit_2011s_LASSO.csv", row.names = FALSE)



# Prediction Model 3 (early 2000s)
corp <- readRDS("corp.rds")
test <- read_csv("Restate_int_test.csv")
train <- read_csv("Restate_int_train.csv")
readability <- read_csv("readability.csv")
linking <- read_csv("linking.csv")
read_final <- readability %>% left_join(linking, by = c("CIK" = "cik"))
read_final <- read_final %>% select(-CIK,-Form,-FLAG) 
read_final <- read_final %>% mutate(year = year(ymd(FDATE)))
read_final <- read_final %>% select(-FDATE)

print(head(read_final))


#obtaining regsic
block <- select(test,c(gvkey,year,Filing)) %>% rbind((select(train,c(gvkey,year,Filing))))
df_SIC <- select(read.csv("sic.csv"), c(gvkey,fyear,sic))
block2 <- left_join(block,df_SIC,by = c("gvkey", "year" = "fyear")) 
block2 <- block2 %>% filter(!is.na(block2$sic))

industry_class <- block2 %>%
  mutate(Filing=paste0(Filing, ".txt")) %>%
  rename(document=Filing, regsic = sic) %>%
  mutate(industry = case_when(
    regsic >=0100 & regsic <= 0999 ~ "Agriculture",
    regsic >=1000 & regsic <= 1499 ~ "Mining",
    regsic >=1500 & regsic <= 1799 ~ "Construction",
    regsic >=2000 & regsic <= 3999 ~ "Manufacturing",
    regsic >=4000 & regsic <= 4999 ~ "Utilities",
    regsic >=5000 & regsic <= 5199 ~ "Wholesale Trade",
    regsic >=5200 & regsic <= 5999 ~ "Retail Trade",
    regsic >=6000 & regsic <= 6799 ~ "Finance",
    regsic >=7000 & regsic <= 8999 ~ "Services",
    regsic >=9100 & regsic <= 9999 ~ "Public Admin" )) %>%
  group_by(document) %>%
  slice(1) %>%
  ungroup()

docs <- docnames(corp)
docs <- data.frame(document=docs)
docs <- docs %>% left_join(industry_class)
docvars(corp, field="industry") <- docs$industry

#Early model params
#log of bullets
bulletpoints <- as.data.frame(sapply(corp, FUN = function(x) log(str_count(string = x, pattern = "[\u2022,\u2023,\u25E6,\u2043,\u2219]")) + 1))
is.na(bulletpoints) <- sapply(bulletpoints, is.infinite)
bulletpoints[is.na(bulletpoints)] <- 0
bulletpoints <- cbind(Filing = rownames(bulletpoints), bulletpoints)
rownames(bulletpoints) <- 1:nrow(bulletpoints)

#length of cleaned file in charas
#what do you mean by cleaned file? is it remove spaces?
processedsize <- as.data.frame(sapply(corp, FUN = function(x) stri_length(x)))
processedsize <- cbind(Filing = rownames(processedsize), processedsize)
rownames(processedsize) <- 1:nrow(processedsize)

#done, are there intentional misstatements made in the doc?
irregular_patterns <- '... fraud* ...|... irregular* ...|... materially false and misleading ...|... violat* of federal securities laws ...
|... violat* securities exchange act ...|... sec ... investigat* ...|... investigat* ... sec ...|... securities and exchange commission ...
investigat* ...|... investigat* ... securities and exchange commission ...|... doj ... investigat* ...|... investigat* ... doj ...
|... department of justice ... investigat* ...|... investigat* ... department of justice ...|... attorney general ... investigat* ...|...
investigat* ... attorney general ...|... u*s* attorney ... investigat* ...|... investigat* ... u*s* attorney ...|
... forensic account* ...|... forensic investigat* ...|... independent* ... investigat* ...|... investigat* ... independent ...|
... retain* ... special legal counsel ...|... audit committee ... retain* ...|... retain*
  ... audit committee ...|... audit committee ... investigat* ...|... investigat* ... audit
committee ...|... former independent auditors ...|... forensic or other account* ...|... retain* ... independent legal counsel ...'

intentional_mis <- as.data.frame(sapply(corp, FUN = function(x) if(str_count(string = x, pattern = irregular_patterns)>0) 1 else 0))
intentional_mis <- cbind(Filing = rownames(intentional_mis), intentional_mis)
rownames(intentional_mis) <- 1:nrow(intentional_mis)

#mean sent length
sentlen_mean <- as.data.frame(sapply(corp, FUN = function(x) mean(stri_length(stri_split_boundaries(paste(x, collapse=". "), type = "sentence")[[1]]))))
sentlen_mean <- cbind(Filing = rownames(sentlen_mean), sentlen_mean)
rownames(sentlen_mean) <- 1:nrow(sentlen_mean)

#stdev sent length
sentlen_stdev <- as.data.frame(sapply(corp, FUN = function(x) sd(stri_length(stri_split_boundaries(paste(x, collapse=". "), type = "sentence")[[1]]),na.rm = TRUE)))
sentlen_stdev <- cbind(Filing = rownames(sentlen_stdev), sentlen_stdev)
rownames(sentlen_stdev) <- 1:nrow(sentlen_stdev)

#stdev word length
#may need to double check if this is correct
wordlen_stdev <- as.data.frame(sapply(corp, FUN = function(x) sd(stri_length(stri_split_boundaries(paste(x, collapse=" "), type = "word", skip_word_none = TRUE)[[1]]),na.rm = TRUE)))
wordlen_stdev <- cbind(Filing = rownames(wordlen_stdev), wordlen_stdev)
rownames(wordlen_stdev) <- 1:nrow(wordlen_stdev)

#stdev para length
#double check if correct.
paralen_stdev <- as.data.frame(sapply(corp, FUN = function(x) sd(stri_length(stri_split_boundaries(paste(x, collapse="\n"), type = "line")[[1]]),na.rm = TRUE)))
paralen_stdev <- cbind(Filing = rownames(paralen_stdev), paralen_stdev)
rownames(paralen_stdev) <- 1:nrow(paralen_stdev)

#no. of exc points
exclaimation_points <- as.data.frame(sapply(corp, FUN = function(x) str_count(string = x, pattern = "!")))
exclaimation_points <- cbind(Filing = rownames(exclaimation_points), exclaimation_points)
rownames(exclaimation_points) <- 1:nrow(exclaimation_points)

#no. of qn marks
questionmarks <- as.data.frame(sapply(corp, FUN = function(x) str_count(string = x, pattern = "\\?")))
questionmarks <- cbind(Filing = rownames(questionmarks), questionmarks)
rownames(questionmarks) <- 1:nrow(questionmarks)

#no. of all cap words
allcaps <- as.data.frame(sapply(corp, FUN = function(x) str_count(string = x, pattern = "\\b[A-Z][A-Z]+\\b")))
allcaps <- cbind(Filing = rownames(allcaps), allcaps)
rownames(allcaps) <- 1:nrow(allcaps)

#left join and read to rds
early2000s <- bulletpoints %>% left_join(alltags) %>% left_join(processedsize) %>% left_join(intentional_mis) %>% left_join(sentlen_mean) %>% 
  left_join(sentlen_stdev) %>% left_join(wordlen_stdev) %>% left_join(paralen_stdev) %>% left_join(exclaimation_points) %>% left_join(questionmarks) %>%
  left_join(allcaps) %>% left_join(read_final)
colnames(early2000s) <- c("Filing","bullets","alltags","charactercount","intentional_mis","sentlen_mean","sentlen_stdev","wordlen_stdev","paralen_stdev","exclaimations","questionmarks","allcaps")


#create the merge and the model
early2000s <- readRDS("early2000s.rds")
early2000s$Filing <- gsub(".txt","",early2000s$Filing) 
early2000s <- early2000s %>% select(-alltags, -charactercount)

read_final <- read_final %>% mutate(gvkey = as.double(gvkey)) 
read_final2 <- read_final[!duplicated(read_final[c("gvkey","year")], fromLast = FALSE), ] %>% na.omit()
read_final2 <- read_final2 %>% select(-LM_Master_Dictionary)

train_2000 <- train %>% left_join(early2000s, by = "Filing") 
train_2000 <- train_2000 %>% left_join(read_final2, by = c("gvkey","year"))
train_2000[is.na(train_2000)] <- 0

equation_2000 <- as.formula("Restate_Int ~ bullets + intentional_mis + sentlen_mean + sentlen_stdev + wordlen_stdev + paralen_stdev + 
  exclaimations + questionmarks + allcaps + Coleman_Liau_Index + Gunning_Fog_Index + averageWordsPerSentence + CharCount + WordCount + FinTerms_Negative +  
  FinTerms_Positive")

fit_2000 <- glm(equation_2000, data = train_2000,family = "binomial")
summary(fit_2000)

test_2000 <- left_join(test,early2000s,by = 'Filing')
test_2000 <- test_2000 %>% left_join(read_final2, by = c("gvkey","year"))
test_2000[is.na(test_2000)]<- 0

#prediction
pred3 <- predict(fit_2000, test_2000, type = "response")
prediction <- data.frame(pred3)

#running unselected mutate model
kaggle_fit_2000s <- mutate(test_2000, Restate_Int = pred3)
kaggle_fit_2000s <- kaggle_fit_2000s %>% select(gvkey, Restate_Int)
write_csv(kaggle_fit_2000s, "Restate_Fit_2000s.csv")

#set test default to 0
test_2000<- test_2000%>% mutate(Restate_Int = 0)

# LASSO for 2000s Model
x_model3 <- model.matrix(equation_2000, data= train_2000)[,-1]
y_model3 <- model.frame(equation_2000, data= train_2000)[,"Restate_Int"]

set.seed(1)

cvfit3 <- cv.glmnet(x= x_model3  ,  
                    y = y_model3,
                    family = "binomial" ,
                    alpha = 1 ,
                    type.measure = "auc" )
plot(cvfit3)
coef(cvfit3, s = 'lambda.min') 
coefplot(cvfit3, lambda='lambda.min', sort='magnitude')

lasso_model3 <- model.matrix(equation_2000, data=test_2000)[,-1]
lasso_pred3 <- predict(cvfit3,lasso_model3, type="response", s = 'lambda.min')
lasso_pred3 <- data.frame(lasso_pred3)

lasso_test3 <- data.frame('gvkey' = test_2000$gvkey, "Restate_Int" = lasso_pred3)
lasso_test3 <- lasso_test3 %>% rename(Restate_Int = "lambda.min")
write.csv(lasso_test3 , "Restate_Fit_2000s_LASSO.csv", row.names = FALSE)



#Prediction Model 4 (BCE Model)

#joining readability and linking csv
readability <- readability[which(readability$Form=='10-K'), ]
linkreadability <- left_join(linking, readability, by=c('cik'='CIK'))
linkreadability$year <- substr(linkreadability$FDATE, 0, 4)
linkreadability$gvkey <- substr(linkreadability$gvkey, 3,7)

#construct train data set
early2000s <- readRDS('early2000s.rds')
early2000s$Filing = str_sub(early2000s$Filing, 1, str_length(early2000s$Filing)-4)
train_GICS_early2000s <- left_join(train_GICS, early2000s, by=c("Filing"))
doc_topics <- readRDS('doc_topics.rds')
doc_topics$Filing = str_sub(doc_topics$Filing, 1, str_length(doc_topics$Filing)-4)
train_GICS_topics <- left_join(train_GICS_early2000s, doc_topics, by=c("Filing"))
train_GICS_topics$gvkey <- as.character(train_GICS_topics$gvkey)
train_GICS_topics$year <- as.character(train_GICS_topics$year)
train_GICS_bce <- left_join(train_GICS_topics, linkreadability, by=c("gvkey", "year"))

# By running the code below, remove 10 extra observations due to duplicate gvkey & year
train_GICS_bce <- train_GICS_bce %>% distinct(gvkey, year, .keep_all = TRUE)

#Construct bCE train set
train_GICS_bce1<- train_GICS_bce%>%
  mutate(log_at = log(at))%>% # log of total asset
  mutate(wc= (act-che)-(lct-dlc))%>% # working for rsst accruals
  mutate(nco= (at-act-ivao)-(lt-lct-dltt))%>% # working for rsst accruals
  mutate(financing = (ivst+ivao)-(dltt+dlc+pstk))%>% # orking for rsst accruals
  mutate(lag_wc= (lag(act)-lag(che))-(lag(lct)-lag(dlc)))%>% # working for rsst accruals
  mutate(lag_nco= (lag(at)-lag(act)-lag(ivao))-(lag(lt)-lag(lct)-lag(dltt)))%>% # working for rsst accruals
  mutate(lag_fin = (lag(ivst)+lag(ivao))-(lag(dltt)+lag(dlc)+lag(pstk)))%>% # working for rsst accruals
  mutate(rsst_acc = (lag_wc+lag_nco+lag_fin)/((lag(at)+lag(at,2))/2)) %>% # rsst accruals
  mutate(chg_rect = (rect-lag(rect)) / (at + lag(at))/2) %>% # change in receivables 
  mutate(chg_inv = (invt-lag(invt)) / (at + lag(at))/2) %>% # change in inventory
  mutate(soft_assets = (at - che - ppent) / ((lag(at)+lag(at,2))/2)) %>% # soft assets
  mutate(pct_chg_cashsales = (sale - (rect-lag(rect))) - (lag(sale) - (lag(rect) - lag(rect, k=2))) / (lag(sale) - (lag(rect) - lag(rect, k=2)))) %>% # cash sales
  mutate(roa = ib / (at+lag(at))/2) %>% # working for chg_roa
  mutate(lag_roa = lag(ib) / ((lag(at)+lag(at,2))/2)) %>% # working for chg_roa 
  mutate(chg_roa = roa-lag_roa) %>% # working for chg_roa
  mutate(issuance = ifelse(sstk>0|dltis>0, 1, 0))%>% # issuance of debt or equity
  mutate(oplease_dum = ifelse(mrc1>0|mrc2>0|mrc3>0|mrc4>0|mrc5>0,1,0)) %>% # operating lease indicator
  mutate(bm = ceq/csho*prcc_f)%>% # book to market 
  mutate(mgr = ifelse(aqp>0 , 1, 0))%>% # merger 1 for successful merger, 0 or neg for failed
  mutate(big_n_aud = ifelse(au == 4|au == 5|au == 6|au == 7, 1, 0))%>%   # big_n_auditors
  mutate(mid_n_aud = ifelse(au == 11|au == 17|au == 21, 1, 0)) %>% # mid tier auditors
  mutate(cffin = (fincf/(at + lag(at))/2))%>% # cffin
  mutate(exfin =ifelse((oancf-(lag(capx,3)+lag(capx,2)+lag(capx))/ 3) /(act) < -0.5, 1, 0)) %>% # exfinn
  mutate(restruct = ifelse(rcp>0 , 1, 0))%>% # restructuring
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), 0))) %>%
  ungroup()

#construct test data set
test_GICS_early2000s <- left_join(testing_GICS, early2000s, by=c("Filing"))
test_GICS_topics <- left_join(test_GICS_early2000s, doc_topics, by=c("Filing"))
test_GICS_topics$gvkey <- as.character(test_GICS_topics$gvkey)
test_GICS_topics$year <- as.character(test_GICS_topics$year)
test_GICS_bce <- left_join(test_GICS_topics, linkreadability, by=c("gvkey", "year"))

#Construct BCE test set
testing_GICS_bce1 <- test_GICS_bce%>%
  mutate(log_at = log(at))%>% #log of total asset
  mutate(wc= (act-che)-(lct-dlc))%>% #working for rsst accruals
  mutate(nco= (at-act-ivao)-(lt-lct-dltt))%>% #working for rsst accruals
  mutate(financing = (ivst+ivao)-(dltt+dlc+pstk))%>% #working for rsst accruals
  mutate(lag_wc= (lag(act)-lag(che))-(lag(lct)-lag(dlc)))%>% #working for rsst accruals
  mutate(lag_nco= (lag(at)-lag(act)-lag(ivao))-(lag(lt)-lag(lct)-lag(dltt)))%>% #working for rsst accruals
  mutate(lag_fin = (lag(ivst)+lag(ivao))-(lag(dltt)+lag(dlc)+lag(pstk)))%>% #working for rsst accruals
  mutate(rsst_acc = (lag_wc+lag_nco+lag_fin)/((lag(at)+lag(at,2))/2)) %>% #rsst accruals
  mutate(chg_rect = (rect-lag(rect)) / (at + lag(at))/2) %>% #change in receivables 
  mutate(chg_inv = (invt-lag(invt)) / (at + lag(at))/2) %>% #change in inventory
  mutate(soft_assets = (at - che - ppent) / ((lag(at)+lag(at,2))/2)) %>% #soft assets
  mutate(pct_chg_cashsales = (sale - (rect-lag(rect))) - (lag(sale) - (lag(rect) - lag(rect, k=2))) / (lag(sale) - (lag(rect) - lag(rect, k=2)))) %>% #cash sales
  mutate(roa = ib / (at+lag(at))/2) %>% #working for chg_roa
  mutate(lag_roa = lag(ib) / ((lag(at)+lag(at,2))/2)) %>% #working for chg_roa 
  mutate(chg_roa = roa-lag_roa) %>% #working for chg_roa
  mutate(issuance = ifelse(sstk>0|dltis>0, 1, 0))%>% #issuance of debt or equity
  mutate(oplease_dum = ifelse(mrc1>0|mrc2>0|mrc3>0|mrc4>0|mrc5>0,1,0)) %>% #operating lease indicator
  mutate(bm = ceq/csho*prcc_f)%>% #book to market 
  mutate(mgr = ifelse(aqp>0 , 1, 0))%>% #merger 1 for successful merger, 0 or neg for failed
  mutate(big_n_aud = ifelse(au== 4 | au== 5| au==6|au==7, 1, 0))%>%   #big_n_auditors
  mutate(mid_n_aud = ifelse(au == 11|au == 17|au == 21, 1, 0)) %>% #mid tier auditors
  mutate(cffin = (fincf/(at + lag(at))/2))%>% #cffin
  mutate(exfin =ifelse((oancf-(lag(capx,3)+lag(capx,2)+lag(capx))/ 3) /(act) < -0.5, 1, 0)) %>% #exfinn
  mutate(restruct = ifelse(rcp>0 , 1, 0))%>% #restructuring
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), 0))) %>%
  ungroup()

# By running the code below, remove 5 extra observations due to duplicate gvkey & year
testing_GICS_bce1 <- testing_GICS_bce1 %>% distinct(gvkey, year, .keep_all = TRUE) %>% mutate(Restate_Int = 0)

#construct the equation
eq_bce1 <- as.formula("Restate_Int ~ log_at + rsst_acc + chg_rect + chg_inv + soft_assets + pct_chg_cashsales + chg_roa + issuance + oplease_dum + bm + mgr + big_n_aud + mid_n_aud + cffin + exfin + restruct + bullets + alltags + sentlen_mean + wordlen_stdev + paralen_stdev + sentlen_stdev + Coleman_Liau_Index + Gunning_Fog_Index + averageWordsPerSentence + CharCount + WordCount + FinTerms_Negative + FinTerms_Positive + allcaps + exclaimations + questionmarks + topic1_weight + topic2_weight + topic3_weight + topic4_weight + topic5_weight + topic6_weight + topic7_weight + topic8_weight + topic9_weight")

pred.model4 <- glm(eq_bce1, data=train_GICS_bce1, family = binomial)
summary(pred.model4)

pred4 <- predict(pred.model4, testing_GICS_bce1, type = "response")
prediction4 <- data.frame(pred4)

# To upload to kaggle
kaggle_fit_bce <- mutate(testing_GICS_bce1, Restate_Int = pred4)
kaggle_fit_bce <- kaggle_fit_bce %>% select(gvkey, Restate_Int)
write_csv(kaggle_fit_bce, "Restate_Fit_bce.csv")  

#LASSO for BCE
x_model4 <- model.matrix(eq_bce1, data= train_GICS_bce1)[,-1]
y_model4 <- model.frame(eq_bce1, data= train_GICS_bce1)[,"Restate_Int"]

set.seed(1)

cvfit4 <- cv.glmnet(x= x_model4  ,  
                    y = y_model4,
                    family = "binomial" ,
                    alpha = 1 ,
                    type.measure = "auc" )
plot(cvfit4)
coef(cvfit4, s = 'lambda.min') 
coefplot(cvfit4, lambda='lambda.min', sort='magnitude')


lasso_model4 <- model.matrix(eq_bce1, data=testing_GICS_bce1)[,-1]
lasso_pred4 <- predict(cvfit4,lasso_model4, type="response", s = 'lambda.min')
lasso_pred4 <- data.frame(lasso_pred4)

lasso_test4 <- data.frame('gvkey' = testing_GICS_bce1$gvkey, "Restate_Int" = lasso_pred4)
lasso_test4 <- lasso_test4 %>% rename(Restate_Int = "lambda.min")
write.csv(lasso_test4 , "Restate_bce_LASSO.csv", row.names = FALSE)

#XGBoost for BCE

# Model setup
library(xgboost)

xvals <- model.matrix(eq_bce1, data=testing_GICS_bce1)[,-1]
yvals <- model.frame(eq_bce1, data=testing_GICS_bce1)[,"Restate_Int"]

set.seed(1)
# These params take some work to pin down
params <- list(max_depth=5,
               eta=0.2,
               gamma=10,
               min_child_weight = 20,
               objective = "binary:logistic")

xgbCV <- xgb.cv(params=params,
                data=x_model4,
                label=as.numeric(y_model4==1),
                nrounds=100,
                eval_metric="auc",
                nfold=10,
                stratified=TRUE)

numTrees <- min(which(xgbCV$evaluation_log$test_auc_mean == 
                        max(xgbCV$evaluation_log$test_auc_mean)))

fit6 <- xgboost(params=params,
                data = x_model4,
                label = as.numeric(y_model4==1),
                nrounds = numTrees,
                eval_metric="auc")

# Display relative importance of variables for prediction
xgb.train.data = xgb.DMatrix(x_model4, label = y_model4, missing = NA)
xgb.test.data = xgb.DMatrix(xvals, label = yvals, missing = NA)
col_names = attr(xgb.train.data, ".Dimnames")[[2]]
imp = xgb.importance(col_names, fit6)
print("Model Importance")
xgb.plot.importance(imp)

# Usual AUC calculation
pred_XGB <- predict(fit6, xvals, type="response")
pred_XGB <- data.frame(pred_XGB)

XGB_test <- data.frame("gvkey" = testing_GICS_bce1$gvkey, "Restate_Int" = pred_XGB)
XGB_test <- XGB_test %>% rename(Restate_Int = "pred_XGB")
write.csv(XGB_test , "Restate_bce_XGB.csv", row.names = FALSE)



# Model 5: Simple Ensemble

# Ensemble: 1990s Model (Training)
pred1_train <- predict(fit_1990s, train_GICS_Model1, type = "response")
prediction1_train <- data.frame(pred1_train)

kaggle_fit_1990s_es <- mutate(train_GICS_Model1, Restate_Int = pred1_train)
kaggle_fit_1990s_es <- kaggle_fit_1990s_es %>% select(gvkey, year, Restate_Int)
#write_csv(kaggle_fit_1990s_es, "Restate_Fit_1990s_es.csv")

# Ensemble: 1990s Model LASSO (Training)
lasso_model_es <- model.matrix(equation_1990, data=train_GICS_Model1)[,-1]
lasso_pred_es <- predict(cvfit1,lasso_model_es, type="response", s = 'lambda.min')
lasso_pred_es <- data.frame(lasso_pred_es)

lasso_train1<- data.frame('gvkey' = train_GICS_Model1$gvkey,'year'= train_GICS_Model1$year, "Restate_Int" = lasso_pred_es)
lasso_train1 <- lasso_train1 %>% rename(Restate_Int = "lambda.min")
#write.csv(lasso_train1 , "Restate_Fit_1990s_LASSO_es.csv", row.names = FALSE)

#Ensemble: 2011s Model (Training)
pred2_train <- predict(fit_2011s, train_GICS_Model2, type = "response")
prediction2_train <- data.frame(pred2_train)

kaggle_fit_2011s_es <- mutate(train_GICS_Model2, Restate_Int = pred2_train)
kaggle_fit_2011s_es <- kaggle_fit_2011s_es %>% select(gvkey, year, Restate_Int)
#write_csv(kaggle_fit_2011s_es, "Restate_Fit_2011s_es.csv")

# Ensemble: 2011s Model LASSO (Training)
lasso_model2_es <- model.matrix(equation_2011, data=train_GICS_Model2)[,-1]
lasso_pred2_es <- predict(cvfit1,lasso_model2_es, type="response", s = 'lambda.min')
lasso_pred2_es <- data.frame(lasso_pred2_es)

lasso_train2<- data.frame('gvkey' = train_GICS_Model2$gvkey,'year'= train_GICS_Model2$year, "Restate_Int" = lasso_pred2_es)
lasso_train2 <- lasso_train2 %>% rename(Restate_Int = "lambda.min")
#write.csv(lasso_train2 , "Restate_Fit_2011s_LASSO_es.csv", row.names = FALSE)

#Ensemble: 2000s Model (Training)
pred3_train <- predict(fit_2000, train_2000, type = "response")
prediction3 <- data.frame(pred3_train)

kaggle_fit_2000s_es <- mutate(train_2000, Restate_Int = pred3_train)
kaggle_fit_2000s_es <- kaggle_fit_2000s_es %>% select(gvkey, year, Restate_Int)
# write_csv(kaggle_fit_2000s_es, "Restate_Fit_2000s_es.csv")

#Ensemble: 2000s Model LASSO (Training)
lasso_model3_es <- model.matrix(equation_2000, data=train_2000)[,-1]
lasso_pred3_es <- predict(cvfit3,lasso_model3_es, type="response", s = 'lambda.min')
lasso_pred3_es <- data.frame(lasso_pred3_es)
 
lasso_train3 <- data.frame('gvkey' = train_2000$gvkey, 'year'= train_2000$year, "Restate_Int" = lasso_pred3_es)
lasso_train3 <- lasso_train3 %>% rename(Restate_Int = "lambda.min")
# write.csv(lasso_train3 , "Restate_Fit_2000s_LASSO_es.csv", row.names = FALSE)

#Ensemble: BCE Model (Training)
predXGB_train <- predict(fit6, x_model4, type="response")
predXGB_train <- data.frame(predXGB_train)

kaggle_XGB_train <- mutate(train_GICS_bce1, Restate_Int = predXGB_train)
kaggle_XGB_train_sel <- kaggle_XGB_train %>% select(gvkey, year, Restate_Int)

#Rename Prediction Value for Ensemble (Training)
train_model5 <- train %>% select(-c(Filing, Date))
kaggle_fit_1990s_es <- rename(kaggle_fit_1990s_es, pred_1990s = "Restate_Int")
lasso_train1 <- rename(lasso_train1, pred_1990s_lasso = "Restate_Int")
kaggle_fit_2011s_es <- rename(kaggle_fit_2011s_es, pred_2011s = "Restate_Int")
lasso_train2 <- rename(lasso_train2, pred_2011s_lasso = "Restate_Int")
kaggle_fit_2000s_es <- rename(kaggle_fit_2000s_es, pred_2000s = "Restate_Int")
lasso_train3 <- rename(lasso_train3, pred_2000s_lasso = "Restate_Int")
kaggle_XGB_train_sel$gvkey <- as.numeric(kaggle_XGB_train_sel$gvkey)
kaggle_XGB_train_sel$year <- as.numeric(kaggle_XGB_train_sel$year)
kaggle_XGB_train_sel <- data.frame('gvkey' = kaggle_XGB_train_sel$gvkey, 'year'= kaggle_XGB_train_sel$year,'pred_bce' = kaggle_XGB_train_sel$Restate_Int)
kaggle_XGB_train_sel <- rename(kaggle_XGB_train_sel, pred_bce = 'predXGB_train')

# Combine all the dfs together for the models used
train_df <- left_join(train, kaggle_fit_1990s_es, by = c("gvkey","year"))
train_df <- left_join(train_df, lasso_train1, by = c("gvkey","year"))
train_df <- left_join(train_df, kaggle_fit_2011s_es, by = c("gvkey","year"))
train_df <- left_join(train_df, lasso_train2, by = c("gvkey","year"))
train_df <- left_join(train_df, kaggle_fit_2000s_es, by = c("gvkey", "year"))
# train_df <- left_join(train_df, lasso_train3, by = c("gvkey", "year"))
train_df <- left_join(train_df, kaggle_XGB_train_sel, by = c("gvkey","year"))
train_df2 <- train_df %>% mutate(Restate_Int = ifelse(Restate_Int==0,0,1))

# Delete the missing variables
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
train_df <- delete.na(train_df, 2)
train_df2 <- delete.na(train_df2, 2)

# Test data
kaggle_fit_1990s <- rename(kaggle_fit_1990s, pred_1990s = "Restate_Int")
lasso_test1 <- rename(lasso_test1, pred_1990s_lasso = "Restate_Int")
kaggle_fit_2011s <- rename(kaggle_fit_2011s, pred_2011s = "Restate_Int")
lasso_test2 <- rename(lasso_test2, pred_2011s_lasso = "Restate_Int")
kaggle_fit_2000s <- rename(kaggle_fit_2000s, pred_2000s = "Restate_Int")
lasso_test3 <- rename(lasso_test3, pred_2000s_lasso = "Restate_Int")
XGB_test <- rename(XGB_test, pred_bce = "Restate_Int")
XGB_test$gvkey = as.numeric(XGB_test$gvkey)

#Combine all the dfs together for the models used
test_df <- left_join(kaggle_fit_1990s, lasso_test1, by = c("gvkey"))
test_df <- left_join(test_df, kaggle_fit_2011s, by = c("gvkey"))
test_df <- left_join(test_df, lasso_test2, by = c("gvkey"))
test_df <- left_join(test_df, kaggle_fit_2000s, by = c("gvkey"))
# test_df <- left_join(test_df, lasso_test3, by = c("gvkey"))
test_df <- test_df %>% left_join(XGB_test, by = c("gvkey")) %>% mutate(Restate_Int = 0)


library(xgboost)

# Prep data
train_x <- model.matrix(Restate_Int ~ ., data=train_df2[,-1])[,-1][,-1]
train_y <- model.frame(Restate_Int ~ ., data=train_df2)[,"Restate_Int"]

set.seed(468435)  #for reproducibility
xgbCV <- xgb.cv(max_depth=5, eta=0.10, gamma=5, min_child_weight = 4,
                subsample = 0.5, objective = "binary:logistic", data=train_x,
                label=train_y, nrounds=100, eval_metric="auc", nfold=10,
                stratified=TRUE, verbose=0)

fit_ens <- xgboost(params=xgbCV$params, data = train_x, label = train_y,
                   nrounds = which.max(xgbCV$evaluation_log$test_auc_mean),
                   verbose = 0)

library(yardstick)
all_x <- model.matrix(Restate_Int ~ ., data=test_df[,-1])[,-1]
pred_ensemble <- predict(fit_ens, all_x, type="response")

prediction_ensemble <- data.frame(pred_ensemble)
pred_es <- data.frame('gvkey' = test_df$gvkey, "Restate_Int" = pred_ensemble)
write.csv(pred_es , "Restate_Fit_Ensemble.csv", row.names = FALSE)

xgb.train.data = xgb.DMatrix(train_x, label = train_y, missing = NA)
col_names = attr(xgb.train.data, ".Dimnames")[[2]]
imp = xgb.importance(col_names, fit_ens)
# Variable importance
xgb.plot.importance(imp)
