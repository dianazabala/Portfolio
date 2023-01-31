#########################################################
# Estimating Likelihood of Readmissions for CABG patients
# Diana Zabala
# December 15, 2022
#########################################################

#Set working directory
setwd("C:/Users/Diana/Documents/Projects/Readmissions")

#Read in dataset
claims <- read.csv(file="C:/Users/Diana/Downloads/USN_claims_test_data.csv", header=TRUE)

#1) Subset the dataset to the population at risk, admissions for patients undergoing isolated 
#coronary artery bypass grafts (CABG), identified by procedure codes of 3610, 3611, 3612,
#3613, 3614, 3615, 3616, 3617, or 3619.

library(tidyverse) #for data wrangling

#create a vector with the relevant (CABG) codes
cabg_procedure_codes <- c(3610, 3611, 3612, 3613, 3614, 3615, 3616, 3617, 3619)

#subset to only the CABG procedure codes. Keep any instance within procedure1-procedure5
claims_cabg <- claims %>% 
  filter( claims[,'procedure1'] %in% cabg_procedure_codes | 
            claims[,'procedure2'] %in% cabg_procedure_codes |
            claims[,'procedure3'] %in% cabg_procedure_codes |
            claims[,'procedure4'] %in% cabg_procedure_codes |
            claims[,'procedure5'] %in% cabg_procedure_codes
          ) 

#2) Identify comorbidities present in the diagnosis codes using the Charlson index.

######Note: Using Charlson index from R package "comorbidity"
library(comorbidity)

###### create data frames that indicate the comorbidities in diagnosis1-diagnosis5 for each
#patient ID. Creates 5 data frames in total, one for each diagnosis code column.
charlson_df1 <- comorbidity(
  x = claims_cabg,
  id = "patientId",
  code = "diagnosis1",
  map = "charlson_icd9_quan",
  assign0 = FALSE
)

charlson_df2 <- comorbidity(
  x = claims_cabg,
  id = "patientId",
  code = "diagnosis2",
  map = "charlson_icd9_quan",
  assign0 = FALSE
)

charlson_df3 <- comorbidity(
  x = claims_cabg,
  id = "patientId",
  code = "diagnosis3",
  map = "charlson_icd9_quan",
  assign0 = FALSE
)

charlson_df4 <- comorbidity(
  x = claims_cabg,
  id = "patientId",
  code = "diagnosis4",
  map = "charlson_icd9_quan",
  assign0 = FALSE
)

charlson_df5 <- comorbidity(
  x = claims_cabg,
  id = "patientId",
  code = "diagnosis5",
  map = "charlson_icd9_quan",
  assign0 = FALSE
)
######

#put all Charlson data frames into a list and merge the data frames together
charlson_list <- list(charlson_df1, charlson_df2, charlson_df3, charlson_df4, charlson_df5)

charlson_merged <- charlson_list %>% reduce(full_join, by='patientId')

#create a data frame that captures the comorbidity information across diagnosis1-diagnosis5:
#for each patient ID, create a 1/0 indicator for each of the 18 conditions with a value 
#of 1 if the condition is present for that patient. 
#Also create comorbs_tot variable to provide the total number of comorbidities for each patient.
charlson_patid <- charlson_merged %>% 
  rowwise() %>%
  mutate(ind_mi = max(across(starts_with("mi")), na.rm = T),
         ind_chf = max(across(starts_with("chf")), na.rm = T),
         ind_pvd = max(across(starts_with("pvd")), na.rm = T),
         ind_cevd = max(across(starts_with("cevd")), na.rm = T),
         ind_dementia = max(across(starts_with("dementia")), na.rm = T),
         ind_cpd = max(across(starts_with("cpd")), na.rm = T),
         ind_rheumd = max(across(starts_with("rheumd")), na.rm = T),
         ind_pud = max(across(starts_with("pud")), na.rm = T),
         ind_mld = max(across(starts_with("mld")), na.rm = T),
         ind_diab = max(across(starts_with("diab")), na.rm = T),
         ind_diabwc = max(across(starts_with("diabwc")), na.rm = T),
         ind_hp = max(across(starts_with("hp")), na.rm = T),
         ind_rend = max(across(starts_with("rend")), na.rm = T),
         ind_canc = max(across(starts_with("canc")), na.rm = T),
         ind_msld = max(across(starts_with("msld")), na.rm = T),
         ind_metacanc = max(across(starts_with("metacanc")), na.rm = T),
         ind_aids = max(across(starts_with("aids")), na.rm = T),
         comorbs_tot = sum(across(starts_with("ind")), na.rm = T)
         )

#Subset the data frame to the patient ID, the comorbidity indicator variables,
#and the total number of comorbidities for each patient.

log_print("create a dataframe with unique patient ID, comorbidity indicator variables,
#and total number of comorbidities for each patient")

comord_inds <- charlson_patid %>% 
  select(patientId, starts_with("ind"), comorbs_tot)

n_distinct(comord_inds$patientId) #make sure there are no duplicates - looks good

#output comorbidity information as csv file
write.csv(comord_inds, "comorbidities.csv", row.names = FALSE)



#3) Identify whether each admission involved a readmission. A readmission here is defined as a
#subsequent hospitalization for the same patientId within 30 days of the index admission.

library(lubridate) #for working with date variables

#first convert admitDate variable to proper date format
claims_cabg$admitDate2 <- dmy(claims_cabg$admitDate)

#sort columns by patient id and admit date
claims_cabg <- arrange(claims_cabg, patientId, admitDate2)

#identify which patient ids had multiple hospital admissions and create date columns 
#for each admission.
claims_cabg_dupes <- claims_cabg %>% 
  group_by(patientId) %>%
  mutate(id = row_number()) %>% #create an admission id for each patient id
  mutate(maxid = max(id)) %>%  #indicates how many total admissions a patient had
  mutate(admit1 = case_when(id==1 ~ admitDate2),
         admit2 = case_when(id==2 ~ admitDate2),
         admit3 = case_when(id==3 ~ admitDate2),
         admit4 = case_when(id==4 ~ admitDate2)
         ) %>% 
  ungroup()

print(table(claims_cabg_dupes$id)) #patients had at most 4 hospital admissions

#create fully populated date variables for each admission id to be able to 
#compare time passed between admissions
claims_cabg_dupes2 <- claims_cabg_dupes %>% 
  mutate(admit1 = coalesce(case_when(id == 1 ~ admitDate2,
                                     id == 2 ~ lag(admitDate2),
                                     id == 3 ~ lag(admitDate2, n=2L),
                                     id == 4 ~ lag(admitDate2, n=3L)), admit1),
         admit2 = coalesce(case_when(id == 2 ~ admitDate2,
                                     id == 3 ~ lag(admitDate2),
                                     id == 4 ~ lag(admitDate2, n=2L)), admit2),
         admit3 = coalesce(case_when(id == 3 ~ admitDate2,
                                     id == 4 ~ lag(admitDate2)), admit3))

#determine how many days have passed since the previous admission
claims_cabg_dupes2$days_passed_1_2 <- claims_cabg_dupes2$admit2-claims_cabg_dupes2$admit1 #days between visits 1&2
claims_cabg_dupes2$days_passed_2_3 <- claims_cabg_dupes2$admit3-claims_cabg_dupes2$admit2 #days between visits 2&3
claims_cabg_dupes2$days_passed_3_4 <- claims_cabg_dupes2$admit4-claims_cabg_dupes2$admit3 #days between visits 3&4

#Create a readmission indicator if equal to or less than 30 days 
#passed between hospital visits
claims_cabg_dupes2$readmission <- ifelse(claims_cabg_dupes2$days_passed_1_2 <= 30, 1,
                                              ifelse(claims_cabg_dupes2$days_passed_2_3 <=30, 1,
                                                     ifelse(claims_cabg_dupes2$days_passed_3_4 <=30, 1, 0)))
#replace NA values with 0
claims_cabg_dupes2$readmission[is.na(claims_cabg_dupes2$readmission)] <- 0

print(table(claims_cabg_dupes2$readmission)) #108 readmissions

#create a dataframe that is unique by patient ID, keeping the row with the most recorded visits for each patient
#Include the number of days between hospital admission and an indicator for readmission
claims_cabg_unique <- claims_cabg_dupes2 %>% filter(id==maxid)
length(unique(claims_cabg_unique$patientId)) #1721 rows



### Create a dataset for modeling with claims and comorbidity info and missing values imputed ###

#combine CABG claims dataframe with comorbidity dataframe. Merge on patient ID.
unique_claims_comorbid <- full_join(claims_cabg_unique, comord_inds, by="patientId")

#check missingness for age variable.
print(table(unique_claims_comorbid$age, useNA = "always")) #206 missing age values. Will need to impute.

#find the median of age
median_age <- median(as.numeric(unique_claims_comorbid$age), na.rm=TRUE)
print(median_age) #67

#impute median age for patients with missing values
unique_claims_comorbid$age[is.na(unique_claims_comorbid$age)] <- median_age 

#output the cleaned and processed dataset that is ready for modeling
write.csv(unique_claims_comorbid, "USN_data_modeling.csv", row.names = FALSE)

#separate data into training and test sets with a random 70%/30% split.
train <- sample_frac(unique_claims_comorbid, size=0.7, replace=FALSE)
test <- anti_join(unique_claims_comorbid, train, by='patientId')

table(train$readmission)
72/(1133+72) # ~6% readmissions

#4) Specify and run a regression model that estimates the likelihood of readmission among patients admitted for CABG surgery. 
#Control for age, systolic blood pressure, and the number of comorbidities present in the admission record.

#model1
m1 <- glm(readmission~ 
            age + 
            systolic + 
            comorbs_tot +
            ahaId +
            ind_mi +
            ind_chf +
            ind_pvd +
            ind_cevd +
            ind_dementia +
            ind_cpd +
            ind_rheumd +
            ind_pud +
            ind_mld +
            ind_diab +
            ind_diabwc +
            ind_hp +
            ind_rend +
            ind_canc +
            ind_msld +
            ind_metacanc, 
            data = train, family = binomial(link = "logit"))

summary(m1)

####### check for multicollinearity issues

#check variance inflation factor (vif)
library(car)

#print(vif(m1))
#Error in vif.default(m1) : there are aliased coefficients in the model
#Error message occurs when there is multicollinearity -- will need to remove variables

#check correlations between input variables: age, systolic bp, hospital id, 
#comorbidity indicators, and number of comorbidities.
library(corrplot)

data_xvars <- unique_claims_comorbid[, c(2, 4, 15, 27:42, 44)] #excluded aids because there are no values of 1
cor_matrix <- cor(data_xvars, method=c("spearman"))
corrplot(cor_matrix, method='number', is.corr = F)
#plot of correlation matrix shows that ind_diab and ind_diabwc are highly correlated (0.79)
#ind_cancer and comorbs_tot are also correlated (0.67).

#Removing ind_diabwc and #ind_cancer variables.
#Retaining ind_diab and comorbs_tot variables.

#model2
m2 <- glm(readmission~ 
            age + #age
            systolic + #systolic blood pressure 
            comorbs_tot + #number of comorbidities
            ahaId +
            ind_mi +
            ind_chf +
            ind_pvd +
            ind_cevd +
            ind_dementia +
            ind_cpd +
            ind_rheumd +
            ind_pud +
            ind_mld +
            ind_diab +
            ind_hp +
            ind_rend +
            ind_msld +
            ind_metacanc, 
          data = train, family = binomial(link = "logit"))

summary(m2)

######## check VIF (multicollinearity) for model 2:
print(vif(m2)) ## all variance inflation factors are under 5. We can proceed with these variables.


print("Create a logistic regression model to predict readmission likelihood, controlling for age, systolic blood pressure, and number of comorbidities")

library(broom) #library allows tidy output for regression  model
m2_output <- tidy(m2)
print(m2_output)
write.csv(m2_output, "model_output_tidy.csv", row.names = FALSE)


####### Check for influential outliers
#cook's D > 4/n are deemed influential
cooksD <- cooks.distance(m2)
n <- 1205 #training sample size
influential <- cooksD[(cooksD > (4/n))]
print(names(influential)) #these are the influential patient ids

# Identify absolute standardized residual values >3 
standard_res <- abs(rstandard(m2))
outliers <- standard_res>3 
table(outliers) #none of them are TRUE, so no outliers

# Find observations which are BOTH outlier (std dev > 3) and highly influential
extreme <- intersect(influential, standard_res) 
print(extreme) #NONE -- no influential outliers

####### linearity of predictor variables with Model 2 logits
#check against generalized additive model (GAM)
library(mgcv)

m2_gam <- gam(readmission~ 
                age + #age
                systolic + #systolic blood pressure
                comorbs_tot + #number of comorbidities
                ahaId +
                ind_mi +
                ind_chf +
                ind_pvd +
                ind_cevd +
                ind_dementia +
                ind_cpd +
                ind_rheumd +
                ind_pud +
                ind_mld +
                ind_diab +
                ind_hp +
                ind_rend +
                ind_msld +
                ind_metacanc, 
              data = train, family = binomial(link = "logit"))

summary(m2_gam)

####### Test whether a logit model fits the data (linearity of predictors with logit)
#if test not significant, then logit model fits data
print(anova(m2, m2_gam, test="Chisq")) #not significant - good!

####### Large sample size
#as general rule, minimum of 10 observations per variable. There are 18 variables
#18*10=180. 1721>180 so assumption is satisfied.

####### Model 2 Concordance
library(survival)
print(concordance(m2)) # 78% concordant on TRAINING set

####### Obtain Model 2 predictions on test dataset (estimate likelihood of readmission for each patient as a probability)
test$p_hat <- predict(m2, type='response', newdata = test)
print(summary(test$p_hat))

####### Model 2 ROC and AUC on TEST set
library(PredictABEL)
PredictABEL::plotROC(test, "readmission", test$p_hat)
#AUC [95% CI] for the model 1 :  0.73 [ 0.636  -  0.825 ]

####### get K-S statistic
library(ROCR)
pred <- prediction(test$p_hat, test$readmission)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
print(c(KS, cutoffAtKS)) #0.4415323 0.0550838

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "blue")

######## get confusion matrix to determine sensitivity and specificity
library(caret)

cutoff <-  0.0550838 #optimal probability cutoff for 1's and 0's based on K-S

test$classified <- ifelse(test$p_hat >= cutoff, 1, 0)
confusionMatrix(factor(test$classified), factor(test$readmission), positive='1')
#           Reference
# Prediction   0   1
#          0 328   5
#          1 168  15

#Sensitivity : 0.70000       
#Specificity : 0.69153

cutoff2 <-  0.050 #trying to improve sensitivity
test$classified2 <- ifelse(test$p_hat >= cutoff2, 1, 0)
confusionMatrix(factor(test$classified2), factor(test$readmission), positive='1')

#output the dataset with predicted likelihood of readmission (p_hats) and 1/0 classifications for readmissions
write.csv(test, "test_predictedvals.csv", row.names = FALSE)

