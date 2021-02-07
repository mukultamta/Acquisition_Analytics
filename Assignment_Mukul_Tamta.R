######################### Assignment - Acquisition Analytics #########################
######################### Submitted by Mukul Tamta #########################################

# Loading libraries

library(dplyr)
library(ggplot2)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)

#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age")

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

# Assigning a unique ID for each prospect

bank_data$prospect_id <- seq.int(nrow(bank_data))


############################### LOGISTICS MODEL CREATION ################################

# Working on backup of bank_data so that bank_data dataset remains intact

bank_data_mod <- bank_data

# Removing binning.age" variable

bank_data_mod <- bank_data_mod[,-c(21)]

#creating dummy variables

bank_data_mod$response <- as.integer(bank_data_mod$response)

bank_data_mod <- dummy.data.frame(bank_data_mod)

bank_data_mod$response <- as.factor(ifelse(bank_data_mod$response == 1, "yes", "no"))


# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data_mod$response, SplitRatio = 0.70)

train <- bank_data_mod[split_indices, ]

test <- bank_data_mod[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

# Model creation
# Not considering 'duration' and 'prospect_id' for model creation

model_1 <- glm(response ~ .-duration -prospect_id, family = "binomial", data = train)

summary(model_1)


# Using stepwise algorithm for removing insignificant variables 

model_2 <- stepAIC(model_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

model_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
  maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
  contactcellular + monthapr + monthjul + monthjun + monthmar + 
  monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
  poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
  nr.employed + `previousMore than_3_times`, family = "binomial", data = train)

# checking summary and vif for model_3 

summary(model_3)

sort(vif(model_3))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'previousMore than_3_times' as it has highest p value

model_4 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
                 maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_4)

sort(vif(model_4))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'jobadmin.' as it has highest p value


model_5 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                 maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_5)

sort(vif(model_5))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'maritaldivorced' as it has highest p value

model_6 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + educationPrimary_Education + educationTertiary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_6)

sort(vif(model_6))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'day_of_weekfri' as it has highest p value

model_7 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + educationPrimary_Education + educationTertiary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct +day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_7)

sort(vif(model_7))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'educationTertiary_Education' as it has highest p value

model_8 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct +day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_8)

sort(vif(model_8))


# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'jobtechnician' as it has highest p value


model_9 <- glm(formula = response ~ jobretired + jobstudent  + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + monthoct +day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_9)

sort(vif(model_9))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'monthoct' as it has highest p value

model_10<- glm(formula = response ~ jobretired + jobstudent  + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_10)

sort(vif(model_10))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'jobstudent' as it has highest p value

model_11<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthapr + monthjul + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_11)

sort(vif(model_11))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'monthjul' as it has highest p value

model_12<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthapr + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_12)

sort(vif(model_12))

# 'monthjun' ,'cons.price.idx' ,'nr.employed' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'monthapr' as it has highest p value

model_13<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                 nr.employed , family = "binomial", data = train)

summary(model_13)

sort(vif(model_13))

# 'monthjun' ,'cons.price.idx' and 'emp.var.rate'  have very high VIF but p value is low. So we will not remove them on the basis of VIF
# Removing 'nr.employed' as it has highest p value

model_14<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx , family = "binomial", data = train)

summary(model_14)

sort(vif(model_14))

# Now all variables have low p value and  'emp.var.rate' and 'cons.price.idx' have VIF more than 2

# Removing 'cons.price.idx' from model_14 and checking its impact on other variables

model_chk1<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.conf.idx , family = "binomial", data = train)

summary(model_chk1)

sort(vif(model_chk1))

# Variables 'monthjun' and 'cons.conf.idx' have a high p value . But all varibales VIF is less then 1.464671 


# Removing 'emp.var.rate' from model_14 and checking its impact on other variables

model_chk2<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                   contactcellular + monthjun + monthmar + 
                   monthmay + monthnov + day_of_weekmon + 
                   campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                   poutcomefailure + cons.price.idx + cons.conf.idx , family = "binomial", data = train)

summary(model_chk2)

sort(vif(model_chk2))

# Variables 'monthjun' and 'cons.conf.idx' have a high p value . But all varibales VIF is less then 1.983514

# So removing 'cons.price.idx' from model_14 as its VIF is more then that of 'emp.var.rate'

model_15<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthjun + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.conf.idx , family = "binomial", data = train)

summary(model_15)

sort(vif(model_15))

# All the VIFs are less then 2. Removing 'monthjun' as it has a high p-value

model_16<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate + cons.conf.idx , family = "binomial", data = train)

summary(model_16)

sort(vif(model_16))

# All the VIFs are less then 2. Removing 'cons.conf.idx' as it has a high p-value

model_17<- glm(formula = response ~ jobretired + educationPrimary_Education + 
                 contactcellular + monthmar + 
                 monthmay + monthnov + day_of_weekmon + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure + emp.var.rate , family = "binomial", data = train)

summary(model_17)

sort(vif(model_17))

# All the VIF's are less then 2. And all p values are significantly less.
# model_17 becomes the final model

final_model <- model_17

# Predicting probabilities of responding for the test data

predictions_logit <- predict(final_model, newdata = test[, -61], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff)
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]

cutoff

# There are 2 values obtained for cutoff 0.07929293 and 0.08919192

# Checking cutoff value of 0.07929293 for final model

predicted_response1 <- factor(ifelse(predictions_logit >= 0.07929293, "yes", "no"))

conf_final1 <- confusionMatrix(predicted_response1, test$response, positive = "yes")

acc <- conf_final1$overall[1]
sens <- conf_final1$byClass[1]
spec <- conf_final1$byClass[2]

acc
# 0.7233733

sens
# 0.7133621

spec
# 0.7246443



# Checking cutoff value of 0.08919192 for final model

predicted_response2 <- factor(ifelse(predictions_logit >= 0.08919192, "yes", "no"))

conf_final2 <- confusionMatrix(predicted_response2, test$response, positive = "yes")

acc <- conf_final2$overall[1]
sens <- conf_final2$byClass[1]
spec <- conf_final2$byClass[2]

acc
# 0.7555034

sens
# 0.6946839

spec
# 0.7632251

# Keeping cutoff value of 0.07929293 for the final model.
# Since this is the best cuttoff value for which all accuracy,specificity and sensitivity are almost equal

predicted_final_response <- factor(ifelse(predictions_logit >= 0.07929293, "yes", "no"))

conf_final <- confusionMatrix(predicted_final_response, test$response, positive = "yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# 0.7233733

sens
# 0.7133621

spec
# 0.7246443

#---------------------------------------------------------    
# ------Model Evaluation----------------------------------

# Appending the probabilities and response variables to the test data

test$predicted_probs <- predictions_logit

test$predicted_response <- predicted_final_response


# As per the comment from TA (Shreyas Trivedi) 
#"To find top 80% prospect, it would be better to do it on whole population but for this assignment, it is fine even if you do it for test data only."

# Creating new dataframe with variables prospect ID, actual response, predicted response, predicted probability of response, duration of call in seconds, and cost of call

final_df <- test[, c("prospect_id", "response", "predicted_response", "predicted_probs", "duration")]

# Calculating cost per call
final_df$Cost_of_Call <- (0.033*(final_df$duration)) + 0.8

# Renaming column names

colnames(final_df) <- c("Prospect_Id", "Actual_Response", "Predicted_Response", "Predicted_Probability", "Duration", "Cost_of_Call")

# sorted the final_data in decreasing order of probability of response

final_df <- final_df[order(final_df$Predicted_Probability, decreasing = T), ]

##########################

#############TASK 4 ############3
#Find the number of top X% prospects you should target to meet the business objective
#Report the average call duration for targeting the top X% prospects to the CMO (report this as a comment in the R file)

lift <- function(labels , predicted_prob,Duration,Cost_of_Call, groups=10) {
  if(is.factor(labels)) labels <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  if(is.factor(Duration)) Duration <- as.integer(as.character(Duration))
  if(is.factor(Cost_of_Call)) Cost_of_Call <- as.integer(as.character(Cost_of_Call))
  helper = data.frame(cbind(labels , predicted_prob, Duration, Cost_of_Call))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket) %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(predicted_prob, na.rm = TRUE),
                                     totdur=sum(Duration, na.rm = TRUE),
                                     avgdur=mean(Duration, na.rm = TRUE),
                                     totalcost=sum(Cost_of_Call, na.rm = TRUE),
                                     avgcost=mean(Cost_of_Call, na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift

final_df$Actual_Response <- as.factor(ifelse(final_df$Actual_Response=="yes",1,0))
final_df$Predicted_Response <- as.factor(ifelse(final_df$Predicted_Response=="yes",1,0))

LG = lift(final_df$Actual_Response, final_df$Predicted_Probability,final_df$Duration,final_df$Cost_of_Call, groups = 10)

View(LG)

#bucket	   total	totalresp	  totdur	     avgdur	    totalcost	   avgcost	  Cumresp	    Gain	      Cumlift
#1	        1236	580.93315	   336935.8	   272.6018	    12107.68	 9.795858	  580.9332	  41.72467	  4.172467
#2	        1236	247.97231	   340259.8	   275.2911	    12217.37	 9.884607	  828.9055	  59.53491	  2.976746
#3	        1235	129.86019	   315288.4	   255.2943	    11392.52	 9.224711	  958.7656	  68.86193	  2.295398
#4	        1236	93.8988	     308642.6	   249.7108	    11174	     9.040457	  1052.6644	  75.60607	  1.890152
#5	        1235	84.56588	   316277.3	   256.095	    11425.15	 9.251135	  1137.2303	  81.67989	  1.633598
#6	        1236	73.91064	   305486.4	   247.1573	    11069.85	 8.956191	  1211.141	  86.98841	  1.449807
#7	        1236	65.34013	   284547.6	   230.2165	    10378.87	 8.397145	  1276.4811	  91.68137	  1.309734
#8	        1235	52.7655	     282874.2	   229.0479	    10322.85	 8.358581	  1329.2466	  95.47117	  1.19339
#9	        1236	35.87793	   303104.9	   245.2305	    10991.26	 8.892607	  1365.1245	  98.04805	  1.089423
#10	        1235	27.17699	   322954.2	   261.5014	    11645.49	 9.429545	  1392.3015	  100	        1

# As per the results of Lift_Gain results it is found that top 80% target is covered in 5th decile
# This implies we need to contact 50% of the customer to reach 80% target.
# Average call duration for 80% prospect is 256.0950 and average cost is 9.251135
# Total  call duration for 80% prospect is 316277.3 and total cost is 11425.15


# Create a Lift Chart
#The x-axis contains the number of prospects contacted; the y-axis contains the ratio: response rate using the model/ response rate without using the model

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")
