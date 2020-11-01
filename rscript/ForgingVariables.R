#Reading in the data
setwd("C:/Users/caixi/Desktop/BCG/Y2S1/BC2406/Team Assignment and Project/DATA/Original Data")
data <- read.csv("Original Data.csv")

#----------------------------------------------------------------------------------------------#
# 1.Generating Average Customer Feedback on Service
randCFS <- rep (0,nrow(data))

#The original code that resulted in Perfect Separation 
for (row in 1:nrow(data))
{
  if (data$Exited[row] == 0){
    randCFS[row] <- mean(sample(x= c(1,2,3,4,5), size = 100, 
                                replace = TRUE, prob = c(0.01,0.06,0.08,0.20,0.65)))
    
  } else {
    randCFS[row] <- mean(sample(x= c(1,2,3,4,5), size = 100, 
                                replace = TRUE, prob = c(0.19,0.22,0.18,0.11,0.3)))
  }
}

#The code that eliminated the problem of Perfect Separation
for (row in 1:nrow(data))
{
  if (data$Exited[row] == 0){
    randCFS[row] <- mean(sample(x= c(1,2,3,4,5), size = 20, 
                                replace = TRUE, prob = c(0.01,0.11,0.13,0.15,0.6)))
    
  } else {
    randCFS[row] <- mean(sample(x= c(1,2,3,4,5), size = 20, 
                                replace = TRUE, prob = c(0.19,0.12,0.18,0.11,0.5)))
  }
}

data$AverageOfCustomerFeedbackOnService <- randCFS



#----------------------------------------------------------------------------------------------#
# 2. Generating number of complaints

randC <- rep (0,nrow(data))
for (row in 1:nrow(data))
{
  if (data$Exited[row] == 0){
    randC[row] <- sample(x=c(0,1,2,3,4,5,6,7,8,9,10), size = 1, replace = TRUE, prob = c(0.61,0.10,0.10,0.09,0.03,0.02,0.01,0.01,0.01,0.01,0.01))
  } else {
    randC[row] <- sample(x=c(0,1,2,3,4,5,6,7,8,9,10), size = 1, replace = TRUE, prob = c(0.48,0.12,0.12,0.11,0.05,0.04,0.03,0.02,0.01,0.01,0.01))
  }
}
data$NumberOfComplaints <- randC



#----------------------------------------------------------------------------------------------#
# 3. Generating Personal Advisor

randPA <- rep (0,nrow(data))
for (row in 1:nrow(data))
{
  if (data$Exited[row] == 0){
    randPA[row] <- rbinom(1, size =1, prob = 0.75)
  } else {
    randPA[row] <- rbinom(1, size =1, prob = 0.5)
  }
}
data$PersonalAdvisor <- randPA



#----------------------------------------------------------------------------------------------#
# 4. Generating Time between Registration and First Investment

randRTI <- sample(x=c(1:105), size = nrow(data), replace = TRUE, prob = rep(1/105, 105))
data$TimeBetweenRegistrationAndFirstInvestment <- randRTI



#----------------------------------------------------------------------------------------------#
# 5. Generating Frequency of Contact

randFOC <- rep(0, nrow(data))
for (row in 1:10000)
{
  if (data$Exited[row] == 0){
    randFOC[row] <- sample(x = c(1:10), size = 1, prob = c(0.025,0.025,0.05,0.1,0.3,0.3,0.1,0.05,0.025,0.025))
  } else {
    randFOC[row] <- sample(x = c(1:10), size = 1, prob = c(0.05,0.1,0.3,0.3,0.1,0.05,0.025,0.025,0.025,0.025))
  }
}
data$FrequencyOfContact <- randFOC



#----------------------------------------------------------------------------------------------#
# 6. Generating Last Contact by a Banker (number of days)

randLCB <- rep(0, nrow(data))
for (row in 1:10000)
{
  if (data$Exited[row] == 0){
    randLCB[row] <- sample(x= c(1:60), size = 1, prob = rep(1/60, 60))
  } else {
    randLCB[row] <- sample(x= c(21:100), size = 1, prob = rep(1/80, 80))
  }
}
data$LastContactByABanker <- randLCB



#----------------------------------------------------------------------------------------------#
# 7. Generating Unresolved Complaint

randUC <- rep (0, nrow(data))

for (row in 1:nrow(data)){
  if (data$NumberOfComplaints[row] == 0){
    randUC[row] <- 0
  }
  else {
    randUC[row] <- rbinom(1, size = 1, prob = 0.08)
  }
}
data$UnresolvedComplaint <- randUC



#----------------------------------------------------------------------------------------------#
# 8. Generating Financial Literacy

randFL <- rep (0,nrow(data))
randFL <- sample(x = c(0,1,2), size = nrow(data),replace = TRUE, prob = c (0.2,0.4,0.4))
data$FinancialLiteracy <- randFL


#----------------------------------------------------------------------------------------------#
#Overwriting the data and saving it in CSV format
write.csv(data, file = "Data(with Created variables).csv")



