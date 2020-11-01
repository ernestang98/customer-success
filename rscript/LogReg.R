#Import datasets and necessary packages
install.packages("factoextra")
library(data.table)
library(caTools)
library(car)
library(ggplot2)
library(factoextra)
setwd('C:/NTU/Y2S1/BC2406 Analytics/group project')
retention <- fread("data.csv", stringsAsFactors = T)

#Prepare data set
cols = ncol(retention)
retention <- retention[,5:cols]
retention$Exited <- factor(retention$Exited)
levels(retention$Exited)
retention$FinancialLiteracy <- factor(retention$FinancialLiteracy)
retention$PersonalAdvisor <- factor(retention$PersonalAdvisor)
retention$UnresolvedComplaint <- factor(retention$UnresolvedComplaint)
retention$IsActiveMember <- factor(retention$IsActiveMember)
summary(retention)


#building a logistic regression model
#split model into train and test set at a ratio of 7:3
set.seed(2)
train <- sample.split(Y = retention$Exited, SplitRatio = 0.7)
trainset <- subset(retention, train == T)
testset <- subset(retention, train == F)

#model with all the variables
m1 <- glm(Exited ~ . , family = binomial, data = trainset)
summary(m1)
# at 5% alpha, only Geography, Gender, Age, Balance, isActiveMember, PersonalAdvisor, NumberOfComplaints, AverageOfCustomerFeedbackOnService, LastContactByABanker and FrequencyOfContact are statisticaly significant.
# a new model is fitted with only these variables.
m2 <- glm(Exited ~ .-(CreditScore+Tenure+NumOfProducts+EstimatedSalary+FinancialLiteracy+UnresolvedComplaint+TimeBetweenRegistrationAndFirstInvestment), family= binomial, data= trainset)
summary(m2)
#odds ratio of the different variables and their CI
OR <- exp(coef(m2))
OR

OR.CI <- exp(confint(m2))
OR.CI
#AverageOfCustomerFeedbackOnService, FrequencyOfCOntact, Gender, IsActiveMember and PersonalAdvisor seem to be the most important based on odds ratio.


boxOdds <- c(OR[c(seq(2,length(OR),1))])
n_ci <-  length(OR.CI)
boxCILow<-OR.CI[c(seq(2,n_ci/2,1))]
boxCIHigh<- OR.CI[c(seq(n_ci/2+2,n_ci,1))]
#----visualisation of the odds ratios-----
boxLabels <- names(boxOdds)
df <- data.frame(yAxis = length(boxLabels):1, boxOdds, boxCILow, boxCIHigh)
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(boxLabels):1, labels = boxLabels) +
  scale_x_continuous(breaks = c(seq(0,1,0.2), seq(1,5,1) )) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  ggtitle("Factors affecting Customer Retention")



# Confusion Matrix on Trainset
threshold <- 0.5
prob.train <- predict(m2, type = 'response')
m2.predict.train <- ifelse(prob.train > threshold, 1, 0)
table1 <- table(Trainset.Actual = trainset$Exited, m2.predict.train, deparse.level = 2)
table1
round(prop.table(table1),3)
# Overall Accuracy
mean(m2.predict.train == trainset$Exited)
#[1] 0.9547013

# Confusion Matrix on Testset
prob.test <- predict(m2, newdata = testset, type = 'response')
m2.predict.test <- ifelse(prob.test> threshold, 1, 0)
table2 <- table(Testset.Actual = testset$Exited, m2.predict.test, deparse.level = 2)
table2
round(prop.table(table2), 3)
# Overall Accuracy
mean(m2.predict.test == testset$Exited)
#[1] 0.9556519

#check adjusted VIF values
vif(m2)

# choosing variables based on backward elimination:
m3<- step(m1)
# the variables chosen by the step function are the same as those chosen based on alpha value. No further analysis done.




#-----------------------------clustering---------------------------------------------------------------

#remove nominal variables before clustering is done
retention1 <- copy(retention)
retention_xnominal <- retention1[,c("Geography", "Gender", "Exited", "IsActiveMember", "PersonalAdvisor", "UnresolvedComplaint"):= NULL]
summary(retention_xnominal)
# use k means to form 5 clusters on the data
retention_xnominal$FinancialLiteracy <- as.numeric(retention_xnominal$FinancialLiteracy)
retention_xnominal <- scale(retention_xnominal)

set.seed(123)

fviz_nbclust(retention_xnominal,FUNcluster= kmeans, "wss")
#no clear optimal clusters 
Cluster <- kmeans(retention_xnominal, 5)
Cluster$centers



fviz_cluster(Cluster, data = retention_xnominal,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#E83628", "#B26678", "#B39799"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


#build a log reg model based on the clusters
clustered_retention <- copy(retention)
clustered_retention$cluster<-as.factor(Cluster$cluster)

set.seed(2)
train <- sample.split(Y = clustered_retention$Exited, SplitRatio = 0.7)
trainset_c <- subset(clustered_retention, train == T)
testset_c <- subset(clustered_retention, train == F)

#model with all the variables
cm1 <- glm(Exited ~ . , family = binomial, data = trainset_c)
summary(cm1)
cm2 <- glm(Exited ~ .-(CreditScore+Tenure+Balance+EstimatedSalary+FinancialLiteracy+NumberOfComplaints+UnresolvedComplaint+TimeBetweenRegistrationAndFirstInvestment), family= binomial, data= trainset_c)
summary(cm2)

OR <- exp(coef(cm2))
OR
OR.CI <- exp(confint(cm2))
OR.CI
#AverageOfCustomerFeedbackOnService, FrequencyOfCOntact, Gender, IsActiveMember and PersonalAdvisor seem to be the most important based on odds ratio.


# Confusion Matrix on Trainset
threshold <- 0.5
prob.train.c <- predict(cm2, type = 'response')
cm2.predict.train <- ifelse(prob.train.c > threshold, 1, 0)
table3 <- table(Trainset.Actual = trainset_c$Exited, cm2.predict.train, deparse.level = 2)
table3
round(prop.table(table3),3)
# Overall Accuracy
mean(cm2.predict.train == trainset_c$Exited)
#[1] 0.9582738

# Confusion Matrix on Testset
prob.test.c <- predict(cm2, newdata = testset_c, type = 'response')
cm2.predict.test <- ifelse(prob.test.c> threshold, 1, 0)
table4 <- table(Testset.Actual = testset_c$Exited, cm2.predict.test, deparse.level = 2)
table4
round(prop.table(table4), 3)
# Overall Accuracy
mean(cm2.predict.test == testset_c$Exited)
#[1] 0.9566522
# accuracy only improved slightly




#---------------building model for external factors----------------------------------------

external <- retention[,.(Exited, CreditScore, Geography, Gender, Age, Balance,NumOfProducts, EstimatedSalary, FinancialLiteracy)]

set.seed(2)
train_e <- sample.split(Y = external$Exited, SplitRatio = 0.7)
trainset_e <- subset(external, train_e == T)
testset_e <- subset(external, train_e == F)

em1 <- glm(Exited ~ . , family = binomial, data = trainset_e)

summary(em1)

# at 1% sig level

em2 <- glm(Exited ~ Gender+Age+Geography+Balance, family= binomial, data= trainset_e)
summary(em2)


OR <- exp(coef(em2))
OR
# top variables by OR are geography and gender

OR.CI <- exp(confint(em2))
OR.CI
#Geography spain includes 1 in CI (decide whether to inlucde this or not)

# Confusion Matrix on Trainset
prob.train.e <- predict(em2, type = 'response')
em2.predict.train <- ifelse(prob.train.e > threshold, 1, 0)
table5 <- table(Trainset.Actual = trainset_e$Exited, em2.predict.train, deparse.level = 2)
table5
round(prop.table(table5),3)
# Overall Accuracy
mean(em2.predict.train == trainset_e$Exited)
#[1] 0.7927979

# Confusion Matrix on Testset
prob.test.e <- predict(em2, newdata = testset_e, type = 'response')
em2.predict.test <- ifelse(prob.test.e > threshold, 1, 0)
table6 <- table(Testset.Actual = testset_e$Exited, em2.predict.test, deparse.level = 2)
table6
round(prop.table(table6), 3)
# Overall Accuracy
mean(em2.predict.test == testset_e$Exited)
#[1] 0.7859286

#----visualising the odds ratio
boxOdds <- c(OR[c(seq(2,length(OR),1))])
n_ci <-  length(OR.CI)
boxCILow<-OR.CI[c(seq(2,n_ci/2,1))]
boxCIHigh<- OR.CI[c(seq(n_ci/2+2,n_ci,1))]
boxLabels <- names(boxOdds)
df <- data.frame(yAxis = length(boxLabels):1, boxOdds, boxCILow, boxCIHigh)
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(boxLabels):1, labels = boxLabels) +
  scale_x_continuous(breaks = c(seq(0,1,0.2), seq(1,5,1) )) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  ggtitle("Factors affecting Customer Retention")


# ---------------------------------------------------------------------------------


#------------------building model for internal factors-----------------------------
internal <- retention[,.(Exited, PersonalAdvisor,IsActiveMember, NumberOfComplaints,AverageOfCustomerFeedbackOnService, UnresolvedComplaint, LastContactByABanker, TimeBetweenRegistrationAndFirstInvestment, FrequencyOfContact)]
#train - test split of 70-30
set.seed(2)
train_i <- sample.split(Y = internal$Exited, SplitRatio = 0.7)
trainset_i <- subset(internal, train_i == T)
testset_i <- subset(internal, train_i == F)

im1 <- glm(Exited ~ . , family = binomial, data = trainset_i)

summary(im1)
# taking cutoff at 0.05, UnresolvedComplaint1 and TimeBetweenRegistrationAndFirstInvestment are not statistically significant


#remove teh insignificant variable
im2 <- glm(Exited ~ .-TimeBetweenRegistrationAndFirstInvestment-UnresolvedComplaint, family= binomial, data= trainset_i)
summary(im2)


OR <- exp(coef(im2))
OR

OR.CI <- exp(confint(im2))
OR.CI
#all exclude 1 in CI
#top variables are average..., personaladvisor, frequencyof contact


# Confusion Matrix on Trainset
threshold1 <- 0.5
prob.train.i <- predict(im2, type = 'response')
im2.predict.train <- ifelse(prob.train.i > threshold1, 1, 0)
table7 <- table(Trainset.Actual = trainset_i$Exited, im2.predict.train, deparse.level = 2)
table7
round(prop.table(table7),3)
# Overall Accuracy
mean(im2.predict.train == trainset_i$Exited)
#[1] 0.9511289


# Confusion Matrix on Testset
prob.test.i <- predict(im2, newdata = testset_i, type = 'response')
im2.predict.test <- ifelse(prob.test.i > threshold1, 1, 0)
table8 <- table(Testset.Actual = testset_i$Exited, im2.predict.test, deparse.level = 2)
table8
round(prop.table(table8), 3)
# Overall Accuracy
mean(im2.predict.test == testset_i$Exited)
#1] 0.9499833


#----visualisation of the odds ratios-----
boxOdds <- c(OR[c(seq(2,length(OR),1))])
n_ci <-  length(OR.CI)
boxCILow<-OR.CI[c(seq(2,n_ci/2,1))]
boxCIHigh<- OR.CI[c(seq(n_ci/2+2,n_ci,1))]
boxLabels <- names(boxOdds)
df <- data.frame(yAxis = length(boxLabels):1, boxOdds, boxCILow, boxCIHigh)

#boxLabels <- c("PersonalAdvisor1","IsActiveMember1", "NumberOfComplaints","AverageOfCustomerFeedbackOnService","LastContactByBanker", "FrequencyOfContact")
#df <- data.frame(yAxis = length(boxLabels):1, 
#                 boxOdds = c(0.3222435, 0.4961460, 1.078272, 0.0003001440, 1.080493, 0.6444372), 
#                 boxCILow = c(0.2512010, 0.3888602, 1.022039, 0.0001687256, 1.072700, 0.6034666), 
#                 boxCIHigh = c(0.4124257, 0.6315201, 1.136498, 0.0005179240, 1.088747, 0.6872519)
#)


p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(boxLabels):1, labels = boxLabels) +
  scale_x_continuous(breaks = c(seq(0,1,0.2), seq(1,5,1) )) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  ggtitle("Factors affecting Customer Retention")









#------------------------------Variable Analysis of Younger vs Older customers------------------------


younger <- retention[Age<39,]
younger.dt <- younger[,.(Exited, PersonalAdvisor,IsActiveMember, NumberOfComplaints,AverageOfCustomerFeedbackOnService, UnresolvedComplaint, LastContactByABanker, TimeBetweenRegistrationAndFirstInvestment, FrequencyOfContact)]



set.seed(2)
train <- sample.split(Y = younger.dt$Exited, SplitRatio = 0.7)
trainset <- subset(younger.dt, train == T)
testset <- subset(younger.dt, train == F)

#model with all the variables
m1_y <- glm(Exited ~ . , family = binomial, data = trainset)
summary(m1_y)
m2_y <- glm(Exited ~ .-(NumberOfComplaints+TimeBetweenRegistrationAndFirstInvestment+UnresolvedComplaint), family= binomial, data= trainset)
summary(m2_y)

#odds ratio of the different variables and their CI
OR <- exp(coef(m2_y))
OR

OR.CI <- exp(confint(m2_y))
OR.CI

boxOdds <- c(OR[c(seq(2,length(OR),1))])
n_ci <-  length(OR.CI)
boxCILow<-OR.CI[c(seq(2,n_ci/2,1))]
boxCIHigh<- OR.CI[c(seq(n_ci/2+2,n_ci,1))]

#----visualisation of the odds ratios-----
boxLabels <- names(boxOdds)
df <- data.frame(yAxis = length(boxLabels):1, boxOdds, boxCILow, boxCIHigh)
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(boxLabels):1, labels = boxLabels) +
  scale_x_continuous(breaks = c(seq(0,1,0.2), seq(1,5,1) )) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  ggtitle("Factors affecting Customer Retention for Younger customers")






older <- retention[Age >=39,]
older.dt <- older[,.(Exited, PersonalAdvisor,IsActiveMember, NumberOfComplaints,AverageOfCustomerFeedbackOnService, UnresolvedComplaint, LastContactByABanker, TimeBetweenRegistrationAndFirstInvestment, FrequencyOfContact)]


set.seed(2)
train <- sample.split(Y = older.dt$Exited, SplitRatio = 0.7)
trainset <- subset(older.dt, train == T)
testset <- subset(older.dt, train == F)

#model with all the variables
m1_o <- glm(Exited ~ . , family = binomial, data = trainset)
summary(m1_o)
m2_o <- glm(Exited ~ .-(UnresolvedComplaint+TimeBetweenRegistrationAndFirstInvestment), family= binomial, data= trainset)
summary(m2_o)

#odds ratio of the different variables and their CI
OR <- exp(coef(m2_o))
OR

OR.CI <- exp(confint(m2_o))
OR.CI

#----visualisation of the odds ratios-----
boxOdds <- c(OR[c(seq(2,length(OR),1))])
n_ci <-  length(OR.CI)
boxCILow<-OR.CI[c(seq(2,n_ci/2,1))]
boxCIHigh<- OR.CI[c(seq(n_ci/2+2,n_ci,1))]
boxLabels <- names(boxOdds)
df <- data.frame(yAxis = length(boxLabels):1, boxOdds, boxCILow, boxCIHigh)
p <- ggplot(df, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = length(boxLabels):1, labels = boxLabels) +
  scale_x_continuous(breaks = c(seq(0,1,0.2), seq(1,5,1) )) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  ggtitle("Factors affecting Customer Retention for Older customers")
