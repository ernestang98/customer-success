setwd("C:/Users/caixi/Desktop/BCG/Y2S1/BC2406/Team Assignment and Project/DATA")
data <- read.csv("Team7_FinalDataset.csv")
library(ggplot2)


plot(data$EstimatedSalary)
boxplot(data$EstimatedSalary)

data.dt$Exited <- factor(data.dt$Exited)
data.dt$Gender <- factor(data.dt$Gender)
data.dt$FrequencyOfContact <- factor(data.dt$FrequencyOfContact)
data.dt$UnresolvedComplaint <- factor(data.dt$UnresolvedComplaint)
data.dt$NumOfProducts <- factor(data.dt$NumberOfComplaints)
data.dt$FinancialLiteracy <- factor(data.dt$FinancialLiteracy)
data.dt$PersonalAdvisor <- factor(data.dt$PersonalAdvisor)
data.dt$IsActiveMember <- factor(data.dt$IsActiveMember)

ggplot(data = data.dt, aes(x = Gender))

ggplot(data = data$Exited, aes(x = Var1, y = Freq))+
  geom_bar(stat = "identity", fill =c("#00BFFF", "#FF6347"))+
  geom_text( aes(label = Freq), vjust = c(+11, +3), color = "White", size = 5)+
  labs(x= "Exited", y = "Frequency")+
  ggtitle("Distribution of Exited")+
  theme(plot.title = element_text(hjust = 0.5))


#Credit Score
plot(data$CreditScore,jitter(data$Exited), 
     xlab = "Credit Score",
     ylab = "Exited",
     main = "Jittered Scatterplot of Exited against\n Credit Score")

boxplot(CreditScore ~ Exited, 
        data = data,
        main = "Boxplots for variable Credit Score")


#Geography
ggplot(data = data, aes(x = Geography, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
                        geom_bar(stat = "identity")+
                        ggtitle("Stacked bar chart: Exited against \n\tGeography (Frequency)")+
                        labs(x = "Geography", y = "Frequency" , fill = "Exited ")+
                        scale_fill_manual(values= c("light green", "orange"))+
                        geom_text(size =3, position = "stack", aes(label = frequency(Exited)))


ggplot(data = data, aes(x = Geography, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
                        geom_bar(stat = "identity", position = "fill")+
                        ggtitle("Stacked bar chart: Exited against \n\tGeography (Proportion)")+
                        labs(x = "Geography", y = "Proportion" , fill = "Exited ")+
                        scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
                        scale_fill_manual(values = c("light blue", "deeppink"))
                

#Gender
ggplot(data = data, aes(x = Gender, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tGender (Frequency)")+
  labs(x = "Gender", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))


ggplot(data = data, aes(x = Gender, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tGender (Proportion)")+
  labs(x = "Gender", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))

#Age
plot(data$Age,jitter(data$Exited), 
     xlab = "Age",
     ylab = "Exited",
     main = "Jittered Scatterplot of Exited against Age")

boxplot(Age ~ Exited, 
        data = data,
        main = "Boxplots for variable Age grouped by Exited")


#Tenure
ggplot(data = data, aes(x = factor(Tenure), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tTenure (Frequency)")+
  labs(x = "Tenure", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))


ggplot(data = data, aes(x = factor(Tenure), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tTenure (Proportion)")+
  labs(x = "Tenure", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))

#Balance
plot(data$Balance,jitter(data$Exited), 
     xlab = "Balance",
     ylab = "Exited",
     main = "Jittered Scatterplot of Exited against Balance")

boxplot(Balance ~ Exited, 
        data = data,
        main = "Boxplots for variable Balance grouped by Exited")

length(which(data$Balance ==0))
length(which(data$Balance ==0 & data$Exited == 0))
length(which(data$Balance ==0 & data$Exited == 1))

#Number of Products : NumOfProducts
ggplot(data = data, aes(x = NumOfProducts, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tNumber of Products (Frequency)")+
  labs(x = "NumOfProducts", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))



ggplot(data = data, aes(x = NumOfProducts, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tNumber of Products (Proportion)")+
  labs(x = "NumOfProducts", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))


#Variable: IsActiveMember
ggplot(data = data, aes(x = factor(IsActiveMember), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tIsActiveMember (Frequency)")+
  labs(x = "IsActiveMember", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))



ggplot(data = data, aes(x = factor(IsActiveMember), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tIsActiveMember (Proportion)")+
  labs(x = "IsActiveMember", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))


#Estimated Salary
plot(data$EstimatedSalary,jitter(data$Exited), 
     xlab = "Estimated Salary",
     ylab = "Exited",
     main = "Jittered Scatterplot of Exited against\nEstimated Salary")

boxplot(EstimatedSalary ~ Exited, 
        data = data,
        main = "Boxplots for Estimated Salary\ngrouped by Exited")

#PersonalAdvisor
ggplot(data = data, aes(x = factor(PersonalAdvisor), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tPersonalAdvisor (Frequency)")+
  labs(x = "PersonalAdvisor", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))



ggplot(data = data, aes(x = factor(PersonalAdvisor), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tPersonalAdvisor (Proportion)")+
  labs(x = "PersonalAdvisor", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))



#FinancialLiteracy
ggplot(data = data, aes(x = FinancialLiteracy, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tFinancialLiteracy (Frequency)")+
  labs(x = "FinancialLiteracy", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))



ggplot(data = data, aes(x = FinancialLiteracy, 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tFinancialLiteracy (Proportion)")+
  labs(x = "FinancialLiteracy", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))

#NumberOfComplaints
ggplot(data = data, aes(x = factor(NumberOfComplaints), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tNumber Of Complaints (Frequency)")+
  labs(x = "Number Of Complaints", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))



ggplot(data = data, aes(x = factor(NumberOfComplaints), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tNumber Of Complaints (Proportion)")+
  labs(x = "Number Of Complaints", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))

#AverageOfCustomerFeedbackOnService
plot(data$AverageOfCustomerFeedbackOnService,jitter(data$Exited), 
     xlab = "Average Of Customer Feedback On Service",
     ylab = "Exited",
     main = "Jittered Scatterplot of Exited against\nAverage Of Customer Feedback On Service")

boxplot(AverageOfCustomerFeedbackOnService ~ Exited, 
        data = data,
        main = "Boxplots for Average Of CustomerFeedback \nOn Service grouped by Exited")

#UnresolvedComplaint

ggplot(data = data, aes(x = factor(UnresolvedComplaint), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tUnresolved Complaint (Frequency)")+
  labs(x = "Unresolved Complaint", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))



ggplot(data = data, aes(x = factor(UnresolvedComplaint), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tUnresolved Complaint (Proportion)")+
  labs(x = "UnresolvedComplaint", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))


#LastContactByABanker

plot(data$LastContactByABanker,jitter(data$Exited), 
     xlab = "Last Contact By A Banker",
     ylab = "Exited",
     main = "Jittered Scatterplot of Exited against\nLast Contact By A Banker")

boxplot(LastContactByABanker ~ Exited, 
        data = data,
        main = "Boxplots for Last Contact By A Banker \ngrouped by Exited")


#TimeBetweenRegistrationAndFirstInvestment
plot(data$TimeBetweenRegistrationAndFirstInvestment,jitter(data$Exited), 
     xlab = "Time Between Registration And First Investment",
     ylab = "Exited",
     main = "Jittered Scatterplot of Exited against\nTime Between Registration And\n First Investment")

boxplot(TimeBetweenRegistrationAndFirstInvestment ~ Exited, 
        data = data,
        main = "Boxplots for Time Between Registration\nAnd First Investment grouped by Exited")

#FrequencyOfContact

ggplot(data = data, aes(x = factor(FrequencyOfContact), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity")+
  ggtitle("Stacked bar chart: Exited against \n\tFrequency Of Contact (Frequency)")+
  labs(x = "Frequency Of Contact", y = "Frequency" , fill = "Exited ")+
  scale_fill_manual(values= c("light green", "orange"))



ggplot(data = data, aes(x = factor(FrequencyOfContact), 
                        y = frequency(Exited), 
                        fill = factor(Exited)))+ 
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("Stacked bar chart: Exited against \n\tFrequency Of Contact (Proportion)")+
  labs(x = "Frequency Of Contact", y = "Proportion" , fill = "Exited ")+
  scale_y_continuous(labels = function(y)paste0(y*100, "%"))+
  scale_fill_manual(values = c("light blue", "deeppink"))







