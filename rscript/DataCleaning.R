setwd("C:/Users/caixi/Desktop/BCG/Y2S1/BC2406/Team Assignment and Project/DATA/Data")
uncleaned_data <- read.csv("Data (with created variables).csv")

##########Checking and cleaning credit_Score, cleaned by multiplying them by 10#########
plot(uncleaned_data$CreditScore)
credit_score.outliner <- subset(uncleaned_data, CreditScore<200) 
credit_score.outliner

uncleaned_data[264,"CreditScore"] <- uncleaned_data[264,"CreditScore"]*10
uncleaned_data[264,"CreditScore"]
uncleaned_data[273,"CreditScore"] <- uncleaned_data[273,"CreditScore"]*10
uncleaned_data[273,"CreditScore"]


##########Checking and cleaning FinancialLiteracy, cleaned by deleting the row#########
summary(uncleaned_data$FinancialLiteracy)
uncleaned_data.subset <- subset(uncleaned_data, !is.na(FinancialLiteracy))
summary(uncleaned_data.subset$FinancialLiteracy)
uncleaned_data <- uncleaned_data.subset

##########Checking and cleaning Last contact by a banker, cleaned by deleting 2 rows and divide by 10 for one row#########
summary(uncleaned_data$LastContactByABanker)
uncleaned_data.subset <- subset(uncleaned_data, !is.na(LastContactByABanker))
summary(uncleaned_data.subset$LastContactByABanker)
uncleaned_data <- uncleaned_data.subset
plot(uncleaned_data$LastContactByABanker)
outlier <- subset(uncleaned_data, LastContactByABanker >100)
uncleaned_data["9998", "LastContactByABanker"] <- uncleaned_data["9998", "LastContactByABanker"] /10
uncleaned_data["9998", "LastContactByABanker"]


######create the cleaned_data and check it###
cleaned_data <- uncleaned_data
summary(cleaned_data)


####write it in CSV file###
write.csv(file = "Team7_FinalDataset.csv", x =cleaned_data)