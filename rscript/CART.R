## Import data set 
library(data.table)
dt <- fread('/Users/ernestang98/Desktop/bc2406proj+shiny/r/myApp/data.csv', stringsAsFactors=T)
summary(dt)


## Prepare data set
dt$HasCrCard<- factor(dt$HasCrCard)
dt$IsActiveMember<- factor(dt$IsActiveMember)
dt$Exited<- factor(dt$Exited)
dt$PersonalAdvisor<- factor(dt$PersonalAdvisor)
dt$FinancialLiteracy<- factor(dt$FinancialLiteracy)
dt$UnresolvedComplaint<- factor(dt$UnresolvedComplaint)
cols = ncol(dt)
pData <- dt[,11:cols]
summary(dt)


## Building CART Model
library(rpart)
library(rpart.plot) 
set.seed(2004)  
CART <- rpart(Exited ~ ., data = pData, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(CART, nn= T, main = "CART Prediction Plot")
print(CART)
printcp(CART)
plotcp(CART, main = "CP Error Plot")


## Deduce CP value - Automated Method
CVerror.cap <- CART$cptable[which.min(CART$cptable[,"xerror"]), "xerror"] + CART$cptable[which.min(CART$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (CART$cptable[i,j] > CVerror.cap) {i <- i + 1}
cp = ifelse(i > 1, sqrt(CART$cptable[i,1] * CART$cptable[i-1,1]), 1)
cp


## Deduce CP value - Manual Method
## Values will change, must observe properly
CVerror.cap.M <- 0.15177 + 0.0084993
CVerror.cap.M
cp.M <- sqrt(0.00212836  * 0.00392927)
cp.M


## Pruning the Tree
pCART <- prune(CART, cp = cp)
rpart.plot(pCART, nn= T, main = "Pruned Tree")
print(pCART)
printcp(pCART)


## Predicting with Pruned CART Model
cart.predict <- predict(pCART, newdata = pData, type = "class")
results <- data.frame(pData, cart.predict)


## Confusion Matrix & Accuracy of Model
table(pData$Exited, results$cart.predict, deparse.level = 2)
mean(results$cart.predict == dt$Exited)