# Setup
setwd("U:/Refugia/Persistance/")
library(randomForest)
library(rpart)

# Load data
valuetable <- read.csv("valuetable.csv")
load("rfm.RData")

# Split data
valuetable$Unburned[valuetable$Unburned == 0] <- "Burned"
valuetable$Unburned[valuetable$Unburned == 1] <- "Unburned"
train.index <- sample(1:nrow(valuetable), 0.8 * nrow(valuetable))
train <- valuetable[train.index, ]
test <- valuetable[-train.index, ]

# Random Forest
rfm <- randomForest(factor(Unburned) ~ ., data = train, importance = T)
rfm2 <- randomForest(factor(Unburned) ~ slope + TRI, data = train, importance = T)
rfm$confusion
#save(rfm, "rfm.RData")
varImpPlot(rfm)



# Cart

cart <- rpart(Unburned ~ TRI + slope + TPI, data = train, method = "class")

#   Predict & accuracy assessment
pred.cart <- predict(cart, test, type = "class")
confMat <- table(test$Unburned, pred.cart)
sum(diag(confMat))/sum(confMat)
t <- test["Unburned"]
accuracy <- sum(diag(confMat))/sum(confMat) #0.5840767


#   Plot CART
par(mfrow = c(1,2), xpd = NA)
plot(cart)
text(cart, use.n = T)
