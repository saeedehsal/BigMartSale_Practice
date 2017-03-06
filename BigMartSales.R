#New direcotory
path <- "C:/Users/i58074/Documents/data"
#Set new directory
setwd(path)


#Load Datasets
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

#Check the dimension
dim(train)
dim(test)

#check the variables and their types in train
str(train)


table(is.na(train))
colSums(is.na(train))
summary(train)

#data visualization
library(ggplot2)
#plot Item_Outlet_Sales vs Item_Visibility
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")
#plot Item_Outlet_Sales vs Outlet_Identifier
ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()
#plot Item_Outlet_Sales vs Item_Type
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")
#plot Item_MRP vs Item_Type
ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")


#data manipulation
dim(train)
dim(test)

test$Item_Outlet_Sales <-  1
combi <- rbind(train, test)

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))

combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility), combi$Item_Visibility) 
levels(combi$Outlet_Size)[1] <- "Other"
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)


library(dplyr)
a <- combi%>%group_by(Outlet_Identifier)%>%tally()
head(a)
names(a)[2] <- "Outlet_Count"
combi <- full_join(a, combi, by = "Outlet_Identifier")

b <- combi%>%group_by(Item_Identifier)%>%tally()
names(b)[2] <- "Item_Count"
head (b)
combi <- merge(b, combi, by = "Item_Identifier")


c <- combi%>%select(Outlet_Establishment_Year)%>%mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)
combi$Outlet_Year <- c$Outlet_Year


q <- substr(combi$Item_Identifier,1,2)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
table(q)
combi$Item_Type_New <- q


library(dummies)
combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'),  sep='_')
str (combi)

combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content, Outlet_Establishment_Year,Item_Type))

new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

#linear regression
linear_model <- lm(Item_Outlet_Sales~., data = new_train)
summary(linear_model)

cor(new_train)
cor(new_train$Outlet_Count, new_train$`Outlet_Type_Grocery Store`)


#we now try a simpler multiple regression model
test$Item_Outlet_Sales <- 1
combi<- rbind(train, test)
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility), combi$Item_Visibility)
levels(combi$Outlet_Size)[1] <- "Other"
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
combi$Year <- 2013 - combi$Outlet_Establishment_Year
library(dplyr)
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

#linear model using Item_Outlet_Sales
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)
par(mfrow=c(2,2))
plot(linear_model)


#linear model using log of Item_Outlet_Sales
linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
summary(linear_model)
par(mfrow=c(2,2))
plot(linear_model)


install.packages("Metrics")
library(Metrics)
rmse(new_train$Item_Outlet_Sales, exp(linear_model$fitted.values))


install.packages("rpart")
install.packages("e1071")
install.packages("rpart.plot")
install.packages("caret")
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)

fitControl <- trainControl(method = "cv", number = 5)
#expand.grid function creates a data frame that includes all the different combinations of vectors or factors
cartGrid <- expand.grid(.cp = (1:50)*0.01)

tree_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

main_tree <- rpart(Item_Outlet_Sales ~ ., data = new_train, control = rpart.control(cp=0.01))

prp(main_tree)

pre_score <- predict(main_tree, type = "vector")
rmse(new_train$Item_Outlet_Sales, pre_score)

#random forest
library(randomForest)
control <- trainControl(method = "cv", number = 5)
rf_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "parRF", trControl = control, prox = TRUE, allowParallel = TRUE)

print(rf_model)

forest_model <- randomForest(Item_Outlet_Sales ~ ., data = new_train, mtry = 15, ntree = 1000)

print(forest_model)
varImpPlot(forest_model)




