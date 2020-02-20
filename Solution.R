
# Setting the working directory
getwd()
setwd("C:/Users/DELL/Desktop/R_projects/Big Mart Sales III")

# Required libraries
library(ggplot2)
library(corrgram)

# Loading data

train <- read.csv("train_kOBLwZA.csv")
test <- read.csv("test_t02dQwI.csv")

# Checking the dimension of train data
dim(train)
dim(test)

#looking top 5 rows
head(train)
tail(train)

head(test)
tail(test)

# Checking the structure and the summary of the data
str(train)
summary(train)

str(test)
summary(test)

names(which(sapply(train, anyNA)))
names(which(sapply(test, anyNA)))

# combining the train and test set
test$Item_Outlet_Sales<-NA
df<-rbind(train,test)

# Checking the newly created dataframe
dim(df)
head(df)
tail(df)
str(df)
summary(df)
table(is.na(df))
sapply(df, is.numeric)
sapply(df, is.factor)
names(df)
names(which(sapply(df, anyNA)))

#### Exploratory Data Analysis on training set

# Visualizing Data

# Item Outlet Sales Histogram

ggplot(train, aes(x=Item_Outlet_Sales)) +
  geom_histogram(binwidth = 200) +
  labs(title = "Item Outlet Sales Histogram", 
       x = "Item Outlet Sales")

# Item Outlet Sales Histogram by Outlet Identifier

ggplot(train, aes(x=Item_Outlet_Sales, fill = Outlet_Identifier)) +
  geom_histogram(binwidth = 200) +
  labs(title = "Item Outlet Sales Histogram", 
       x = "Item Outlet Sales")


# Item Outlet Sales Histogram by Outlet Identifier (using facet_wrap)

ggplot(train, aes(x=Item_Outlet_Sales, fill = Outlet_Identifier)) +
  geom_histogram(binwidth = 200) +
  facet_wrap(~ Outlet_Identifier) +
  labs(title = "Item Outlet Sales Histogram", 
       x = "Item Outlet Sales")

# Item Outlet Sales by Item MRP and Outlet Identifier

ggplot(df, aes(x = Item_MRP, y = Item_Outlet_Sales)) +
  geom_point() +
  facet_wrap(~ Outlet_Identifier) +
  labs(title = "Item Outlet Sales by Item MRP and Outlet Identifier",
       x = "Item MRP",
       y = "Item Outlet Sales")

# Box plot (for outlier detection)

ggplot(df, aes(x = Outlet_Identifier,y = Item_Outlet_Sales)) +
  geom_boxplot() +
  labs(title = "Sales by Outlet Identifier",
       x = "Outlet Identifier",
       y = "Item Outlet Sales")

# Pre-processing on df

# Imputing the na values in the Item_Weight variable 

unique(df$Item_Weight)
sum(is.na(df$Item_Weight))
summary(df$Item_Weight)

df$Item_Weight[is.na(df$Item_Weight)] <-  mean(df$Item_Weight, na.rm = TRUE)

# Item_Fat_Content
unique(df$Item_Fat_Content)
# There are only two types of fat content based items 
# they are Low Fat and Regular
# But the dataset has repetitive entries i.e. LF, low fat, Low Fat, reg Regular
# So we need to clean the levels into just two categories

levels(df$Item_Fat_Content)[levels(df$Item_Fat_Content) %in% c("LF","low fat")] <- "Low Fat"
levels(df$Item_Fat_Content)[levels(df$Item_Fat_Content)%in% c("reg")] <- "Regular"
levels(df$Item_Fat_Content)

# Proportion of fat content based items
prop.table(table(df$Item_Fat_Content))  


# Item_Visibility
unique(df$Item_Visibility)
summary(df$Item_Visibility)
sum(df$Item_Visibility == 0)
# Since, Item_Visibility has values such as O.. which doesn't make any sense
# so finding values with 0 in item_visibility and replacing it with the mean found above in item_visibility

df$Item_Visibility[df$Item_Visibility == 0] <-  mean(df$Item_Visibility)

# Outlet_Size
unique(df$Outlet_Size)
levels(df$Outlet_Size)
# There is a level, which is blank 
# so we have introduce another level to replace the blank ones

levels(df$Outlet_Size)[levels(df$Outlet_Size) == ""] <- "Others"
unique(df$Outlet_Size)

# Creating training and test data

X_train<-df[1:nrow(train),]
X_test<-df[-(1:nrow(train)),]
X_test$Item_Outlet_Sales<-NULL

# Model Builing

model_0 <- lm(Item_Outlet_Sales~.,X_train)
summary(model_0)

result <- predict(model_0, newdata = X_test)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"linear0.csv",row.names = F)

model_00 <- lm(log(Item_Outlet_Sales)~.,X_train)
summary(model_00)

result <- predict(model_00, newdata = X_test)
result <- exp(result)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"log0.csv",row.names = F)

# Decision Tree Model
library(rpart)
set.seed(100)
dt0 <- rpart(formula = Item_Outlet_Sales~.,
             data = X_train)
summary(dt0)
plot(dt0)
text(dt0)

result <- predict(dt0, newdata = X_test)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"dt0.csv",row.names = F)


# Random Forest Model
library(randomForest)
set.seed(100)
rf0 <-randomForest(formula = Item_Outlet_Sales ~ Item_Visibility + Item_Weight + Item_Fat_Content + 
                     Item_Type + Item_MRP + Outlet_Establishment_Year + Outlet_Size + Outlet_Identifier +
                     Outlet_Location_Type + Outlet_Type,
                   data = X_train , ntree=500) 
summary(rf0)
plot(rf0)

result <- predict(rf0, newdata = X_test)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"rf0.csv",row.names = F)


# Generalized Boosted Models (GBM)
library(gbm)
set.seed(100)
fit_gbm0 <- gbm(Item_Outlet_Sales ~ Item_Visibility + Item_Weight + Item_Fat_Content + 
                  Item_Type + Item_MRP + Outlet_Establishment_Year + Outlet_Size + Outlet_Identifier +
                  Outlet_Location_Type + Outlet_Type
                , distribution = "gaussian"
                , n.trees = 1000
                , shrinkage = 0.01
                , interaction.depth = 4
                , n.minobsinnode = 10
                , data=X_train)
fit_gbm0
plot(fit_gbm0)
summary(fit_gbm0)

result <- predict(fit_gbm0, newdata = X_test, n.trees = 1000)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"gbm0.csv",row.names = F)


# Xgboost Model
set.seed(100)
library(xgboost)
model_xgb0 <- xgboost(data.matrix(X_train[,-12]),
                  label = X_train$Item_Outlet_Sales,
                  booster="gbtree",
                  objective="reg:linear",
                  nrounds=500,
                  eta=0.02,
                  max_depth=8, 
                  subsample=0.6,
                  colsample_bytree=0.85, 
                  min_child_weight=1, 
                  eval_metric="rmse")

result <- predict(model_xgb0, data.matrix(X_test))
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"xgboost0.csv",row.names = F)

importance_matrix <- xgb.importance(colnames(X_train$data), model = model_xgb0)

xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")

#xg boost bagging

preds<-vector("list",length=50)
for(i in 1:50){
  print(paste('training model:', i))
  model <- xgboost(data.matrix(X_train[,-12]),
                   label = X_train$Item_Outlet_Sales,
                   booster="gbtree",obective="reg:linear",
                   eta=0.02,
                   nrounds=250,
                   nfold=5,
                   eta=0.02, 
                   max_depth=5, 
                   subsample=0.6,
                   colsample_bytree=0.85,
                   min_child_weight=1, 
                   eval_metric="rmse")
  
  print(paste('applying prediction:', i))
  preds[[i]] <- predict(model, data.matrix(X_test))
}

com_preds <- colMeans(do.call(rbind, preds))
result <- matrix(com_preds,nrow = 5681, ncol = 1, byrow = T)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"bagging0.csv",row.names = F)

################################################################

# Relabelling the Item_Identifier into just 3 main categories
df$I_I = substr(df$Item_Identifier, 1,2)
table(df$I_I)

df$I_I[df$I_I == "DR"] <- "Drinks"
df$I_I[df$I_I == "FD"] <- "Food_Consumables"
df$I_I[df$I_I == "NC"] <- "Non_Consumables"

# Outlet_Identifier
unique(df$Outlet_Identifier)
df$Outlet_Identifier <- factor(df$Outlet_Identifier,
                               levels = c("OUT010", "OUT013", "OUT017", "OUT018", "OUT019", "OUT027",
                               "OUT035", "OUT045", "OUT046", "OUT049"),
                               labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Item_Fat_Content
table(df$Item_Fat_Content)
df$Item_Fat_Content <- factor(df$Item_Fat_Content,
                              levels = c("Low Fat", "Regular"),
                              labels = c(1, 2))

# Outlet_Size
df$Outlet_Size
unique(df$Outlet_Size)
table(df$Outlet_Size)
df$Outlet_Size <- factor(df$Outlet_Size,
                         levels = c("Others", "High", "Medium", "Small"),
                         labels = c(0, 1, 2, 3))

# Outlet_Location_Type
df$Outlet_Location_Type
unique(df$Outlet_Location_Type)
table(df$Outlet_Location_Type)
df$Outlet_Location_Type <- factor(df$Outlet_Location_Type,
                         levels = c("Tier 1", "Tier 2", "Tier 3"),
                         labels = c(1, 2, 3))


# Outlet_Type
df$Outlet_Type
unique(df$Outlet_Type)
table(df$Outlet_Type)
df$Outlet_Type <- factor(df$Outlet_Type,
                         levels = c("Grocery Store", "Supermarket Type1", "Supermarket Type2", "Supermarket Type3"),
                         labels =  c(0, 1, 2, 3))

# I_I
df$I_I
unique(df$I_I)
table(df$I_I)
df$I_I <- factor(df$I_I,
                 levels = c("Drinks", "Food_Consumables", "Non_Consumables"),
                 labels = c(1, 2, 3))

# Item_Type
df$Item_Type
unique(df$Item_Type)
levels(df$Item_Type)
table(df$Item_Type)

df$Item_Type <- factor(df$Item_Type,
                       levels = c("Baking Goods", "Breads", "Breakfast", "Canned", "Dairy",
                                  "Frozen Foods", "Fruits and Vegetables", "Hard Drinks", "Health and Hygiene", "Household",
                                  "Meat", "Others", "Seafood", "Snack Foods", "Soft Drinks","Starchy Foods"),
                       labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8,
                                  9, 10, 11, 12, 13, 14, 15))

# Item_Identifier                       
#df$Item_Identifier <- NULL

# Creating training and test data

X_train1<-df[1:nrow(train),]
X_test1<-df[-(1:nrow(train)),]
X_test1$Item_Outlet_Sales<-NULL

# Model Builing

# Linear Model
model_01 <- lm(Item_Outlet_Sales~.,X_train1)
summary(model_01)

result <- predict(model_01, newdata = X_test1)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"linear_1.csv",row.names = F)

# Log Model
model_001 <- lm(log(Item_Outlet_Sales)~.,X_train1)
summary(model_001)

result <- predict(model_001, newdata = X_test1)
result <- exp(result)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"log_1.csv",row.names = F)

# Decision Tree Model
library(rpart)
set.seed(100)
dt01 <- rpart(formula = Item_Outlet_Sales~.,
             data = X_train1)

summary(dt01)
plot(dt01)
text(dt01)

result <- predict(dt01, newdata = X_test1)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"tree_1.csv",row.names = F)

# Random Forest Model
library(randomForest)
set.seed(100)
rf01 <-randomForest(formula = Item_Outlet_Sales~. ,
                   data = X_train1[-1] , ntree=500) 

summary(rf01)
plot(rf01)

result <- predict(rf01, newdata = X_test1)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"rf1.csv",row.names = F)

# Generalized Boosted Models (GBM)
library(gbm)
set.seed(100)
fit_gbm01 <- gbm(Item_Outlet_Sales ~.
                 , distribution = "gaussian"
                 , n.trees = 1000
                 , shrinkage = 0.01
                 , interaction.depth = 4
                 , n.minobsinnode = 10
                 , data= X_train1[-1])

fit_gbm01
plot(fit_gbm01)
summary(fit_gbm01)

result <- predict(fit_gbm01, newdata = X_test1, n.trees = 1000)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"gbm_1.csv",row.names = F)

# Xgboost Model

set.seed(100)
library(xgboost)
model_xgb1 <- xgboost(data.matrix(X_train1[,c(-12)]),
                     label = X_train1$Item_Outlet_Sales,
                     booster="gbtree",
                     objective="reg:linear",
                     nrounds=500,
                     eta=0.02,
                     max_depth=8, 
                     subsample=0.6,
                     colsample_bytree=0.85, 
                     min_child_weight=1, 
                     eval_metric="rmse")


result <- predict(model_xgb1, data.matrix(X_test1))
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"xgboost_1.csv",row.names = F)

importance_matrix1 <- xgb.importance(colnames(X_train1$data), model = model_xgb1)

xgb.plot.importance(importance_matrix1, rel_to_first = TRUE, xlab = "Relative importance")


names(X_train1)
names(X_test1)

#xg boost bagging

preds1<-vector("list",length=50)
for(i in 1:50){
  print(paste('training model:', i))
  model <- xgboost(data.matrix(X_train1[,-12]),
                   label = X_train1$Item_Outlet_Sales,
                   booster="gbtree",obective="reg:linear",
                   eta=0.02,
                   nrounds=250,
                   nfold=5,
                   eta=0.02, 
                   max_depth=5, 
                   subsample=0.6,
                   colsample_bytree=0.85,
                   min_child_weight=1, 
                   eval_metric="rmse")
  
  print(paste('applying prediction:', i))
  preds[[i]] <- predict(model, data.matrix(X_test1))
}

com_preds1 <- colMeans(do.call(rbind, preds))

result <- matrix(com_preds1,nrow = 5681, ncol = 1, byrow = T)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"bagging_1.csv",row.names = F)
