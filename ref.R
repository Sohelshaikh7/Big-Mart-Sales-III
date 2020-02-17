# Setting the working directory
getwd()
setwd("C:/Users/DELL/Desktop/R_projects/Big Mart Sales III")

train<-read.csv(file="train_kOBLwZA.csv", sep=",", header=T)
test<-read.csv(file="test_t02dQwI.csv", sep=",", header = T)
summary(train)
test$Item_Outlet_Sales<-NA
total<-rbind(train,test)
table(is.na(total))
summary(total)

#Item weight 
total$Item_Weight[is.na(total$Item_Weight)]<-mean(total$Item_Weight,na.rm=T)
boxplot(total$Item_Weight)

#Item fat content
levels(total$Item_Fat_Content)[levels(total$Item_Fat_Content)%in% c("LF","low fat")]<-"Low Fat"
levels(total$Item_Fat_Content)[levels(total$Item_Fat_Content)%in% c("reg")]<-"Regular"

#Outlet size
levels(total$Outlet_Size)[levels(total$Outlet_Size)==""]<-"others"

#item type
# table(total$Item_Identifier)
# total$Item_Identifier<-as.character(total$Item_Identifier)
# total$Item_Identifier<-substr(total$Item_Identifier,1,2)
# total$type[total$Item_Identifier=="FD"]="Food"
# total$type[total$Item_Identifier=="DR"]="Drinks"
# total$type[total$Item_Identifier=="NC"]="Non-consumable"
# total$type=as.factor(total$type)

#year
total$year<-2013-total$Outlet_Establishment_Year
summary(total$year)

#visibility
total$Item_Visibility[total$Item_Visibility==0]<-median(total$Item_Visibility)
summary(total$Item_Visibility)

total$Outlet_Establishment_Year<-NULL



X_train<-total[1:nrow(train),]
X_test<-total[-(1:nrow(train)),]
X_test$Item_Outlet_Sales<-NULL

lm
fit<-lm(log(Item_Outlet_Sales)~.,X_train)
summary(fit)

#xg boost

library(xgboost)
model_xg<-xgboost(data.matrix(X_train[,-11]),label = X_train$Item_Outlet_Sales,booster="gbtree",objective="reg:linear",nrounds=350,eta=0.02,max_depth=8, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")
pred1<-predict(model_xg,data.matrix(X_test))
sub4=data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=pred1)
write.csv(sub4,"subxg.csv",row.names = F)
names<-dimnames(data.matrix(X_train[,-11]))[[2]]
abc<-xgb.importance(names,model=model_xg)
xgb.plot.importance(abc)

#after xg boost of importance (not helping 1167)
total$year<-NULL
total$Outlet_Size<-NULL
total$Item_Fat_Content<-NULL
total$Outlet_Location_Type<-NULL

#xg boost bagging
preds<-vector("list",length=50)
for(i in 1:50){
  print(paste('training model:', i))
  model <- xgboost(data.matrix(X_train[,-11]),label = X_train$Item_Outlet_Sales,booster="gbtree",obective="reg:linear",eta=0.02,nrounds=250,nfold=5,eta=0.02, max_depth=5, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")
  
  print(paste('applying prediction:', i))
  preds[[i]] <- predict(model, data.matrix(X_test))
}

com_preds <- colMeans(do.call(rbind, preds))
result <- matrix(com_preds,nrow = 5681, ncol = 1, byrow = T)
result1 <- data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test$Outlet_Identifier,Item_Outlet_Sales=result)
write.csv(result1,"sub_bag.csv",row.names = F)