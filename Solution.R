
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

# Item Outlet Sales Histogram by Outlet Identifier

ggplot(train, aes(x=Item_Outlet_Sales, fill = Outlet_Identifier)) +
  geom_histogram(binwidth = 200) +
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


# Item Outlet Sales Histogram by Item_Weight

ggplot(train, aes(x=Item_Outlet_Sales, fill = factor(Item_Weight))) +
  geom_histogram(binwidth = 200) +
  labs(title = "Item Outlet Sales Histogram", 
       x = "Item Outlet Sales")



# Pre-processing on df

# Imputing the na values in the Item_Weight variable 