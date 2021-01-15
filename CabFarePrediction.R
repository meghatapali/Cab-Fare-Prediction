rm(list=ls())


#Set working directory
setwd("D:/Edwisor/Project 1")

#Check working directory
getwd()

# loading all required librares.
install.packages (c("corrgram",  "randomForest","geosphere", "usdm", "DMwR"))

# loading the data
train=read.csv("train_cab.csv",header=TRUE)
test=read.csv("test.csv",header=TRUE)

# exploring the data.
str(train)
str(test)
summary(train)
summary(test)
head(train,5) 
head(train,5)

# converting the features in the required data types.
train$fare_amount = as.numeric(as.character(train$fare_amount))
train$passenger_count=round(train$passenger_count)

########### data cleaning  #############

# fare amount cannot be less than one 
# considering fare amount 453 as max and removing all the fare amount greater than 453, as chances are
# very less of fare amount having 4000 and 5000 ...etc
train[which(train$fare_amount < 1 ),]
nrow(train[which(train$fare_amount < 1 ),]) # to show the count i.e.,5
train = train[-which(train$fare_amount < 1 ),]  # removing those values.
train[which(train$fare_amount>453),]
nrow(train[which(train$fare_amount >453 ),]) # to show the count i.e., 2
train = train[-which(train$fare_amount >453 ),]  # removing those values.

# passenger count cannot be Zero
# even if we consider suv max seat is 6, so removing passenger count greater than 6.
train[which(train$passenger_count < 1 ),]
nrow(train[which(train$passenger_count < 1 ),]) # to show count, that is 58
train=train[-which(train$passenger_count < 1 ),] # removing the values
train[which(train$passenger_count >6 ),]
nrow(train[which(train$passenger_count >6 ),]) # to show count, that is 20
train=train[-which(train$passenger_count >6 ),] # removing the values

#Checking passenger count in test data
nrow(test[which(test$passenger_count < 1 ),])
nrow(test[which(test$passenger_count > 6 ),])

# Latitudes range from -90 to 90.Longitudes range from -180 to 180.
# Removing which does not satisfy these ranges.
print(paste('pickup_longitude above 180=',nrow(train[which(train$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(train[which(train$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(train[which(train$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(train[which(train$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(train[which(train$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(train[which(train$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(train[which(train$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(train[which(train$dropoff_latitude > 90 ),])))
train = train[-which(train$pickup_latitude > 90),] # removing one data point

# Also we will see if there are any values equal to 0.
nrow(train[which(train$pickup_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
nrow(train[which(train$dropoff_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
# removing those data points.
train=train[-which(train$pickup_longitude == 0 ),]
train=train[-which(train$dropoff_longitude == 0),]

#Checking the same in test
print(paste('pickup_longitude above 180=',nrow(test[which(test$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(test[which(test$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(test[which(test$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(test[which(test$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(test[which(test$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(test[which(test$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(test[which(test$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(test[which(test$dropoff_latitude > 90 ),])))

# Also we will see if there are any values equal to 0.
nrow(test[which(test$pickup_longitude == 0 ),])
nrow(test[which(test$pickup_latitude == 0 ),])
nrow(test[which(test$dropoff_longitude == 0 ),])
nrow(test[which(test$pickup_latitude == 0 ),])


# checking for missing values.
sum(is.na(train))
sum(is.na(test))
train=na.omit(train) # we have removed the missing values...as they are less...likely 77 missing values.
sum(is.na(train))  

# deriving the new features using pickup_datetime and coordinated provided.
# new features will be year,month,day_of_week,hour
# Convert pickup_datetime from factor to date time
train$pickup_datetime=as.Date(train$pickup_datetime)
pickup_time = strptime(train$pickup_datetime,format='%Y-%m-%d %H:%M:%S UTC')
train$date = as.integer(format(train$pickup_date,"%d"))
train$mnth = as.integer(format(train$pickup_date,"%m"))
train$yr = as.integer(format(train$pickup_date,"%Y"))


# for test data set.
test$pickup_datetime=as.Date(test$pickup_datetime)
pickup_time = strptime(test$pickup_datetime,format='%Y-%m-%d %H:%M:%S UTC')
test$date = as.integer(format(test$pickup_date,"%d"))# Monday = 1
test$mnth = as.integer(format(test$pickup_date,"%m"))
test$yr = as.integer(format(test$pickup_date,"%Y"))

#create new variable
library(geosphere)
train$distance= distHaversine(cbind(train$pickup_longitude, train$pickup_latitude), cbind(train$dropoff_longitude,train$dropoff_latitude))
#the output is in metres, Change it to kms
train$distance=as.numeric(train$dist)/1000

#same for testdata
test$distance= distHaversine(cbind(test$pickup_longitude, test$pickup_latitude), cbind(test$dropoff_longitude,test$dropoff_latitude))
#the output is in metres, Change it to kms
test$distance=as.numeric(test$dist)/1000



# removing the features, which were used to create new features.
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,pickup_datetime))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,pickup_datetime))
str(train)
summary(train)

#Removing the outliers in distance column

nrow(train[which(train$distance ==0 ),])
nrow(test[which(test$distance==0 ),])
nrow(train[which(train$distance >130 ),]) # considering the distance 130 as max and considering rest as outlier.
nrow(test[which(test$distance >130 ),])

# removing the data points by considering the above conditions,
train=train[-which(train$distance ==0 ),]
train=train[-which(train$distance >130 ),]
test=test[-which(test$distance ==0 ),]

# feature selection
numeric_index = sapply(train,is.numeric) #selecting only numeric
numeric_data = train[,numeric_index]
cnames = colnames(numeric_data)
cnames

#Correlation analysis for numeric variables
library(corrgram)
corrgram(train[,numeric_index], main = "Correlation Plot")
#Features are not highly correlated with each other. 
#And label "fare_amount" is highly correlated with distance feature.

#removing date
# pickup_weekdat has p value greater than 0.05 
train = subset(train,select=-date)
#remove from test set
test = subset(test,select=-date)

#Data Visualization and Feature Scaling
d=density(train$fare_amount)
plot(d,main="distribution")
polygon(d,col="green",border="red")

D=density(train$distance)
plot(D,main="distribution")
polygon(D,col="green",border="red")

A=density(test$distance)
plot(A,main="distribution")
polygon(A,col="black",border="red")

# since skewness of  variable is high, apply log transform to reduce the skewness


# log transformation.
train$fare_amount=log1p(train$fare_amount)
test$distance=log1p(test$distance)
train$distance=log1p(train$distance)

# checking back features after transformation.
d=density(train$fare_amount)
plot(d,main="distribution")
polygon(d,col="green",border="red")
D=density(train$distance)
plot(D,main="distribution")
polygon(D,col="red",border="black")
A=density(test$distance)
plot(A,main="distribution")
polygon(A,col="black",border="red")


###check multicollearity
 library(usdm)
 vif(train[,-1])
 #No variable from the 5 input variables has collinearity problem. 
 #The linear correlation coefficients ranges between: 
 #min correlation ( mnth ~ passenger_count ):  -0.001868147 
 #max correlation ( yr ~ mnth ):  -0.1091115 
 


 
# model building
# preparing the data
set.seed(1200)
Train.index = sample(1:nrow(train), 0.8 * nrow(train))
Train = train[ Train.index,]
Test  = train[-Train.index,]

#head(Test[,2:5],5)
TestData=test


# linear regression
linear_model=lm(fare_amount~.,data=Train)
summary(linear_model)
predict_lm=predict(linear_model,Test[,2:5])
#predict_test=predict(linear_model,TestData)
library(DMwR)
regr.eval(Test[,1],predict_lm)
#mae        mse       rmse       mape 
#0.17515868 0.07527370 0.27436053 0.07621996 


# decision tree regressor
library(rpart)
DT=rpart(fare_amount~.,data=Train, method='anova')
predictions_tree=predict(DT,Test[,2:5])
#predictions_test=predict(DT,TestData)
summary(DT)
regr.eval(Test[,1],predictions_tree)
#mae        mse       rmse       mape 
#0.19168951 0.07668177 0.27691474 0.08409804

# random forest regressor
library(randomForest)
random_model = randomForest(fare_amount~ ., Train[,1:5], ntree = 500)
RF_Predictions = predict(random_model, Test[,2:5])
regr.eval(Test[,1],RF_Predictions)
#mae        mse       rmse       mape 
#0.21834016 0.09150787 0.30250268   0.09690444

##############Model Selection and Final Tuning##########################

#Random Forest with using mtry = 2 that is fixing only two variables to split at each tree node 
library(randomForest)
random_model_tuning = randomForest(fare_amount~ ., Train[,1:5], ntree = 15, mtry= 2, importance= TRUE)
RF_Predictions = predict(random_model_tuning, Test[,2:5])
regr.eval(Test[,1],RF_Predictions)
#mae        mse       rmse       mape 
#0.17583970 0.06578149 0.25647903 0.07780042 

#Random Forest
pred_data$fare_amount = predict(random_model_tuning, test)

write.csv(pred_data, "Predicted_Data.csv", row.names = F)







