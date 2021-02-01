library(openxlsx)
data=  read.xlsx("Training.xlsx")
str(data)
View(data)
summary(data)
attach(data)

#univariate analysis
head(data)
attach(data)
hist(Target.Variable)

hist(Page.likes)
hist(Page.Checkins)
hist(Page.talking.about)
hist(Page.Category)
hist(Feature.5)
hist(Feature.6)
hist(Feature.7)
hist(Feature.8)
hist(Feature.9)
hist( Feature.10)
hist( Feature.11)
hist(Feature.12)
hist(Feature.13)
hist(Feature.14)
hist(Feature.15)
hist(Feature.16)
hist(Feature.17)
hist(Feature.18)
hist(Feature.19)
hist(Feature.20)
hist(Feature.21)
hist(Feature.22)
hist(Feature.23)
hist(Feature.24)
hist(Feature.25)
hist(Feature.26)
hist(Feature.27)
hist(Feature.28)
hist(Feature.29 )
hist(CC1)
hist(CC2)
hist(CC3)
hist(CC4)
hist(CC5)
hist(Base.Time)
hist(Post.Length)
hist(Post.Share.Count)
hist(H.local)

table(Post.published.weekday)

install.packages("vcd")
library(vcd)

ggplot(data= data) + geom_bar(aes(x = Post.published.weekday ))

library(DataExplorer)
plot_intro(data = data)



DataExplorer::plot_correlation(data$Target.Variable,data$Page.likes)

install.packages("esquisse")
library(esquisse)
esquisse()

esquisse::esquisser()

barplot(data$Base.DateTime.weekday,data$Target.Variable)



plot_correlation(
  data,
  type = c("continuous"),
  maxcat = 20L,
  cor_args = list(),
  geom_text_args = list(),
  title = NULL,
  ggtheme = theme_gray(),
  theme_config = list(legend.position = "bottom", axis.text.x = element_text(angle =
                                                                               90))
  
)

str(data)

#transforming the 2 categorical varibale to factors. 

data$Post.published.weekday=as.factor(data$Post.published.weekday)
data$Base.DateTime.weekday=as.factor(data$Base.DateTime.weekday)

str(data)




#eliminating  ID and post promotion status

data=data[c(-1,-39)]
str(data)



#now, using the KNN method, to impute missing value in the dataset. 

#since cc5 is difference of cc3 and cc2 , replacing the values in cc5 as differnce in cc3 cc2. 

CC2 = data$CC2[is.na(data$CC5)]
CC3 = data$CC3[is.na(data$CC5)]

CC5 = CC2- CC3

data$CC5[is.na(data$CC5)]=CC5


#now using KNN to impute the missing values

library(VIM)


dataKNN=kNN(data = data ,variable = c("Page.likes","Page.Checkins","Page.talking.about","Page.Category","Feature.29","Feature.27","Feature.25","Feature.20","Feature.22", "Feature.18","Feature.10", "Feature.13","Feature.7","Feature.15","CC1","CC4"),k=10)


dataKNN=dataKNN[c(1:41)]
summary(dataKNN)

#checking for missing value via plot


library(DataExplorer)
plot_missing(dataKNN)



library(caret)
library(caTools)


#splitting the data into train and test set. 

index = sample.split(dataKNN$Target.Variable, SplitRatio = 0.7)
train = subset(dataKNN, index = T)
test = subset(dataKNN, index= F)

#Treating the outliers. 


capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}


#outlier treatment for training set

train$Page.likes=capOutlier(train$Page.likes)
train$Page.Checkins=capOutlier(train$Page.Checkins)
train$Page.talking.about=capOutlier(train$Page.talking.about)
train$Feature.5=capOutlier(train$Feature.5)
train$Feature.6=capOutlier(train$Feature.6)
train$Feature.7=capOutlier(train$Feature.7)
train$Feature.8=capOutlier(train$Feature.8)
train$Feature.9=capOutlier(train$Feature.9)
train$Feature.10=capOutlier(train$Feature.10)
train$Feature.11=capOutlier(train$Feature.11)
train$Feature.12=capOutlier(train$Feature.12)
train$Feature.13=capOutlier(train$Feature.13)
train$Feature.14=capOutlier(train$Feature.14)
train$Feature.15=capOutlier(train$Feature.15)
train$Feature.16=capOutlier(train$Feature.16)
train$Feature.17=capOutlier(train$Feature.17)
train$Feature.18=capOutlier(train$Feature.18)
train$Feature.19=capOutlier(train$Feature.19)
train$Feature.20=capOutlier(train$Feature.20)
train$Feature.21=capOutlier(train$Feature.21)
train$Feature.22=capOutlier(train$Feature.22)
train$Feature.23=capOutlier(train$Feature.23)
train$Feature.24=capOutlier(train$Feature.24)
train$Feature.25=capOutlier(train$Feature.25)
train$Feature.26=capOutlier(train$Feature.26)
train$Feature.27=capOutlier(train$Feature.27)
train$Feature.28=capOutlier(train$Feature.28)
train$Feature.29=capOutlier(train$Feature.29)
train$CC1=capOutlier(train$CC1)
train$CC2=capOutlier(train$CC2)
train$CC3=capOutlier(train$CC3)
train$CC4=capOutlier(train$CC4)
train$CC5=capOutlier(train$CC5)
train$Target.Variable=capOutlier(train$Target.Variable)
train$Base.Time=capOutlier(train$Base.Time)
train$Post.Length=capOutlier(train$Post.Length)
train$Post.Share.Count=capOutlier(train$Post.Share.Count)
train$H.local=capOutlier(train$H.local)
train$Page.Category=capOutlier(train$Page.Category)



#treating outlier for test set. 




test$Page.likes=capOutlier(test$Page.likes)
test$Page.Checkins=capOutlier(test$Page.Checkins)
test$Page.talking.about=capOutlier(test$Page.talking.about)
test$Feature.5=capOutlier(test$Feature.5)
test$Feature.6=capOutlier(test$Feature.6)
test$Feature.7=capOutlier(test$Feature.7)
test$Feature.8=capOutlier(test$Feature.8)
test$Feature.9=capOutlier(test$Feature.9)
test$Feature.10=capOutlier(test$Feature.10)
test$Feature.11=capOutlier(test$Feature.11)
test$Feature.12=capOutlier(test$Feature.12)
test$Feature.13=capOutlier(test$Feature.13)
test$Feature.14=capOutlier(test$Feature.14)
test$Feature.15=capOutlier(test$Feature.15)
test$Feature.16=capOutlier(test$Feature.16)
test$Feature.17=capOutlier(test$Feature.17)
test$Feature.18=capOutlier(test$Feature.18)
test$Feature.19=capOutlier(test$Feature.19)
test$Feature.20=capOutlier(test$Feature.20)
test$Feature.21=capOutlier(test$Feature.21)
test$Feature.22=capOutlier(test$Feature.22)
test$Feature.23=capOutlier(test$Feature.23)
test$Feature.24=capOutlier(test$Feature.24)
test$Feature.25=capOutlier(test$Feature.25)
test$Feature.26=capOutlier(test$Feature.26)
test$Feature.27=capOutlier(test$Feature.27)
test$Feature.28=capOutlier(test$Feature.28)
test$Feature.29=capOutlier(test$Feature.29)
test$CC1=capOutlier(test$CC1)
test$CC2=capOutlier(test$CC2)
test$CC3=capOutlier(test$CC3)
test$CC4=capOutlier(test$CC4)
test$CC5=capOutlier(test$CC5)
test$Target.Variable=capOutlier(test$Target.Variable)
test$Base.Time=capOutlier(test$Base.Time)
test$Post.Length=capOutlier(test$Post.Length)
test$Post.Share.Count=capOutlier(test$Post.Share.Count)
test$H.local=capOutlier(test$H.local)
test$Page.Category=capOutlier(test$Page.Category)



#next scaling the data, pre processing it and using boxcox transformation. 


process = preProcess(train , method = c('scale' , 'center' , 'BoxCox'))
process


#find the predicted values for both dataset


train1 = predict(process,train)

test1 = predict(process , test)




#creating Random forest model 


library(randomForest)
RFmodel =randomForest(Target.Variable ~ ., data = train1, mtry = 3, nodesize = 10,ntree = 501,importance =TRUE)
RFmodel
plot(RFmodel)
importance.variables=importance(RFmodel)
plot(importance.variables)

#prediction

pred = predict(RFmodel , newdata = test1)
head(pred)

#rmse score
RMSE(pred,test1$Target.Variable)
mape(test1$Target.Variable , pred)









#Creating Linear model


lmmodel = lm(Target.Variable~.,data=train)
summary(lmmodel)
plot(lmmodel)

prediction = predict(lmmodel,newdata = test)
mape(test$Target.Variable,prediction)
par(mfrow=c(2,2))
plot(lmmodel)


RMSE(prediction,test1$Target.Variable)
mape(test1$Target.Variable , prediction)





#SVM model
control = trainControl(method = "repeatedcv" , number = 10 , repeats = 3)

SVMLinear = train(target_variable~.,data = SVMTrain,method = "svmLinear",trControl=control,preProcess = c("center", "scale"),tuneLength = 10)


pred = predict(SVMLinear , newdata = test1)
head(pred)

RMSE(pred,test1$Target.Variable)
mape(test1$Target.Variable , pred)


#Ensemblemodel






# now doing stepwise regression. 
library(MASS)
multiplelr=lm(Target.Variable~.,data=train1)

stepAIC(multiplelr,direction="both")








#now performing linear model with cross validation on the data.


control = trainControl(method = "repeatedcv" , number = 10 , repeats = 3)
model = train(Target.Variable~. , data= train1 , method = 'lm' , trControl = control)
model
summary(model)

#prediction


pred = predict(model , newdata = test1)
head(pred)

library(Metrics)
mape(test1$Target.Variable , pred)



















