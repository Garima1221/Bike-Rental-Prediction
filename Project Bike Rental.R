#clean R environment 
rm(list = ls())

#set working directory 
setwd("C:/Users/Garima/Downloads/Edwisor/Project Bike Rental")

#load data
data_set = read.csv("day.csv")

##load required libraries
required_lib = c("corrplot","tree","randomForest","usdm","ggplot2")
install.packages(required_lib)
lapply(required_lib,require,character.only = TRUE)

#structure of the data
str(data_set)

## summary of the data
summary(data_set)


## Check for missing values in the data
sum(is.na(data_set))
## no missing value



## feature instant and dteday can be dropped out as 
#the instant is only a serial number and does not indicate any useful information
#whereas for the dteday feature most of the information that can be extracted from a date data is already available
#casual and registered are leaky variables , as cnt = casual+registered
data_set = subset(data_set,select = -c(instant,dteday,casual,registered))

## Univariate analysis
categorical_features = c("season","yr","mnth","holiday","weekday","workingday","weathersit")
numerical_features = c("temp","atemp","hum","windspeed")


##################### visualisations ###############################
## univariate  plot 
for(i in numerical_features){
  dev.new()
  hist(data_set[,i],col = "blue",main = i,xlab = i,breaks = 30,cex.lab = 1,cex.axis = 1)
}

for(i in categorical_features){
  dev.new()
  barplot(table(data_set[,i]),col = "blue",main = i,xlab = i,ylab = "frequency", cex.lab = 1,cex.axis = 1)
}

## bivariate plots
for(i in numerical_features){
  dev.new()
  plot(data_set[,i],data_set$cnt,col = "red",xlab = i,ylab = "bike count")
}


## aggregate category wise 
for(i in categorical_features){
  category_count = aggregate(data_set$cnt,by = list(Category = data_set[,i]),FUN = sum)
  dev.new()
  p = ggplot(category_count,aes(category_count$Category,category_count$x)) + geom_bar(stat="identity",fill = "blue") + labs(y = "Bike Count",x=i)
  print(p)
}

#####################Feature scaling############################################
#normalisation 
data_set$cnt = (data_set$cnt-min(data_set$cnt))/(max(data_set$cnt)-min(data_set$cnt))
#data_set$cnt = log(data_set$cnt)
######################## outlier analysis#####################################
##capping outlier to max and min value of whisker
dev.off()
for(i in numerical_features){
  dev.new()
  b = boxplot(data_set[,i])
  print(b)
  boxplot_value = boxplot.stats(data_set[,i])
  outlier_value = boxplot_value$out
  stats_value = boxplot_value$stats
  print(names(data_set[i]))
  print(outlier_value)
  data_set[which(data_set[,i]<stats_value[1]),i] = stats_value[1]
  data_set[which(data_set[,i]>stats_value[5]),i] = stats_value[5]
  b2 = boxplot(data_set[,i])
  print(b2)
}

## correlation 
#correlation_matrix = cor(data_set[,numerical_features])
#corrplot(correlation_matrix,method = "number",type = 'lower')
## remove temp as temp and atemp highly correlated

## multicoliinearity 
vif(data_set[,numerical_features])
##temp can be dropped

## ANOVA
for(i in categorical_features){
  print(i)
  aov_summary = summary(aov(data_set$cnt~data_set[,i],data = data_set))
  print(aov_summary)
  
}

data_set  = subset(data_set,select = -c(temp,weekday,workingday,holiday))

###################### Sampling #############################################
# 70% of the data is in train set and 30% in test set
set.seed(08012019)
train = sample(nrow(data_set),0.70*nrow(data_set))
test = - train

#################### Model Development #####################################
## inspite of using several variations in the forms of predictor variables ,unable to remove nonlinearity
## R2 and RSE both were deterioting thus we left the linear model as it is 
modelLR = lm(data_set$cnt~.,data = data_set,subset = train)
summary(modelLR)
plot(modelLR)
predictLR = predict(modelLR,data_set[test,])
plot(data_set$cnt[test],col="blue")
lines(predictLR,col = "red")
## test error
sqrt(mean((data_set$cnt[test]-predictLR)^2))
## 0.1484
## Linear model will not work well here thus we move to other methods 
## decision tree 
modelDT = tree(data_set$cnt~.,data = data_set,subset = train)
summary(modelDT)
modelDT
plot(modelDT)
text(modelDT,pretty = 0)
predictDT = predict(modelDT,data_set[test,])
## test error 
sqrt(mean(data_set$cnt[test]- predictDT)^2)
##0.007

## cross validation 
#cv.modelDT = cv.tree(modelDT)
#plot(cv.modelDT$size,cv.modelDT$dev,type = 'b')

## plot shows deviance to remain constant after size of 6 
## thus we prune the tree to check whether there is any improvement in the model

#prune.modelDT = prune.tree(modelDT,best = 6)
#plot(prune.modelDT)
#text(prune.modelDT,pretty = 0)

#predictDT = predict(prune.modelDT,data_set[test,])
## test error 
#sqrt(mean(data_set$cnt[test]- predictDT)^2)
##0.012
##no improvement ,thus we do not prune the tree

## random forest 
modelRF = randomForest(data_set$cnt~.,data = data_set,subset = train,ntree=100)
varImpPlot(modelRF)
importance(modelRF)
predictRF = predict(modelRF,data_set[test,])
## test error 
sqrt(mean((data_set$cnt[test]- predictRF)^2))
##0.1265

#library(caret)
#control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
#mtry <- sqrt(ncol(data_set))
#rf_random <- train(data_set$cnt~., data=data_set, method="rf", tuneLength=7, trControl=control)
#print(rf_random)
#plot(rf_random)

