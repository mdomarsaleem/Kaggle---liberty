
library(ggplot2)
library(randomForest)
library(readr)

set.seed(1)

train <- read_csv("train.csv")
test <- read_csv("test.csv")

# We'll convert all the characters to factors so we can train a randomForest model on them
extractFeatures <- function(data) {
  character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
  for (col in character_cols) {
    data[,col] <- as.factor(data[,col])
  }
  return(data)
}

trainFea <- extractFeatures(train)
testFea  <- extractFeatures(test)

 t = trainFea
 t$Hazard = NULL
 tot = rbind(t,testFea)
 tot$T2_V13 = as.numeric(tot$T2_V13)
 tot$T2_V12 = as.numeric(tot$T2_V12)
 tot$T1_V4 = as.numeric(tot$T1_V4)
 tot$T1_V5 = as.numeric(tot$T1_V5)
 tot$T1_V6 = as.numeric(tot$T1_V6)
 tot$T1_V7 = as.numeric(tot$T1_V7)
 tot$T1_V8 = as.numeric(tot$T1_V8)
 tot$T1_V9 = as.numeric(tot$T1_V9)
 tot$T1_V11 = as.numeric(tot$T1_V11)
 tot$T1_V12 = as.numeric(tot$T1_V12)
 tot$T1_V15 = as.numeric(tot$T1_V15)
 tot$T1_V16 = as.numeric(tot$T1_V16)
 tot$T1_V17 = as.numeric(tot$T1_V17)
 tot$T2_V3 = as.numeric(tot$T2_V3)
 tot$T2_V5 = as.numeric(tot$T2_V5)
 tot$T2_V11 = as.numeric(tot$T2_V11)
 tot$T2_V12 = as.numeric(tot$T2_V12)
 pre1 = preProcess(tot)
 Norm = predict(pre1,tot)
 KMC = kmeans(Norm,centers=6,iter.max=1000)
 tot$cluster = KMC$cluster
 trainFea$cluster = tot$cluster[1:50999]
 testFea$cluster = tot$cluster[51000:101999]
cat("Training model\n")
rf <- randomForest(trainFea[,3:34], trainFea$Hazard, ntree=500, imp=TRUE, sampsize=10000, do.trace=TRUE)
numFolds = trainControl( method = "cv", number = 10 )

rf = train(trainFea[,3:34], trainFea$Hazard,  
           method="rf", nodesize=5, ntree=500, metric="ROC", trControl=numFolds,do.trace=TRUE)


cat("Making predictions\n")
submission <- data.frame(Id=test$Id)
submission$Hazard <- predict(rf, extractFeatures(testFea[,2:33]))
write_csv(submission, "1_random_forest_benchmark.csv")


t$T2_V13 = as.numeric(t$T2_V13)
t$T2_V12 = as.numeric(t$T2_V12)
t$T1_V4 = as.numeric(t$T1_V4)
t$T1_V5 = as.numeric(t$T1_V5)
t$T1_V6 = as.numeric(t$T1_V6)
t$T1_V7 = as.numeric(t$T1_V7)
t$T1_V8 = as.numeric(t$T1_V8)
t$T1_V9 = as.numeric(t$T1_V9)
t$T1_V11 = as.numeric(t$T1_V11)
t$T1_V12 = as.numeric(t$T1_V12)
t$T1_V15 = as.numeric(t$T1_V15)
t$T1_V16 = as.numeric(t$T1_V16)
t$T1_V17 = as.numeric(t$T1_V17)
t$T2_V3 = as.numeric(t$T2_V3)
t$T2_V5 = as.numeric(t$T2_V5)
t$T2_V11 = as.numeric(t$T2_V11)
t$T2_V12 = as.numeric(t$T2_V12)



t$T11_V1  = t$T1_V1^2
t$T11_V2  =t$T1_V2^2
t$T11_V3  =t$T1_V3^2
t$T11_V4  =t$T1_V4^2
t$T11_V5  =t$T1_V5^2
t$T11_V6  =t$T1_V6^2
t$T11_V7   =t$T1_V7^2
t$T11_V8  =t$T1_V8^2
t$T11_V9   =t$T1_V9^2
t$T11_V10  =t$T1_V10^2
t$T11_V11  =t$T1_V11^2
t$T11_V12  =t$T1_V12^2
t$T11_V13  =t$T1_V13^2
t$T11_V14  =t$T1_V14^2
t$T11_V15  =t$T1_V15^2
t$T11_V16 =t$T1_V16^2
t$T11_V17  =t$T1_V17^2
t$T21_V1   =t$T2_V1^2
t$T21_V2   =t$T2_V2^2
t$T21_V3   =t$T2_V3^2
t$T21_V4  =t$T2_V4^2
t$T21_V5   =t$T2_V5^2
t$T21_V6  =t$T2_V6^2
t$T21_V7   =t$T2_V7^2
t$T21_V8   =t$T2_V8^2
t$T21_V9   =t$T2_V9^2
t$T21_V10  =t$T2_V10^2
t$T21_V11  =t$T2_V11^2
t$T21_V12  =t$T2_V12^2
t$T21_V13  =t$T2_V13^2
t$T21_V14  =t$T2_V14^2
t$T21_V15  =t$T2_V15^2
t$T12_V1  =t$T1_V1^3
t$T12_V2  =t$T1_V2^3
t$T12_V3 =t$T1_V3^3
t$T12_V4  =t$T1_V4^3
t$T12_V5  =t$T1_V5^3
t$T12_V6  =t$T1_V6^3
t$T12_V7   =t$T1_V7^3
t$T12_V8   =t$T1_V8^3
t$T12_V9   =t$T1_V9^3
t$T12_V10  =t$T1_V10^3
t$T12_V11  =t$T1_V11^3
t$T12_V12  =t$T1_V12^3
t$T12_V13  =t$T1_V13^3
t$T12_V14  =t$T1_V14^3
t$T12_V15  =t$T1_V15^3
t$T12_V16  =t$T1_V16^3
t$T12_V17  =t$T1_V17^3
t$T22_V1   =t$T2_V1^3
t$T22_V2   =t$T2_V2^3
t$T22_V3   =t$T2_V3^3
t$T22_V4   =t$T2_V4^3
t$T22_V5   =t$T2_V5^3
t$T22_V6   =t$T2_V6^3
t$T22_V7   =t$T2_V7^3
t$T22_V8   =t$T2_V8^3
t$T22_V9   =t$T2_V9^3
t$T22_V10  =t$T2_V10^3
t$T22_V11  =t$T2_V11^3
t$T22_V12  =t$T2_V12^3
t$T22_V13  =t$T2_V13^3
t$T22_V14  =t$T2_V14^3
t$T22_V15 =t$T2_V15^3
library(caret)
rf = randomForest(Hazard~.,data=t,do.trace=TRUE,imp=TRUE,ntree=200)
varImp(rf)
a = prcomp(tot)
nzv = nearZeroVar(tot,saveMetrics=TRUE)
df = predict(a,newdata=tot)[,1:10]
df1 = data.frame(df)

varImpPlot(rf,type=2)