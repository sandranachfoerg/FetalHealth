rm(list=ls())

fetalhealth= read.csv(file.choose(), header= TRUE)

attach(fetalhealth)
names(fetalhealth)

str(fetalhealth)

library(class)

## Standardize some of the variables

stand.BV= scale(BV)
stand.ASTV= scale(ASTV)
stand.MVSTV= scale(MVSTV)
stand.PTALTV= scale(PTALTV)
stand.MVLTV= scale(MVLTV)
stand.HistWidth= scale(HistWidth)
stand.HistMin= scale(HistMin)
stand.HistMax= scale(HistMax)
stand.HistMode = scale(HistMode)
stand.HistMean= scale(HistMean)
stand.HistMedi= scale(HistMedi)
stand.HistVar= scale(HistVar)

input.var= cbind(stand.BV, stand.ASTV, stand.MVSTV,HistNumPeaks,
                 stand.PTALTV, stand.MVLTV,stand.HistWidth,stand.HistMin,
                 stand.HistMax,stand.HistMode,stand.HistMean, stand.HistMedi, 
                 stand.HistVar, acc,FM, UC, LD, SD,PD, HistNumZeros,HistTend)

head(input.var)

## 5-fold CV:  KNN

set.seed(1)

k = 5

accuracy= matrix(0, 10,5)

folds = sample(1:5, nrow(input.var), replace= TRUE)

for (j in 1:10)
{
  for (i in 1:5)
  {
    training.set = input.var[folds!=i,]
    testing.set= input.var[folds ==i,]
    training.true= FH[folds!=i]
    testing.true= FH[folds==i]
    
    knn.prediction= knn(training.set, testing.set, training.true, k= j)
    table(knn.prediction, testing.true)
    
    accuracy[j,i]= mean(knn.prediction==testing.true)
    
  }
}

cv.accuracy= apply(accuracy, 1, mean)
plot(cv.accuracy, main= "5-Fold Cross-Validation", type="b", xlab="K-Value",ylab="Accuracy level")

which.max(cv.accuracy)

j= 3
i = 5

accuracy[j,i]= mean(knn.prediction==testing.true)


## 10-FOLD CV- KNN

set.seed(2)

k = 10

accuracym2= matrix(0, 10,10)

folds = sample(1:10, nrow(input.var), replace= TRUE)

for (j in 1:10)
{
  for (i in 1:10)
  {
    M2training.set = input.var[folds!=i,]
    M2testing.set= input.var[folds ==i,]
    M2training.true= FH[folds!=i]
    M2testing.true= FH[folds==i]
    
    M2knn.prediction= knn(M2training.set, M2testing.set, M2training.true, k= j)
    table(M2knn.prediction, M2testing.true)
    
    accuracym2[j,i]= mean(M2knn.prediction==M2testing.true)
    
  }
}

M2cv.accuracy= apply(accuracy, 1, mean)
which.max(M2cv.accuracy)

j= 10
i = 3

accuracym2[j,i]= mean(M2knn.prediction==M2testing.true)

plot(M2cv.accuracy, main= "10-Fold Cross-Validation", type="b", xlab="K-Value",ylab="Accuracy level")
## 5- fold is better!


