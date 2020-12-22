rm(list=ls())

fetalhealth= read.csv(file.choose(), header= TRUE)

attach(fetalhealth)

names(fetalhealth)

## We use an ordinal logistic regression model with holdout method 80/20

set.seed(1)
train = sample(1:nrow(fetalhealth), 0.8 * nrow(fetalhealth))
training.data= fetalhealth[train, ]
testing.data = fetalhealth[-train, ]
testing.true= FH[-train]

str(fetalhealth)

FH= as.ordered(FH)
str(FH)

library(MASS)
library(effects)

#After looking at the variables, we can see that Histogram Tendency is a qualitative
## variable that has been transformed into a quantitative. We must specify that by
# doing the following: We also change the response variable into a factor 

HistTend = as.factor(HistTend)

#Fit the model using training data
m2= polr(as.factor(FH)~.-HistMax- FM- MVSTV- MVLTV- HistNumPeaks- HistNumZeros- HistMean- HistMedi- HistTend, data= training.data, Hess= TRUE)

summary(m2)


ctable= coef(summary(m2))
p = pnorm(abs(ctable[,"t value"]), lower.tail= FALSE)*2
ctable= cbind(ctable, 'p value'= p)

#Holding everything else constant, 
#an increase in value of accelerations by one unit, 
#decrease the expected value of fetal health in log odds by 796.

#Holding everything else constant, 
#an increase in value of PD by one unit, 
#increase the expected value of fetal health in log odds by 2044.

# The expected odds of normal fetal health category when other variables
# assume a value of zero 12.4.

# We see that there is no p-value which makes it hard to know which variables
# are significant. We can create a p-value.


#Prediction made on the test data

pred_prob= predict(m2, training.data[1:5,], type= 'prob')

M2predict1= predict(m2, testing.data)

M2table= table(M2predict1,testing.true)

#Accuracy rate
accuracy= mean(M2predict1== testing.true)

#Error rate
error= mean(M2predict1!= testing.true)

library(AppliedPredictiveModeling)

transparentTheme(trans = .4)
library(caret)
featurePlot(x = fetalhealth[,1:21], 
            y = fetalhealth$FH, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))



# 5-Fold CV - Ordinal Logistic Regression

set.seed(2)
k= 5

folds= sample(1:5, nrow(fetalhealth), replace= TRUE)

accuracy= rep(0,5)


for (i in 1:5)
{
  m3= polr(as.factor(FH)~.-HistMax- FM- MVSTV- MVLTV- HistNumPeaks- HistNumZeros- HistMean- HistMedi- HistTend, Hess= TRUE, data= fetalhealth[folds!=i,])
  test.fh= fetalhealth[folds==i,]
  
  m3predict= predict(m3, testing.data)
  
  test.true1= FH[folds==i]
  accuracy[i]= mean((m3predict-test.true1)^2)
}

i = 5

accuracy[i]= mean((m3predict-test.true1)^2)
