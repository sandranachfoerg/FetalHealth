rm(list= ls())

fetalh= read.csv(file.choose(), header = TRUE)

attach(fetalh)
names(fetalh)

#Classification Tree

# Step 1: split the training and testing set

set.seed(1)
train = sample(1:nrow(fetalh), nrow(fetalh)*0.8)
train.set= fetalh[train,]
test.set= fetalh[-train,]
test.true= FH[-train]

library(tree)

# Step 2: build the tree model
fetal.tree.model= tree(as.factor(FH)~., data= train.set)

#Step 3: find size of tree that is the best

fetal.cv.tree= cv.tree(fetal.tree.model, K= 10, FUN= prune.misclass)

plot(fetal.cv.tree)
#Based on the result 17 is the best 

#Step 4: Prune the tree

fetal.prune.model= prune.tree(fetal.tree.model, best= 17)
plot(fetal.prune.model)
text(fetal.prune.model, pretty = 0)

# Step 5: Prediction

fetal.prune.predict= predict(fetal.prune.model, test.set, type= 'class')
table(fetal.prune.predict, test.true)

#Overall Accuracy:  TP/ Observations

# Forest 

library(randomForest)

set.seed(6)

as.factor()

bag.fetalh= randomForest(as.factor(FH)~., data= train.set)
bag.fetalh1= randomForest(as.factor(FH)~., data= train.set, mtry = 21, importance= TRUE)

predict1.fetalh= predict(bag.fetalh, test.set)

predict2.fetalh= predict(bag.fetalh1, test.set)

cm= table(predict1.fetalh, test.true)
cm2= table(predict2.fetalh, test.true)

accuracy = table(predict1.fetalh == test.true)
accuracy1= table(predict2.fetalh== test.true)


## Random forest

set.seed(2)

rf.fetalh= randomForest(as.factor(FH)~., data= train.set, mtry= 5, importance= TRUE)



predict3.fetalh= predict(rf.fetalh, test.set)

cm3= table(predict3.fetalh, test.true)

importance(rf.fetalh, main= 'Random Forest')
varImpPlot(rf.fetalh, main= 'Random Forest')
