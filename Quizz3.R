library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)


1 Subset the data to a training set and testing set based on the Case variable in the data set. 
2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
3. In the final model what would be the final model prediction for cases with the following variable values:
  a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 


##### Step 1 
inTrain <- createDataPartition(y=segmentationOriginal$Case, p=0.75, list=FALSE)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

##### Step 2
set.seed(125)
dim( training)
modelFit <- train( Case ~ . , data = training, method = "rpart" )
)
#####  Hypothese a 
sum(
  segmentationOriginal$TotalIntenCh2 > 23000
  & segmentationOriginal$TotalIntenCh2 < 24000
  & segmentationOriginal$FiberWidthCh1 == 10 
  & segmentationOriginal$PerimStatusCh1 ==2 
)





#####  Hypothese b 
sum(
  segmentationOriginal$TotalIntenCh2 > 50000
  & segmentationOriginal$TotalIntenCh2 < 51000
  & segmentationOriginal$FiberWidthCh1 == 10 
  & segmentationOriginal$VarIntenCh4 ==100
)


#####  Hypothese c
sum(
  segmentationOriginal$TotalIntenCh2 > 57000
  & segmentationOriginal$TotalIntenCh2 < 58000
  & segmentationOriginal$FiberWidthCh1 == 8 
  & segmentationOriginal$VarIntenCh4 ==100
)

#####  Hypothese d
sum(
  segmentationOriginal$FiberWidthCh1 == 8 
  & segmentationOriginal$VarIntenCh4 ==100
  & segmentationOriginal$PerimStatusCh1 == 2 
)



####################################"
question 3   

library(pgmm)
data(olive)
olive = olive[,-1]

str( olive$Area)

olive$Area<-as.factor(olive$Area)
##### Step 1 

ModelTree<-tree ( Area ~ .  , olive)
newdata = as.data.frame(t(colMeans(olive[,-1])))
newdata = as.data.frame(t(colMeans(olive)))

newdata
res <-predict (ModelTree,  newdata)
res
str(olive)
res
summary(ModelTree) 

res
olive[,-1]
colnames((olive))

]
##### --> C'est étrange on a un variable 


#### -------------> Reponse 2.875 2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata


##### Question 4  
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

colnames(trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

trainSA$chd<-as.factor(trainSA$chd)
ModelFit<-train (chd ~ age + alcohol + obesity + tobacco + typea + ldl
                 , method ="glm" , data=trainSA)


Prediction <-predict  (ModelFit,trainSA )
Prediction <-as.numeric(Prediction) 

missClass ( trainSA, Prediction )

chd
colnames( SAheart) 
colnames()

Then set the seed to 13234 and fit a logistic regression model 
(method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd)
as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco,
type-A behavior, and low density lipoprotein cholesterol as predictors.
Calculate the misclassification rate for your model using this function 
and a prediction on the "response" scale:
  
  