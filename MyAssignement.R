

##### question -> input data -> features -> algorithm -> parameters -> evaluation

library( ggplot2)
library ( caret)
setwd( "C:/Users/Emmanuel/Documents/Manu/Tech/R stuff/Practical Machine Learning/Assignement")
PMLTrain<-read.csv (  file = "pml-training.csv")


#########################  Preprocessing data
colPML<-colnames( PMLTrain)

##### Check column with missing values
NAcol <-sapply (colPML, MyNa<-function (x) { sum ( is.na(PMLTrain[,x]) )}  )

####  NAcol column with NA value zero NAvalue 
#### We keep PMLTrain column  with zero  NA value
PMLTrainInUse<-PMLTrain[NAcol==0]
PMLTrainNotUse<-PMLTrain[NAcol!=0]
####   93 columns kept over  160 

#####  Remove Factor columns  --  Factor column contains DIV0 value 
colPML<-colnames( PMLTrainInUse)
Factorcol <-sapply (colPML, MyFactor<-function (x) { sum ( x!="classe" && is.factor(PMLTrainInUse[,x]) )}  )
PMLTrainInUse<-PMLTrainInUse[Factorcol==0]

length(colnames(PMLTrain))
length(colnames(PMLTrainInUse))
####   56 columns kept over  160  


#### Remove non Usable Variable 
PMLTrainInUse$"user_name" <-NULL
PMLTrainInUse$"X" <-NULL
PMLTrainInUse$"raw_timestamp_part_1" <-NULL
PMLTrainInUse$"raw_timestamp_part_2" <-NULL


####### Column with most variability
nzv<-nearZeroVar( PMLTrainInUse, saveMetrics = TRUE )
nzv

colnames(PMLTrainInUse) 
summary(PMLTrainInUse) 

#####GRoup variable by  type 
### Not useful  
# colPMLInUse<-colnames(PMLTrainInUse)
# ForearmCol<- grep ( "_forearm",  colPMLInUse)
# BeltCol<-colPML[ grep ( "_belt",  colPMLInUse)]
# ArmCol<-colPML[ grep ( "_arm",  colPMLInUse)]
# DumbbellCol<-colPML[ grep ( "dumbbell",  colPMLInUse)]
# 
# length(ForearmCol) + length( BeltCol) + length ( ArmCol)+ length ( DumbbellCol)
# length(colPML)

# featurePlot(x = PMLTrainInUse[,c("magnet_dumbbell_z", "accel_forearm_y")],
#             y = PMLTrainInUse$classe,
#             plot = "pair",
#             ## Add a key at the top
#             auto.key = list(columns = 4))
# 
# featurePlot(x=PMLTrainInUse[,colPMLInUse[ForearmCol]],
#             y = PMLTrainInUse$classe,
#             plot="pairs")



dim( PMLTrainInUse)


#### Preparing training and test set 
inTrain <- createDataPartition(y=PMLTrainInUse$classe, p=0.70, list=FALSE)
training <- PMLTrainInUse[inTrain,]
testing <- PMLTrainInUse[-inTrain,]
dim( training)




################### PCA 
preProc <- preProcess(PMLTrainInUse[,-54],method="pca",pcaComp=20)
trainPC <- predict(preProc,training[,-54])
testPC <- predict(preProc,testing[,-54])


###################   random Forest
####  Cross validation  :   5  Fold 
trainControl(method =  "cv", number = 5, verboseIter = TRUE, seeds =  123  ) 

#####  Use RAndom Forest algorithm on the trainPC  
modelFit <- train(training$classe ~ .,method="rf",data=trainPC)

#### View In ssample error // optimistic view of the error rate   
confusionMatrix(training$classe,trainPC)


#### View Out of sample error with the test set  
confusionMatrix(testing$classe,predict(modelFit,testPC))

--- > Random Forest +  PCA 20 tres bon 


########################."  glm  
trainPC <- predict(preProc,(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)


##### Fold1 
date()
trainPC <- predict(preProc,train1[,-54])
modelFit1 <- train(train1$classe ~ .,method="rf",data=trainPC)

testPC <- predict(preProc,PMLTrainInUse[test1,-54])
Conf1<-confusionMatrix(test1$classe,predict(modelFit,testPC))


##### Fold2 
date()
trainPC <- predict(preProc,train2[,-54])
modelFit2 <- train(train2$classe ~ .,method="rf",data=trainPC)

testPC <- predict(preProc,PMLTrainInUse[test2,-54])
Conf2<-confusionMatrix(test2$classe,predict(modelFit,testPC))

##### Fold3 
date()
trainPC <- predict(preProc,train3[,-54])
modelFit3 <- train(train3$classe ~ .,method="rf",data=trainPC)

testPC <- predict(preProc,PMLTrainInUse[test2,-54])
Conf3<-confusionMatrix(test3$classe,predict(modelFit,testPC))

date()


##########  
Average  Error rates



################################
####  Try glm 






