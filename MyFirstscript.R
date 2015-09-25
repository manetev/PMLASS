

##### question -> input data -> features -> algorithm -> parameters -> evaluation

library( ggplot2)
library ( caret)
setwd( "C:/Users/Flomanu/Documents/Manu/Tech/R stuff/Practical Machine Learning/Assignement")

PMLTrain<-read.csv (  file = "pml-training.csv")

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

colnames(PMLTrainInUse) 
summary(PMLTrainInUse) 

#####  EXplore via plotting
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")
qplot(wage,colour=education,data=training,geom="density")



#####
colPMLInUse<-colnames(PMLTrainInUse)
ForearmCol<- grep ( "_forearm",  colPMLInUse)
BeltCol<-colPML[ grep ( "_belt",  colPMLInUse)]
ArmCol<-colPML[ grep ( "_arm",  colPMLInUse)]
DumbbellCol<-colPML[ grep ( "dumbbell",  colPMLInUse)]

length(ForearmCol) + length( BeltCol) + length ( ArmCol)+ length ( DumbbellCol)
length(colPML)

featurePlot(x = PMLTrainInUse[,c("magnet_dumbbell_z", "accel_forearm_y")],
            y = PMLTrainInUse$classe,
            plot = "pair",
            ## Add a key at the top
            auto.key = list(columns = 4))

featurePlot(x=PMLTrainInUse[,colPMLInUse[ForearmCol]],
            y = PMLTrainInUse$classe,
            plot="pairs")

qplot(wage,colour=education,data=training,geom="density")



#### # We concentrate on ForearmCol
PMLTrainInUse[,colPML[ForearmCol]]


dim( PMLTrainInUse)

str( c(1,2))
#### Preparing training and test set 
inTrain <- createDataPartition(y=PMLTrainInUse$classe,
                               p=0.75, list=FALSE)
training <- PMLTrainInUse[inTrain,]
testing <- PMLTrainInUse[-inTrain,]
dim( training)



M <- abs(cor(training[,-54]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

####### Column with most variability
MostVar<-unique ( rownames(Q) ) 


nzv<-nearZeroVar( PMLTrain, saveMetrics = TRUE )


set.seed(32343)
preProc <- preProcess(PMLTrainInUse[,-54],method="pca",pcaComp=20)
trainPC <- predict(preProc,training[,-54])
modelFit <- train(training$classe ~ .,method="rf",data=trainPC)
testPC <- predict(preProc,testing[,-54])
confusionMatrix(testing$classe,predict(modelFit,testPC))

--- > Random Forest +  PCA 20 tres bon 

 
trainPC <- predict(preProc,(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

