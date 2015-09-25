
### Assignement for the Machine Learning Course  
## question -> input data -> features -> algorithm -> parameters -> evaluation



library( ggplot2)
library ( caret)

## Get the training data 
PMLTrain<-read.csv (  file = "pml-training.csv")

#########################  Preprocessing data
colPML<-colnames( PMLTrain)

##### Check columns with missing values. We gone exclude them  
NAcol <-sapply (colPML, MyNa<-function (x) { sum ( is.na(PMLTrain[,x]) )}  )

####  NAcol column with NA value zero NAvalue 
#### We keep PMLTrain column  with zero  NA value
PMLTrainInUse<-PMLTrain[NAcol==0]
PMLTrainNotUse<-PMLTrain[NAcol!=0]
####   93 columns kept over  160 

##### Checks columns with DIV0  value
##### Remove Factor columns  --  Factor column contains DIV0 value 
colPML<-colnames( PMLTrainInUse)
Factorcol <-sapply (colPML, MyFactor<-function (x) { sum ( x!="classe" && is.factor(PMLTrainInUse[,x]) )}  )
PMLTrainInUse<-PMLTrainInUse[Factorcol==0]

#####  Check how many columns remain 56 columns kept over  160  
length(colnames(PMLTrain))
length(colnames(PMLTrainInUse))


#### Remove non Usable Variable for prediction  
PMLTrainInUse$"user_name" <-NULL
PMLTrainInUse$"X" <-NULL
PMLTrainInUse$"raw_timestamp_part_1" <-NULL
PMLTrainInUse$"raw_timestamp_part_2" <-NULL


####### Column with most variability
nzv<-nearZeroVar( PMLTrainInUse, saveMetrics = TRUE )
nzv

####  At least, the selected features look usable for prediction  
###colnames(PMLTrainInUse) 
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
##  Not convinced by exploratory analysis

dim( PMLTrainInUse)

#### Preparing training and test set 
inTrain <- createDataPartition(y=PMLTrainInUse$classe, p=0.70, list=FALSE)
training <- PMLTrainInUse[inTrain,]
testing <- PMLTrainInUse[-inTrain,]
dim( training)


################### PCA 
#### Perform a  Principal Decomposition analysis capturing  90%  a f the cumulative variance 
preProc <- preProcess(PMLTrainInUse[,-54],method="pca",thresh = 0.90)
### Decomposition results in 19  PC vectors 

####  Apply the rotation  matrix to  the train set and test set.  
trainPC <- predict(preProc,training[,-54])
testPC <- predict(preProc,testing[,-54])


###################   random Forest
####  Cross validation  :   5  Folds  5 should be good to  analyse the average error rate, 
####   but random forest lasts too long. Need to  take  care of my family and result look  goods.  
trainControl(method =  "cv", number = 1, verboseIter = TRUE, seeds =  123  ) 


#####  Use RAndom Forest algorithm on the trainPC  
modelFit <- train(training$classe ~ .,method="rf",data=trainPC)

### Model is generated with Random forest algorithm . A Cross validation is used to get ea better idea of the error rate  
modelFit

# mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
# 2    0.9637577  0.9541418  0.002755723  0.003487378
# 10    0.9579457  0.9467982  0.003010547  0.003799889
# 19    0.9473902  0.9334525  0.005416401  0.006846233

###  Accuracy  is good. close to 1  

#### View In sample error // optimistic view of the error rate   
confusionMatrix(training$classe,predict(modelFit,trainPC))

##Confusion matrix is good. But might be due to overfitting.  

#### View Out of sample error with the test set  
confusionMatrix(testing$classe,predict(modelFit,testPC))

# ---- REsult look good,  as specificity and sensity are high
# Class: A Class: B Class: C Class: D Class: E
# Sensitivity            0.9780   0.9649   0.9331   0.9709   0.9944
# Specificity            0.9936   0.9914   0.9925   0.9875   0.9969
# Pos Pred Value         0.9839   0.9640   0.9649   0.9357   0.9861
# Neg Pred Value         0.9912   0.9916   0.9854   0.9945   0.9988
# Prevalence             0.2862   0.1934   0.1803   0.1579   0.1823
# Detection Rate         0.2799   0.1866   0.1682   0.1533   0.1813
# Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
# Balanced Accuracy      0.9858   0.9781   0.9628   0.9792   0.9956

###   Out of sample error looks quite good too. For sensitivity and specificity.  quite impressive.    

## Let's check if this can be use for prediction

######  Prediction Assignement
Prob<-read.csv (  file = "pml-testing.csv")
str( Prob)

#####  Apply Preprocessing to  the prob data  // this should  be included in a user defined function, but no time
ProbInUSe<-Prob[NAcol==0]
ProbInUSe<-ProbInUSe[Factorcol==0]
ProbInUSe$"user_name" <-NULL
ProbInUSe$"X" <-NULL
ProbInUSe$"raw_timestamp_part_1" <-NULL
ProbInUSe$"raw_timestamp_part_2" <-NULL


#####  Apply rotation to problem items   
ProbInUSePC<-predict ( preProc, ProbInUSe[,-54])


##### Apply Model on the prob data 
ProbPredict<-predict (modelFit,ProbInUSePC)


###  And that all done.  
###  Remaining for the submission formatting.   


#####  For submission  convert to  character vector  
ProbInUSePCStr<-as.character(ProbPredict)


####  Use propoosed function to  generate the result   
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

##### Write down the result.  
pml_write_files(ProbInUSePCStr)


