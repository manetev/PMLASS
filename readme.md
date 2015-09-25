## This repo is to be used for assignment projet in Practical Machine Learning Course

It contains : 
- R files with comments according to  the readme.md 
- The compiled notebook in HTML format
- csv  tests and train files

The following explained all the detail step

## Analysis study

## Step  1 - Get the data from the csv  and apply preprocessing 
The following preprocessing has been applied : 
-  remove columns with NA value  
-  remove columns with DIV0 value  
-  remove columns no suitable for predicting (  timestamp, user ,  ..  ) 

Checks if column with near zero variability remains 

## Exploratory graphic analysis 
Several tests graphics has been produced, but as i consider as not meaningfull ,  i removed them.  
I would be interested if you  get other ideas.  

## PCA decomposition  
We check if a Principal Analysis decomposition can be useful.  
We tried to  capture  90%  of the variance. 
This result is a decomposition  with  19  rotation vectors. That's far less than the  60 initial columns.  

The rotation is applied to  the test and training set as part of the preprocessing.    

## Error rate 
To  get an idea of the error rate,  the cross validation has been choosen. With a relatively low K to  limit the variance.   
K = 5 has been choosen first,   however, time is limited. As the train function was lasting more than 45 min, i ve decided to  reduce to  one.  
As the results were good. It kept it.  

# Train method Random forest algorithm  
At first, glm method was choosen. A linear model for this problem does not fit.  Results were poor.  
Then, i  choose random forest, as try, for the reputation.  And it appears to get good results.    


On the training set, error rate are quite good and confusion matix is perfect.  However, some overfitting could be ther , so we need to  check  on the test set.  

On the test set, confusion matrix seems to  operate well.   

At last, we predict the 20 items requested, and include  them in the files to  be uploaded.  

Access to  the HTML files with execution and comments  here [here](https://github.com/manetev/PMLASS/blob/master/MyAssignement.html)

Thanks for reading.  Running out of time for more.  

    


 

 

 