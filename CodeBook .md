#Code Book








Activity_labels.txt -  Links the class labels with their activity name.


-1 WALKING
-2 WALKING_UPSTAIRS
-3 WALKING_DOWNSTAIRS
-4 SITTING
-5 STANDING
-6 LAYING




The following files are available for the train and test data. Their descriptions are equivalent. 


- 'train/y_train.txt': Training labels.
- 'test/y_test.txt': Test labels.
Each row represents an activity as described in the activity label for each window sample. Range from 1 to 6


The following files are available for the train and test data. Their descriptions are equivalent. 


- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 


Training and Testing Variables Contained in 
'train/X_train.txt': Training set.




From the 561 variables on the training and test files the following variables were extracted for the assignments.  The criteria was mean or std in the variable name.


- 1:               tBodyAcc-mean()-X
- 2:               tBodyAcc-mean()-Y
- 3:               tBodyAcc-mean()-Z
- 4:            tGravityAcc-mean()-X
- 5:            tGravityAcc-mean()-Y
- 6:            tGravityAcc-mean()-Z
- 7:           tBodyAccJerk-mean()-X
- 8:           tBodyAccJerk-mean()-Y
- 9:           tBodyAccJerk-mean()-Z
-10:              tBodyGyro-mean()-X
-11:              tBodyGyro-mean()-Y
-12:              tBodyGyro-mean()-Z
-13:          tBodyGyroJerk-mean()-X
-14:          tBodyGyroJerk-mean()-Y
-15:          tBodyGyroJerk-mean()-Z
-16:              tBodyAccMag-mean()
-17:           tGravityAccMag-mean()
-18:          tBodyAccJerkMag-mean()
-19:             tBodyGyroMag-mean()
-20:         tBodyGyroJerkMag-mean()
-21:               fBodyAcc-mean()-X
-22:               fBodyAcc-mean()-Y
-23:               fBodyAcc-mean()-Z
-24:           fBodyAcc-meanFreq()-X
-25:           fBodyAcc-meanFreq()-Y
-26:           fBodyAcc-meanFreq()-Z
-27:           fBodyAccJerk-mean()-X
-28:           fBodyAccJerk-mean()-Y
-29:           fBodyAccJerk-mean()-Z
-30:       fBodyAccJerk-meanFreq()-X
-31:       fBodyAccJerk-meanFreq()-Y
-32:       fBodyAccJerk-meanFreq()-Z
-33:              fBodyGyro-mean()-X
-34:              fBodyGyro-mean()-Y
-35:              fBodyGyro-mean()-Z
-36:          fBodyGyro-meanFreq()-X
-37:          fBodyGyro-meanFreq()-Y
-38:          fBodyGyro-meanFreq()-Z
-39:              fBodyAccMag-mean()
-40:          fBodyAccMag-meanFreq()
-41:      fBodyBodyAccJerkMag-mean()
-42:  fBodyBodyAccJerkMag-meanFreq()
-43:         fBodyBodyGyroMag-mean()
-44:     fBodyBodyGyroMag-meanFreq()
-45:     fBodyBodyGyroJerkMag-mean()
-46: fBodyBodyGyroJerkMag-meanFreq()
-47:                tBodyAcc-std()-X
-48:                tBodyAcc-std()-Y
-49:                tBodyAcc-std()-Z
-50:             tGravityAcc-std()-X
-51:             tGravityAcc-std()-Y
-52:             tGravityAcc-std()-Z
-53:            tBodyAccJerk-std()-X
-54:            tBodyAccJerk-std()-Y
-55:            tBodyAccJerk-std()-Z
-56:               tBodyGyro-std()-X
-57:               tBodyGyro-std()-Y
-58:               tBodyGyro-std()-Z
-59:           tBodyGyroJerk-std()-X
-60:           tBodyGyroJerk-std()-Y
-61:           tBodyGyroJerk-std()-Z
-62:               tBodyAccMag-std()
-63:            tGravityAccMag-std()
-64:           tBodyAccJerkMag-std()
-65:              tBodyGyroMag-std()
-66:          tBodyGyroJerkMag-std()
-67:                fBodyAcc-std()-X
-68:                fBodyAcc-std()-Y
-69:                fBodyAcc-std()-Z
-70:            fBodyAccJerk-std()-X
-71:            fBodyAccJerk-std()-Y
-72:            fBodyAccJerk-std()-Z
-73:               fBodyGyro-std()-X
-74:               fBodyGyro-std()-Y
-75:               fBodyGyro-std()-Z
-76:               fBodyAccMag-std()
-77:       fBodyBodyAccJerkMag-std()
-78:          fBodyBodyGyroMag-std()
-79:      fBodyBodyGyroJerkMag-std()




Data was combined into final file “totaldatafinalmean”


Activity variable was created using the activity label.


Script is available in tidydatafile


Another script was created to produce the tidy file “tidyutil”




# Libray used


-library(dplyr)
-library(utils)        )
-library(reshape2)        
-library(plyr)