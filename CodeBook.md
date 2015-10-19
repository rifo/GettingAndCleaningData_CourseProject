Code book 
=================

* This document provides information about the generated output and file by the script `run_analysis.R`, provided in this repository.

* The data used by this analysis come from: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

"The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain."

* The file generated by the script "tidyTable.txt" contains a table, following the tidy data principles (Each measured variable is in one column, each different observation of that variable is in a different row).

The columns of the file are respectively:
**"SUBJECT": the id of the subject under observation
**"ACTIVITY": the name of the observed activity (one of "LAYING", "SITTING", "STANDING", "WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"

** the average of each selected variable for each activity and each subject. The selected variables are only the measurements on the mean and standard deviation for each measurement. For details on single measurements, see below.
"TIME__BODY_ACCELLERATION_MEAN_X" 
"TIME__BODY_ACCELLERATION_MEAN_Y" 
"TIME__BODY_ACCELLERATION_MEAN_Z" 
"TIME__BODY_ACCELLERATION_STANDARDDEVIATION_X" 
"TIME__BODY_ACCELLERATION_STANDARDDEVIATION_Y" 
"TIME__BODY_ACCELLERATION_STANDARDDEVIATION_Z" 
"TIME_GRAVITYACCELLERATION_MEAN_X" 
"TIME_GRAVITYACCELLERATION_MEAN_Y" 
"TIME_GRAVITYACCELLERATION_MEAN_Z" 
"TIME_GRAVITYACCELLERATION_STANDARDDEVIATION_X" 
"TIME_GRAVITYACCELLERATION_STANDARDDEVIATION_Y" 
"TIME_GRAVITYACCELLERATION_STANDARDDEVIATION_Z" 
"TIME__BODY_ACCELLERATIONJERK_MEAN_X" 
"TIME__BODY_ACCELLERATIONJERK_MEAN_Y" 
"TIME__BODY_ACCELLERATIONJERK_MEAN_Z" 
"TIME__BODY_ACCELLERATIONJERK_STANDARDDEVIATION_X" 
"TIME__BODY_ACCELLERATIONJERK_STANDARDDEVIATION_Y" 
"TIME__BODY_ACCELLERATIONJERK_STANDARDDEVIATION_Z" 
"TIME__BODY_GYROSCOPE_MEAN_X" 
"TIME__BODY_GYROSCOPE_MEAN_Y" 
"TIME__BODY_GYROSCOPE_MEAN_Z" 
"TIME__BODY_GYROSCOPE_STANDARDDEVIATION_X" 
"TIME__BODY_GYROSCOPE_STANDARDDEVIATION_Y" 
"TIME__BODY_GYROSCOPE_STANDARDDEVIATION_Z" 
"TIME__BODY_GYROSCOPEJERK_MEAN_X" 
"TIME__BODY_GYROSCOPEJERK_MEAN_Y" 
"TIME__BODY_GYROSCOPEJERK_MEAN_Z" 
"TIME__BODY_GYROSCOPEJERK_STANDARDDEVIATION_X" 
"TIME__BODY_GYROSCOPEJERK_STANDARDDEVIATION_Y" 
"TIME__BODY_GYROSCOPEJERK_STANDARDDEVIATION_Z" 
"TIME__BODY_ACCELLERATIONMAGNITUTE_MEAN_" 
"TIME__BODY_ACCELLERATIONMAGNITUTE_STANDARDDEVIATION_" 
"TIME_GRAVITYACCELLERATIONMAGNITUTE_MEAN_" 
"TIME_GRAVITYACCELLERATIONMAGNITUTE_STANDARDDEVIATION_" 
"TIME__BODY_ACCELLERATIONJERKMAGNITUTE_MEAN_" 
"TIME__BODY_ACCELLERATIONJERKMAGNITUTE_STANDARDDEVIATION_" 
"TIME__BODY_GYROSCOPEMAGNITUTE_MEAN_" 
"TIME__BODY_GYROSCOPEMAGNITUTE_STANDARDDEVIATION_" 
"TIME__BODY_GYROSCOPEJERKMAGNITUTE_MEAN_" 
"TIME__BODY_GYROSCOPEJERKMAGNITUTE_STANDARDDEVIATION_" 
"FREQUENCY__BODY_ACCELLERATION_MEAN_X" 
"FREQUENCY__BODY_ACCELLERATION_MEAN_Y" 
"FREQUENCY__BODY_ACCELLERATION_MEAN_Z" 
"FREQUENCY__BODY_ACCELLERATION_STANDARDDEVIATION_X"
"FREQUENCY__BODY_ACCELLERATION_STANDARDDEVIATION_Y" 
"FREQUENCY__BODY_ACCELLERATION_STANDARDDEVIATION_Z" 
"FREQUENCY__BODY_ACCELLERATION_MEANFREQUENCY_X" 
"FREQUENCY__BODY_ACCELLERATION_MEANFREQUENCY_Y" 
"FREQUENCY__BODY_ACCELLERATION_MEANFREQUENCY_Z" 
"FREQUENCY__BODY_ACCELLERATIONJERK_MEAN_X" 
"FREQUENCY__BODY_ACCELLERATIONJERK_MEAN_Y" 
"FREQUENCY__BODY_ACCELLERATIONJERK_MEAN_Z" 
"FREQUENCY__BODY_ACCELLERATIONJERK_STANDARDDEVIATION_X" 
"FREQUENCY__BODY_ACCELLERATIONJERK_STANDARDDEVIATION_Y" 
"FREQUENCY__BODY_ACCELLERATIONJERK_STANDARDDEVIATION_Z" 
"FREQUENCY__BODY_ACCELLERATIONJERK_MEANFREQUENCY_X" 
"FREQUENCY__BODY_ACCELLERATIONJERK_MEANFREQUENCY_Y" 
"FREQUENCY__BODY_ACCELLERATIONJERK_MEANFREQUENCY_Z" 
"FREQUENCY__BODY_GYROSCOPE_MEAN_X" 
"FREQUENCY__BODY_GYROSCOPE_MEAN_Y" 
"FREQUENCY__BODY_GYROSCOPE_MEAN_Z" 
"FREQUENCY__BODY_GYROSCOPE_STANDARDDEVIATION_X" 
"FREQUENCY__BODY_GYROSCOPE_STANDARDDEVIATION_Y" 
"FREQUENCY__BODY_GYROSCOPE_STANDARDDEVIATION_Z" 
"FREQUENCY__BODY_GYROSCOPE_MEANFREQUENCY_X" 
"FREQUENCY__BODY_GYROSCOPE_MEANFREQUENCY_Y" 
"FREQUENCY__BODY_GYROSCOPE_MEANFREQUENCY_Z" 
"FREQUENCY__BODY_ACCELLERATIONMAGNITUTE_MEAN_" 
"FREQUENCY__BODY_ACCELLERATIONMAGNITUTE_STANDARDDEVIATION_" 
"FREQUENCY__BODY_ACCELLERATIONMAGNITUTE_MEANFREQUENCY_" 
"FREQUENCY__BODY_ACCELLERATIONJERKMAGNITUTE_MEAN_" 
"FREQUENCY__BODY_ACCELLERATIONJERKMAGNITUTE_STANDARDDEVIATION_" "FREQUENCY__BODY_ACCELLERATIONJERKMAGNITUTE_MEANFREQUENCY_" 
"FREQUENCY__BODY_GYROSCOPEMAGNITUTE_MEAN_" 
"FREQUENCY__BODY_GYROSCOPEMAGNITUTE_STANDARDDEVIATION_" 
"FREQUENCY__BODY_GYROSCOPEMAGNITUTE_MEANFREQUENCY_" 
"FREQUENCY__BODY_GYROSCOPEJERKMAGNITUTE_MEAN_" 
"FREQUENCY__BODY_GYROSCOPEJERKMAGNITUTE_STANDARDDEVIATION_" 
"FREQUENCY__BODY_GYROSCOPEJERKMAGNITUTE_MEANFREQUENCY_"


* The original measurese comes from the provided dataset in input:

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean


