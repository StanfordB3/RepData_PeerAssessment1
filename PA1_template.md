# Reproducible Research - Project 1

This report describes analysis performed on the activity data collected from a tracking device for monitoring and improving health. The primary objective of the this report is to emphasize reproducibility of the work done. This is possible because the executable code alongwith the output/results are available within the same document. Additionally, the approach allows a researcher to capture his/her reasoning for the approach.  

The dataset was avalailable at [Activity Monitoring[(https://d396qusza40orc.cloudfront.net/repdata_data_Factivity.zip). 

The following code chunk was used to download the zip data file.

#### Download the zip file in Rdata and unzip in to local foldet 

The execution of the following block has been disabled!
``` {r download, echo = TRUE}
## The execution of the following block has been disabled 
if (file.exists("/activity.zip")) {
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    DestFile <- "activity.zip"
    DestFile = paste0("/Users/pb/RData/", DestFile)
    download.file(fileURL, destfile = DestFile, method = "curl")
    library(lubridate)
    now()
    unzip(DestFile)
}
```

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
-   date: The date on which the measurement was taken in YYYY-MM-DD format
-   interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

#### 1.  Code for reading in the dataset and/or processing the 
The zip file unzips into a single csv file called *activity.csv*.

#####  Loading and preprocessing the data

-   1.1 - Loading the data ("read.csv")

``` {r readCSV, echo=TRUE}
act <- read.csv("activity.csv", header = TRUE)
```

The *activity,csv* are stored in a data frame *act*. 

-   1.2 -  "For Processing Data" see below
The steps involved in processing of the data are detailed step-by-step and associated processing code below.

#### 2.  Histogram of the total number of steps taken each day

``` {r histogram1, echo=TRUE}
# Histogram with MISSING Data
hist(act$steps, col="tomato", xlab="Steps", main="Hostogram of Steps with Missing Data")  
```

The histogram above shows the frequncy of number of steps. 

#### 3.  Mean and median number of steps taken each day
##### What is mean total number of steps taken per day?
-   3.1 - Calculate the total number of steps taken per day
``` {r totalNumber, echo=TRUE}
# Calculate TOTAL numbers of steps taken each day
Steps2 <- tapply(act$steps, act$date, sum)
Steps <- as.numeric(Steps2)
dd <- dimnames(Steps2)
dd <- as.Date(dd[[1]][])
plot(dd, Steps, xlab="Date", pch=19,  ylab="Total Number of Steps", main="Total Number of Steps Taken Each Day")
```


-   3.2 - Make a histogram of the total number of steps taken each day
Note: Histogram is shown above

-   3.3 - Calculate and report the mean and median of the total number of steps taken per day

We emploty R's *sapply* function to calculate them *mean, median, and total number& if steps taken each day.  The results are plotted as a function of date. 
``` {r }
#---
#  What is mean total number of steps taken per day?

MNN <- tapply(act$steps, act$date, mean)
MN <- as.numeric(MNN)
dd <- dimnames(MNN)
dd <- as.Date(dd[[1]][])
plot(dd, MN, xlab="Date", pch=19, col="tomato", ylab="Number of Mean Steps", main="Mean Number of Steps Taken Per Day")

#---
# Calculate MEDIAN for each day
MDD <- tapply(act$steps, act$date, mean)
MD <- as.numeric(MDD)
dd <- dimnames(MDD)
dd <- as.Date(dd[[1]][])
plot(dd, MD, xlab="Date", pch=20,  ylab="Median Steps by Day", main="Median Values by Day", col="blue", cex=2)
text(dd[22], -1.0, "Note: Median value is skewed by 0 steps in 5 mts interval")
```

#### 4.  Time series plot of the average number of steps taken
##### What is the average daily activity pattern?
``` {r averageSteps, echo = TRUE}
# For Average steps each day we will simply devide Total Steps by # of Obsevations

#  Q: What is the average daily activity pattern?
AVE <- Steps / as.numeric(table(act$date))  
AVE1 <- tapply(act$steps, act$interval, mean, na.rm = TRUE)
Interval <- dimnames(AVE1)
Interval <- Interval[[1]]
```

- 4.1 - Make a time eries plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days (y-axis)
``` {r TimeSeries1, echo = TRUE}

plot(Interval, AVE1, xlab="Time Interval of the Day", pch=19,  ylab="Average Steps", main="Average Daily Activity Pattern", lwd=2, type="l")

```

-   4.2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
``` {r TimeSeries2, echo = TRUE}
# Find the index of the "Interval" with Maximum number of steps
idx <- which(AVE1 == max(AVE1))
# Print the interval value:
paste("Interval with Maximum steps:", Interval[idx])
```


#### 5.  The 5-minute interval that, on average, contains the maximum number of steps
Note - (see above.)


#### 6. Code to describe and show a strategy for imputing missing data
##### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

-   6.1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
``` {r imputingMissing1, echo = TRUE}
n <- 1:nrow(act)
for (i in 1:nrow(act))  n[i] <- sum(is.na(act[i, 1]))
# Number of columns with missing values
sum(n > 0 )
paste("Number of Missing Rows (NA): ", as.character(sum(n > 0)))
```

-   6.2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` {r imputingMissing2, echo = TRUE}
# Method for imputing missing value
#  Imputing using linear interpolation 
# We will use linear interpolation along day for imputing data 
## Step 1 - Arrange number of steps by by Dates
library(tidyr)
act1 <- spread(act, date, steps)
## Step 2. - make a copy of act1 to store the results
act2 <- act1 

## Step 3 - Generate the index of non-missing values 
y1 <- which(!is.na(act1[1, 2:62]))  # Indeces of NON missing values
y1 <- y1 + 1
## Step 3 - Generate the index of missing values 
y2 <- which(is.na(act1[1, 2:62]))   # Indeces of missing values
y2 <- y2 + 1 # 1 is added to leave interval values untouched
## Step 4 - calculate the mean for each 5 minute interval
for (i in 1:nrow(act1)) {
    mn <- mean(as.numeric(act1[i, y1]))
    act2[i, y2] <- mn   #  replace NA's in the i-th row with mean "mn"
}
```

-   6.3 - Create a new dataset that is equal to the original dataset but with the missing data filled in.
``` {r imputingMissing3, echo = TRUE}
## Step 5 - Now gather the date column 
act1 <- gather(act2, date, steps, -interval)  # act1 is new IMPUTED data framehistjo
```

-   6.4 - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` {r histogram2, echo = TRUE}
hist(act1$steps, col="green", xlab="Steps", main="Histogram of Steps with Imputed Data") 
```

#### 8.  Panel plot comparing the average number of steps taken per 
5-minute interval across weekdays and weekends
##### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

-   8.1 - Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.
``` {r panelPlot1, echo = TRUE}
# Add a column to act and assign it to act2
# act2 <- mutate(act1, weekdays = weekdays(as.POSIXlt(date)))
# act2$weekdays <- factor(act2$weekdays, 
#             levels = c("Satureday", "Sunday", Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
# labels = c("Weekend","Weekend","Weekday","Weekday","Weekday","Weekday","Weekday")
# 
# act2$weekdays <- factor(act2$weekdays, 
#             levels = c(1:7), 
# labels = c("Weekend","Weekend","Weekday","Weekday","Weekday","Weekday","Weekday")

library(dplyr)
library(chron)

##  Instert columns with weekdays substituded for dates
act2 <- mutate(act1, weekdays = weekdays(as.POSIXlt(date)), Weekdays = !is.weekend(date))

##  Seperate data into Weekdays and Weekends data frames
WeekDays <- filter(act2, Weekdays) 
WeekEnds <- filter(act2, !Weekdays)

##  Calculate the averages for 
WdaysAve <- tapply(WeekDays$steps, WeekDays$interval, mean, na.rm = TRUE)
WendsAve <- tapply(WeekEnds$steps, WeekEnds$interval, mean, na.rm = TRUE)
# Interval <- dimnames(AVE3)
# Interval <- Interval[[1]]
```


-   8.2 - Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


``` {r panelPlot2, echo = TRUE}
par(mfcol = c(2, 1), mar = c(4, 4, 2, 1))
#  Plot of Weekdays activities pattern
plot(Interval, WdaysAve, xlab="Time Interval of the Day", pch=19,  ylab="Average Steps", main="Weekdays: Average Daily Activity Pattern", lwd=2, type="l")

#  Plot of Weekdends activities pattern
plot(Interval, WendsAve, xlab="Time Interval of the Day", pch=19,  ylab="Average Steps", main="Weekend: Average Daily Activity Pattern", lwd=2, type="l")
```

The chart above shows time series of steps averaged over 5-minutes interval for the activity data collected over two months.  The panels shows the pattern of activities during the weekdays and the weekends.  The difference in an individual's activity during weekdays and weekends is noticiabl.
