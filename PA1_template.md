# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Check if file unzipped before
if(!file.exists('activity.csv')) {
  
  #Unzip the data set file
  unzip('activity.zip')
}

# Load the csv to a data frame
data <- read.csv("activity.csv", sep=",", colClasses = "character")
```


## What is mean total number of steps taken per day?

```r
# Convert steps to numeric
data$steps <- as.numeric(data$steps)

# Convert date data to date format
data$date <- as.Date(data$date)

# Aggregate steps
data2 <- aggregate(steps ~ date, data, sum)

# Calculating Total Mean and Median
dataMean <- mean(data2[,"steps"])
dataMedian <- median(data2[,"steps"])
```

#### The Histogram of "Total number of steps taken each day"

```r
library(ggplot2)
qplot(data2$steps, geom="histogram", main="Total number of steps taken each day", binwidth=1000, xlab="Steps", fill=I("blue"), col=I("red"), alpha=I(.2))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

#### The total mean is: 10766.19
#### The total median is: 10765


## What is the average daily activity pattern?


```r
data$interval <- as.numeric(data$interval)
dataAverages <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)
ggplot(dataAverages, aes(interval, steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)


```r
row <- dataAverages[which(dataAverages$steps == max(dataAverages$steps, na.rm=TRUE)),]
maxAverageInterval <- row$interval
```
#### The time interval that contains the maximum number of steps: 835

## Imputing missing values

### Calculating the total number of missing values in the dataset

```r
naData <- subset(data, is.na(data$steps)|is.na(data$interval))
naDataCount <- nrow(naData)
```

#### The total number of missing values in the dataset: 2304


```r
dataImputted <- data

dataImputted$interval <- as.numeric(dataImputted$interval)
dataImputted$steps <- as.numeric(dataImputted$steps)

dataMeanPerInterval <- aggregate(steps ~ interval, dataImputted, mean, rm.na=TRUE)

dataImputted$steps <- apply(dataImputted, 1, function(x)
	{
  	steps <- as.numeric(x["steps"])
  	interval <- as.numeric(x["interval"])
  
    if(is.na(steps)) {
      
      stepsMean <- dataMeanPerInterval$steps[(dataMeanPerInterval$interval == interval)]
      
      # print(class(stepsMean))

      x["steps"] <- stepsMean
    }
  
    as.numeric(x["steps"])
		
	})
```

#### Histogram of the total number of steps taken each day after data are imputted:




```r
 qplot(dataImputted$steps, geom="histogram", main="Total number of steps taken each day", binwidth=20, xlab="Steps", fill=I("blue"), col=I("red"), alpha=I(.2))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)


```r
dataAverages <- aggregate(steps ~ interval, dataImputted, mean, na.rm=TRUE)
ggplot(dataAverages, aes(interval, steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)

## Are there differences in activity patterns between weekdays and weekends?
