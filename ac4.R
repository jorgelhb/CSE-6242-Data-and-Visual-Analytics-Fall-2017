# Activity: Time Series Analysis

library(zoo)  # basic time series package
library(xts)  # eXtensible Time Series package

data_dir <- "data"
label_dir <- "labeled_windows"

load_ts <- function(csv_filename) {
    # Load and return time series data from a CSV file.
    #
    # Params:
    # - csv_filename: CSV file with two columns: timestamp, value
    #
    # Returns:
    # - s: time series data of type xts

    df <- read.csv(csv_filename, stringsAsFactors=FALSE)
    
    # TODO: convert timestamp column to POSIX datetime
    Posix_column=as.POSIXlt(df[,1])
    df <-data.frame(Posix_column=as.POSIXlt(df[,1]),value=df[,2])
    
    # TODO: create xts time series from dataframe
    s=xts(df[,2], order.by = df[,1])
    
    return(s)  # return time series
}

find_anomalies <- function(s, window_size=11, threshold=4) {
    # Find anomalous data points in a time series.
    #
    # Params:
    # - s: time series data, as returned by load_ts()
    # - window_size: size of window used to compute rolling statistics
    # - threshold: parameter used to identify outliers
    #
    # Returns: A list with the following named items:
    # - s [input]
    # - window_size [input]
    # - threshold [input]
    # - s.mean: rolling mean
    # - s.sd: rolling standard deviation (s.d.)
    # - anomalies: anomalous data points, as a subset of s

    # TODO: Compute rolling mean
    # Hint: use rollapply() with align = 'right' and fill = 'extend'
    s.mean=rollapply(s, width=window_size, FUN=mean, align = 'right', fill = 'extend')
    
    # TODO: Compute rolling standard deviation
    s.sd=rollapply(s, width=window_size, FUN=sd, align = 'right', fill = 'extend')
    # TODO: Find anomalies
    # Hint: Look for data points that are more than (threshold * s.d.) away from mean
    anomalies_rows=array() # find all the anomalies rows, and saved here
    for (i in c(1:length(s))){
      temp=abs(s[[i]]-s.mean[[i]])
      if (temp>threshold*s.sd[[i]]) 
        anomalies_rows=c(anomalies_rows, i)
    }
    anomalies=s[anomalies_rows[!is.na(anomalies_rows)]]
  
    # TODO: Filter anomalies to only keep extrema
    # Hint: Look for peaks and troughs

    # TODO(optional): Further filtering to reduce duplicates and false positives

    # Return results as a named list (include input params as well)
    return(list(s=s, window_size=window_size, threshold=threshold,
                s.mean=s.mean, s.sd=s.sd, anomalies=anomalies))
}

analyze <- function(csv_filename, window_days=3, threshold=4) {
    # Analyze a time series, looking for anomalies.
    #
    # Params:
    # - csv_filename: CSV file with two columns: timestamp, value
    # - window_days: no. of days to include in moving window
    # - threshold: parameter passed on to find_anomalies()
    #
    # Returns:
    # - s: results returned by find_anomalies()

    s <- load_ts(csv_filename)  # load time series data from CSV file

    # Compute samples per day to set rolling window size
    avg_delta <- difftime(index(s)[length(s)], index(s)[1], units='secs') / length(s)
    samples_per_day <- 24 * 60 * 60 / as.numeric(avg_delta)
    window_size <- as.integer(window_days * samples_per_day)  # no. of days * samples_per_day

    # Find anomalies
    res <- find_anomalies(s, window_size, threshold)
    cat(paste(csv_filename, ": window_size = ", window_size, ", threshold = ", threshold, sep=""), end="\n")
    cat(length(res$anomalies), "anomalies found", end="\n")
    #print(res$anomalies)

    # Pass on results returned by find_anomalies()
    return(res)
}

visualize <- function(res, wins=NA, title="Anomaly Detection Results") {
    # Visualize the results of anomaly detection.
    #
    # Params:
    # - res: anomaly detection results, as returned by find_anomalies()
    # - wins: optional windows to be highlighted
    # - title: main title for the plot
    #
    # Returns: Nothing

    # Plot original time series, with optional highlight windows
    if(!is.na(wins) && nrow(wins) > 0) {
        plot(res$s, type="n", main=title)  # create a blank plot first
        rect(wins$beg, min(res$s), wins$end, max(res$s), col="#CCCCEE77", border=NA)  # add highlights
        lines(res$s)  # then draw the time series
    } else {
        plot(x=index(res$s),y=coredata(res$s),type="l", main=title,
             xlab="Time",
             ylab="data",
             ylim=c(0-max(res$s.sd),max(res$s)*1.1))
    }
    
    # TODO: Show moving average
    lines(x=index(res$s.mean),y=coredata(res$s.mean),type="l", col="green")
    # TODO: Draw margins at mean +/- (threshold * s.d.)
    lines(x=index(res$s.sd),y=coredata(res$s.mean)-coredata(res$s.sd),type="l", col="blue")
    lines(x=index(res$s.sd),y=coredata(res$s.mean)+coredata(res$s.sd),type="l", col="blue")
    # TODO: Mark anomalies
    points(x=index(res$anomalies),y=coredata(res$anomalies), col="red")
    legend("topleft", c("raw_data","moving average","margins","anomalies"),
           lty=c(1,1,1,0),col=c("black","green","blue","red"),pch = c(NA_integer_,NA_integer_,NA_integer_,1))
}

# NOTE: Do not put any code outside the functions or the following "main" block
if(getOption("run.main", default=TRUE)) {
    # Analyze
    csv_filename <- "realAWSCloudwatch/ec2_cpu_utilization_5f5533.csv"
    res <- analyze(paste(data_dir, csv_filename, sep="/"), window_days=3, threshold=4)

    # Visualize (with ground truth windows highlighted)
    wins <- read.csv(paste(label_dir, csv_filename, sep="/"), stringsAsFactors=FALSE)  # ground truth windows
    wins$beg <- as.POSIXct(wins$beg)  # convert to POSIX datetime
    wins$end <- as.POSIXct(wins$end)
    visualize(res, wins=wins, title=paste("Anomaly Detection Results", csv_filename, sep="\n"))
}
