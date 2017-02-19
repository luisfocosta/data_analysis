date_time_conv <- function (date, time) {
   return (strptime(paste(date, time), "%d/%m/%Y %H:%M:%S"))
}

load_data <- function () {
   filename <- "household_power_consumption.txt"
   #data <- read.csv.sql(filename,
   #   sql = "select * from file where date='2007-02-01' or date='2007-02-02'")
   awk_cmd <- paste("awk 'BEGIN {FS=\";\"} {if ($1 == \"Date\" || $1 == \"1/2/2007\" || $1 == \"2/2/2007\") print $0}'",
      filename) 
   data <- read.csv(pipe(awk_cmd),
      sep=";",
      na="?",
      header=TRUE)
   data$Time <- date_time_conv (data$Date, data$Time)
   data$Date <- as.Date(data$Date, "%d/%m/%Y")
   #data$Day <- weekdays(data$Date)
   return (data)
}

initiate_device <- function (file) {
   png(filename=file, width=480, height=480)
}