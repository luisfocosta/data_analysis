source("dataload.R")

plot2 <- function(data=NULL, destination="screen") {
   if(is.null(data))
      data <- load_data()
   if (destination == "png") initiate_device ("plot2.png")
   par(mfrow=c(1,1))
   plot(data$Time, data$Global_active_power,
        ylab="Global Active Power (kilowatts)",
        xlab="",
        type="l")
   if (destination == "png") dev.off()
}