source("dataload.R")

plot3<-function(data=NULL, destination="png") {
   if(is.null(data))
      data <- load_data()
   if (destination == "png") initiate_device ("plot3.png")
   par(mfrow=c(1,1))
   plot(data$Time, data$Sub_metering_1,
        ylab="Energy sub metering",
        xlab="",
        type="l",
        col="black")
   points(data$Time, data$Sub_metering_2,col="red", type="l")
   points(data$Time, data$Sub_metering_3,col="blue", type="l")
   legend_text = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
   legend_col = c("black", "red", "blue")
   legend ("topright",
           c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           col=c("black", "red", "blue"),
           lty=1)
   if (destination == "png") dev.off()
}