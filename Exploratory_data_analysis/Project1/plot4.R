source("dataload.R")

plot4<-function(data=NULL, destination="screen") {
   if(is.null(data))
      data <- load_data()
   if (destination == "png") initiate_device ("plot4.png")
   par(mfrow=c(2,2)) #2 by 2 plots
   #plot1 - top left
   plot(data$Time, data$Global_active_power,
        ylab="Global Active Power (kilowatts)",
        xlab="",
        type="l")
   #plot 2 - top right
   plot(data$Time, data$Voltage,
        ylab="Voltage",
        xlab="date/time",
        type="l")
   #plot 3 - bottom left
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
   #Plot 4 - bottom right
   plot(data$Time, data$Global_reactive_power,
        ylab="Global_reactive_power",
        xlab="date/time",
        type="l")
   
   if (destination == "png") dev.off()
}