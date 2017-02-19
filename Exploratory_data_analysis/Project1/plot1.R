source("dataload.R")

plot1 <- function(data=NULL, destination="png") {
   if(is.null(data))
      data <- load_data()
   if (destination == "png") initiate_device ("plot1.png")
   par(mfrow=c(1,1))
   hist(data$Global_active_power,
        xlab="Global Active Power (kilowatts)",
        col="red",
        main="Global Active Power")
   if (destination == "png") dev.off()
}