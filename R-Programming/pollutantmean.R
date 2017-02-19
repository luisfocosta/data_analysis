pollutantmean <- function (directory, pollutant, id = 1:332) {
   sum_vals <- 0
   num_rows <- 0
   calc_mean <- 0
   if (pollutant == "sulfate" || pollutant == "nitrate") {
      for (i in id) {
         filename <- paste ("./", directory, "/", formatC(i, width=3, flag = "000"), ".csv", sep="")
         data <- read.csv (filename)
         sum_vals <- sum_vals + sum(data[pollutant], na.rm = TRUE)
         num_rows <- num_rows + sum(!is.na(data[pollutant]))
      }
      calc_mean <- sum_vals / num_rows
   }
   else
      calc_mean <- 0
   return(round(calc_mean,3))
}

complete <- function (directory, id = 1:332) {
   lengt <- length(id)
   table_data <- data.frame(lengt, 2)
   colnames(table_data) <- c("id", "nobs")
   ndx <- 1
   for (i in id) {
      filename <- paste ("./", directory, "/", formatC(i, width=3, flag = "000"), ".csv", sep="")
      data <- read.csv (filename)
      table_data[ndx, 1] <- i
      table_data[ndx, 2] <- sum(complete.cases(data))
      ndx <- ndx +1
   }
   return(table_data)
}

corr <- function (directory, threshold = 0) {
   all_files <- as.character (list.files (directory))
   filenames <- paste (directory, "/", all_files, sep="")
   corr <- c()
   for (i in 1:332) {
      data <- na.omit(read.csv(filenames[i]))
      if (nrow(data) >= threshold) {
         correlation <- cor(data$sulfate, data$nitrate)
         if (!is.na(correlation))
               corr <- append (corr, correlation)
      }
   }
   corr
}