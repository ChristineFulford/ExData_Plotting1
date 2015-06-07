setwd("c:/users/christine/R Programs/Exploratory Data Analysis/Project 1/exdata-data-household_power_consumption")
library(dplyr)

## bring in the data and get it into the appropriate format
loadAndCleanData <- function()
{
	df <- read.table("household_power_consumption.txt", sep=";", header=TRUE, na.strings="?", stringsAsFactors=FALSE)

	## add a new column RDate that is the R Date version of the Date field for easier filtering
	df$RDate <- as.Date(df$Date, "%d/%m/%Y")

	## subset out the desired dates only - Feb 1 - 2, 2007
	wantedDate1 <- as.Date("2007-02-01")
	wantedDate2 <- as.Date("2007-02-02")
	df <- df[(df$RDate == wantedDate1 | df$RDate == wantedDate2), ] 
 
	## add a new column DateTime which is the Date and Time combined (a character vector)
	df <- mutate(df, DateTime = paste(Date, Time, sep=" "))

	## add a new column RDateTime that is the R DateTime version of the DateTime field
	## This makes plotting the datetime possible
	df$RDateTime <- strptime(df$DateTime, "%d/%m/%Y %H:%M:%S")
	
	## return the data frame
	df
}

## plot #4
plot4 <- function()
{
	df <- loadAndCleanData()

	## lay out 4 separate plots row-wise
	## a smaller font size makes the legend text fit better
	par(mfrow = c(2, 2), cex = 0.64)

	## top left plot is the same as plot #2
	with(df, plot(RDateTime, Global_active_power, ylab = "Global Active Power", xlab = "", type = "l"))

	## top right plot 
	with(df, plot(RDateTime, Voltage, ylab = "Voltage", xlab = "datetime", type = "l"))

	## bottom left plot is the same as plot 3
	with(df, {
		plot(RDateTime, Sub_metering_1, ylab = "Energy sub metering", xlab = "", type = "l")
		points(RDateTime, Sub_metering_2, type = "l", col="red")
		points(RDateTime, Sub_metering_3, type = "l", col="blue")

	})
	legend("topright", lty = 1, col = c("black", "red", "blue"), 
		legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

	## bottom right plot 
	with(df, plot(RDateTime, Global_reactive_power, xlab = "datetime", type="l"))

	## create a png file.  Default size is 480 x 480
	dev.copy(png, file="plot4.png")
	dev.off()
}


