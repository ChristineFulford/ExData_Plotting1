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

## plot #1
plot1 <- function()
{
	df <- loadAndCleanData()
	hist(df$Global_active_power, col="red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")

	## create a png file.  Default size is 480 x 480
	dev.copy(png, file="plot1.png")
	dev.off()
}


