# 1.Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(plyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$type <- as.factor(NEI$type)
NEI$year <- as.numeric(NEI$year)

totalPerYear <- ddply(NEI, c("year"), 
                      function(df)sum(df$Emissions, na.rm=TRUE))
plot(totalPerYear$year, totalPerYear$V1, type="l", xlab="Year",
	ylab="PM2.5 (tons)", main="Tons of PM2.5 Generated Per Year")
dev.copy(png, file ="plot1.png",width = 480, height = 480)
dev.off()
