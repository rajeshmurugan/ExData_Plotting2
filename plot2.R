# 2.Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

library(plyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$type <- as.factor(NEI$type)
NEI$year <- as.numeric(NEI$year)
# Extract Batimore city information
totalPerYear <- ddply(NEI[NEI$fips == "24510",], c("year"), 
                      function(df)sum(df$Emissions, na.rm=TRUE))
plot(totalPerYear$year, totalPerYear$V1, type="l", xlab="Year", ylab="PM2.5 (tons)",
	main="Tons of PM2.5 Generated Per Year in Baltimore City, MD")
dev.copy(png, file ="plot2.png",width = 480, height = 480)
dev.off()
