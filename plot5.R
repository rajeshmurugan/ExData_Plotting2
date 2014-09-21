# 5.R:How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

library(plyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$type <- as.factor(NEI$type)
NEI$year <- as.numeric(NEI$year)

#Extract all motor vehicular emissions
vehicleCodes <- grep("^(22010|22300)", NEI$SCC)
# Extract Batimore city information
vehicleCodes <- intersect(which(NEI$fips == "24510"), vehicleCodes)

totalPerYear <- ddply(NEI[vehicleCodes,], c("year"), 
                      function(df)sum(df$Emissions, na.rm=TRUE))
ggplot(data=totalPerYear, aes(x=year, y=V1)) +
  geom_line() +
  xlab("Year") +
  ylab("PM2.5 (tons)") +
  ggtitle("PM2.5 from Motor Vehicles vs. Year in the Baltimore City, MD area")
dev.copy(png, file ="plot5.png",width = 480, height = 480)
dev.off()
