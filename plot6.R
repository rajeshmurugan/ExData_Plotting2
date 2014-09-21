# 6.Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

library(plyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$type <- as.factor(NEI$type)
NEI$year <- as.numeric(NEI$year)

#Extract all motor vehicular emissions
vehicleCodes <- grep("^(22010|22300)", NEI$SCC)
vehicleCodes <- intersect(union(which(NEI$fips == "24510"),
                                which(NEI$fips == "06037")),
                          vehicleCodes)

totalPerYear <- ddply(NEI[vehicleCodes,], c("year", "fips"), 
                      function(df)sum(df$Emissions, na.rm=TRUE))
names(totalPerYear) <- c("year", "region", "emissions")

totalPerYear$region <- as.factor(totalPerYear$region)
levels(totalPerYear$region) <- c("Los Angeles", "Baltimore")

ggplot(data=totalPerYear, aes(x=year, y=emissions, group=region, colour=region)) +
  geom_line() +
  xlab("Year") +
  ylab("PM2.5 (tons)") +
  ggtitle("PM2.5 from Motor Vehicles vs. Year in Baltimore and Los Angeles")

dev.copy(png, file ="plot6.png",width = 480, height = 480)
dev.off()
