# 3.Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

library(plyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$type <- as.factor(NEI$type)
NEI$year <- as.numeric(NEI$year)
# Extract Batimore city information
totalPerYear <- ddply(NEI[NEI$fips == "24510",], c("year", "type"), 
                      function(df)sum(df$Emissions, na.rm=TRUE))
ggplot(data=totalPerYear, aes(x=year, y=V1, group=type, colour=type)) +
  geom_line() +
  xlab("Year") +
  ylab("PM2.5 (tons)") +
  ggtitle("PM2.5 vs. Year by Source Type")
dev.copy(png, file ="plot3.png",width = 480, height = 480)
dev.off()





