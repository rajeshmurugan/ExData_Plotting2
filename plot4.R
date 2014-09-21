# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library(plyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$type <- as.factor(NEI$type)
NEI$year <- as.numeric(NEI$year)
# Extract Coal combustion information from SSC
coalCodes <- SCC[grep(".*Coal.*", SCC$Short.Name),"SCC"]
totalPerYear <- ddply(NEI[NEI$SCC %in% coalCodes,], c("year"), 
                      function(df)sum(df$Emissions, na.rm=TRUE))

ggplot(data=totalPerYear, aes(x=year, y=V1)) +
  geom_line() +
  xlab("Year") +
  ylab("PM2.5 (tons)") +
  ggtitle("PM2.5 from Coal-Related Sources vs. Year")
dev.copy(png, file ="plot4.png",width = 480, height = 480)
dev.off()
