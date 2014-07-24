## Exploratory Data Analysis 
## Project 2, July 2014 
## By Tobias Crabtree

require(ggplot2)
require(plyr) 

## Downloaded data on July 12, 2014 from course website. Data is from http://www.epa.gov/ttn/chief/eiinformation.html.

## read unzipped files that contains the data 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##Using the base plotting system, make a plot showing the total PM2.5 emission from 
##all sources for each of the years 1999, 2002, 2005, and 2008.

### Subset data for emissions by the years
data <- subset(NEI,year== 1999 | 2002 | 2005 | 2008)

## using plyr to sum Emissions by year
dataSum <- ddply(data, .(year), summarize, total=sum(Emissions))

### copy plot to a PNG file
dev.copy(png, file="plot1.png", width=480, height=480)

## plot
plot(dataSum, main="Total emissions from PM2.5 by year")
dev.off()

##2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
##from 1999 to 2008? Use the base plotting system to make a plot answering this question.

### Subset data for emissions by the years for Baltimore City, MD
dataBaltimore <- subset(data, fips=="24510")

### using plyr to sum Emissions by year
dataSumBaltimore <- ddply(dataBaltimore, .(year), summarize, total=sum(Emissions))

### copy plot to a PNG file
dev.copy(png, file="plot2.png", width=480, height=480)

### plot
plot(dataSumBaltimore, main="Total PM2.5 emissions for Baltimore City, MD")

dev.off()

##3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
##which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
##Which have seen increases in emissions from 1999–2008? 
##Use the ggplot2 plotting system to make a plot answer this question.

## summarize data by year and by type for Baltimore City, MD
dataSumBaltimoreType <- ddply(dataBaltimore, .(year, type), summarize, total=sum(Emissions))

## copy plot to a PNG file
dev.copy(png, file="plot3.png", width=480, height=480)

## plot using ggplot2 plotting system
ggplot(dataSumBaltimoreType, aes(year, log(total))) + geom_point(size=3) + 
        geom_smooth(method="lm") + facet_grid(type~.) + 
        ggtitle("Total PM2.5 Emissions for Baltimore City, MD") 

dev.off()

##4. Across the United States, how have emissions from coal combustion-related sources changed 
##from 1999–2008?

## extract coal codes from SCC file
coal <- as.character(SCC$SCC[grep("coal", SCC$EI.Sector, ignore.case=TRUE)])

## subset data for coal
dataCoal <- subset(data, SCC %in% coal)

## sum emissions
dataCoalSum <- ddply(dataCoal, .(year), summarize, total=sum(Emissions))

## copy plot to a PNG file
dev.copy(png, file="plot4.png", width=480, height=480)

## plot
ggplot(dataCoalSum, aes(year, total)) + geom_point(size=3) + 
        geom_smooth(method="lm") +  ggtitle("Total PM2.5 Emissions for coal sources") 

dev.off()


##5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

## subset data for vehicles by using On-road type
dataMV <- subset(dataBaltimore, type=="ON-ROAD")

## sum emissions
dataMVSum <- ddply(dataMV, .(year), summarize, total=sum(Emissions))

## copy plot to a PNG file
dev.copy(png, file="plot5.png", width=480, height=480)

## plot

ggplot(dataMVSum, aes(year, total)) + geom_point(size=3) + 
        geom_smooth(method="lm") +  ggtitle("Total Emissions from motor vehicles in Baltimore") 

dev.off()


##6. Compare emissions from motor vehicle sources in Baltimore City with emissions from 
##motor vehicle sources in Los Angeles County, California (fips == "06037"). 
##Which city has seen greater changes over time in motor vehicle emissions?

### Subset data for emissions by the years for Baltimore City, MD and Los Angeles, CA
dataBaltLA <- subset(data, fips=="24510" | fips=="06037" )

## subset data for vehicles by using On-road type
dataBaltLAMV <- subset(dataBaltLA, type="ON-ROAD")

### using plyr to sum Emissions by year
dataSumBaltLAMV <- ddply(dataBaltLAMV, .(year, fips), summarize, total=sum(Emissions))

## copy plot to a PNG file
dev.copy(png, file="plot6.png", width=480, height=480)

## plot
ggplot(dataSumBaltLAMV, aes(year, log(total))) + geom_point(size=3) + 
        geom_smooth(method="lm") + facet_grid(fips~.) +  
        ggtitle("Total PM2.5 Emissions for LA (06037) & Baltimore (24510)") + labs(y="log PM2.5")

dev.off()

