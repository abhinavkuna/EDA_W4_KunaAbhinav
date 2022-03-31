## EDA Week04 by Kuna Abhinav (All 6 plots in one script)

## read in emissions data and classification code
emissions_data <- readRDS("summarySCC_PM25.rds")
class_code <- readRDS("Source_Classification_Code.rds")

## add up the total emissions for each year
sum_by_year <- aggregate(emissions_data$Emissions, by=list(year=emissions_data$year), FUN=sum)

## create the plot
png(filename = "plot1.png")
plot(sum_by_year$year, sum_by_year$x, type = "l", 
     main = "Total Emissions of PM2.5 in Baltimore City",
     ylab = "Total emissions of PM2.5 (tons)",
     xlab = "Year")
dev.off()

## read in emissions data and classification code
emissions_data <- readRDS("summarySCC_PM25.rds")
class_code <- readRDS("Source_Classification_Code.rds")

## subset to just data from Baltimore and then add up emissions for each year
baltimore_data <- subset(emissions_data, emissions_data$fips=="24510")
baltimore_data_year <- aggregate(baltimore_data$Emissions, by=list(baltimore_data$year), 
                                 FUN=sum)
## create plot
png(filename = "plot2.png")
plot(baltimore_data_year$Group.1, baltimore_data_year$x, type = "l", 
     main = "Total Emissions of PM2.5 in Baltimore City", xlab = "year", 
     ylab = "Total Emissions (tons)")
dev.off()

## read in emissions data and classification code
emissions_data <- readRDS("summarySCC_PM25.rds")
class_code <- readRDS("Source_Classification_Code.rds")

## subset to just data from Baltimore and sum up emissions by variable "type"
baltimore_data <- subset(emissions_data, emissions_data$fips=="24510")
baltimore_type_year <- aggregate(baltimore_data$Emissions, 
                                 by=list(baltimore_data$type, baltimore_data$year), 
                                 FUN=sum)
colnames(baltimore_type_year) <- c("Type", "Year", "Emissions")

## create plot with ggplot2
library(ggplot2)
png(filename = "plot3.png")
qplot(Year, Emissions, data = baltimore_type_year, color = Type, geom = "line") +
  ggtitle("Total Emissions of PM2.5 in Baltimore City By pollutant type") + 
  ylab("Total Emissions (tons)") + 
  xlab("Year") 
dev.off()

## read in emissions data and classification code
emissions_data <- readRDS("summarySCC_PM25.rds")
class_code <- readRDS("Source_Classification_Code.rds")

## fetch all records involving coal
coal_class_code <- class_code[grepl("Coal", class_code$Short.Name), ]
coal_emissions_data <- emissions_data[emissions_data$SCC %in% coal_class_code$SCC, ]

## sum up emissions by year
coal_emissions_year <- aggregate(coal_emissions_data$Emissions, 
                                 by=list(coal_emissions_data$year), FUN=sum)
colnames(coal_emissions_year) <- c("year", "emissions")

## create a plot showing coal related emissions across the US from 1999-2008
png(filename = "plot4.png")
plot(coal_emissions_year$year, coal_emissions_year$emissions, type = "o",
     xlab = "Year",
     ylab = "Total Emissions (tons)",
     main = "Coal Related Emissions of PM2.5 in US from 1999-2008")
dev.off()

## read in emissions data and classification code
emissions_data <- readRDS("summarySCC_PM25.rds")
class_code <- readRDS("Source_Classification_Code.rds")

## subset data from Baltimore City from type "on road"
baltimore_car_data <- subset(emissions_data, emissions_data$fips=="24510" 
                             & emissions_data$type=="ON-ROAD")
baltimore_car_year <- aggregate(baltimore_car_data$Emissions, 
                                by=list(baltimore_car_data$year), FUN=sum)
colnames(baltimore_car_year) <- c("Year", "Emissions")

## create plot showing car related emissions in Baltimore City from 1999-2008
png(filename = "plot5.png")
plot(baltimore_car_year$Year, baltimore_car_year$Emissions,
     type = "o",
     xlab = "year",
     ylab = "Total Emissions (tons)",
     main = "Total Emissions of PM2.5 related to motor Vehicles",
     sub = "Baltimore City")
dev.off()

## read in emissions data and classification code
emissions_data <- readRDS("summarySCC_PM25.rds")
class_code <- readRDS("Source_Classification_Code.rds")

## subset data from Baltimore City, LA county and from type "on road"
baltLA_car_data <- subset(emissions_data, emissions_data$fips=="24510" |
                            emissions_data$fips=="06037" &
                            emissions_data$type=="ON-ROAD")
baltLA_car_year <- aggregate(baltLA_car_data$Emissions, 
                             by=list(baltLA_car_data$fips, baltLA_car_data$year),
                             FUN=sum)
colnames(baltLA_car_year) <- c("City", "Year", "Emissions")

## create plot comparing emissions from motor vehicles in Baltimore and LA from 1999-2008
library(ggplot2)
png(filename = "plot6.png")
qplot(Year, Emissions, data = baltLA_car_year, color = City, geom = "line") +
  ggtitle("Emissions of PM2.5 in Baltimore City (24510) and LA County (06037)") + 
  ylab("Total Emissions from motor vehicles (tons)") + 
  xlab("Year") 
dev.off()