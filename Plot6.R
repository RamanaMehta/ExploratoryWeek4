url1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
destfile1 <- "destfile.zip"

if(!file.exists(destfile1)) {
    download.file(url1, 
                  destfile = destfile1, 
                  method = "curl")
    unzip(destfile1, exdir = ".")
}


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(NEI)

tot.emissions.year <- NEI %>%  # Group by year and summarize total emissions across the board
    group_by(year) %>%
    summarize(Total.Emissions = sum(Emissions, na.rm = TRUE))

tot.emissions.year

vehicle.scc <- SCC[grep("[Vv]eh", SCC$Short.Name), ]

emissions.motor.la <- NEI %>% 
    subset(fips == "06037" & NEI$SCC %in% vehicle.scc$SCC) %>%
    merge(y = vehicle.scc, by.x = "SCC", by.y = "SCC") %>%
    group_by(year) %>%
    summarize(Vehicle.Emissions.Type = sum(Emissions, na.rm = TRUE))

emissions.motor.baltimore2 <- cbind(emissions.motor.baltimore, "City" = rep("Baltimore", 4))
emissions.motor.la2 <- cbind(emissions.motor.la, "City" = rep("LA", 4))

emissions.motor.comp <- rbind(emissions.motor.baltimore2, emissions.motor.la2)

emissions.motor.comp.plot <- ggplot(emissions.motor.comp, aes(year, Vehicle.Emissions.Type, col = City)) +
    geom_point(size = 4, 
               alpha = 1/3) +
    xlab("Year") +
    ylab("Total Emissions [Tons]") +
    ggtitle("Comparison of Total Annual Vehicle Emissions in Baltimore and Los Angeles")

emissions.motor.comp.plot

emissions.motor.la.2008 <- emissions.motor.la[emissions.motor.la$year  == 2008, 2]
emissions.motor.la.1999 <- emissions.motor.la[emissions.motor.la$year  == 1999, 2]


delta.la <- emissions.motor.la.2008 - emissions.motor.la.1999
abs(delta.la) > abs(delta.baltimore)
