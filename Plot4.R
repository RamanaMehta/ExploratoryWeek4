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

SCC.coal.comb <- SCC[grep("[Cc]oal",SCC$EI.Sector),]
NEI.sub.coal <- subset(NEI, 
                       NEI$SCC %in% SCC.coal.comb$SCC)


NEI.coal.comb <- merge(x = NEI.sub.coal, 
                       y = SCC, 
                       by.x = "SCC", 
                       by.y = "SCC")

NEI.coal.comb.tot <- NEI.coal.comb %>% 
    group_by(year) %>%
    summarize(Total.Coal.Comb = sum(Emissions, na.rm = TRUE))

NEI.coal.comb.plot <- ggplot(NEI.coal.comb.tot, aes(year, Total.Coal.Comb))

NEI.coal.comb.plot <- NEI.coal.comb.plot + 
    geom_point(color = "red", 
               size = 4, 
               alpha = 1/3) + 
    xlab("Year") +
    ylab("Total Emissions [Tons]") +
    ggtitle("Total Annual Coal Combustion Emissions in the US")

NEI.coal.comb.plot


NEI.coal.comb.tot.2008 <- NEI.coal.comb.tot[NEI.coal.comb.tot$year == 2008, 2]
NEI.coal.comb.tot.1999 <- NEI.coal.comb.tot[NEI.coal.comb.tot$year == 1999, 2]

NEI.coal.comb.delta <- NEI.coal.comb.tot.2008 - NEI.coal.comb.tot.1999
NEI.coal.comb.delta
