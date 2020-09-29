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


tot.emissions.type <- NEI %>% 
    subset(fips == "24510") %>%
    group_by(year, type) %>%
    summarize(Total.Emissions.Type = sum(Emissions, na.rm = TRUE))

emissions.type <- ggplot(data = tot.emissions.type, aes(year, Total.Emissions.Type))

emissions.type <- emissions.type + 
    geom_point(color = "red", 
               size = 4, 
               alpha = 1/3) + 
    facet_grid(. ~ type) +
    xlab("Year") +
    ylab("Total Emissions [Tons]") +
    ggtitle("Total Annual Emissions in Baltimore by Year")

emissions.type