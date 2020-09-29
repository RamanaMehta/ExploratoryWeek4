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

with(tot.emissions.year, # plot data 
     plot(x = year, 
          y = Total.Emissions, 
          ylab = "Total Annual Emissions [Tons]", 
          xlab = "Year",
          main = "Total Annual Emissions in the US by Year",
          cex = 2,
          pch = 2,
          col = "red",
          lwd = 3))

# Find delta between 2008 and 1999
tot.emissions.2008 <- tot.emissions.year[tot.emissions.year$year == 2008, 2]
tot.emissions.1999 <- tot.emissions.year[tot.emissions.year$year == 1999, 2]

delta.tot.emissions <- tot.emissions.2008 - tot.emissions.1999

tot.emissions.baltimore <- NEI %>%
    subset(fips == "24510") %>%
    group_by(year) %>%
    summarize(Total.Emissions.Baltimore = sum(Emissions, 
                                              na.rm = TRUE))

with(tot.emissions.baltimore, 
     plot(x = year, 
          y = Total.Emissions.Baltimore, 
          ylab = "Total Annual Emissions [Tons]", 
          xlab = "Year",
          main = "Total Annual Emissions in Baltimore by Year",
          cex = 2,
          pch = 2,
          col = "red",
          lwd = 3))

tot.emissions.baltimore.2008 <- tot.emissions.baltimore[tot.emissions.baltimore$year == 2008, 2]
tot.emissions.baltimore.1999 <- tot.emissions.baltimore[tot.emissions.baltimore$year == 1999, 2]

delta.emissions.baltimore <- tot.emissions.baltimore.2008 - tot.emissions.baltimore.1999


