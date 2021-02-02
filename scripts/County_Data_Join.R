# This script combines the county level economic, demographic and fiscal
# measures from various government agencies and Social Explorer
library(tidyverse)

# Read in previously processed datasets
econ <- read.csv("../../data/processed/County_Data/County_Data_Full.csv",
                 as.is = TRUE,
                 colClasses = c("character",
                                rep('numeric', 40)))
fin <- read.csv("../../data/processed/County_Data/County_Finance_Data.csv",
                as.is = TRUE,
                colClasses = c("numeric",
                               rep('character', 3),
                               rep('numeric', 25)))
se <- read.csv("../../data/processed/County_Data/SEDataFull.csv",
               as.is = TRUE,
               colClasses = c('numeric',
                              rep('character', 3),
                              rep('numeric', 46)))

# Add state fips and county fips to econ data set
econ <- econ %>% mutate(State = str_sub(FIPS, 1, 2),
                        County = str_sub(FIPS, 3, 5))

# Rename state and county columns in fin
fin <- fin %>% rename(State = FIPS.Code.State, County = FIPS.Code.County)

# Merge on Year, FIPS, State and County
full_county <- econ %>% full_join(fin) %>% full_join(se) %>% 
  select(Year, FIPS, State, County,everything())

unique(full_county$State)
# Some data here for Puerto Rico (72) and an unknown area (90). Quick search
# suggests that 90 is probably an island territory.

length(unique(full_county$FIPS))
sum(is.na(full_county$County))

# Fill in finance data for NYC using NYC aggregates (this data will be the same for 
# all included counties)
nyc_fips <- c('36005', '36047', '36081', '36085')
nyc_years <- seq(1957, 2012, by = 5)
for (i in 1:length(nyc_years)){
  full_county[full_county$FIPS %in% nyc_fips & full_county$Year == nyc_years[i], 44:68]<-
    full_county[full_county$FIPS == '36061' & full_county$Year == nyc_years[i], 44:68]
}


# Write to csv
write.csv(full_county, file = "../../data/processed/Full_County_Data.csv",
          row.names = FALSE)

#write.csv(colnames(full_county), "../../data/processed/Data_README.csv",
#         row.names = FALSE)
