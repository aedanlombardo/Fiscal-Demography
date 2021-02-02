# This script pulls in economic data at the county level and combines it to
# create a County Economic Data Set. This includes House Price Index,
# Annual Personal Income, Annual Gross Income, GDP in Current Dollars
# GDP in 2012 dollars, Labor Force, and Poverty Rate
library(tidyverse)

# Read in HPI data
HPI <- readxl::read_xlsx("../../data/raw/County_Level_Data/HPI_AT_BDL_county.xlsx",
                         skip = 7, col_types = c("text", "text",
                                                 "text", "numeric",
                                                 "skip", "numeric",
                                                 "numeric", "numeric"),
                         col_names = c("State", "County", "FIPS", "Year",
                                       "HPI", "HPI_1990_Base", "HPI_2000_Base"))

################################################################################
# Read in API 
CAINC <- read.csv("../../data/raw/County_Level_Data/Annual_Personal_Income/CAINC1__ALL_AREAS_1969_2018.csv",
                  as.is = TRUE)

# Keep only necessary columns
CAINC <- CAINC[, c(1, 2, 5, 9:ncol(CAINC))]

# Trim whitespace from FIPS and get rid of lines for states, regions and U.S.
CAINC$GeoFIPS <- trimws(CAINC$GeoFIPS)
CAINC <- CAINC[substr(CAINC$GeoFIPS, 3, 5) != "000", ] 

# Make columns numeric
CAINC[, 4:ncol(CAINC)] <- lapply(CAINC[, 4:ncol(CAINC)], function(col){
  as.numeric(col)
})

# Clean GeoName (get rid of asterisks, parens, add tildes)
CAINC$GeoName <- gsub("\\*|\\s\\(.*\\)", "", CAINC$GeoName)
CAINC$GeoName <- gsub("\xf1", "ñ", CAINC$GeoName)

# Add State and County columns
CAINC$State <- substr(CAINC$GeoName, nchar(CAINC$GeoName) - 1,
                      nchar(CAINC$GeoName))
CAINC$County <- substr(CAINC$GeoName, 1, nchar(CAINC$GeoName) - 4)

# Reorganize and rename columns
CAINC <- CAINC[, c(1, 54, 55, 3:53)]
colnames(CAINC)[1] <- "FIPS"

# Get rid of rows that have NA for FIPS
CAINC <- CAINC[!is.na(CAINC$FIPS), ]
CAINC <- CAINC[!is.na(CAINC$LineCode),]

# Make dfs for each variable and then merge

# Api need to multiply by 1000 to get nominal dollar amounts
api <- CAINC %>% filter(LineCode == 1) %>%
  pivot_longer(cols = X1969:X2018, names_to = 'Year', values_to = 'api',
               names_prefix = 'X', names_ptypes = list('Year' = numeric())) %>%
  select(-LineCode) %>% mutate(api = api * 1000)

pop <- CAINC %>% filter(LineCode == 2) %>%
  pivot_longer(cols = X1969:X2018, names_to = 'Year', values_to = 'api_pop',
               names_prefix = 'X', names_ptypes = list('Year' = numeric())) %>%
  select(-LineCode)

pc <- CAINC %>% filter(LineCode == 3) %>%
  pivot_longer(cols = X1969:X2018, names_to = 'Year', values_to = 'api_pc',
               names_prefix = 'X', names_ptypes = list('Year' = numeric())) %>%
  select(-LineCode)

CAINC_long <- api %>% full_join(pop) %>% full_join(pc)
################################################################################
# Read in GDP in Current Dollars
CAGDP2 <- read.csv("../../data/raw/County_Level_Data/Current_GDP/CAGDP2__ALL_AREAS_2001_2018.csv",
                   as.is = TRUE)

# Keep only line code 1 and necessary columns
CAGDP2 <- CAGDP2[CAGDP2$LineCode == 1, c(1, 2, 9:ncol(CAGDP2))]

# Trim whitespace from FIPS and get rid of lines for states, regions and U.S.
CAGDP2$GeoFIPS <- trimws(CAGDP2$GeoFIPS)
CAGDP2 <- CAGDP2[substr(CAGDP2$GeoFIPS, 3, 5) != "000", ] 

# Make columns numeric and multiply by 1000 to since current #'s are in 000's
CAGDP2[, 3:ncol(CAGDP2)] <- lapply(CAGDP2[, 3:ncol(CAGDP2)], function(col){
  as.numeric(col) * 1000
})

# Clean GeoName (get rid of asterisks, parens, add tildes)
CAGDP2$GeoName <- gsub("\\*|\\s\\(.*\\)", "", CAGDP2$GeoName)
CAGDP2$GeoName <- gsub("\xf1", "ñ", CAGDP2$GeoName)

# Add State and County columns
CAGDP2$State <- substr(CAGDP2$GeoName, nchar(CAGDP2$GeoName) - 1,
                       nchar(CAGDP2$GeoName))
CAGDP2$County <- substr(CAGDP2$GeoName, 1, nchar(CAGDP2$GeoName) - 4)

# Reorganize and rename columns
CAGDP2 <- CAGDP2[, c(1, 21, 22, 3:20)]
colnames(CAGDP2)[1] <- "FIPS"

# Get rid of rows that have NA for FIPS
CAGDP2 <- CAGDP2[!is.na(CAGDP2$FIPS), ]

# Reshape to long format to match HPI
CAGDP2_long <- reshape(CAGDP2, varying = colnames(CAGDP2)[4:ncol(CAGDP2)],
                       v.names = "GDP_Curr_Dollars", timevar = "Year",
                       times = as.numeric(gsub("X", "",
                                               colnames(CAGDP2)[4:ncol(CAGDP2)])),
                       direction = "long")

# Drop id column
CAGDP2_long <- CAGDP2_long[, -6]

################################################################################
# Get real GDP in chained 2012 dollars
CAGDP9 <- read.csv("../../data/raw/County_Level_Data/Real_GDP_Chained/CAGDP9__ALL_AREAS_2001_2018.csv",
                   as.is = TRUE)

# Keep only line code 1 and necessary columns
CAGDP9 <- CAGDP9[CAGDP9$LineCode == 1, c(1, 2, 9:ncol(CAGDP9))]

# Trim whitespace from FIPS and get rid of lines for states, regions and U.S.
CAGDP9$GeoFIPS <- trimws(CAGDP9$GeoFIPS)
CAGDP9 <- CAGDP9[substr(CAGDP9$GeoFIPS, 3, 5) != "000", ] 

# Make columns numeric and multiply by 1000 to since current #'s are in 000's
CAGDP9[, 3:ncol(CAGDP9)] <- lapply(CAGDP9[, 3:ncol(CAGDP9)], function(col){
  as.numeric(col) * 1000
})

# Clean GeoName (get rid of asterisks, parens, add tildes)
CAGDP9$GeoName <- gsub("\\*|\\s\\(.*\\)", "", CAGDP9$GeoName)
CAGDP9$GeoName <- gsub("\xf1", "ñ", CAGDP9$GeoName)

# Add State and County columns
CAGDP9$State <- substr(CAGDP9$GeoName, nchar(CAGDP9$GeoName) - 1,
                       nchar(CAGDP9$GeoName))
CAGDP9$County <- substr(CAGDP9$GeoName, 1, nchar(CAGDP9$GeoName) - 4)

# Reorganize and rename columns
CAGDP9 <- CAGDP9[, c(1, 21, 22, 3:20)]
colnames(CAGDP9)[1] <- "FIPS"

# Get rid of rows that have NA for FIPS
CAGDP9 <- CAGDP9[!is.na(CAGDP9$FIPS), ]

# Reshape to long format to match HPI
CAGDP9_long <- reshape(CAGDP9, varying = colnames(CAGDP9)[4:ncol(CAGDP9)],
                       v.names = "RGDP_Chain_2012", timevar = "Year",
                       times = as.numeric(gsub("X", "",
                                               colnames(CAGDP9)[4:ncol(CAGDP9)])),
                       direction = "long")
# Drop id column
CAGDP9_long <- CAGDP9_long[, -6]

################################################################################
# Merge BEA and HPI data
GDP <- merge(CAGDP2_long, CAGDP9_long, all = TRUE)
BEA_data <- merge(CAINC_long, GDP, all = TRUE)

# Merge with HPI excluding county names because they use different naming convention
HPI_BEA <- merge(HPI, BEA_data, all = TRUE, by = c("FIPS", "Year"))

# Make sure each entry has a county name and state name
HPI_BEA$State <- ifelse(is.na(HPI_BEA$State.y), HPI_BEA$State.x, HPI_BEA$State.y)
HPI_BEA$County <- ifelse(is.na(HPI_BEA$County.y), HPI_BEA$County.x,
                         HPI_BEA$County.y)

# Reorder columns
HPI_BEA <- HPI_BEA[, c("FIPS", "State", "County", "Year", "api", 'api_pop',
                       'api_pc', "HPI",
                       "HPI_1990_Base", "HPI_2000_Base", "GDP_Curr_Dollars",
                       "RGDP_Chain_2012")]

################################################################################
# Get newest AGI information (not an xls for each state)
files <- dir("../../data/raw/County_Level_Data/Annual_Gross_Income",
             full.names = TRUE)
# Has a deleted word doc in there for some reason, need to remove that
files <- files[1:8]
names(files) <- paste0("X", seq(2010, 2017))

files[1] <- lapply(files[1], function(file){
  readxl::read_xls(file, skip = 6,
                   col_types = c(rep("text", 4),
                                 rep("skip", 5),
                                 "numeric",
                                 rep("skip", 63)),
                   col_names = c("State_Code",
                                 "State",
                                 "County_Code",
                                 "County",
                                 "AGI"))
})

files[2:8] <- lapply(files[2:8], function(file){
  read.csv(file, as.is = TRUE,
           colClasses = c(rep("character", 4),
                          rep("numeric", 70)))[, c("STATEFIPS", "STATE",
                                                   "COUNTYFIPS", "COUNTYNAME",
                                                   "A00100")]
})

# Make colnames uniform across years
for (i in 1:length(files)){
  colnames(files[[i]]) <- colnames(files[[1]])
}

# Add year column 
for (i in 1:length(files)){
  files[[i]]$Year <- as.numeric(gsub("X", "", names(files)))[i]
}

# Collapse into one data frame
AGI_10_17 <- do.call(rbind, files)

# Make all State codes length 2 and county codes length 3
temp_s <- nchar(AGI_10_17$State_Code)
AGI_10_17$State_Code[which(temp_s == 1)] <- paste0("0",
                                                   AGI_10_17$State_Code[
                                                     which(temp_s == 1)
                                                     ])
# Get rid of rows that have additional text
AGI_10_17 <- AGI_10_17[which(nchar(AGI_10_17$State_Code) <= 2), ]
temp_c <- nchar(AGI_10_17$County_Code)
AGI_10_17$County_Code[which(temp_c == 2)] <- paste0("0",
                                                    AGI_10_17$County_Code[
                                                      which(temp_c == 2)
                                                      ])
AGI_10_17$County_Code[which(temp_c == 1)] <- paste0("00",
                                                    AGI_10_17$County_Code[
                                                      which(temp_c == 1)
                                                      ])
# Create column for full FIPS and reorder columns
AGI_10_17$FIPS <- paste0(AGI_10_17$State_Code, AGI_10_17$County_Code)
AGI_10_17 <- AGI_10_17[, c("FIPS", "State", "County", "Year",
                           "AGI")]

# Get rid of state and region totals
AGI_10_17 <- AGI_10_17[substr(AGI_10_17$FIPS, 3, 5) != "000",] 

# Get AGI in 1000's 
AGI_10_17$AGI <- AGI_10_17$AGI * 1000

################################################################################
# Get older AGI info (one xls per state)
files_2 <- dir("../../data/raw/County_Level_Data/Annual_Gross_Income",
               full.names = TRUE)
files_2 <- lapply(files_2[9:length(files_2)], function(folder){
  dir(folder, full.names = TRUE)
})
names(files_2) <- paste0("X", seq(1989, 2009))
for (i in 1:19){
  names(files_2[[i]]) <- c(state.abb[1:8], "DC", state.abb[9:50])
}
for (i in 20:21){
  names(files_2[[i]]) <- sort(c(state.abb[1:8], "DC", state.abb[9:50]))
}

# 89-07 have a different format than 08-09
files_2[1:19] <- lapply(files_2[1:19], function(year){
  lapply(year, function(state){
    readxl::read_xls(state, skip = 8,
                     col_types = c(rep("text", 3),
                                   rep("skip", 2),
                                   "numeric",
                                   rep("skip", 3)),
                     col_names = c("State_Code",
                                   "County_Code",
                                   "County_Name",
                                   "AGI"))
  })
})

files_2[20:21] <- lapply(files_2[20:21], function(year){
  lapply(year, function(state){
    readxl::read_xls(state, skip = 7,
                     col_types = c(rep("text", 3),
                                   rep("skip", 2),
                                   "numeric",
                                   rep("skip", 3)),
                     col_names = c("State_Code",
                                   "County_Code",
                                   "County_Name",
                                   "AGI"))
  })
})

for (i in 1:length(files_2)){
  for (j in 1:length(files_2[[i]])){
    files_2[[i]][[j]]$Year <- as.numeric(gsub("X", "", names(files_2)[i]))
    files_2[[i]][[j]]$State <- names(files_2[[i]])[j]
  }
}

# Collapse into one df
files_2 <- lapply(files_2, function(year){
  do.call(rbind, year)
})

AGI_89_09 <- do.call(rbind, files_2)

# Correct FIPS so states are two chars and counties are 3, multiply AGI by 1000
AGI_89_09$AGI <- AGI_89_09$AGI * 1000
temp_s <- nchar(AGI_89_09$State_Code)
AGI_89_09$State_Code[which(temp_s == 1)] <- paste0("0",
                                                   AGI_89_09$State_Code[
                                                     which(temp_s == 1)
                                                     ])

# Keep only entries with state codes of length 2
AGI_89_09 <- AGI_89_09[which(nchar(AGI_89_09$State_Code) <= 2), ]
temp_c <- nchar(AGI_89_09$County_Code)
AGI_89_09$County_Code[which(temp_c == 2)] <- paste0("0",
                                                    AGI_89_09$County_Code[
                                                      which(temp_c == 2)
                                                      ])
AGI_89_09$County_Code[which(temp_c == 1)] <- paste0("00",
                                                    AGI_89_09$County_Code[
                                                      which(temp_c == 1)
                                                      ])

# Add FIPS column, reorder and rename columns for merging
AGI_89_09$FIPS <- paste0(AGI_89_09$State_Code, AGI_89_09$County_Code)
AGI_89_09 <- AGI_89_09[, c("FIPS", "State", "County_Name", "Year", "AGI")]
colnames(AGI_89_09)[3] <- "County"

# Get rid of state and regional totals 
AGI_89_09 <- AGI_89_09[substr(AGI_89_09$FIPS, 3, 5) != "000", ]

################################################################################
# Combine all AGI data
AGI <- rbind(AGI_89_09, AGI_10_17)

# Merge AGI data with BEA and HPI data
all_data <- merge(AGI, HPI_BEA, all = TRUE, by = c("FIPS", "Year"))

# Make sure each entry has a state and county
all_data$State <- ifelse(is.na(all_data$State.y), all_data$State.x,
                         all_data$State.y)
all_data$County <- ifelse(is.na(all_data$County.y), all_data$County.x,
                          all_data$County.y)

# Reorder columns and drop unnecessary columns
all_data <- all_data[, c("FIPS", "State", "County", "Year", "AGI",
                         "api", 'api_pop', 'api_pc', "HPI", "HPI_1990_Base",
                         "HPI_2000_Base",
                         "GDP_Curr_Dollars", "RGDP_Chain_2012")]

# Found an interesting bug
all_data[4859, "County"] <- "Wade Hampton"

###############################################################################
# Data pull for labor force data
library(tidyverse)

# Read in labor force xlsx files
files <- dir("../../data/raw/County_Level_Data/Labor_Force_Data",
             full.names = TRUE)
x <- lapply(files, function(xlsx){
  readxl::read_xlsx(xlsx, skip = 5,
                    col_names = c("STATE_FIPS", "COUNTY_FIPS", "COUNTY",
                                  "Year", "LBR_FORCE", "EMPLYD", "UNEMPLYD",
                                  "UNEMPLYMNT_RATE"),
                    col_types = c("skip", rep("text", 3), "numeric", "skip",
                                  rep("numeric", 4)))
})

# Turn into one large df
df <- do.call(rbind, x)

# Get rid of row where there are no FIPS codes
df <- df[!is.na(df$STATE_FIPS), ]

# Create columns that conform to previous data set format (1 col for FIPS,
# state and county in two columns, Year, then labor stats)
df$FIPS <- paste0(df$STATE_FIPS, df$COUNTY_FIPS)
df$State <- substr(df$COUNTY, start = nchar(df$COUNTY) - 1,
                   stop = nchar(df$COUNTY))
df$County <- substr(df$COUNTY, start = 1, stop = nchar(df$COUNTY) - 4)

# Reorder columns for uniformity and drop unused columns
bls_df <- df[, c("FIPS", "State", "County", "Year", "LBR_FORCE", "EMPLYD",
                 "UNEMPLYD", "UNEMPLYMNT_RATE")]

# Fix rows for DC
bls_df$State[bls_df$State == "ia"] <- "DC" 
bls_df$County[bls_df$State == "DC"] <- "District of Columbia"

################################################################################
# Read in poverty rate data
pov_rate_df <- readxl::read_xlsx("../../data/raw/County_Level_Data/Poverty-Rates-by-County-1960-2010.xlsm",
                                 sheet = "Data", skip = 1,
                                 col_types = c("skip", rep("text", 3),
                                               rep("numeric", 18)))

# Get rid of rows for state totals and US and where FIPS is NA
pov_rate_df <- pov_rate_df[pov_rate_df$County != "State Total", ]
pov_rate_df <- pov_rate_df[pov_rate_df$State != "United States", ]
pov_rate_df <- pov_rate_df[!is.na(pov_rate_df$FIPS), ]

# Make all FIPS codes length 5 (states with FIPS 01-09 and DC)
pov_rate_df$FIPS[pov_rate_df$FIPS == "11"] <- "11001"
pov_rate_df$County[pov_rate_df$FIPS == "11001"] <- "District of Columbia"
pov_rate_df$FIPS[which(nchar(pov_rate_df$FIPS) == 4)] <-
  paste0("0", pov_rate_df$FIPS[which(nchar(pov_rate_df$FIPS) == 4)])

# Reshape df to long format
pov_rate_df_long <- reshape(as.data.frame(pov_rate_df),
                            varying = matrix(4:21, ncol = 6, byrow = TRUE),
                            v.names = c("Poverty_Rate", "Population",
                                        "People_in_Poverty"),
                            timevar = "Year",
                            times = seq(1960, 2010, by = 10),
                            direction = "long")
rownames(pov_rate_df_long) <- NULL

################################################################################
# Merge labor and poverty rate data
lab_pov_data <- merge(bls_df, pov_rate_df_long, by = c("FIPS", "Year"),
                      all = TRUE)

# Create single county and state columns
lab_pov_data$State <- ifelse(is.na(lab_pov_data$State.x), lab_pov_data$State.y,
                             lab_pov_data$State.x)
lab_pov_data$County <- ifelse(is.na(lab_pov_data$County.x), lab_pov_data$County.y,
                              lab_pov_data$County.x)

# Reorder columns
lab_pov_data <- lab_pov_data %>% select(-State.x, -State.y, -County.x, -County.y)
lab_pov_data <- lab_pov_data %>% select(-id)
lab_pov_data <- lab_pov_data %>% select(Year, FIPS, State, County, everything())

# Bring in population estimates
pop_data <- read.csv("../../data/processed/County_Data/Pop_90_18.csv",
                     colClasses = c("numeric",
                                    "character", rep("numeric", 25)))

# Merge population and labor/poverty data
pop_lab_pov <- merge(pop_data, lab_pov_data, by = c("FIPS", "Year"),
                     all = TRUE)
pop_lab_pov <- pop_lab_pov %>% rename(Total_Pop = Total)
pop_lab_pov <- pop_lab_pov %>% select(FIPS, Year, State, County, everything())

# Read in previous county data for large merge
cnty_v2 <- merge(pop_lab_pov, all_data, by = c('FIPS', "Year"),
                 all = TRUE)

# Get rid of County and State columns (let's try to add these back in later)
cnty_v2 <- cnty_v2 %>% select(-County.x, -County.y, -State.x, -State.y) %>% 
  select(FIPS, Year, Total_Pop, everything())

# Create new csv for full dataframe
write.csv(cnty_v2, "../../data/processed/County_Data/County_Data_Full.csv",
          row.names = FALSE)
