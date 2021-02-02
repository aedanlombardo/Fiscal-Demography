# This script pulls in County Area finance data from the Census bureau
# This data is stored in an mdb file and 2 txt files
library(Hmisc)
library(tidyverse)
library(tidyselect)
db <- mdb.get("../../data/raw/County_Level_Data/County_Area_Fin/County_Area_Finances.mdb",
              tables = c("1_Revenues", "2_ExpendituresA", "3_ExpendituresB"))
revs <- db[[1]]
expsA <- db[[2]]
expsB <- db[[3]]
exps <- merge(expsA, expsB)
county_area_fin <- merge(revs, exps)

# Select only desired variables
county_area_fin <- county_area_fin %>%
  select(Year4, FIPS.Code.State, FIPS.Code.County,
         Total.Revenue, Total.Rev.Own.Sources, General.Revenue,
         Gen.Rev.Own.Sources, Total.Taxes, Property.Tax..T01.,
         Total.Gen.Sales.Tax..T09., Total.Select.Sales.Tax,
         Total.IG.Revenue, Total.Fed.IG.Revenue,
         Total.State.IG.Revenue, Tot.Chgs.and.Misc.Rev,
         Total.Expenditure, General.Expenditure,
         Direct.General.Expend, Fire.Prot.Direct.Exp,
         Police.Prot.Direct.Exp, Total.Educ.Direct.Exp,
         Elem.Educ.Direct.Exp, Total.High.Ed.Dir.Exp,
         Health...Hosp.Dir.Exp, Health.Direct.Expend,
         Public.Welf.Direct.Exp)

# Change FIPS to character
county_area_fin <- county_area_fin %>% rename(Year = Year4) %>%
  mutate(FIPS.Code.State = as.character(FIPS.Code.State),
         FIPS.Code.County = as.character(FIPS.Code.County))

# Fill in 0s in FIPS
county_area_fin$FIPS.Code.State <-
  ifelse(nchar(county_area_fin$FIPS.Code.State) == 1,
         paste0("0", county_area_fin$FIPS.Code.State),
         county_area_fin$FIPS.Code.State)
county_area_fin$FIPS.Code.County <-
  ifelse(nchar(county_area_fin$FIPS.Code.County) == 2,
         paste0("0", county_area_fin$FIPS.Code.County),
         ifelse(nchar(county_area_fin$FIPS.Code.County) == 1,
         paste0("00", county_area_fin$FIPS.Code.County),
         county_area_fin$FIPS.Code.County))

# Get rid of old FIPS columns and reorganize so FIPS is at front
county_area_fin <- county_area_fin %>%
  mutate(FIPS = paste0(FIPS.Code.State, FIPS.Code.County)) %>%
  select(Year, FIPS, FIPS.Code.State, FIPS.Code.County, everything())

# -11111 is code for data not being published in that year for that measure
# I think changing these to NA is more useful
county_area_fin[county_area_fin == -11111] <- NA

# Multiply everything by 1000 to get data in true values
county_area_fin <- county_area_fin %>%
  mutate_at(vars(-Year, -FIPS, -FIPS.Code.State, -FIPS.Code.County), ~. * 1000)

# Calculate Non-property taxes (Total taxes - Property taxes)
county_area_fin <- county_area_fin %>%
  mutate(Non.Property.Taxes = Total.Taxes - Property.Tax..T01.,
         Tot.Local.IG.Revenue = NA)

# Order and name cols like TPC
county_area_fin <- county_area_fin %>% 
  select(Year, FIPS, FIPS.Code.State, FIPS.Code.County,
         Total.Revenue, Total.Rev.Own.Sources, General.Revenue,
         Gen.Rev.Own.Sources, Total.Taxes, Property.Tax..T01.,
         Non.Property.Taxes, Total.Gen.Sales.Tax..T09.,
         Total.Select.Sales.Tax, Total.IG.Revenue, Total.Fed.IG.Revenue,
         Total.State.IG.Revenue, Tot.Local.IG.Revenue, Tot.Chgs.and.Misc.Rev,
         Total.Expenditure, General.Expenditure, Direct.General.Expend,
         Total.Educ.Direct.Exp, Elem.Educ.Direct.Exp, Total.High.Ed.Dir.Exp,
         Fire.Prot.Direct.Exp, Health...Hosp.Dir.Exp, Health.Direct.Expend,
         Police.Prot.Direct.Exp, Public.Welf.Direct.Exp)

# Remove excess data
rm(db, exps, expsA, expsB, revs)

# Read in 2007 and 2012 data and publication aggregates
cnty_fin_07 <- read.csv("../../data/raw/County_Level_Data/County_Area_Fin/County Area Finances 2007/County_Area_Finances_2007.txt",
                        colClasses = c(rep("character", 4),
                                       rep("numeric", 5)))
cnty_fin_12 <- read.csv("../../data/raw/County_Level_Data/County_Area_Fin/County Area Finances 2012/County_Area_Finances_2012.txt",
                        colClasses = c(rep("character", 4),
                                       rep("numeric", 5)))
agg_key <- readxl::read_xlsx("../../data/raw/County_Level_Data/County_Area_Fin/County Area Finances 2012/County_Area_Finances_2012.xlsx",
                             sheet = "Specs_for_Totals_&_Subtotals",
                             skip = 2, col_types = c('skip', 'text', 'text'),
                             col_names = c('Measure', 'Codes'))

# Split item codes for each aggregate
agg_key$Codes <- str_split(agg_key$Codes, ", ")

# Grab only aggregates that we want
agg_key <- agg_key[c(1:9, 11:12, 22, 54, 65, 66, 69, 71, 73, 77, 81, 83, 91, 92),]
agg_key$Measure[agg_key$Measure == "Direct Expenditure"] <- "General Expenditure"

# Add year column, pivot
cnty_fin_07$Year <- 2007
cnty_fin_12$Year <- 2012

cnty_fin_07_wide <- cnty_fin_07 %>% pivot_wider(id_cols = c(Year, ID, State, County),
                                                values_from = Amt,
                                                names_from = Code)
cnty_fin_12_wide <- cnty_fin_12 %>% pivot_wider(id_cols = c(Year, ID, State, County),
                                                values_from = Amt,
                                                names_from = Code)
rm(cnty_fin_07, cnty_fin_12)

# Get aggregate columns (multiply measures by 1000 to get nominal dollars)
agg_cols <- rep(NA, 23)
names(agg_cols) <- agg_key$Measure
for (i in 1:nrow(agg_key)){
  measure07 <- cnty_fin_07_wide %>% select(any_of(agg_key$Codes[[i]])) %>% 
    transmute(Total = rowSums(., na.rm = TRUE) * 1000)
  measure12 <- cnty_fin_12_wide %>% select(any_of(agg_key$Codes[[i]])) %>% 
    transmute(Total = rowSums(., na.rm = TRUE) * 1000)
  agg_cols[i] <- rbind(measure07, measure12)
}
rm(measure07, measure12)

# Create 2007-2012 data frame with aggregate columns and add rbinded years and FIPS
cnty_fin_07_12 <- data.frame(Year = append(cnty_fin_07_wide$Year, cnty_fin_12_wide$Year),
                             ID = append(cnty_fin_07_wide$ID, cnty_fin_12_wide$ID),
                             State = append(cnty_fin_07_wide$State, cnty_fin_12_wide$State),
                             County = append(cnty_fin_07_wide$County, cnty_fin_12_wide$County),
                             agg_cols, stringsAsFactors = FALSE)

# Add total.rev.own.sources and non.property taxes and reorder columns to match
# data for 1957-2002
cnty_fin_07_12 <- cnty_fin_07_12 %>%
  mutate(Total.Rev.Own.Sources = Total.Revenue - Intergovernmental.Revenue,
         Non.Property.Taxes = Total.Taxes - Property.Tax) %>% 
  select(Year, ID, State, County, Total.Revenue, Total.Rev.Own.Sources,
         General.Revenue, General.Revenue.Own.Sources, Total.Taxes,
         Property.Tax, Non.Property.Taxes, General.Sales, Selective.Sales.Tax,
         Intergovernmental.Revenue, From.Federal, From.State, From.Local,
         Charges.and.Miscellaneous.General.Revenue, Total.Expenditure,
         General.Expenditure, Direct.General.Expenditures, Education,
         Elementary.and.Secondary.Education, Higher.Education,
         Fire.Protection, Hospitals, Health, Police.Protection,
         Public.Welfare)

# Get ID to FIPS key for 2007 and 2012
fips_key07 <- readxl::read_xlsx("../../data/raw/County_Level_Data/County_Area_Fin/County Area Finances 2007/County_Area_Finances_2007_updated.xlsx",
                                sheet = "County_ID_Lookup",
                                col_types = c(rep('text', 3),
                                              rep('skip', 6),
                                              rep('text', 2),
                                              'skip'))
fips_key12 <- readxl::read_xlsx("../../data/raw/County_Level_Data/County_Area_Fin/County Area Finances 2012/County_Area_Finances_2012.xlsx",
                                sheet = "County_ID_Lookup",
                                col_types = c(rep('text', 3),
                                              rep('skip', 7),
                                              rep('text', 2),
                                              'skip', 'skip'))
# Change ID to FIPS
for (i in 1:nrow(cnty_fin_07_12)){
  if(cnty_fin_07_12$Year[i] == 2007){
    cnty_fin_07_12$State[i] <- 
      fips_key07$`FIPS State`[fips_key07$ID == cnty_fin_07_12$ID[i]]
    cnty_fin_07_12$County[i] <-
      fips_key07$`FIPS County`[fips_key07$ID == cnty_fin_07_12$ID[i]]
  }
  else if (cnty_fin_07_12$Year[i] == 2012){
    cnty_fin_07_12$State[i] <-
      fips_key12$`FIPS State`[fips_key12$ID == cnty_fin_07_12$ID[i]]
    cnty_fin_07_12$County[i] <-
      fips_key12$`FIPS County`[fips_key12$ID == cnty_fin_07_12$ID[i]]
  }
}
# Change ID now that State and County are FIPS
cnty_fin_07_12$ID <- str_c(cnty_fin_07_12$State, cnty_fin_07_12$County)

# Match colnames for 2007-2012 and 1957-2002 and rbind
colnames(cnty_fin_07_12) <- colnames(county_area_fin)
county_area_fin_full <- rbind(county_area_fin, cnty_fin_07_12)

# Drop rows for USA/State Totals
county_area_fin_full <- county_area_fin_full %>%
  filter(!is.na(FIPS.Code.State))


# Write to csv
write.csv(county_area_fin_full, "../../data/processed/County_Data/County_Finance_Data.csv",
          row.names = FALSE)

