# Fiscal Demography

## Background
The scripts in this repo were created as part of my work with [Rourke O'Brien](https://sites.google.com/site/rourkeobrien/home?authuser=0). We were interested in better understanding the relationship between demography and fiscal policy at county level from 1970-present. This work was meant to build on the analysis of this relationship at the state level as laid out by Josh McCabe in ["Rich State, Poor State"](https://www.niskanencenter.org/rich-state-poor-state/). These scripts take care of the majority of the data cleaning and merging necessary to create an easily explorable dataset out of data from multiple government agencies.

## Scripts Included
1. County_Fin_Pull.R: This script handles county level fiscal measures from the [Census Bureau's Census of Governments](https://www.census.gov/programs-surveys/cog.html)
2. County_Econ_Pull.R: This script handles county level economic measures sourced from several different agencies such as the IRS, BLS and BEA
3. County_Data_Join.R: This script handles the merging of the output of the above scripts with demographic data sourced from the CDC and from the Census Bureau's ACS

# Data
Due to the constraints for file uploads to GitHub, I have chosen to store the data for this project in a Google Drive folder.
