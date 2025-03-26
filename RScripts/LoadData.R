# every dataframe made in GetDataFrames.R
# This code gets run if working outside of the project / repo
# Will get deleted at some point just afraid of deleting for now 

library(dplyr)
library(tidyverse)
library(tigris)
library(sf)

##### County Vectors #####

western_counties <- c("Alleghany", "Ashe", "Avery", "Buncombe", "Burke", "Caldwell",
                      "Cherokee", "Clay", "Graham", "Haywood", "Henderson", "Jackson",
                      "Macon", "Madison", "McDowell", "Mitchell", "Polk", "Rutherford",
                      "Swain", "Transylvania", "Watauga", "Wilkes", "Yancey")

piedmont_counties <- c("Alamance", "Alexander", "Anson", "Cabarrus", "Caswell", "Catawba", 
                       "Chatham", "Cleveland", "Davidson", "Davie", "Durham", "Forsyth", "Franklin", 
                       "Gaston", "Granville", "Guilford", "Iredell", "Lee", "Lincoln", "Mecklenburg", 
                       "Montgomery", "Moore", "Orange", "Person", "Randolph", "Richmond", "Rockingham", 
                       "Rowan", "Stanly", "Stokes", "Surry", "Union", "Vance", 
                       "Wake", "Warren", "Yadkin")

coastal_counties <-c("Bertie", "Beaufort", "Brunswick", "Camden", "Carteret", "Chowan", 
                     "Craven", "Currituck", "Dare", "Gates", "Hertford", "Hyde", "New Hanover", 
                     "Onslow", "Pamlico", "Pasquotank", "Pender", "Perquimans", "Tyrrell", "Washington",
                     "Bladen", "Columbus", "Cumberland", "Duplin", "Edgecombe", "Greene", "Halifax", "Harnett", "Hoke", 
                     "Johnston", "Jones", "Lenoir", "Martin", "Nash", "Northampton", "Pitt", "Robeson", "Sampson", 
                     "Scotland", "Wayne", "Wilson")

#####




# dams and basins
NCdams <- read_csv("Final Datasets/AllNCDams.csv")
NCdams_censustracts <- read_csv("Final Datasets/NCDams_Censustracts.csv") #dams with census tract info
NCdams_basins <- read_csv("Final Datasets/NCdams_Basins.csv") 
NCdams_censusandbasin <- read_csv("Final Datasets/NCdams_censusandbasin.csv") #has dams and their respective census and basin info

#SOVI 
  # just for RPL Themes and Aggregate
NCsovi_censustract <- read_csv("Final Datasets/NCsovi_censustract.csv")
NCsovi_county <- read_csv("Final Datasets/NCsovi_county.csv")

# FINAL (entire combination of every dam and its corresponding SOVI RPL themes)
finaldata_censustract <- read_csv("Final Datasets/finaldata_censustract.csv")
finaldata_county <- read_csv("Final Datasets/finaldata_county.csv")

######################OLD##########################

# NCdams_blockgroups <- read_csv("NCDamsandBlockgroups.csv") #dams with blockgroup info assigned 

# # EJ Screen 
# NCejscreen_censustract <- read_csv("NCejscreen_censustract.csv")
# # NC NRI
# NCnri_clean <- read_csv("NRI_NC_Clean.csv")
# # Both NRI and EJScreen Data by Census Tract (no dam info)
# NRI_EJ_merge <- read_csv("NRI_EJ_merge.csv")
# ### final data (combination of dams, their census tract designation, basin, NRI data by census tract, and EJ data by census tract)
# finaldata <- read_csv("finaldata.csv")



