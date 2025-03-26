## Documentation of every change made to get to final data tables from initial data sources

# Initial Data Sources: 
  # Uses dams in North Carolina from Army Corps of Engineers https://nid.sec.usace.army.mil/#/ #
  # Gets shape data, blockgroups/census tracts from R tigris package, which are from https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
  # Gets SOVI variables from the CDC at the census tract level, https://www.atsdr.cdc.gov/place-health/php/svi/svi-data-documentation-download.html
  # Gets NC address set from NC OneMap, https://www.nconemap.gov/datasets/909e8b7d03e04aecaf7a1afc03376074_0/explore
  # Shapefiles of NC major river basins, https://data-ncdenr.opendata.arcgis.com/datasets/major-river-basins

### DON'T CHANGE/RUN THIS UNLESS YOU WANT TO CHANGE BASE DATAFRAMES ###
### LoadData.R is sourced at beginning of other scripts to access ###

##### BLOCKGROUP DAM DATA #####
    #gets final NCdams_blockgroups, which has the latitude/longitude and shape information for the blockgroup the dams are within 

NCdams <- read_csv("Original Datasets/NCdams_NID.csv") %>% # Every dam in NC from NID
  rename_all(~ gsub(" ", "_", .)) %>%   # Add _ instead of spaces
  rename_all(~ gsub("[()]", "", .))     # Remove parentheses
write_csv(NCdams, "Final Datasets/AllNCDams.csv")

# Add spatial data columns to the NCdams dataset (makes possible to match to censustracts)
NCdams_sf <- st_as_sf(NCdams, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) 

##### CENSUS TRACT DAM DATA #####

# Pull census tracts from tigris / Census
NC_censustracts <- tracts(state = 37) %>% 
  st_transform(crs = 4326)

# Match census tract info to the dams dataset
NCdams_censustracts <- st_join(NCdams_sf, NC_censustracts, join = st_within) %>% # Spatial join with NC census tracts  
  select(GEOID, everything()) # put census tract identifier first
write_csv(NCdams_censustracts, "Final Datasets/NCDams_Censustracts.csv")

##### RIVER BASIN DAM DATA #####

# Read in river basin shapefiles from NCDEQ
river_basins <- st_read("Original Datasets/Major_Basins") %>%
  select(-c(PlanLink, GlobalID)) %>% 
  rename(Basin_Sq_Miles = Sq_Miles) %>%
  rename(Basin_Acres = Acres) %>%
  rename(Basin_Name = Name) %>%
  st_transform(crs = 4326)
NCdams_basins <- st_join(NCdams_sf, river_basins, join = st_intersects) 
write_csv(NCdams_basins, "Final Datasets/NCdams_Basins.csv")

NCdams_basinsonly <- NCdams_basins %>%  #federal ID of the dam along with the info for the basins (basin, basin_sq_miles, basin_acres, basin_name)
  select(Federal_ID, Basin, Basin_Sq_Miles, Basin_Acres, Basin_Name) %>% # Select relevant basin columns
  st_drop_geometry()

NCdams_censusandbasin <- left_join(NCdams_censustracts, NCdams_basinsonly, by = "Federal_ID") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(CountyID = substr(GEOID, 1, 5)) %>%
  mutate(CountyID = as.character(CountyID))
write_csv(NCdams_censusandbasin, "Final Datasets/NCdams_censusandbasin.csv")

##### SOVI DATA #####
# SOVI dataset at the census tract level directly from CDC 
NCsovi_censustract <- read_csv("Original Datasets/NCsovi_censustract_2022.csv") %>%
  rename(GEOID = FIPS) %>%
  select(GEOID, E_TOTPOP, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES) %>%
  mutate(across(starts_with("RPL_THEME"), ~ ifelse(. < 0 | . > 1, NA, .))) %>% # Set those without 2022 data to NA
  mutate(GEOID = as.character(GEOID))
write_csv(NCsovi_censustract, "Final Datasets/NCsovi_censustract.csv") 

# SOVI dataset from the county level directly from CDC
NCsovi_county <- read_csv("Original Datasets/NCsovi_county_2022.csv", col_types = cols(FIPS = "c")) %>%
  rename(CountyID = FIPS) %>%
  select(CountyID, E_TOTPOP, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES) %>%
  mutate(across(starts_with("RPL_THEME"), ~ ifelse(. < 0 | . > 1, NA, .))) %>% # Set those without 2022 data to NA
  mutate(CountyID = as.character(CountyID))
write_csv(NCsovi_county, "Final Datasets/NCsovi_county.csv")

##### Final dataframes #####

#Get counties
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

# Combine dams with SOVI census-level information
finaldata_censustract <- NCsovi_censustract %>% 
  right_join(NCdams_censusandbasin, by = "GEOID") %>% 
  select(GEOID, County, Dam_Name, NID_ID, everything()) %>%
  mutate(Region = case_when(  # adds a region column
    County %in% western_counties ~ "Western",
    County %in% piedmont_counties ~ "Piedmont",
    County %in% coastal_counties ~ "Coastal",
    TRUE ~ "Other"))

# Combine dams with SOVI county level information
finaldata_county <- NCsovi_county %>% 
  right_join(NCdams_censusandbasin, by = "CountyID") %>%
  select(CountyID, County, Dam_Name, NID_ID, everything()) %>%
  mutate(Region = case_when(  # adds a region column
    County %in% western_counties ~ "Western",
    County %in% piedmont_counties ~ "Piedmont",
    County %in% coastal_counties ~ "Coastal",
    TRUE ~ "Other"))

write_csv(finaldata_censustract, "Final Datasets/finaldata_censustract.csv")
write_csv(finaldata_county, "Final Datasets/finaldata_county.csv")

###### Addresses ######
#addresses <- read_csv("Original Datasets/AddressDataset.csv")

##### Blockgroups (no longer use, but keeping in case) #####

# NC_blockgroups <- block_groups(state = 37) %>% # Get shape files for census tracts from tigris 
#   st_transform(NC_blockgroups, crs = 4326) 
# 
# NCdams_blockgroups <- st_join(NCdams_sf, NC_blockgroups, join = st_within) %>%
#   select(GEOID, everything()) # join the blockgroup info to the dams
# write_csv(NCdams_blockgroups, "NCDamsandBlockgroups.csv")


