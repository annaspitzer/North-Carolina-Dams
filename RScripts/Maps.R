### Plots ###

source("RScripts/LoadData.R")

######################## SOVI BY COUNTY MAP W/ HIGH HAZARD DAMS ###########################

highhazard_coordinates <- finaldata_county %>%
  select(Dam_Name, Latitude, Longitude, Hazard_Potential_Classification) %>% 
  filter(Hazard_Potential_Classification == "High")

NC_counties <- counties(state = 37) %>%
  st_transform(crs = 4326) %>%
  select(GEOID, geometry) %>%
  rename(CountyID = GEOID) #this is only five digits as opposed to census or blockgroup IDs

# Make dataframe that's just CountyID, geometry, and SOVI score
NCsovi_county_sf <- left_join(NC_counties, NCsovi_county, by = "CountyID") %>%
  select(CountyID, RPL_THEMES) %>%
  st_as_sf()

# GGplot that shades county by SOVI with high hazard dams overlaid
ggplot(data = NCsovi_county_sf) +
  geom_sf(aes(fill = RPL_THEMES), color = "black", size = 0.2) + 
  geom_point(data = highhazard_coordinates, aes(x = Longitude, y = Latitude),
             color = "grey", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "SoVI Score Groups") +
  labs(title = "Social Vulnerability Index (SoVI) by County") +
  labs(subtitle = "High Hazard Potential Dams Overlayed") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

################### CONDITION ASSESSMENT PLOT ################################

damcondition <- finaldata_county %>%
  filter(Hazard_Potential_Classification == "High") %>%
  select(Dam_Name, Latitude, Longitude, Condition_Assessment) %>%
  mutate(Condition_Assessment = factor(Condition_Assessment, 
                                       levels = c("Satisfactory", "Fair", 
                                                  "Poor", "Unsatisfactory", 
                                                  "Not Rated or Unavailable")))

damcondition_clean <- damcondition %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  mutate(Condition_Assessment = case_when(
    Condition_Assessment %in% c("Not Rated", "Not Available") ~ "Not Rated/Unavailable",
    TRUE ~ Condition_Assessment
  )) %>%
  mutate(Condition_Assessment = factor(Condition_Assessment, 
                                       levels = c("Satisfactory", "Fair", 
                                                  "Poor", "Unsatisfactory", 
                                                  "Not Rated/Unavailable")))  # Define custom order

ggplot(NCsovi_county_sf) +
  geom_sf(finll = "gray", color = "black", size = 0.05) +  # Plot the county boundaries
  geom_point(data = damcondition_clean, 
             aes(x = Longitude, y = Latitude, color = Condition_Assessment), 
             size = 1, alpha = 0.8) +  # Add points for dam conditions
  scale_color_manual(values = c("Satisfactory" = "darkgreen", 
                                "Fair" = "#FDDA0D", 
                                "Unsatisfactory" = "red", 
                                "Poor" = "#420D09", 
                                "Not Rated/Unavailable" = "gray"),
                     drop = FALSE) +  
  theme_minimal() +
  labs(title = "High-Hazard Dams in North Carolina by Condition",
       color = "Condition Assessment",
       x = "Longitude",
       y = "Latitude")

######################## HIGH HAZARD DAMS OVER BASINS ###########################

# Read shapefiles from NCDEQ of the major river basins
river_basins <- st_read("Original Datasets/Major_Basins") %>%
  select(-c(PlanLink, GlobalID)) %>%
  rename(Basin_Sq_Miles = Sq_Miles) %>%
  rename(Basin_Acres = Acres) %>%
  rename(Basin_Name = Name)

# shapefile of the locations of high hazard dams
highhazard_sf <- finaldata_county %>%
  filter(Hazard_Potential_Classification == "High") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#plot the dams over the basins
ggplot() +
  geom_sf(data = river_basins, fill = "grey", color = "black", size = 0.1) +
  geom_sf(data = highhazard_sf, aes(color = "red"), shape = 16, size = .5) +
  labs(title = "River Basins and High Hazard Dams in NC",
       subtitle = "Locations of high hazard dams overlayed on river basins",
       fill = "Basin Name",
       color = "High Hazard Dams") +
  theme_minimal() +
  theme(legend.position = "bottom")

######################## SOVI BY CENSUS TRACT MAP W/ HIGH HAZARD DAMS ###########################

# takes way too long for the whole state, but keep in case you ever want to edit to zoom into a


NC_tracts <- tracts(state = 37) %>%
  st_transform(crs = 4326) %>%
  select(GEOID, geometry)

NCsovi_censustract_sf <- left_join(NC_tracts, NCsovi_censustract, by = "GEOID") %>%
  select(GEOID, E_TOTPOP, RPL_THEMES) %>%
  mutate(RPL_THEMES = ifelse(RPL_THEMES < 0 | RPL_THEMES > 1, NA, RPL_THEMES)) %>%  # Set out-of-range values to NA
  # filter(E_TOTPOP > 0) %>%  ## not important right now, but the SOVI population estimates don't always match the actual census 
  st_as_sf()

ggplot(data = NCsovi_censustract_sf) +
  geom_sf(aes(fill = RPL_THEMES), color = "black", size = 0.2) + 
  geom_point(data = highhazard_coordinates, aes(x = Longitude, y = Latitude),
             color = "grey", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "SoVI Score Groups") +
  labs(title = "Social Vulnerability Index (SoVI) by Census Tract") +
  labs(subtitle = "High Hazard Potential Dams Overlayed") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())




