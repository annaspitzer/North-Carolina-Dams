library(dplyr)
library(tidyverse)
library(sf)
library(tigris)

### STATS BY COUNTY) ###
summary_county <- finaldata_county %>%
  group_by(CountyID, County) %>%
  summarise(
    total_dams = ifelse(all(is.na(NID_ID)), 0, n_distinct(NID_ID)),
    population = round(mean(E_TOTPOP)),
    dams_p_person = (total_dams / population),
    high_hazard = sum(Hazard_Potential_Classification == "High", na.rm = TRUE),
    hh_dams_p_person = high_hazard / population,
    avg_year_completed = round(mean(Year_Completed, na.rm = TRUE)),
    avg_age = 2024 - round(mean(Year_Completed, na.rm = TRUE)),
    prop_less_than_25_feet = mean(NID_Height_Category == "Less than 25 feet", na.rm = TRUE),
    prop_25_50_feet = mean(NID_Height_Category == "25-50 feet", na.rm = TRUE),
    prop_51_100_feet = mean(NID_Height_Category == "51-100 feet", na.rm = TRUE),
    prop_greater_than_100_feet = mean(NID_Height_Category == "Greater than 100 feet", na.rm = TRUE),
    prop_undetermined = mean(NID_Height_Category == "Undetermined", na.rm = TRUE),
    avg_distance_to_city = mean(Distance_to_Nearest_City_Miles, na.rm = TRUE),
    avg_drainage_area = mean(Drainage_Area_Sq_Miles, na.rm = TRUE),
    avg_volume = mean(Volume_Cubic_Yards, na.rm = TRUE),
    prop_low_risk = mean(Hazard_Potential_Classification == "Low", na.rm = TRUE),
    prop_significant_risk = mean(Hazard_Potential_Classification == "Significant", na.rm = TRUE),
    # SOVI themes
    RPL_1 = mean(RPL_THEME1, na.rm = TRUE),
    RPL_2 = mean(RPL_THEME2, na.rm = TRUE),
    RPL_3 = mean(RPL_THEME3, na.rm = TRUE),
    RPL_4 = mean(RPL_THEME4, na.rm = TRUE),
    RPL_total = mean(RPL_THEMES, na.rm = TRUE)  
    ) 


########## OLD ##############

# Regional statistics for poster
summary_region <- finaldata_county %>%
  filter(Region %in% c("Western", "Piedmont", "Coastal")) %>%
  group_by(Region) %>%
  summarise(
    `High Hazard Dams (HHD)` = sum(Hazard_Potential_Classification == "High", na.rm = TRUE),
    Population = sum(E_TOTPOP[!duplicated(CountyID)], na.rm = TRUE),  # Summing unique County population
    `Mean Age of Dam` = mean(2024-Year_Completed, na.rm = TRUE),
    `HHD / Person` = `High Hazard Dams (HHD)` / Population,
    `Mean Distance to City` = mean(Distance_to_Nearest_City_Miles, na.rm = TRUE),
    `Mean Storage` = mean(Volume_Cubic_Yards, na.rm = TRUE),
    `Storage / Person` = `Mean Storage` / Population,
    .groups = "drop"
  )

