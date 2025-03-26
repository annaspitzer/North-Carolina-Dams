### RIVER BASIN MAP ###

source("R Code/LoadData.R")

river_basins <- st_read("Original Datasets/Major_Basins") %>%
  select(-c(PlanLink, GlobalID)) %>%
  rename(Basin_Sq_Miles = Sq_Miles) %>%
  rename(Basin_Acres = Acres) %>%
  rename(Basin_Name = Name)

highhazard_sf <- finaldata_county %>%
  filter(Hazard_Potential_Classification == "High") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot() +
  geom_sf(data = river_basins, fill = "grey", color = "black", size = 0.1) +
  geom_sf(data = highhazard_sf, aes(color = "red"), shape = 16, size = .5) +
  labs(title = "River Basins and High Hazard Dams in NC",
       subtitle = "Locations of high hazard dams overlayed on river basins",
       fill = "Basin Name",
       color = "High Hazard Dams") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Population of the Basins, all a mess

# NC_censustracts <- tracts(state = 37) %>% 
#   st_transform(crs = 4326)
# river_basins <- st_transform(river_basins, crs = 4326)
# tract_basin <- st_join(NC_censustracts, river_basins, join = st_within)
# tract_basin <- tract_basin %>%
#   select(GEOID, everything())
# 
# nearest_basin <- st_nearest_feature(NC_censustracts, river_basins)
# tract_basin_nearest <- NC_censustracts %>%
#   mutate(nearest_basin_id = nearest_basin) %>%
#   left_join(river_basins %>% select(Basin_Name), by = c("nearest_basin_id" = "rowid"))

aggregated_by_basin <- finaldata_county %>%
  group_by(Basin_Name) %>%
  summarise(
    `High Hazard Dams (HHD)` = sum(Hazard_Potential_Classification == "High", na.rm = TRUE),
    # Population = sum(POPULATION, na.rm = TRUE),
    `Mean Age of Dam` = mean(2024-Year_Completed, na.rm = TRUE),
    # `HHD / Person` = `High Hazard Dams (HHD)` / Population,
    `Mean Distance to City` = mean(Distance_to_Nearest_City_Miles, na.rm = TRUE),
    `Mean Storage` = mean(`Normal_Storage_Acre-Ft`, na.rm = TRUE),
    `Mean Drainage Area` =mean(Drainage_Area_Sq_Miles, na.rm=TRUE),
    # `Storage / Person` = `Mean Storage` / Population,
    .groups = "drop"
  ) %>%
  mutate_if(is.numeric, round, 2)

