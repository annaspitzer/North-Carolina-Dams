
source("RScripts/LoadData.R")

## mostly old stuff but keeping in case

# Owner vs Dam Condition Stuff

ownership_condition_summary <- finaldata_county %>%
  group_by(Primary_Owner_Type, Condition_Assessment) %>%
  summarise(dam_count = n(), .groups = "drop") %>%
  arrange(Primary_Owner_Type, desc(dam_count))

ownership_condition_summary

ggplot(ownership_condition_summary, aes(x = reorder(Primary_Owner_Type, -dam_count), y = dam_count, fill = Condition_Assessment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Dam Ownership by Condition Assessment",
    x = "Primary Owner Type",
    y = "Number of Dams",
    fill = "Condition Assessment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Primary_Owner_Type vs Condition_Assessment
ownership_summary <- finaldata_county %>%
  filter(Hazard_Potential_Classification == "High") %>%
  group_by(Primary_Owner_Type, Condition_Assessment) %>%
  summarise(dam_count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Condition_Assessment,
    values_from = dam_count,
    values_fill = 0
  ) %>%
  mutate(Total_Dams = rowSums(across(-Primary_Owner_Type), na.rm = TRUE)) %>%
  select(Primary_Owner_Type, Total_Dams, everything())

print(summary_table)
