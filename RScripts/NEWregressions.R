# Correlation for high hazard and EJ supplementary demographic index

summary_county_reg <- summary_county %>%
  filter(high_hazard > 5)

spearman_sovi <- cor.test(summary_county_reg$high_hazard, summary_county_reg$RPL_total, method = "spearman")

#plot for EJ Demographic Index
ggplot(summary_county_reg, aes(x = high_hazard, y = RPL_total)) +
  geom_point() +  # Add scatter points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(
    title = paste0(
      "Relationship Between EJ Demographic Indicator Index and High Hazard Dams\n",
      "Spearman's rho = ", round(spearman_sovi$estimate, 2), 
      ", p-value = ", signif(spearman_sovi$p.value, 3)
    ),
    x = "Total High Hazard Dams",
    y = "Average Dem Index Score"
  ) +
  theme_minimal()



######################

# Function to calculate Pearson's correlation and p-value for each region
calculate_correlation <- function(data, var1, var2) {
  cor_test <- cor.test(data[[var1]], data[[var2]], method = "pearson")
  return(c(correlation = cor_test$estimate, p_value = cor_test$p.value))
}

# Calculate Pearson's correlation for each region
region_correlations <- censusdata %>%
  group_by(Region) %>%
  summarise(
    correlation = cor(SOVI_SCORE, POPULATION, method = "pearson", use = "complete.obs"),
    p_value = cor.test(SOVI_SCORE, POPULATION, method = "pearson")$p.value,
    .groups = "drop"
  )

# Calculate Pearson's correlation for the entire state of NC (without region grouping)
state_correlation <- censusdata %>%
  summarise(
    Region = "NC",  # Label for the state
    correlation = cor(SOVI_SCORE, POPULATION, method = "pearson", use = "complete.obs"),
    p_value = cor.test(SOVI_SCORE, POPULATION, method = "pearson")$p.value
  )

# Combine the region correlations with the state correlation
final_correlations <- bind_rows(region_correlations, state_correlation)

# View the resulting table of correlation coefficients and p-values
print(final_correlations)

