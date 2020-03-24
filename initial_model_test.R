# Test model:

results[nrow(effective_R0), ] <- NA

results$Date <- effective_R0$date

results$effective_R0[1] <- effective_R0$R0[1]
results$trust_population[1] = trust_population
results$UHB_share[1] = UHB_share
results$total_beds[1] = total_beds
results$total_ICUs[1] = total_ICUs
results$occupied_beds_percentage[1] = occupied_beds_percentage
results$occupied_ICU_percentage[1] = occupied_ICU_percentage
results$newly_incubating[1] = 10
results$total_incubating[1] = 10
results$newly_infectious[1] = 0
results$total_infectious[1] = 0
results$newly_recovering[1] = 0
results$total_recovering[1] = 0
results$newly_recovered[1] = 0
results$total_recovered[1] = 0
results$newly_died[1] = 0
results$total_died[1] = 0
results$susceptible_population[1] = trust_population
results$susceptible_percentage[1] = 1
results$newly_hospitalised[1] = 0
results$hospitalised_released[1] = 0
results$total_hospitalised[1] = 0
results$beds_overloaded[1] = FALSE
results$newly_require_ICU[1] = 0
results$ICU_released[1] = 0
results$total_require_ICU[1] = 0
results$ICU_overloaded[1] = FALSE

for(i in as.list(results$Date[-1])){
  results <- model_new_day(results_df = results
                           , effective_R0_df = effective_R0
                           , demographics = demographics
                           , date_to_model = i
                           , trust_population = trust_population
                           , UHB_share = UHB_share
                           , total_beds = total_beds
                           , total_ICUs = total_ICUs
                           , occupied_beds_percentage = occupied_beds_percentage
                           , occupied_ICU_percentage = occupied_ICU_percentage
                           , recovery_period_days = recovery_period_days
                           , incubation_period_days = incubation_period_days
                           , contagious_period_days = contagious_period_days
                           , hospitalisation_period_days = hospitalisation_period_days
                           , ICU_period_days = ICU_period_days
                           , overloaded_beds_fatality_multiplier = overloaded_beds_fatality_multiplier
                           , overloaded_ICU_fatality_multiplier = overloaded_ICU_fatality_multiplier
  )
}


library(ggplot2)

ggplot2::ggplot(data = results, mapping = aes(Date, total_died)) +
  geom_line()