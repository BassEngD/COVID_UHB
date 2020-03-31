#https://penn-chime.phl.io/

# Discrete-time SIR modeling of infections/recovery
# The model consists of individuals who are either Susceptible (S), Infected (I), or Recovered (R).
# The epidemic proceeds via a growth and decline process.
# This is the core model of infectious disease spread and has been in use in epidemiology for many years.

# γ = inverse of recovery period (1/x days)
# β = τ × c = effective contact rates = 
# transmissibility (τ) multiplied by the average number of people exposed (c)

# R0 = β/γ
# T(d) = doubling time. Number of days taken to double the number of cases in a susceptible population
# estimated to be 7-10 days

# g = 2^(1/T(d)) − 1
# Since the rate of new infections in the SIR model is g = βS−γ, 
# β is a function of the initial population size of susceptible individuals.
# β=(g+γ)

#S(t+1) = (−β S(t) I(t) ) + S(t)        # Susceptible
#I(t+1) = (β S(t) I(t) − γI(t)) + I(t)  # Infected
#R(t+1) = (γI(t)) + R(t)                # Recovered

gamma_value <- function(recovery_period){
  1 / recovery_period
}

g_value <- function(time_to_double){
  2^(1/time_to_double) - 1
}

beta_value <- function(recovery_period, time_to_double, contact_modifier = 1){
  contact_modifier * (gamma_value(recovery_period) + g_value(time_to_double))
}

d_s <- function(current_S, current_I, current_R, recovery_period, time_to_double, contact_modifier){
  N <- current_S + current_I + current_R
  ds_dt <- current_S * current_I * beta_value(recovery_period, time_to_double, contact_modifier) / N
  return(ds_dt)
}

next_S <- function(current_S, current_I, current_R, recovery_period, time_to_double, contact_modifier){
  susceptible <- current_S - d_s(current_S, current_I, current_R, recovery_period, time_to_double, contact_modifier)
  susceptible <- round(susceptible, digits = 0)
  return(susceptible)
}

next_I <- function(current_S, current_I, current_R, recovery_period, time_to_double, contact_modifier){
  infected <- current_I + d_s(current_S, current_I, current_R, recovery_period, time_to_double, contact_modifier) - (current_I * gamma_value(recovery_period))
  infected <- round(infected, digits = 0)
  return(infected)
}

next_R <- function(current_I, current_R, recovery_period){
  recovered <- current_R + (gamma_value(recovery_period) * current_I)
  recovered <- round(recovered, digits = 0)
  return(recovered)
}

begin_social_distancing <- function(simulation_data, social_distancing_start_date, contact_modifier){
  dates_to_change <- which(simulation_data$date >= social_distancing_start_date)
  simulation_data$contact_modifier[dates_to_change] <- contact_modifier
  return(simulation_data)
}

new_simulation_table <- function(start_date, end_date, recovery_period
                                 , time_to_double, starting_population
                                 , starting_infected, hospitalisation_rate
                                 , require_ICU_rate
                                 ){
  simulation_data <- data.frame(stringsAsFactors = FALSE
                                , date = seq.Date(from = start_date
                                                  , to = end_date
                                                  , by = "days"
                                                  )
                                , recovery_period = recovery_period
                                , time_to_double = time_to_double
                                , contact_modifier = 1
                                # , total_beds = total_beds
                                # , fraction_beds_for_covid = fraction_beds_for_covid
                                # , total_ICU = total_ICU
                                # , fraction_ICU_for_covid = fraction_ICU_for_covid
                                , newly_infected = NA_integer_
                                , newly_symptomatic = NA_integer_
                                , newly_recovered = NA_integer_
                                , newly_hospitalised = NA_integer_
                                , newly_requiring_ICU = NA_integer_
                                , newly_left_hospital = NA_integer_
                                , newly_left_ICU = NA_integer_
                                , total_incubating = NA_integer_
                                , total_symptomatic = NA_integer_
                                , total_recovered = NA_integer_
                                , total_in_hospital = NA_integer_
                                , total_in_ICU = NA_integer_
                                , susceptible = NA_integer_     # based on SIR model
                                , infected = NA_integer_        # based on SIR model
                                , recovered = NA_integer_       # based on SIR model
                                , hospitalised = NA_integer_    # based on SIR model
                                , require_ICU = NA_integer_     # based on SIR model
                                , confirmed_hospitalisations = NA_integer_
                                )

  simulation_data$newly_infected[1] <- 0
  simulation_data$newly_symptomatic[1] <- starting_infected
  simulation_data$newly_recovered[1] <- 0
  simulation_data$newly_hospitalised[1] <- round(starting_infected * hospitalisation_rate)
  simulation_data$newly_requiring_ICU[1] <- round(starting_infected * hospitalisation_rate * require_ICU_rate)
  simulation_data$newly_left_hospital[1] <- 0
  simulation_data$newly_left_ICU[1] <- 0
  
  simulation_data$susceptible[1] <- starting_population
  simulation_data$infected[1] <- starting_infected
  simulation_data$total_incubating[1] <- 0
  simulation_data$total_symptomatic[1] <- simulation_data$infected[1]
  simulation_data$total_recovered[1] <- 0
  simulation_data$total_in_hospital[1] <- round(starting_infected * hospitalisation_rate)
  simulation_data$total_in_ICU[1] <- round(starting_infected * hospitalisation_rate * require_ICU_rate)
  simulation_data$recovered[1] <- 0
  
  return(simulation_data)
}

model_infection <- function(simulation_data
                            , incubation_period
                            , hospital_LOS
                            , ICU_LOS
                            , hospitalisation_rate
                            , require_ICU_rate
                            ){
  for(i in seq(nrow(simulation_data))){
    if(i == 1){
      next()
    }
    simulation_data$susceptible[i] <- next_S(current_S = simulation_data$susceptible[i-1]
                                             , current_I = simulation_data$infected[i-1]
                                             , current_R = simulation_data$recovered[i-1]
                                             , recovery_period = simulation_data$recovery_period[i-1]
                                             , time_to_double = simulation_data$time_to_double[i-1]
                                             , contact_modifier = simulation_data$contact_modifier[i-1]
                                             )
    simulation_data$infected[i] <- next_I(current_S = simulation_data$susceptible[i-1]
                                          , current_I = simulation_data$infected[i-1]
                                          , current_R = simulation_data$recovered[i-1]
                                          , recovery_period = simulation_data$recovery_period[i-1]
                                          , time_to_double = simulation_data$time_to_double[i-1]
                                          , contact_modifier = simulation_data$contact_modifier[i-1]
                                          )
    simulation_data$newly_infected[i] <- d_s(current_S = simulation_data$susceptible[i-1]
                                             , current_I = simulation_data$infected[i-1]
                                             , current_R = simulation_data$recovered[i-1]
                                             , recovery_period = simulation_data$recovery_period[i-1]
                                             , time_to_double = simulation_data$time_to_double[i-1]
                                             , contact_modifier = simulation_data$contact_modifier[i-1]
                                             )
    simulation_data$newly_infected[i] <- round(simulation_data$newly_infected[i],0)
    
    newly_symptomatic_day <- i - incubation_period
    simulation_data$newly_symptomatic[i] <- ifelse(newly_symptomatic_day > 0
                                                   , simulation_data$newly_infected[newly_symptomatic_day]
                                                   , 0
                                                   )
    
    newly_recovered_day <- i - recovery_period
    simulation_data$newly_recovered[i] <- ifelse(newly_recovered_day > 0
                                                   , simulation_data$newly_infected[newly_recovered_day]
                                                   , 0
                                                 )
    
    simulation_data$newly_hospitalised[i] <- simulation_data$newly_symptomatic[i] * hospitalisation_rate
    simulation_data$newly_hospitalised[i] <- round(simulation_data$newly_hospitalised[i], 0)
    simulation_data$newly_requiring_ICU[i] <- simulation_data$newly_hospitalised[i] * require_ICU_rate
    simulation_data$newly_requiring_ICU[i] <- round(simulation_data$newly_requiring_ICU[i], 0)
    
    incubating_days <- (i-incubation_period):i
    incubating_days <- incubating_days[which(incubating_days > 0)]
    simulation_data$total_incubating[i] <- sum(simulation_data$newly_infected[incubating_days], na.rm = TRUE)
    
    left_hospital_day <- i - hospital_LOS
    simulation_data$newly_left_hospital[i] <- ifelse(left_hospital_day > 0
                                                     , simulation_data$newly_hospitalised[left_hospital_day]
                                                     , 0
                                                     )
    left_ICU_day <- i - ICU_LOS
    simulation_data$newly_left_ICU[i] <- ifelse(left_ICU_day > 0
                                                , simulation_data$newly_requiring_ICU[left_ICU_day]
                                                , 0
                                                )
    
    simulation_data$total_recovered[i] <- sum(simulation_data$newly_recovered, na.rm = TRUE)
    simulation_data$total_incubating[i] <- sum(simulation_data$newly_infected, na.rm = TRUE) - simulation_data$total_recovered[i]
    simulation_data$total_in_hospital[i] <- sum(simulation_data$newly_hospitalised, na.rm = TRUE) - sum(simulation_data$newly_left_hospital, na.rm = TRUE)
    simulation_data$total_in_ICU[i] <- sum(simulation_data$newly_requiring_ICU, na.rm = TRUE) - sum(simulation_data$newly_left_ICU, na.rm = TRUE)
    simulation_data$total_symptomatic[i] <- sum(simulation_data$newly_symptomatic, na.rm = TRUE) - sum(simulation_data$newly_recovered, na.rm = TRUE)
    
    
    simulation_data$recovered[i] <- next_R(current_I = simulation_data$infected[i-1]
                                           , current_R = simulation_data$recovered[i-1]
                                           , recovery_period = simulation_data$recovery_period[i-1]
                                           )
    simulation_data$hospitalised[i] <- simulation_data$infected[i] * hospitalisation_rate
    simulation_data$hospitalised[i] <- round(simulation_data$hospitalised[i], 0)
    simulation_data$require_ICU[i] <- simulation_data$hospitalised[i] * require_ICU_rate
    simulation_data$require_ICU[i] <- round(simulation_data$require_ICU[i], 0)
  }
  return(simulation_data)
}

plot_simulation <- function(simulation_data){
  tidy_simulation <- pivot_longer(data = simulation_data[, c(1,13,15,16)]
                                  , cols = -date
                                  , names_to = "variable"
  )
  tidy_simulation$variable <- as.factor(tidy_simulation$variable)  

  simulation_plot <- ggplot(data = tidy_simulation
                            , mapping = aes(x = date
                                            , y = value
                                            , group = variable
                                            )
                            ) +
    labs(title = "Model Output") +
    xlab("Date") +
    ylab("Number of People") +
    geom_line(aes(linetype = variable)) +
    scale_y_continuous(trans='log10')
  
  return(simulation_plot)
}







recovery_period <- 18
time_to_double <- 10
incubation_period <- 4
contact_modifier <- 0.3 # beta is modified by this factor to represent social distancing. 1 by default (no measures taken)
social_distancing_start_date <- as.Date("2020-03-23")
starting_population <- 1200000
starting_infected <- 500
starting_recovered <- 0
start_date <- as.Date("2020-03-31")
end_date <- as.Date("2021-03-31")
# total_beds <- 500
# fraction_beds_for_covid <- 0.6
# total_ICU <- 50
# fraction_ICU_for_covid <- 0.75
hospitalisation_rate <- 0.10  # percentage of cases requiring hospital admission
require_ICU_rate <- 0.20      # percentage of _admissions_ requiring ICU
hospital_LOS <- 11            # average length of stay in hospital
ICU_LOS <- 8                  # average length of stay in ICU

# simulation_data <- new_simulation_table(start_date
#                                         , end_date
#                                         , recovery_period
#                                         , time_to_double
#                                         , starting_population
#                                         , starting_infected
#                                         , hospitalisation_rate
#                                         , require_ICU_rate
#                                         ) %>%
#   begin_social_distancing(social_distancing_start_date
#                           , contact_modifier
#                           ) %>%
#   model_infection(incubation_period
#                   , hospital_LOS
#                   , ICU_LOS
#                   , hospitalisation_rate
#                   , require_ICU_rate
#                   )
# 
# plot(simulation_data %>%
#        plot_simulation())