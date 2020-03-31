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

recovery_period <- 14
time_to_double <- 4
contact_modifier <- 0.5 # beta is modified by this factor to represent social distancing. 1 by default (no measures taken)
social_distancing_start_date <- as.Date("2020-03-15")
starting_population <- 1200000
starting_infected <- 10
starting_recovered <- 0
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2020-07-01")
# total_beds <- 500
# fraction_beds_for_covid <- 0.6
# total_ICU <- 50
# fraction_ICU_for_covid <- 0.75
hospitalisation_rate <- 0.10  # percentage of cases requiring hospital admission
require_ICU_rate <- 0.20      # percentage of _admissions_ requiring ICU

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
                              , susceptible = NA_integer_
                              , infected = NA_integer_
                              , recovered = NA_integer_
                              , hospitalised = NA_integer_
                              , require_ICU = NA_integer_
                              )

simulation_data$susceptible[1] <- starting_population
simulation_data$infected[1] <- starting_infected
simulation_data$recovered[1] <- 0
simulation_data <- begin_social_distancing(simulation_data, social_distancing_start_date, contact_modifier)

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
  simulation_data$recovered[i] <- next_R(current_I = simulation_data$infected[i-1]
                                         , current_R = simulation_data$recovered[i-1]
                                         , recovery_period = simulation_data$recovery_period[i-1]
                                         )
  simulation_data$hospitalised[i] <- simulation_data$infected[i] * hospitalisation_rate
  simulation_data$hospitalised[i] <- round(simulation_data$hospitalised[i], 0)
  simulation_data$require_ICU[i] <- simulation_data$hospitalised[i] * require_ICU_rate
  simulation_data$require_ICU[i] <- round(simulation_data$require_ICU[i], 0)
}