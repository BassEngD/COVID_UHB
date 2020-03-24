library(dplyr)
library(purrr)
# Progression of illness in simple model

#   Incubating -> (incubation period)
#-> Infectious -> (contagious period) 
#-> recovering -> (recovery period - contagious period) 
#-> recovered or died


demographics <- data.frame(
    age_group_id = 1:9
    , age_group = c("0-9"
                    , "10-19"
                    , "20-29"
                    , "30-39"
                    , "40-49"
                    , "50-59"
                    , "60-69"
                    , "70-79"
                    , "80+"
    )
    , hospitalisation_percentage = c(0.001
                                     , 0.003
                                     , 0.002
                                     , 0.032
                                     , 0.049
                                     , 0.102
                                     , 0.166
                                     , 0.243
                                     , 0.273
    )
    , require_ICU_percentage = c(0.050
                                 , 0.050
                                 , 0.050
                                 , 0.050
                                 , 0.063
                                 , 0.122
                                 , 0.274
                                 , 0.432
                                 , 0.709
    )
    , baseline_fatality_percentage = c(0.00002
                                       , 0.00006
                                       , 0.0003
                                       , 0.0008
                                       , 0.0015
                                       , 0.0060
                                       , 0.0220
                                       , 0.0510
                                       , 0.0930
    )
    , age_demographic = c(0.12
                          , 0.13
                          , 0.14
                          , 0.13
                          , 0.12
                          , 0.13
                          , 0.11
                          , 0.07
                          , 0.04
                          )
)

default_R0 <- 2.4
trust_population <- 1200000

effective_R0 <- data.frame(date = seq.Date(from = as.Date.character("2020-03-01", format = "%Y-%m-%d")
                                           , to = as.Date.character("2020-05-31", format = "%Y-%m-%d")
                                           , by = "day"
                                            )
                            , R0 = rep(default_R0, times = 92)
                            )

UHB_share <- 0.9 #percentage of patients in area being served by UHB
total_beds <- 1000
total_ICUs <- 50
occupied_beds_percentage <- 0.6 # beds that are being used by non-covid patients
occupied_ICU_percentage <- 0.6 # ICUs that are being used by non-covid patients

recovery_period_days <- 14
incubation_period_days <- 5
contagious_period_days <- 4

hospitalisation_period_days <- 7 # how long after hospitalisation is an admission released
ICU_period_days <- 5 # how long after hospitalisation is an admission needing ICU released (from ICU, not hospital)

overloaded_beds_fatality_multiplier <- 1.05 # increase the fatality rate if beds are overloaded
overloaded_ICU_fatality_multiplier <- 1.25 # increase the fatality rate if ICUs are overloaded

infections_per_day_per_infectious <- function(contagious_period_days, R0){
    contagious_period_days / R0
}

recovering_patients <- function(results_df, date_to_model, recovery_period_days, contagious_period_days){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    recovering_period_days_ago_row_number <- (new_date_row_number - recovery_period_days) + contagious_period_days
    if(recovering_period_days_ago_row_number < 1){
        return(0)
    } else {
        recovering <- results_df$newly_incubating[recovering_period_days_ago_row_number]
        return(recovering)
    }
}

# patients will either recover or die at the end of the recovery period
recover_patients <- function(results_df, date_to_model, recovery_period_days){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    recovery_period_days_ago_row_number <- new_date_row_number - recovery_period_days
    if(recovery_period_days_ago_row_number < 1){
        return(0)
    } else {
        recovered <- results_df$newly_incubating[recovery_period_days_ago_row_number]
        return(recovered)
    }
}

recover_hospitalised_patients <- function(results_df, date_to_model, hospitalisation_period_days){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    recovery_period_days_ago_row_number <- new_date_row_number - hospitalisation_period_days
    if(recovery_period_days_ago_row_number < 1){
        return(0)
    } else {
        recovered <- results_df$newly_hospitalised[recovery_period_days_ago_row_number]
        return(recovered)
    }
}

recover_ICU_patients <- function(results_df, date_to_model, ICU_period_days){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    recovery_period_days_ago_row_number <- new_date_row_number - ICU_period_days
    if(recovery_period_days_ago_row_number < 1){
        return(0)
    } else {
        recovered <- results_df$newly_require_ICU[recovery_period_days_ago_row_number]
        return(recovered)
    }
}

patient_deaths <- function(results_df
                           , date_to_model
                           , recovered_patients # number
                           , demographics
                           , overloaded_beds_fatality_multiplier
                           , overloaded_ICU_fatality_multiplier
){
    # A % of 'recovering' patients will die based on age groups and overloading
    
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    
    ICU_overloaded <- results_df$ICU_overloaded[new_date_row_number - 1]
    beds_overloaded <- results_df$beds_overloaded[new_date_row_number - 1]
    
    if(recovered_patients < 1){
        deaths <- tibble(age_group_id = 1:9
                         , deaths = rep(0, times = 9)
        )
    } else {
        deaths <- tibble(age_group_id = 1:9
                         , deaths = purrr::map_dbl(demographics$baseline_fatality_percentage
                           , function(baseline_fatality_percentage
                              , recovering_patients
                              , beds_overloaded
                              , ICU_overloaded
                              , overloaded_beds_fatality_multiplier
                              , overloaded_ICU_fatality_multiplier){
                                    round(recovering_patients * (baseline_fatality_percentage *
                                        ifelse(beds_overloaded
                                               , overloaded_beds_fatality_multiplier
                                               , 1
                                        ) *
                                        ifelse(ICU_overloaded
                                               , overloaded_ICU_fatality_multiplier
                                               , 1
                                                )
                                        )
                                    , digits = 0
                                    )
                           }
                           , recovering_patients = recovered_patients 
                           , beds_overloaded = beds_overloaded
                            , ICU_overloaded = ICU_overloaded
                           , overloaded_beds_fatality_multiplier = overloaded_beds_fatality_multiplier
                           , overloaded_ICU_fatality_multiplier = overloaded_ICU_fatality_multiplier
                         ))
    }
    total_deaths <- sum(deaths$deaths)
    return(total_deaths)
}


# infectious means that the condition has ended its incubation period
# this is also the time when the patient will be hospitalised if necessary
become_infectious <- function(results_df, date_to_model, incubation_period_days){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    incubation_period_days_ago_row_number <- new_date_row_number - incubation_period_days
    if(incubation_period_days_ago_row_number < 1){
        return(0)
    } else {
        infectious <- results_df$newly_incubating[incubation_period_days_ago_row_number]
        return(infectious)
    }
}

hospitalise_patients <- function(new_cases_infectious, demographics){
    demo <- tibble(age_group_id = 1:9
           , hospitalised = purrr::map_dbl(demographics$hospitalisation_percentage
                , function(hospitalisation_percentage, new_cases_infectious){
                    round(new_cases_infectious * hospitalisation_percentage, digits = 0)
                }
                , new_cases_infectious = new_cases_infectious
                )
           )
    total <- sum(demo$hospitalised)
    total <- round(total, 0)
    return(total)
}

ICU_patients <- function(new_cases_infectious, demographics){
    demo <- tibble(
        age_group_id = 1:9
       , hospitalised = purrr::map2_dbl(demographics$hospitalisation_percentage
                                        , demographics$require_ICU_percentage
                                       , function(hospitalisation_percentage, ICU_percentage, new_cases_infectious){
                                           round(
                                               round(
                                                   new_cases_infectious * hospitalisation_percentage
                                                   , digits = 0) *
                                            ICU_percentage
                                            , digits = 0
                                            )
                                       }
                                       , new_cases_infectious = new_cases_infectious
       )
    )
    total <- sum(demo$hospitalised)
    total <- round(total, 0)
    return(total)
}

susceptible_population <- function(results_df
                                   , date_to_model
                                   , trust_population
                                   ){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    # recovered patients includes deaths, but including these would double-count them since they are also removed from the trust population.
    recovered_patients <- results_df$total_recovered[new_date_row_number - 1] -
        results_df$total_died[new_date_row_number - 1]
    currently_infected <- results_df$total_incubating[new_date_row_number - 1] + 
        results_df$total_infectious[new_date_row_number - 1] + 
        results_df$total_recovering[new_date_row_number - 1]
    
    susceptible <- trust_population - recovered_patients - currently_infected
    susceptible <- ifelse(susceptible < 0, 0, susceptible)
    return(susceptible)
}

new_infections <- function(results_df
                           , date_to_model
                           , contagious_period_days
                           ){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    R0 <- results_df$effective_R0[new_date_row_number - 1]
    percentage_susceptible <- results_df$susceptible_percentage[new_date_row_number - 1]
    
    newly_infected <- results_df$total_infectious[new_date_row_number - 1] * 
        infections_per_day_per_infectious(contagious_period_days, R0) * 
        percentage_susceptible
    newly_infected <- round(newly_infected, 0)
    return(newly_infected)
}



model_new_day <- function(results_df
                          , effective_R0_df
                          , demographics
                          , date_to_model
                          , trust_population
                          , UHB_share
                          , total_beds
                          , total_ICUs
                          , occupied_beds_percentage
                          , occupied_ICU_percentage
                          , recovery_period_days
                          , incubation_period_days
                          , contagious_period_days
                          , hospitalisation_period_days
                          , ICU_period_days
                          , overloaded_beds_fatality_multiplier
                          , overloaded_ICU_fatality_multiplier
                        ) {
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    effective_R0 <- effective_R0_df[[new_date_row_number, "R0"]]
    trust_population <- results_df$trust_population[1] - results_df$total_died[new_date_row_number - 1]
    
    # recovered patients (includes deaths, despite name)
    newly_recovered <- recover_patients(results_df, date_to_model, recovery_period_days)
    total_recovered <- newly_recovered + results_df$total_recovered[new_date_row_number - 1]
    newly_released_hosp <- recover_hospitalised_patients(results_df, date_to_model, hospitalisation_period_days)
    newly_released_ICU <- recover_ICU_patients(results_df, date_to_model, ICU_period_days)
    
    #recovered patients that died
    newly_died <- patient_deaths(results_df
                                 , date_to_model
                                 , newly_recovered
                                 , demographics
                                 , overloaded_beds_fatality_multiplier
                                 , overloaded_ICU_fatality_multiplier)
    total_died <- newly_died + results_df$total_died[new_date_row_number - 1]
    
    # recovering patients (after infectious period, before recovered)
    newly_recovering <- recovering_patients(results_df, date_to_model, recovery_period_days, contagious_period_days)
    total_recovering <- newly_recovering + results_df$total_recovering[new_date_row_number - 1] - newly_recovered
    
    # cases at end of incubation period
    newly_infectious <- become_infectious(results_df, date_to_model, incubation_period_days)
    total_infectious <- (results_df$total_infectious[new_date_row_number - 1] - newly_recovering) + newly_infectious
    
    # this is when they become hospitalised
    newly_hospitalised <- hospitalise_patients(new_cases_infectious = newly_infectious, demographics)
    newly_require_ICU <- ICU_patients(new_cases_infectious = newly_infectious, demographics) 
    
    susceptible <- susceptible_population(results_df, date_to_model, trust_population)
    susceptible_percentage <- susceptible / trust_population
    
    total_hospitalised <- results_df$total_hospitalised[new_date_row_number - 1] - newly_released_hosp + newly_hospitalised
    total_require_ICU <- results_df$total_require_ICU[new_date_row_number - 1] + newly_require_ICU - newly_released_ICU
    
    newly_incubating <- new_infections(results_df, date_to_model, contagious_period_days)
    total_incubating <- results_df$total_incubating[new_date_row_number - 1] + newly_incubating - newly_infectious
    
    new_day_model <- tibble(Date = as.Date(date_to_model)
                            , effective_R0 = effective_R0
                            , trust_population = trust_population
                            , UHB_share = UHB_share
                            , total_beds = total_beds
                            , total_ICUs = total_ICUs
                            , occupied_beds_percentage = occupied_beds_percentage
                            , occupied_ICU_percentage = occupied_ICU_percentage
                            , virus_characteristics = list(tibble(
                                recovery_period_days = recovery_period_days
                                , incubation_period_days = incubation_period_days
                                , contagious_period_days = contagious_period_days)
                                )
                            , newly_incubating = newly_incubating
                            , total_incubating = total_incubating
                            , newly_infectious = newly_infectious
                            , total_infectious = total_infectious
                            , newly_recovering = newly_recovering
                            , total_recovering = total_recovering
                            , newly_recovered = newly_recovered
                            , total_recovered = total_recovered
                            , newly_died = newly_died
                            , total_died = total_died
                            
                            , susceptible_population = susceptible
                            , susceptible_percentage = susceptible_percentage
                            
                            , newly_hospitalised = newly_hospitalised
                            , hospitalised_released = newly_released_hosp
                            , total_hospitalised = total_hospitalised
                            , beds_overloaded = total_hospitalised > total_beds
                            , newly_require_ICU = newly_require_ICU
                            , ICU_released = newly_released_ICU
                            , total_require_ICU = total_require_ICU
                            , ICU_overloaded = total_require_ICU > total_ICUs
                            )
    
    
    results_df[new_date_row_number,] <- new_day_model[1,]
    return(results_df)
}

empty_date <- integer()
class(empty_date) <- "Date"

results <- tibble(Date = empty_date
                  , effective_R0 = numeric()
                  , trust_population = integer()
                  , UHB_share = numeric()
                  , total_beds = numeric()
                  , total_ICUs = numeric()
                  , occupied_beds_percentage = numeric()
                  , occupied_ICU_percentage = numeric()
                  , virus_characteristics = list()
                  , newly_incubating = numeric()
                  , total_incubating = numeric()
                  , newly_infectious = numeric()
                  , total_infectious = numeric()
                  , newly_recovering = numeric()
                  , total_recovering = numeric()
                  , newly_recovered = numeric()
                  , total_recovered = numeric()
                  , newly_died = numeric()
                  , total_died = numeric()
                  
                  , susceptible_population = numeric()
                  , susceptible_percentage = numeric()
                  
                  
                  , newly_hospitalised = numeric()
                  , hospitalised_released = numeric()
                  , total_hospitalised = numeric()
                  , beds_overloaded = logical()
                  , newly_require_ICU = numeric()
                  , ICU_released = numeric()
                  , total_require_ICU = numeric()
                  , ICU_overloaded = logical()
)