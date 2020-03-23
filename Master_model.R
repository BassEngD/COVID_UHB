library(dplyr)
library(purrr)
# Progression of illness in simple model

#   Infected -> (incubation period)
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
    , hospitalization_percentage = c(0.001
                                     , 0.003
                                     , 1.002
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
                                           , to = as.Date.character("2020-03-30", format = "%Y-%m-%d")
                                           , by = "day"
                                            )
                            , R0 = rep(default_R0, times = 30)
                            )

UHB_share <- 0.9 #percentage of patients in area being served by UHB
total_beds <- 1000
occupied_beds_percentage <- 0.6 # beds that are being used by non-covid patients

recovery_period_days <- 14
incubation_period_days <- 5
contagious_period_days <- 4

hospitalization_period_days <- 14 # how long after hospitalization is an admission released
ICU_period_days <- 14 # how long after hospitalization is an admission needing ICU released

overloaded_beds_fatality_multiplier <- 1.05 # increase the fatality rate if beds are overloaded
overloaded_ICU_fatality_multiplier <- 1.25 # increase the fatality rate if ICUs are overloaded

infections_per_day_per_infectious <- function(contagious_period_days, R0){
    contagious_period_days / R0
}

# patients will either recover or die at the end of the recovery period
recover_patients <- function(results_df, date_to_model, recovery_period_days){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    recovery_period_days_ago_row_number <- new_date_row_number - recovery_period_days
    if(recovery_period_days_ago_row_number < 1){
        return(0)
    } else {
        recovered <- results_df$newly_infected[recovery_period_days_ago_row_number]
        return(recovered)
    }
}

patient_deaths <- function(results_df
                           , date_to_model
                           , recovery_period_days
                           , demographics
                           , overloaded_beds_fatality_multiplier
                           , overloaded_ICU_fatality_multiplier
                           ){
    recovered_patients <- recover_patients(results_df, date_to_model, recovery_period_days)
    
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    
    ICU_overloaded <- results_df$ICU_overloaded[new_date_row_number - 1]
    beds_overloaded <- results_df$beds_overloaded[new_date_row_number - 1]
    
    if(recovered_patients < 1){
        deaths <- tibble(age_group_id = 1:9
                         , deaths = rep(0, times = 9)
                         )
    } else {
        deaths <- tibble(age_group_id = 1:9
                         , deaths = purrr::map_dbl(demographics_df$baseline_fatality_percentage
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
                            , beds_overloaded = 
                            , ICU_overloaded
                            , overloaded_beds_fatality_multiplier
                            , overloaded_ICU_fatality_multiplier
        ))
    }
    return(deaths)
}

# recover_hospitalized_patients <- function(results_df, date_to_model, hospitalization_period_days){
#     new_date_row_number <- which(results_df[["Date"]] == date_to_model)
#     recovery_period_days_ago_row_number <- new_date_row_number - hospitalization_period_days
#     if(recovery_period_days_ago_row_number < 1){
#         return(0)
#     } else {
#         recovered <- results_df$newly_infected[recovery_period_days_ago_row_number]
#         return(recovered)
#     }
# }
# 
# recover_ICU_patients <- function(results_df, date_to_model, ICU_period_days){
#     new_date_row_number <- which(results_df[["Date"]] == date_to_model)
#     recovery_period_days_ago_row_number <- new_date_row_number - ICU_period_days
#     if(recovery_period_days_ago_row_number < 1){
#         return(0)
#     } else {
#         recovered <- results_df$newly_infected[recovery_period_days_ago_row_number]
#         return(recovered)
#     }
# }

# infectious means that the condition has ended its incubation period
# this is also the time when the patient will be hospitalized if necessary
become_infectious <- function(results_df, date_to_model, incubation_period_days){
    new_date_row_number <- which(results_df[["Date"]] == date_to_model)
    incubation_period_days_ago_row_number <- new_date_row_number - incubation_period_days
    if(incubation_period_days_ago_row_number < 1){
        return(0)
    } else {
        infectious <- results_df$newly_infected[incubation_period_days_ago_row_number]
        return(infectious)
    }
}

hospitalise_patients <- function(new_cases_infectious, demographics_df){
    tibble(age_group_id = 1:9
           , hospitalized = purrr::map_dbl(demographics_df$hospitalization_percentage
                , function(hospitalization_percentage, new_cases_infectious){
                    round(new_cases_infectious * hospitalization_percentage, digits = 0)
                }
                , new_cases_infectious = new_cases_infectious
                )
           )
}

ICU_patients <- function(new_cases_infectious, demographics_df){
    tibble(age_group_id = 1:9
           , hospitalized = purrr::map_dbl(demographics_df$require_ICU_percentage
                                           , function(ICU_percentage, new_cases_infectious){
                                               round(new_cases_infectious * ICU_percentage, digits = 0)
                                           }
                                           , new_cases_infectious = new_cases_infectious
           )
    )
}

susceptible_population <- function(initial_population, recovered_patients, currently_infected, patients_died){
    initial_population - recovered_patients - currently_infected - patients_died
}

percentage_susceptible <- function(susceptible_population, initial_population, patients_died){
    susceptible_population / (initial_population - patients_died)
}

new_infections <- function(percentage_susceptible, currently_infectious, infections_per_day_per_infectious){
    currently_infected * infections_per_day_per_infectious * percentage_susceptible
}



model_new_day <- function(results_df
                          , effective_R0_df
                          , demographics_df
                          , date_to_model
                          , trust_population
                          , UHB_share
                          , total_beds
                          , occupied_beds_percentage
                          , occupied_ICU_percentage
                          , recovery_period_days
                          , incubation_period_days
                          , contagious_period_days
                          , hospitalization_period_days
                          , ICU_period_days
                        ) {

    # recovering patients
    newly_recovered <- recover_patients(results_df, date_to_model, recovery_period_days)
    newly_released_hosp <- recover_hospitalized_patients(results_df, date_to_model, hospitalization_period_days)
    newly_released_ICU <- recover_ICU_patients(results_df, date_to_model, ICU_period_days)
    
    # cases at end of incubation period
    newly_infectious <- new_infectious(results_df, date_to_model, incubation_period_days)
    
    
    susceptible <- susceptible_population(
        initial_population = trust_population
        , recovered_patients = recover_patients(new_cases_recovery_days_ago =
                                                    )
    )
    
    susceptible_percentage <- percentage_susceptible(susceptible_population = susceptible
                                                     , initial_population = trust_population
                                                     , patients_died = results_df$total_deaths[new_date_row_number - 1]
                                                     )
    
    newly_infected <- new_infections()
    
    
    newly_hospitalized <- hospitalised_patients()
    newly_require_ICU <- ICU_patients()

    
    new_day_model <- tibble(Date = date_to_model
                            , effective_R0 = effective_R0_df[[new_date_row_number, "R0"]]
                            , trust_population = trust_population
                            , UHB_share = UHB_share
                            , total_beds = total_beds
                            , occupied_beds_percentage = occupied_beds_percentage
                            , occupied_ICU_percentage = occupied_ICU_percentage
                            , virus_characteristics = list(tibble(recovery_period_days = recovery_period_days
                                                                  , incubation_period_days = incubation_period_days
                                                                  , contagious_period_days = contagious_period_days)
                                                           )
                            , newly_infected = numeric()
                            , total_infected = numeric()
                            , newly_infectious = numeric()
                            , total_infectious = numeric()
                            , newly_recovering = numeric()
                            , total_recovering = numeric()
                            , newly_recovered = numeric()
                            , total_recovered = numeric()
                            
                            , susceptible_population = numeric()
                            , susceptible_percentage = numeric()
                            
                            
                            , newly_hospitalised = list() # tibble with values for each age group
                            , hospitalised_released = numeric()
                            , total_hospitalised = numeric()
                            , beds_overloaded = logical()
                            , newly_require_ICU = numeric()
                            , ICU_released = numeric()
                            , total_require_ICU = numeric()
                            , ICU_overloaded = logical()
                            , deaths_today = numeric() # based on total deaths and overloaded status
                            )
    
    
    results_df <- rbind(results_df, new_day_model)
    return(results_df)
}

empty_date <- integer()
class(empty_date) <- "Date"

results <- tibble(Date = empty_date
                  , effective_R0 = numeric()
                  , trust_population = integer()
                  , UHB_share = numeric()
                  , total_beds = numeric()
                  , occupied_beds_percentage = numeric()
                  , occupied_ICU_percentage = numeric()
                  , virus_characteristics = list()
                  , newly_infected = numeric()
                  , total_infected = numeric()
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
                  
                  
                  , newly_hospitalised = list() # tibble with values for each age group
                  , hospitalised_released = numeric()
                  , total_hospitalised = numeric()
                  , beds_overloaded = logical()
                  , newly_require_ICU = numeric()
                  , ICU_released = numeric()
                  , total_require_ICU = numeric()
                  , ICU_overloaded = logical()
)