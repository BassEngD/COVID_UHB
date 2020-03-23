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
total_beds <- 1000
occupied_beds_percentage <- 0.6 # beds that are being used by non-covid patients

recovery_period_days <- 14
incubation_period_days <- 5
contagious_period_days <- 4

#? infections_per_day_per_person <- contagious_period_days / R0


