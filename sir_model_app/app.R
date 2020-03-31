library(shiny)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 hospitalisation rate projection"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput('simulation_Range',
                           label = 'Date of simulation'
                           , start = as.Date("2020-03-01")
                           , end = as.Date("2020-12-31")
                        )
            , numericInput("recovery_period", "Recovery Time:", 14, min = 1, max = 100)
            , numericInput("time_to_double", "Time for cases to double:", 6, min = 1, max = 30)
            , numericInput("incubation_period", "Incubation Period:", 6, min = 1, max = 15)
            , numericInput("contact_modifier", "Contact Modifier:", 0.5, min = 0, max = 1, step = 0.05)
            , numericInput("starting_population", "Total Population:", 1000000, min = 10000, max = 250000000)
            , numericInput("hospitalisation_rate", "Hospitalisation Rate:", 0.2, min = 0, max = 1, step = 0.05)
            , numericInput("require_ICU_rate", "ICU Transfer Rate:", 0.1, min = 0, max = 1, step = 0.05)
            , numericInput("hospital_LOS", "Average Length of Stay:", 11, min = 1, max = 60)
            , numericInput("ICU_LOS", "Average Length of Stay (ICU):", 8, min = 1, max = 60)
            , dateInput('social_distancing_start_date'
                        , label = 'Date to Start Social Distancing'
                        , value = "2020-03-23"
                        )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("infectious_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(tidyverse)
    library(ggplot2)
    
    source("../SIR_model_functions.R")
    
    output$infectious_plot <- renderPlot({
        new_simulation_table(start_date = input$simulation_Range[[1]]
                             , end_date = input$simulation_Range[[2]]
                             , recovery_period = input$recovery_period
                             , time_to_double = input$time_to_double
                             , starting_population = input$starting_population
                             , starting_infected = 10
                             , hospitalisation_rate = input$hospitalisation_rate
                             , require_ICU_rate = input$require_ICU_rate
        ) %>%
            begin_social_distancing(social_distancing_start_date = input$social_distancing_start_date
                                    , contact_modifier = input$contact_modifier
            ) %>%
            model_infection(incubation_period = input$incubation_period
                            , hospital_LOS = input$hospital_LOS
                            , ICU_LOS = input$ICU_LOS
                            , hospitalisation_rate = input$hospitalisation_rate
                            , require_ICU_rate = input$require_ICU_rate
            ) %>%
            plot_simulation()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)