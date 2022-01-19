library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
options(rsconnect.max.bundle.size=400*1024^2)
# source data and analysis
source("hesitancy maps2.R")
sf::sf_use_s2(FALSE)

ui <- dashboardPage( skin = "purple",
                   
          dashboardHeader(title = "Geospatial associations between COVID-19 vaccine uptake, hesitancy, demographics, social vulnerability, and case/death rates", titleWidth = '100%'
                          ),
          
          dashboardSidebar(
            
         
            width = 320,
            
            sidebarMenu(
              tags$style(type = "text/css", 
                         ".irs-grid-text:nth-child(-2n+18) {color: white}",
                         ".irs-grid-text:nth-child(2n+20) {color: white}",
                         ".irs-grid-pol:nth-of-type(-n+18) {background:white}",
                         ".irs-grid-pol:nth-of-type(n+18) {background:white}"),

              dateInput(
                inputId="datez2",
                label = "Select a Date to Subset Data",
                min = "2020-12-13",
                max = Sys.Date(),
                value = Sys.Date()-1),
              
              br(),
              uiOutput("datehelp"),
              br(),
              
              selectInput(inputId = "xvar",
                          label = "Select a variable of interest",
                          choices = c("Percent Fully Vaccinated" = "series_complete_pop_pct",
                                      "Percent 12+ Fully Vaccinated" =  "series_complete_12pluspop_pct",
                                      "Percent 65+ Fully Vaccinated" = "series_complete_65pluspop_pct",
                                      "Percent Vaccinated with Booster" = "booster_doses_vax_pct",
                                      
                                      "Percent 18+ Vaccinated with Booster" = "booster_doses_18plus_vax_pct",
                                      "Percent 50+ Vaccinated with Booster" = "booster_doses_50plus)vax_pct",
                                      "Percent 65+ Vaccinated with Booster" = "booster_doses_65plus_vax_pct",
                                      "Case Rate Per 10,000 Population" = "cases",
                                      "Death Rate Per 10,000 Population" = "deaths",
                                      "Percent of Cases who Died" = "deaths_per_case",
                                      "CDC Percent Estimated Hesitant" = "estimated_hesitant",
                                      "CDC Social Vulnerability Index" = "social_vulnerability_index",
                                      "Percent Below the Poverty Line" = "poverty_total",
                                      "Percent Males Below the Poverty Line" = "poverty_male",
                                      "Percent Female Below the Poverty Line" = "poverty_female",
                                      "Percent Male" = "pct_male",
                                      "Percent Female" = "pct_female",
                                      "Percent White/Caucasian Race Alone" = "white",
                                      "Percent Black/African American Race Alone" = "black",
                                      "Percent American Indian or Alaskan Native Alone" = "ai_an",
                                      "Percent Asian Alone" = "asian",
                                      "Percent Native Hawaiian or Other Pacific Islander Alone" = "nh_opi",
                                      "Percent Other/Multiple Race" = "other"),
                          selected = "estimated_hesitant"),
              selectInput(inputId = "yvar",
                          label = "Select another variable",
                          choices = c("Percent Fully Vaccinated" = "series_complete_pop_pct",
                                      "Percent 12+ Fully Vaccinated" =  "series_complete_12pluspop_pct",
                                      "Percent 65+ Fully Vaccinated" = "series_complete_65pluspop_pct",
                                      "Percent Vaccinated with Booster" = "booster_doses_vax_pct",
                                      
                                      "Percent 18+ Vaccinated with Booster" = "booster_doses_18plus_vax_pct",
                                      "Percent 50+ Vaccinated with Booster" = "booster_doses_50plus)vax_pct",
                                      "Percent 65+ Vaccinated with Booster" = "booster_doses_65plus_vax_pct",
                                      "Case Rate Per 10,000 Population" = "cases",
                                      "Death Rate Per 10,000 Population" = "deaths",
                                      "Percent of Cases who Died" = "deaths_per_case",
                                      "CDC Percent Estimated Hesitant" = "estimated_hesitant",
                                      "CDC Social Vulnerability Index" = "social_vulnerability_index",
                                      "Percent Below the Poverty Line" = "poverty_total",
                                      "Percent Males Below the Poverty Line" = "poverty_male",
                                      "Percent Female Below the Poverty Line" = "poverty_female",
                                      "Percent Male" = "pct_male",
                                      "Percent Female" = "pct_female",
                                      "Percent White/Caucasian Race Alone" = "white",
                                      "Percent Black/African American Race Alone" = "black",
                                      "Percent American Indian or Alaskan Native Alone" = "ai_an",
                                      "Percent Asian Alone" = "asian",
                                      "Percent Native Hawaiian or Other Pacific Islander Alone" = "nh_opi",
                                      "Percent Other/Multiple Race" = "other"),
                          selected = "series_complete_12pluspop"),
             
              br(),
              
              sliderInput(inputId = "comp_slider",
                          label = "Select Required 'Data Completeness'",
                          min = 0,
                          max = 100,
                          value = 90,
                          step = 1,
                          round = T,
                          animate = F),
              br(),
              #helpText("Check figure legend for date of data pull. If date is different than what you selected, see the 'how to use this application tab'. This is due to data not being avaialble yet for the date selected."),
              
              br(),
              menuItem("View the map for the selected date", tabName = "plot"),
              menuItem("How to use this application", tabName = "how"),
              menuItem("About these analyses", tabName = "about"),
              menuItem("Where can I access the raw data?", tabName = "data"),
          br(),
          
          HTML(paste(strong("Created by:"), 
                     tags$ul(
                       tags$li("Timothy Wiemken, PhD"),
                       tags$li("Farid Khan, MPH"),
                       tags$li("John McLaughlin, PhD"),
                       tags$li("Jacob Clarke MD"),
                       tags$li("Christopher Prener, PhD" )
                       ))),
          
          br(),
          p(a("Email Tim", target="_blank", href="mailto:timothy.wiemken@pfizer.com")), 
          br()
          )
          ),
              
          dashboardBody(
            
            ### changing theme
            # shinyDashboardThemes(
            #   theme = "grey_light"),
            
            tabItems(
              tabItem(tabName = "plot",
                      tags$h2(""),
                      addSpinner(leafletOutput("mapz", height = "90vh"), spin = "folding-cube", color="black"),
                      p()
                      ),
              
              tabItem(tabName = "how",
                      tags$h2("How to use this application"),
                      
                      "The default date is yesterday's date, as COVID-19 
                      vaccine data are only available for the previous day.
                      Further, data for the prior day is not updated until late morning/
                      early afternoon. Due to this, if no data are available, the application
                      will automatically select data from two days prior. The date of data pull
                      used for the map is displayed in the figure legend.",
                      p(),
                      "If you would like to look at the changes in these patterns
                      over time, select a different date. The data downloads and analysis 
                      will take several seconds, so please be patient while the new map 
                      loads.",
                      p(),
                      "Please be aware that some states and/or counties do not report 
                      COVID-19 vaccine data at all geographies so they will be noted 
                      as not having data available. The minimum and default required 
                      data completeness is 1%",
                      p()
                      ),
              
              tabItem(tabName = "about",
                      tags$h2("About these analyses"),
                      
                      "The data analysis includes computing bivariable Moran's I statistics, using 1000 simulations to identify global and local spatial autocorrelations.
                      Next, the Queen's adjacency matrix approach is used for weighting and Local Areas of Spatial Interaction (LISA) patterns are identified for mapping.",
                      p(),
                      "To interpret the maps, you can use the legend to identify the patterns of autocorrelation between the two variables you selected.",
                      p(),
                      "Using these maps in practice is possible though should be done with some caution given the lack of adjustment for various factors such as socioeconomic deprivation and 
                      only utilization of estimated hesitancy per CDC definitions. These analyses will be added at a later date to facilitate more generalizable use in practice. 
                      Despite these limitations, for hesitancy and uptake correlations, areas identified as High Hesitancy and High Uptake should be evaluated as 'positive deviants' - identifying the public perceptions 
                      behind hesitancy and how they overcame this hesitancy to ensure high vaccine uptake. Counties with high hesitancy and low uptake should also be a focus of 
                      intervention as they represent high risk areas for new outbreaks. Additionally, counties with low hesitancy and low uptake represent areas that should be intervened 
                      upon for increasing vaccine uptake given their estimated low hesitancy."),
                      
              tabItem(tabName = "data",
                      tags$h2("Where can I access the raw data?"),
                      
                      tags$a(href="https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw", 
                             "Data on estimated vaccine hesitancy are obtained on-demand from the Centers for Disease Control and Prevention (CDC)."),
                      p(),
                      tags$a(href="https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh", 
                             "Data on COVID vaccine uptake (fully vaccinated) are also obtained on-demand from the Centers for Disease Control and Prevention (CDC)."),
                      p(),
                      tags$a(href="https://github.com/nytimes/covid-19-data", 
                             "Data on daily case and death counts are obtained on-demand from the New York Times."),
                      p(),
                      tags$a(href="https://data.census.gov/cedsci/table?t=Counts,%20Estimates,%20and%20Projections&g=0100000US.050000&y=2019&d=ACS%201-Year%20Estimates%20Data%20Profiles&tid=ACSDP1Y2019.DP05&hidePreview=true&moe=false&tp=true", 
                             "Data on county population denominator data for rate calculations are obatained from a local file extracted from the 2019 1-year American Community Survey population estimates."),
                            
                      )
              )
                      

)
)


server <- function(input, output, session){
  
  output$datehelp <- renderUI({
    tags$div(
      id = "cite",
      'Check figure legend for date of data pull. If date is different than what you selected, data for selected date are not yet available and the previous day\'s data are used.',
      style = "color: white;
            width: 100%;
            white-space: pre-line"
    )
  })
      output$mapz <- renderLeaflet({yo(datez = as.character(input$datez2), xvar = input$xvar, yvar = input$yvar, complete.sub = input$comp_slider)
      })
      

      
        }
  
  shinyApp(ui, server)
  
  
