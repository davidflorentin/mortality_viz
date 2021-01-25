
#install.packages("shiny")
#install.packages("plotly")
#install.packages("dplyr")
#install.packages("tidyr")

library(tidyr)
library(shiny)
library(plotly)
library(dplyr)

#load source data for plots
popDeath <- read.csv("popDeath.csv")
mortCause <- read.csv("mortCause.csv")

#user interface
ui <- fluidPage(includeCSS("viz.css"),

  tags$header(h1("How people die: England & Wales"),
  #tags$p("In England & Wales"),
  fluidRow(column(3,
  #set up cohort select input
  selectInput(inputId = "cohort", label = "Birth period (age in 2019)",
              c("1935-1939 (80-84)" = 1939,
                "1940-1944 (75-79)" = 1944,
                "1945-1949 (70-74)" = 1949,
                "1950-1954 (65-69)" = 1954,
                "1955-1959 (60-64)" = 1959,
                "1960-1964 (55-59)" = 1964,
                "1965-1969 (50-54)" = 1969,
                "1970-1974 (45-49)" = 1974,
                "1975-1979 (40-44)" = 1989,
                "1980-1984 (35-39)" = 1984,
                "1985-1989 (30-34)" = 1989,
                "1990-1994 (25-29)" = 1994,
                "1995-1999 (20-24)" = 1999,
                "2000-2004 (15-19)" = 2004,
                "2005-2009 (10-14)" = 2009,
                "2010-2014 (5-9)" = 2014),
              selected = 1969)),
  column(6,
  #set up radio buttons for sex selection
  radioButtons(inputId = "sex", label = "Sex", inline = TRUE, choices = c("Male" = "1", "Female" = "2"))))),
  tags$div(class = "spacer"), #prevents elements loading behind fixed header
  
  tags$div(
    tags$p("This dashboard takes the number of births in a 5-year period and progressively subtracts registered deaths from that cohort as they age.
           Births are based on ONS population data for the ", tags$a(href = "https://webarchive.nationalarchives.gov.uk/20160111174808/http://www.ons.gov.uk/ons/publications/re-reference-tables.html?edition=tcm%3A77-215593",
           "20th Century"), " and ", tags$a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/the21stcenturymortalityfilespopulationdataset",
           "21st Century. "),"Deaths are based on the ONS mortality files for the ", tags$a(href = "https://webarchive.nationalarchives.gov.uk/20160108034247tf_/http://www.ons.gov.uk/ons/rel/subnational-health1/the-20th-century-mortality-files/20th-century-deaths/index.html",
           "20th Century"), " and ", tags$a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset",
           "21st Century.")),
    tags$hr()),
    
  #plot graphs
  tags$div(
  plotlyOutput(outputId = "timeseries", height = 350),
  tags$p("The bar graph shows the top 4 causes of death (and 'Other') for each age group. Click on a bar to see the full breakdown below."),
  plotlyOutput(outputId = "stackedBar", height = 350)),
  
  tags$div(
  plotlyOutput(outputId = "causeDetail")),
  
  #footer information
  tags$hr(),
  tags$div(
  tags$p("Cause of death is based on ICD version in use at time of death"),
  tags$p("Data sources:"),
  tags$a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset",
           "21st Century Mortality dataset, ONS"),
  tags$br(),
  tags$a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/the21stcenturymortalityfilespopulationdataset",
         "21st Century Population dataset, ONS"),
  tags$br(),
  tags$a(href = "https://webarchive.nationalarchives.gov.uk/20160108034247tf_/http://www.ons.gov.uk/ons/rel/subnational-health1/the-20th-century-mortality-files/20th-century-deaths/index.html",
         "20th Century Mortality Files, ONS"),
  tags$br(),
  tags$a(href = "https://webarchive.nationalarchives.gov.uk/20160111174808/http://www.ons.gov.uk/ons/publications/re-reference-tables.html?edition=tcm%3A77-215593",
         "20th Century Population Files, ONS")
  ))


server <- function(input, output) {
  #first graph of time series
  output$timeseries <- renderPlotly({
    subset(popDeath, U_yearBorn == input$cohort & sex == input$sex)-> selectedCohort
   
    plot_ly(width = 800, height = 300) %>% 
      layout(xaxis = list(title = "Last year of period", fixedrange = TRUE),
             yaxis = list(title = "% alive*", fixedrange=TRUE),
             title = paste("Death of a cohort:", if_else(input$sex == 1, "Males", "Females"),
                           "born", as.numeric(input$cohort)-4, "-", input$cohort)) %>% 
      add_trace(data = selectedCohort, x = ~U_year, y = ~PercentLiving, mode = "lines+markers",
                text = c(paste("5 year period ending:",selectedCohort$U_year, "\nPercent alive:", round(selectedCohort$PercentLiving,1))),
                hoverinfo = 'text') %>% config(displayModeBar = F)

  })
  

    output$stackedBar <- renderPlotly({
    
    subset(mortCause, U_yearBorn == input$cohort & sex == input$sex) %>% 
      group_by(U_year, sex) %>%
      arrange(U_year, sex, -pctOfDeaths) %>% 
      mutate(id = row_number()) %>% 
        #label top 4 causes for each U_year and group the rest as 'Other'
      mutate(cause = if_else(id > 4, "Other", as.character(cause))) %>% 
      ungroup() %>% 
      group_by(U_year, cause) %>% 
      summarise(pctOfDeaths = sum(pctOfDeaths)) %>% 
        #create text for mouse hover
      mutate(hovertext = paste(
        paste("Age:", paste(U_year-as.numeric(input$cohort), U_year-as.numeric(input$cohort)+4, sep = "-")),
        paste("Cause:", cause),
        paste("% deaths:", pctOfDeaths),
        sep ="\n"),
        yearLabel = paste(paste(as.numeric(U_year) - 4, U_year, sep="-"),
                          paste(U_year-as.numeric(input$cohort), U_year-as.numeric(input$cohort)+4, sep = "-"), sep="/")) -> mainCauses
    
      #plot stacked bar chart of main causes
    mainCauses %>% 
      plot_ly(source = "source", x = ~yearLabel, y=~pctOfDeaths, type ="bar", color=~cause, width = 800, height =320,
              text = c(mainCauses$hovertext), hoverinfo ='text') %>% 
      layout(barmode = "stack", xaxis = list(title = "Year/Age", fixedrange=TRUE), yaxis = list(title = "% of deaths", fixedrange=TRUE),
             title = paste("Cause of deaths", if_else(as.numeric(input$cohort)-4 > 1970, as.numeric(input$cohort)-4, 1970),"-2019:",
                           if_else(input$sex == 1, "Males", "Females"),
                           "born", as.numeric(input$cohort)-4, "to", input$cohort)) %>%
      config(displayModeBar = F)
  })
  

    output$causeDetail <- renderPlotly({
      
      #get event_data so it updates when the stack bar chart is hovered over
      eventdata <- event_data("plotly_click", source = "source")
      
      validate(need(!is.null(eventdata) & eventdata$x[1] > 1965, "Select a column on the bar chart to show detailed causes of death"))
      
      selectedYear <- if_else(is.null(substr(eventdata$x[1],6,9)), "2019", substr(eventdata$x[1],6,9))
      
      #filter mortality cause data by selected cohort and sex information
      mortCause %>% filter(U_yearBorn == input$cohort & sex == input$sex & U_year == selectedYear) -> causeDeath
      
      #plot as horizontal bar chart
      plot_ly(y = causeDeath$cause, x = causeDeath$pctOfDeaths, type = "bar", orientation = 'h', width = 600, height = 400) %>%
        layout(xaxis = list(title = "% of deaths"),
               title = paste("Cause of deaths", as.numeric(selectedYear)-4, "-", selectedYear, if_else(input$sex == 1, ": Males aged", ": Females aged"),
                             as.numeric(selectedYear) - as.numeric(input$cohort),"-", as.numeric(selectedYear) - as.numeric(input$cohort) + 4)) %>% 
        config(displayModeBar = F)
      
    })
    
}

shinyApp(ui = ui, server = server)
