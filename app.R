
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

  tags$header(h1("How we die: England & Wales"),
  #tags$p("In England & Wales"),
  fluidRow(column(3,
  #set up cohort select input
  selectInput(inputId = "cohort", label = "Birth period (age in 2015)",
              c("1916-1920 (95-100)" =  1920,
                "1921-1925 (90-94)" = 1925,
                "1926-1930 (85-89)" = 1930,
                "1931-1935 (80-84)" = 1935,
                "1935-1940 (75-79)" = 1940,
                "1941-1945 (70-74)" = 1945,
                "1946-1950 (65-69)" = 1950,
                "1951-1955 (60-64)" = 1955,
                "1956-1960 (55-59)" = 1960,
                "1961-1965 (50-54)" = 1965,
                "1966-1970 (45-49)" = 1970,
                "1971-1975 (40-44)" = 1975,
                "1976-1980 (35-39)" = 1980,
                "1981-1985 (30-34)" = 1985,
                "1986-1990 (25-29)" = 1990,
                "1991-1995 (20-24)" = 1995,
                "1996-2000 (15-19)" = 2000,
                "2001-2005 (10-14)" = 2005,
                "2006-2010 (5-9)" = 2010),
              selected = 1970)),
  column(6,
  #set up radio buttons for sex selection
  radioButtons(inputId = "sex", label = "Sex", inline = TRUE, choices = c("Male" = "1", "Female" = "2"))))),
  tags$div(class = "spacer"), #prevents elements loading behind fixed header
  
  #plot graphs
  tags$div(
  plotlyOutput(outputId = "timeseries", height = 350),
  plotlyOutput(outputId = "stackedBar", height = 350)),
  
  tags$div(
  plotlyOutput(outputId = "causeDetail")),
  
  #footer information
  tags$hr(),
  tags$div(
  tags$p("*'% alive' is calculated based on births minus registered deaths and does not account for migration."),
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
                          paste(U_year-as.numeric(input$cohort), U_year-as.numeric(input$cohort)+4, sep = "-"), sep="\n")) -> mainCauses
    
      #plot stacked bar chart of main causes
    mainCauses %>% 
      plot_ly(source = "source", x = ~yearLabel, y=~pctOfDeaths, type ="bar", color=~cause, width = 800, height =320,
              text = c(mainCauses$hovertext), hoverinfo ='text') %>% 
      layout(barmode = "stack", xaxis = list(title = "Year/Age", fixedrange=TRUE), yaxis = list(title = "% of deaths", fixedrange=TRUE),
             title = paste("Cause of deaths", if_else(as.numeric(input$cohort)-4 > 1971, as.numeric(input$cohort)-4, 1971),"-2015:",
                           if_else(input$sex == 1, "Males", "Females"),
                           "born", as.numeric(input$cohort)-4, "to", input$cohort)) %>%
      config(displayModeBar = F)
  })
  

    output$causeDetail <- renderPlotly({
      
      #get event_data so it updates when the stack bar chart is hovered over
      eventdata <- event_data("plotly_click", source = "source")
      
      validate(need(!is.null(eventdata) & eventdata$x[1] > 1970, "Select a time period to show detailed causes of death"))
      
      selectedYear <- if_else(is.null(substr(eventdata$x[1],6,9)), "2015", substr(eventdata$x[1],6,9))
      
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
