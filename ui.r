library(leaflet)

# Choices for drop-downs

navbarPage("COVID-19", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),
        #plotOutput('chosenmap'),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 400, height = "auto",

        h2("COVID-19"),


        tags$hr(),
        numericInput("zipvar", "Zip Code", value=10004),
        selectInput("displayvar", "Variable", c("Cases"="cases", "Deaths"="deaths", "New Cases"="new_cases", "New Deaths"="new_deaths", "New Cases 7 Day Average"="new_cases_7d_avg", "New Deaths 7 Day Average"="new_deaths_7d_avg", "Case History"="cases_norm", "Cases per 100,000"="cases_pop", "Death History"="deaths_norm", "Deaths per 100,000"="deaths_pop"), selected="cases_norm"),
        uiOutput('datevarui'),
        tags$hr(),
        plotOutput("cases_timeseries", height = 200),
        plotOutput("deaths_timeseries", height = 250)
        )
      ),

      tags$div(id="cite",
        'Data compiled by the New York Times.'
      )
    ),

  tabPanel("Data explorer",
    DT::dataTableOutput("covidtable")
  )


)
