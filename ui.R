# ui.R

library(shiny)
library(leaflet)

# Define UI for dataset viewer application
shinyUI(bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$div(style="background-color: #FFFF00; align='center'", 
             tags$h3(tags$header("Florida Restaurant Health Inspection Grades"))),
    leafletOutput("mymap", width = "80%", height = "80%"),
    absolutePanel(top = 70, right = 4, draggable=TRUE, width = "250px",
                style = "background-color: #ffffff;
                  border-style: solid; border-color: gray", 
                tags$h3("Health Grades"),
                tags$p("Web application uses data from state government agencies
                         to determine a health rating score based on restaurant
                         health inspection reports"),
                tags$p("Database can be filtered based on search controls below
                        for restaurant business name and available zip codes or cities in 
                        Florida currently. Clicking on the circle marker provides 
                       the algorhitm-based Health Grade. Also, the customer review
                       link will open up the Yelp entry in another broswer window."),
                tags$hr(),
                  selectInput("plotlevel", "Search by business name, zip code, or city.",
                              choices=c("Business Name", "Zip Code", "City"),
                              selected="Business Name"),
                conditionalPanel(
                    condition = "input.plotlevel=='Business Name'", uiOutput("uiRest")),
                  conditionalPanel(
                      condition = "input.plotlevel=='Zip Code'", uiOutput("uiZip")),
                  conditionalPanel(
                      condition = "input.plotlevel=='City'", uiOutput("uiCity"))
    ),
    tags$div(style="background-color: #FFFF00; align='center'", 
              tags$h5("Use zoom (+/-) to control view."),
              tags$p("Showing: ", textOutput("Text")))
))
    
