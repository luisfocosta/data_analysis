library(shiny)

ui <- fluidPage(
   titlePanel("Analysis of property and crop damage in the USA caused by weather events"),
   sidebarPanel(
      p("Select the parameters (year range and weather event type) to plot the total damage caused weather events"),
      p("the graphic will be regenerated using the parameters you provided."),
      p("(wait a few seconds to acommodate any processing delay)"),
      p("Good luck!"),
      sliderInput(inputId="YearRange",label="Year range",min=1950,max=2011,value=c(2000,2005)),
      checkboxGroupInput(inputId="Event",label="Weather event Type",choices=event_types)
      ),
   mainPanel(plotOutput("distPlot"))
)