library(shiny)

## Processing
storm_data <- read.csv("storm_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings="NA")
years_from <- min(storm_data$YEAR)
years_to <- max(storm_data$YEAR)
event_types <- unique(sort(storm_data$EVTYPE))

shinyServer <- function(input, output) {

   dat <- reactive({
      date_range_from <- input$YearRange[[1]]
      date_range_to <- input$YearRange[[2]]
      #print(date_range_from)
      #print(date_range_to)
      #print(input$Event)
      plot_data <- storm_data[storm_data$EVTYPE %in% input$Event &
                     (storm_data$YEAR>=date_range_from & storm_data$YEAR<=date_range_to),]
      #print(plot_data)
      plot_data
   })
      output$distPlot <- renderPlot ({
         ggplot(data=dat(), aes(x=YEAR, y=DAMAGE/1000000,fill=EVTYPE, group=EVTYPE)) +
         geom_bar(stat="identity") + xlab ("Year") + ylab ("$ Damage (million usd)") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
         ggtitle("Damage / Event Type") + 
         scale_fill_discrete(name="Weather\nEvent\nType")
      })
   
   
   #ggplot(data=plot_data, aes(x=YEAR, y=HEALTHIMPACT/1000,fill=EVTYPE, group=EVTYPE)) +
   #geom_bar(stat="identity") + xlab ("Event Type") + ylab ("# Health Impact (Injuries+Fatalities x 1000)") +
   #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   #ggtitle("Public Health impact / Event Type")
}