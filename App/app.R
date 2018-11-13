#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(haven)
library(fs)
library(readxl)
library(lubridate)
library(kableExtra)
library(janitor)
library(leaflet)


latlong <- read_rds("latlong.rds")


ui <- fluidPage(
  
  titlePanel("Polling Data from NYT's Upshot"), 
  h4("Forecasted Advantage vs. Actual Advantage of Republican candidates"), 
  leafletOutput("mymap"),
  h5("Weighted polling data from the New York Times' Upshot was used to calculate the projected advantage of Republican candidates over their Democratic opponents.
     Actual advantage is defined as the vote margin garnered as of November 9th. Note that this final polling data is not pulled dynamically, meaning that 
     elections that are currently considering run-offs or recounts may not be correctly labelled. Information on winning candidate is also provided for each of the 60
     congressional districts, as well as 5 Senate races and 1 Governor's race."), 
  uiOutput("tab")
  
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pal <- colorFactor(palette = c("red", "blue", "#A0A0A0"), 
                     levels = c("Republican", "Democrat", "Undecided"))
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB") %>% 
      addCircleMarkers(radius = ifelse(str_detect(latlong$district, pattern = ("[sengov]")), 6, 4), 
                       lat = latlong$latitude, 
                       lng = -latlong$longitude, 
                       label = latlong$district,
                       color = pal(latlong$win_party),
                       popup = paste(latlong$district, "<br>", "<br>", 
                                     "Republican Advantage:", latlong$repAdv, "<br>",
                                     "Actual Advantage:", latlong$actualAdv, "<br>",
                                     "Winner:", latlong$win_name)) %>% 
      addLegend(pal = pal, 
                values = c("Republican", "Democrat", "Undecided"),
                # opacity of .5, title of Sector, and position of topright
                opacity = 0.5, title = "Winning Party", position = "bottomright")
  })
  
  url <- a("sonyakalara", href="https://github.com/sonyakalara/181113_NYTupshot")
  output$tab <- renderUI(tagList("Link to github repository:", url))
}


# Run the application 
shinyApp(ui = ui, server = server)
