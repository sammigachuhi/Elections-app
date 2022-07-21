# this is an app the shows name of presiding officer, ward they are presiding over
# results of different electoral presidential candidates (five). 
# the app will show the total no. of voters for each ward, and results for each of the five 
# presidential candidates
# the web map should show the total no. of voters in each constituency.


library(shiny)
library(htmltools)
library(dplyr)
library(tools)
library(leaflet)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(DT)

wards <- read.csv('wards.csv', header = T)
attach(wards)
wards_shapefile <- readOGR(dsn='wards_shapefile.shp')

# create ui that show the name of presiding officer
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = 'presiding_officer',
        label = 'Name of presiding officer',
        placeholder = 'ex. Tom Omondi'
      ),
      
      # option to select the ward
      selectInput(
        inputId = 'selected_wards',
        label = 'Choose the ward you are presiding over',
        choices = c(wards$ward)
      ),
      
      # show the total number of voters who came to vote
      numericInput(
        inputId = 'total_voters',
        label = 'Total voters who came to vote',
        value = 0,
        min = 0,
        max = NA
      ),
      
      # show the results of the five different presidential candidates
      toTitleCase('David Waihiga Mwaure'),
      numericInput(
        inputId = 'votes_david',
        label = 'Votes for David Waihiga Mwaure',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('George Wajackoyah'),
      numericInput(
        inputId = 'votes_george',
        label = 'Votes for George Wajackoyah',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('Raila Odinga'),
      numericInput(
        inputId = 'votes_raila',
        label = 'Votes for Raila Odinga',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('Reuben Kigame'),
      numericInput(
        inputId = 'votes_reuben',
        label = 'Votes for Reuben Kigame',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('William Ruto'),
      numericInput(
        inputId = 'votes_william',
        label = 'Votes for William Ruto',
        value = 0,
        min = 0,
        max = NA
      ),
      toTitleCase('Presidential candidates'),
      checkboxGroupInput(
        inputId = 'selected_candidate',
        label = 'Choose the presidential candidate',
        choices = c('David Waihiga Mwaure',
                    'George Wajackoyah',
                    'Raila Odinga',
                    'Reuben Kigame',
                    'William Ruto')),
      actionButton(
        inputId = 'submit',
        label = 'Submit results'
      ),
      
      # text to display
      textOutput(outputId = 'presiding_officer_name'),
    ),
    # mainpanel
    mainPanel(
      leafletOutput(outputId = 'wards_map', width = '100%', height = 400), # webmap showing no. of voters who came, % of votes for each of the five presidential candidates (clickable)
      DTOutput(outputId = 'table_react') # is reactive to the total votes cast (mandatory) and is reactive to the names of the presidential candidates to show the votes cast to each
    )
  )
)

# define the server function 

server <- function(input, output, session){
  
  table_results <- eventReactive(input$submit, {
    req(input$selected_wards)
    wards %>%
      select(c(ward, county, subcounty, voters)) %>%
      filter(ward %in% input$selected_wards) 
  })
  
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = wards_shapefile, stroke = T, fillColor = 'red', 
                fillOpacity = 0.2, popup = wards_shapefile$ward)
  
  output$wards_map <- renderLeaflet(map)
  
  # create a reactive table showing total no. of voters for each ward, and results for each of the five 
  # presidential candidates
  output$table_react <- renderDT(table_results())
  
  output$presiding_officer_name <- renderText({paste0('Presiding officer: ', input$presiding_officer)})
}



# create the shiny app object
shinyApp(ui, server)














