library(shiny)
library(tidyverse)
library(pryr)
library(DT)
library(tools)
library(shinythemes)

# Define UI for data upload app ----
ui <- shinyUI(fluidPage(theme = shinytheme("cosmo"),
  
  # App title ----
  titlePanel("Buses per hour"),
  br(),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3, 
      
      # Input: Upload GTFS ----
      fileInput("zip", "Upload GTFS",
                multiple = FALSE,
                accept = c(".zip")),
      
      tags$hr(),
      
      # Line filter
      uiOutput("routes_selected_dt"),
      
      tags$hr(),
      
      # Select which types of day types to see
      radioButtons(inputId = "selected_service",
                         label = "Select day type(s):",
                         choices = c("Weekday" = 1, 
                                     "Saturday" = 2, 
                                     "Sunday" = 3),
                         selected = 1),
      
      br(),
      
      downloadButton("downloadData", "Download"),
      
      tags$hr(),
      #Link to Remix
      tags$a("Go to Remix", href = "https://www.remix.com/")
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      #Create different tabs
      tabPanel(title = "Yearly values", 
                DT:: dataTableOutput(outputId = "stops")
               )
               
      )
  )
))

# Define server logic to read selected file ----
options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output, session) {
  
  #get the GTFS path to use it later
  gtfs <- reactive({
    req(input$zip)
    input$zip$datapath
  })
  
  #Create a temp directory
  exdir <- reactive({
    req(gtfs())
    substring(gtfs(), 1, nchar(gtfs())-4)
  })
  
  #Get the list of files from the GTFS
  files <- reactive({
    req(exdir())
    unzip(gtfs(), list = FALSE, exdir = exdir())
  })
  
  #Define all the data frames I need
  #Create the path to find the files
  routes <- reactive({
    req(files())
    routes_path <- paste(exdir(), 'routes.txt', sep = '/')
    read_csv(routes_path) %>% 
      mutate(route_numeric_id = seq(1, nrow(read_csv(routes_path))) )
  }) 
  
  all_routes <- reactive({
    req(routes())
    sort(unique(routes()$route_long_name))
  }) 
  
  trips <- reactive({
    req(files(), routes())
    trips_path <- paste(exdir(), 'trips.txt', sep = '/')
    read_csv(trips_path) %>% 
      left_join(routes()) %>% 
      select(-route_id, -route_short_name, -route_long_name, -route_type, -route_color, -route_text_color, -block_id, -agency_id)
  }) 
  
  stops <- reactive({
    req(files())
    stops_path <- paste(exdir(), 'stops.txt', sep = '/')
    read_csv(stops_path, col_types= cols(stop_id = col_character())) %>% 
      mutate(stop_numeric_id = seq(1, nrow(read_csv(stops_path))) )
  }) 
  
  #Now that I have all the files I read and built stop_times
  stop_times <- reactive({
    req(files())
    stop_times_path <- paste(exdir(), 'stop_times.txt', sep = '/')
    read_csv(stop_times_path, col_types= cols(arrival_time = col_character(), departure_time = col_character(), stop_id = col_character())) %>% 
      left_join(trips()) %>%
      left_join(stops()) %>% 
      left_join(routes()) %>% 
      select(-stop_name, -stop_lat, -stop_lon, -stop_id, -route_id, -agency_id, -route_short_name, -route_type,
             -route_color, -route_text_color) %>% 
      mutate(arrival_time = ifelse((as.integer(substr(arrival_time, 1, 2)) + as.integer(substr(arrival_time, 4, 5))/60) < 24,
                                   as.integer(substr(arrival_time, 1, 2)) + as.integer(substr(arrival_time, 4, 5))/60,
                                   (as.integer(substr(arrival_time, 1, 2)) + as.integer(substr(arrival_time, 4, 5))/60) - 24),
             departure_time = ifelse((as.integer(substr(departure_time, 1, 2)) + as.integer(substr(departure_time, 4, 5))/60) < 24,
                                     as.integer(substr(departure_time, 1, 2)) + as.integer(substr(departure_time, 4, 5))/60,
                                     (as.integer(substr(departure_time, 1, 2)) + as.integer(substr(departure_time, 4, 5))/60) -24)) 
  }) 
  
  #Take the span defined into account to create the time windows
  span_hours <- 1
  
  #Create the time window for all day
  day_start <- 0
  day_end <- 24
  
  starting_time <-seq(day_start, day_end -span_hours, span_hours)
  ending_time <- seq(day_start + span_hours, day_end, span_hours)
  time_windows_1 <- data.frame(window_id = seq(1:length(starting_time)) ,starting_time, ending_time)
  time_windows <- mutate(time_windows_1, time = paste(as.character(time_windows_1[ , 2]),'00', sep = ':' ) )
  
  # We choose the day type to use
  filtered <- reactive({
    req(stop_times())
    if(!is.null(input$selected_route)){
      stop_times() %>% 
        filter(service_id %in% input$selected_service) %>% 
        filter(route_long_name %in% input$selected_route) %>% 
        mutate(window_id = floor(arrival_time) + 1) %>% 
        group_by_at(vars(stop_numeric_id, window_id)) %>% 
        count(window_id)
    }
    else {
      stop_times() %>% 
        filter(service_id %in% input$selected_service) %>% 
        mutate(window_id = floor(arrival_time) + 1) %>% 
        group_by_at(vars(stop_numeric_id, window_id)) %>% 
        count(window_id) 
    }
  })
   
  output_table <- reactive({
    req(filtered(), time_windows)
    if(!is.null(input$selected_route)){
      filtered() %>% 
        left_join(stops()) %>% 
        left_join(time_windows) %>% 
        group_by_at(vars(stop_id, time)) %>% 
        select(stop_id, stop_name, time, n)
    } 
    else {
      filtered() %>% 
        left_join(stops()) %>% 
        left_join(time_windows) %>% 
        group_by_at(vars(stop_id, time)) %>% 
        select(stop_id, stop_name, time, n)
    }
  })
  
  # Pivot times into columns to make it easier to understand
  output_table_1 <- reactive({
    req(output_table())
    reshape(as.data.frame(output_table()),
            v.names = "n", 
            idvar = "stop_id",
            timevar = "time", 
            direction = "wide")
  })
  
  hours_ok <- reactive({
    req(output_table_1())
    substr(names(output_table_1())[3:length(names(output_table_1()))], 3, nchar(output_table_1()[,]) )
  })
    
  output_table_2 <- reactive({
    req(output_table_1())
    testdata <- output_table_1()
    colnames(testdata) <- c("stop_id", "stop_name", hours_ok())
    testdata
  })
  
  # names(output_table) <- c("stop_id", "stop_name", hours_ok)
  # 
  # output_table[is.na(output_table)] <- 0
  
  #Render list of routes
  output$routes_selected_dt <- renderUI({
    selectInput(inputId = "selected_route",
                label = "Filter lines:",
                choices = all_routes() ,
                selected = '',
                multiple = TRUE,
                selectize = TRUE)
  })
  
  # Yearly Information output
  output$stops <- renderDataTable({
    if(is.null(gtfs()))
      return ()
    datatable(data = output_table_2(),
              options = list(pageLength = 10, lengthMenu = c(10, 20, 40)),
              rownames = FALSE)
  })
  
 # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(paste("Buses per hour", date(),sep = "-"), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(output_table_2(), file, na = '0')
    }
  )

 }

# Create Shiny app ----
shinyApp(ui, server)