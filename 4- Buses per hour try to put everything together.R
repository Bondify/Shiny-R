# install.packages('DT')

library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(pryr)
library(ggplot2)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                fileInput("zip", "Upload GTFS",
                          multiple = FALSE,
                          accept = c(".zip"),
                          placeholder = "Choose a GTFS"),
                #Test output
                DT::dataTableOutput(outputId = "moviestable"),
                
                # Line filter
                uiOutput("routes_selected_dt")
                )
                
  #               sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
  #                           value = range(quakes$mag), step = 0.1
  #               ),
  #               selectInput("colors", "Color Scheme",
  #                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
  #               ),
  #               checkboxInput("legend", "Show legend", TRUE)
                  # Input: Upload GTFS ----
                  
                 
  
)

# Make the size of accepted files bigger
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
    read_csv(routes_path)
  }) 
  
  all_routes <- reactive({
    req(routes())
    sort(unique(routes()$route_long_name))
  }) 
  
  trips <- reactive({
    req(files())
    trips_path <- paste(exdir(), 'trips.txt', sep = '/')
    read_csv(trips_path)
  }) 
  
  stops <- reactive({
    req(files())
    stops_path <- paste(exdir(), 'stops.txt', sep = '/')
    read_csv(stops_path)
  }) 
  
  shapes <- reactive({
    req(files())
    shapes_path <- paste(exdir(), 'shapes.txt', sep = '/')
    read_csv(shapes_path)
  }) 
  
  stop_times <- reactive({
    req(files())
    stop_times_path <- paste(exdir(), 'stop_times.txt', sep = '/')
    read_csv(stop_times_path, col_types= cols(arrival_time = col_character(), departure_time = col_character())) %>% 
      left_join(trips()) %>% 
      select(trip_id, arrival_time, departure_time, stop_id, stop_sequence, service_id) %>% 
      mutate(arrival_time = ifelse((as.integer(substr(arrival_time, 1, 2)) + as.integer(substr(arrival_time, 4, 5))/60) < 24,
                                   as.integer(substr(arrival_time, 1, 2)) + as.integer(substr(arrival_time, 4, 5))/60,
                                   (as.integer(substr(arrival_time, 1, 2)) + as.integer(substr(arrival_time, 4, 5))/60) - 24),
             departure_time = ifelse((as.integer(substr(departure_time, 1, 2)) + as.integer(substr(departure_time, 4, 5))/60) < 24,
                                     as.integer(substr(departure_time, 1, 2)) + as.integer(substr(departure_time, 4, 5))/60,
                                     (as.integer(substr(departure_time, 1, 2)) + as.integer(substr(departure_time, 4, 5))/60) -24)) 
  })
  
  
  # -------------------------------------------------------------------
  # FOR THE BUS COUNT
  # -------------------------------------------------------------------
  # Time span for time windows
  span <- 60
  span_hours <- span/60
  
  # Define the time windows
  day_start <- 0
  day_end <- 24
  
  starting_time <- seq(day_start, day_end -span_hours, span_hours)
  ending_time <- seq(day_start + span_hours, day_end, span_hours)
  time_windows_1 <- data.frame(window_id = seq(1:length(starting_time)) ,starting_time, ending_time)
  time_windows <- time_windows_1 %>% 
    mutate(x_axis = paste(as.character(time_windows_1[ , 2]),'00', sep = ':' ) )
   
    
    #  mutate(x_axis = ifelse(
    #   time_windows[,"starting_time"] - floor(time_windows[,"starting_time"]) == 0, #impares,
    #   paste(as.character(floor(time_windows[ , 2])),'00', sep = ':' ),
    #   paste(as.character(floor(time_windows[ , 2])),as.character((time_windows[,"starting_time"] -
    #                                                                 floor(time_windows[,"starting_time"]))/span_hours*span), sep = ':' )
    # ))
  
  
  # Only use weekday service
  # stop_times_1 <- reactive({
  #   req(stop_times())
  #   stop_times() %>% 
  #     left_join(trips()) %>% 
  #     select(trip_id, arrival_time, departure_time, stop_id, stop_sequence, service_id) %>% 
  #     filter(service_id == 1)
  # })
  
  # Create a data frame to add the time window for each trip
  # stop_window_id <- reactive({
  #   req(stop_times_1())
  #   as.data.frame(seq(1:nrow(stop_times_1())))
  # })
  # 
  # stop_window_id_1 <- reactive({
  #   datos <- stop_window_id()
  #   colnames(datos) <- c("window_id")
  #   datos
  # })
  
  # stop_times_2 <- reactive({
  #   req(stop_window_id_1(), stop_times_1())
  #   cbind(stop_times_1(), stop_window_id_1())
  # })
  
  filtered <- reactive({
    req(stop_times())
    stop_times() %>% 
      filter(stop_id == input$map_marker_click$id &
               service_id == 1 ) %>% 
      mutate(window_id = floor(arrival_time) + 1) %>% 
      group_by(window_id) %>% 
      count(window_id)

  })

  
  # #For get the data for one line
  # filtered_line <- stop_window_id_1() %>% 
  #   left_join(routes) %>% 
  #   select(window_id, route_short_name) %>% 
  #   filter(route_short_name == '16') %>% 
  #   group_by(window_id, route_short_name) %>% 
  #   count(window_id)
    
  # -------------------------------------------------------------------
  # FOR DRAWING LINES AND STOPS
  # ------------------------------------------------------------------- 
  # Take only the route chosen in the filter
  route_chosen <- reactive({
    req(routes())
    routes() %>% 
      filter(route_long_name %in% input$selected_route)
  })
  
  # Take the color from it
  color <- reactive({
    req(route_chosen())
    paste('#', route_chosen()$route_color, sep = '')
  })

  # Get only one shape of all the possible ones for this route
  # then we should be able to plot all the different patterns
  # for each route
  shape <- reactive({
    req(trips())
    trips() %>%
      filter(route_id == route_chosen()$route_id)
  })

  shape_chosen <- reactive({
    req(shape())
    shape()[1,]
  })

  # Get all the points of the shape
  shape_points <- reactive({
    req(shape_chosen(), shapes())
    shapes() %>%
      filter(shape_id == shape_chosen()$shape_id)
  })

  #Get the stops for this trip
  trip_chosen <- reactive({
    req(stop_times(), shape_chosen(), shape_points())
    stop_times() %>%
      filter(trip_id == shape_chosen()$trip_id)
  })

  stops_chosen <- reactive({
    req(stops(), trip_chosen())
    stops() %>%
      filter(stop_id %in% trip_chosen()$stop_id)
  })
  
  #Calculate bounds of the map
  min_lon <- reactive({
    min(stops_chosen()$stop_lon)
  })
  min_lat <- reactive({
    min(stops_chosen()$stop_lat)
  }) 
  max_lon <- reactive({
    max(stops_chosen()$stop_lon)
  }) 
  max_lat <- reactive({
    max(stops_chosen()$stop_lat) 
  })
  
  #Create the labels
  labels <- reactive({
    req(stops_chosen())
    sprintf(
      stops_chosen()$stop_name
    ) %>% lapply(htmltools::HTML)
  })

  route_label_1 <- reactive({
    req(routes(), shape_chosen())
    routes() %>%
      filter(route_id == unique(shape_chosen()$route_id)) %>%
      select(route_short_name)
  })

  route_label <- reactive({
    req(route_label_1())
    sprintf(
      as.character(route_label_1())
     ) %>% lapply(htmltools::HTML)
  })

  #Render list of routes
   output$routes_selected_dt <- renderUI({
     selectInput(inputId = "selected_route",
                 label = "Filter lines:",
                 choices = all_routes() ,
                 selected = '98',
                 multiple = FALSE,
                 selectize = TRUE)
  })
  
 # Create data table
  # output$moviestable <- DT::renderDataTable({
  #   req(route_chosen())
  #   DT::datatable(data = stops_chosen(),
  #                 options = list(pageLength = 10),
  #                 rownames = FALSE)
  # })

  #Render the map 
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(stops_chosen()) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles(providers$Stamen.TonerLabels) #%>% 
      # addProviderTiles("Stamen.TonerLite") %>%
      # fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
  })
  #?leafletProxy
  # Incremental changes to the map (in this case, showing lines and stops
  # depending what the users chooses) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observeEvent(input$selected_route, {
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircleMarkers(stops_chosen()$stop_lon, stops_chosen()$stop_lat,
                       layerId = stops_chosen()$stop_id,
                       radius = 7, weight = 1,
                       #color = color(),
                       stroke = FALSE,
                       fillColor = color(),
                       fillOpacity = 0.7
                       # label = labels(),
                       # labelOptions = labelOptions(
                       #   style = list("font-weight" = "normal", padding = "3px 8px"),
                       #   textsize = "15px",
                       #   direction = "top"
                       # )
      ) %>%
      addPolylines(shape_points()$shape_pt_lon, shape_points()$shape_pt_lat,
                   color = color(),
                   opacity = 1
                   # highlightOptions = highlightOptions(
                   #   weight = 5,
                   #   color = color(),
                   #   opacity = 1,
                   #   #dashArray = "",
                   #   #fillOpacity = 0.7,
                   #   bringToFront = TRUE)#,
                   # label = route_label(),
                   # labelOptions = labelOptions(
                   #   style = list("font-weight" = "normal", padding = "3px 8px"),
                   #   textsize = "15px",
                   #   direction = "top")
      ) %>% 
      fitBounds(min_lon() - .001, min_lat() - .001, max_lon() + .001, max_lat() + .001)
      
  })
  
  observeEvent(input$map_marker_click, {
    output$moviestable <- DT::renderDataTable({
      req(filtered())
      DT::datatable(data = filtered(),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })


        # leafletProxy("map", session) %>%
    #   removeMarker(input$map_marker_click$id)
  })
  
}

shinyApp(ui, server)
