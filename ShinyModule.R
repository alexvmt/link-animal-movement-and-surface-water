library("shiny")
library("move2")
library("sf")
library("shinyWidgets")
library("dplyr")
library("leaflet")
library("leaflet.extras")
library("RColorBrewer")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

# set limits and default for lag
limit_lower_lag <- 0
limit_upper_lag <- 12
default_lag <- 0

# set limits and default for lead
limit_lower_lead <- 0
limit_upper_lead <- 12
default_lead <- 0

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  # showcase to access a file ('auxiliary files') that is 
  # a) provided by the app-developer and 
  # b) can be overridden by the workflow user.
  fileName <- paste0(getAppFilePath("yourLocalFileSettingId"), "polygons.shp")

  # load polygons
  polygons <- read_sf(fileName)

  # get dates for date slider (dates must be of character type)
  dates <- sort(unique(polygons$date))
  
  tagList(
    titlePanel("Link Animal Movement and Surface Water"),
    fluidRow(
      column(1,
             selectInput(ns("individual"),
                         "Individual",
                         choices = c("all")),
             style = "z-index:1002;"),
      column(1,
             numericInput(ns("lag"),
                          "Lag",
                          default_lag,
                          min = limit_lower_lag,
                          max = limit_upper_lag)),
      column(1,
             numericInput(ns("lead"),
                          "Lead",
                          default_lead,
                          min = limit_lower_lead,
                          max = limit_upper_lead)),
      column(9,
             sliderTextInput(ns("date"),
                             "Date",
                             grid = TRUE,
                             force_edges = TRUE,
                             choices = dates,
                             selected = dates[1],
                             width = "100%"))
    ),
    fluidRow(
      column(12,
             leafletOutput(ns("map")))
    )
  )
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  current <- reactiveVal(data)
  
  # get values for individual dropdown
  observe({
    
    # wait until the data is loaded
    if (is.null(data)) return()
    unique_individuals <- sort(as.character(unique(mt_track_id(data))))
    keys <- c(unique_individuals, "all")
    values <- c(unique_individuals, "all")
    key_value_list <- setNames(values, keys)
    updateSelectInput(session,
                      "individual",
                      choices = key_value_list,
                      selected = c("all" = "all"))
    
  })
  
  # load polygons
  fileName <- paste0(getAppFilePath("yourLocalFileSettingId"), "polygons.shp")
  polygons <- read_sf(fileName)

  # filter polygons
  rctv_polygons_filtered <- reactive({

    # filter polygons according to input from date slider (dates must be of character type)
    polygons_filtered <- polygons %>%
      filter(date == input$date)

    polygons_filtered

  })
  
  # process data
  rctv_data_processed <- reactive({
    
    # ensure that data is in epsg 4326
    data_transformed <- st_transform(data, 4326)
    
    # extract relevant data from move2 object and create dataframe
    individuals <- mt_track_id(data_transformed)
    timestamps <- mt_time(data_transformed)
    long <- st_coordinates(data_transformed)[, 1]
    lat <- st_coordinates(data_transformed)[, 2]
    data_processed <- data.frame(individuals, timestamps, long, lat) %>%
      mutate(individuals = as.character(individuals))
    
    data_processed
    
  })
  
  # filter processed data
  rctv_data_processed_filtered <- reactive({
    
    # load reactive data
    data_processed <- rctv_data_processed()
    
    # filter data according to selected individual
    if(input$individual != "all") {
      
      data_processed_filtered <- data_processed %>% 
        filter(individuals == input$individual)
      
    } else {
      
      data_processed_filtered <- data_processed
      
    }
    
    # get min and max date (cast dates to date type because we subtract lag/add lead)
    min_date <- as.Date(input$date) - input$lag
    max_date <- as.Date(input$date) + input$lead
    
    # filter data according to defined date range
    data_processed_filtered <- data_processed_filtered %>% 
      mutate(date = as.Date(timestamps)) %>% 
      filter(between(date, min_date, max_date))
    
    # get individuals
    individuals <- sort(unique(data_processed_filtered$individuals))
    
    list(data_processed_filtered = data_processed_filtered,
         individuals = individuals)
    
  })
  
  # create map
  rctv_map <- reactive({
    
    # load reactive data
    polygons_filtered <- rctv_polygons_filtered()
    data_processed <- rctv_data_processed()
    data_processed_filtered <- rctv_data_processed_filtered()$data_processed_filtered
    individuals <- rctv_data_processed_filtered()$individuals
    
    # set map colors and parameters
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
    col_vector <- tail(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), -4)
    line_opacity <- 0.8
    line_weight <- 2
    circle_opacity <- 0.5
    circle_fill_opacity <- 0.3
    legend_opacity <- 0.6
    
    # store individual colors
    individual_colors <- col_vector[1:length(individuals)]
    
    # create start and end icons
    start_icon <- awesomeIcons(icon = "map-pin",
                               library = "fa",
                               markerColor = "white")
    end_icon <- awesomeIcons(icon = "map-pin",
                             library = "fa",
                             markerColor = "red")
    
    # get bounding box of roi
    min_long <- min(data_processed$long)
    max_long <- max(data_processed$long)
    min_lat <- min(data_processed$lat)
    max_lat <- max(data_processed$lat)
    
    # create map with scale, tiles, controls and roi bounding box
    map <- leaflet() %>% 
      addTiles() %>% 
      addScaleBar(position = "topleft") %>% 
      addProviderTiles("Esri.WorldTopoMap",
                       group = "TopoMap") %>% 
      addProviderTiles("Esri.WorldImagery",
                       group = "Aerial") %>% 
      addLayersControl(position = "topleft",
                       baseGroups = c("StreetMap", "Aerial"),
                       overlayGroups = c("ROI", "Water", "Tracks", "Start", "End"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      addRectangles(lng1 = min_long,
                    lat1 = max_lat,
                    lng2 = max_long,
                    lat2 = min_lat,
                    color = "black",
                    fillColor = "transparent",
                    group = "ROI")
    
    # add polygons to map
    map <- map %>%
      addPolygons(data = polygons_filtered,
                  stroke = FALSE,
                  fillColor = "blue",
                  fillOpacity = 0.3,
                  group = "Water")
    
    # populate map
    for (i in seq(along = individuals)) {
      
      # filter data for individual
      data_processed_filtered_individual <- data_processed_filtered[data_processed_filtered$individuals == individuals[i], ] %>%
        arrange(timestamps)
      
      # add lines and points
      map <- map %>% 
        addPolylines(data = data_processed_filtered_individual,
                     lng = ~long,
                     lat = ~lat,
                     color = individual_colors[i],
                     opacity = line_opacity,
                     weight = line_weight,
                     label = ~timestamps,
                     group = "Tracks")
      
      # get first and last long, lat and time
      first_long <- head(data_processed_filtered_individual, 1)$long
      first_lat <- head(data_processed_filtered_individual, 1)$lat
      first_time <- head(data_processed_filtered_individual, 1)$timestamps

      last_long <- tail(data_processed_filtered_individual, 1)$long
      last_lat <- tail(data_processed_filtered_individual, 1)$lat
      last_time <- tail(data_processed_filtered_individual, 1)$timestamps

      # add markers
      map <- map %>%
        addAwesomeMarkers(lng = first_long,
                          lat = first_lat,
                          icon = start_icon,
                          label = paste0("Start of ", individuals[i], " at: ", first_time),
                          group = "Start") %>%
        addAwesomeMarkers(lng = last_long,
                          lat = last_lat,
                          icon = end_icon,
                          label = paste0("End of ", individuals[i], " at: ", last_time),
                          group = "End")
      
    }
    
    # add legend if all individuals are selected
    if (input$individual == "all") {
      
      map <- map %>%
        addLegend(position = "topright",
                  colors = individual_colors,
                  opacity = legend_opacity,
                  labels = individuals)
      
    }
    
    map
    
  })
  
  output$map <- renderLeaflet({ rctv_map() })
  
  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({ current() }))
  
}
