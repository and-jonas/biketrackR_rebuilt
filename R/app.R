rm(list = ls())

# ================================================================================================= -
# Prepare Environment ----
# ================================================================================================= -
# 
.libPaths("C:/Benutzer/jande/Dokumente/R/R-4.1.3")
# list.of.packages <- c("Rtools", "shiny", "shinyFiles", "leaflet", "sf", "OpenStreetMap",
#                       "shinydashboard", "shinyWidgets", "tidyverse", "stringr", "rdrop2", "httpuv", "plotly")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages, dependencies = TRUE, repos = 'https://stat.ethz.ch/CRAN/')

library(shiny)
library(shinyFiles)
library(leaflet)
library(sf)
library(OpenStreetMap)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(stringr)
options(shiny.maxRequestSize = 30*1024^2)
# setwd("C:/Users/jande/RProjects/biketrackR/biketracker_rebuilt")
source("funcs.R")
library(rdrop2)
library(httpuv)
library(lubridate)
library(plotly)

# ================================================================================================= -

# Tweaks to the checkboxgroupinput
tweaks <-
  list(tags$head(tags$style(HTML("
                                 .multicol {
                                   height: 750px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */
                                   -moz-column-count: 5;    /* Firefox */
                                   column-count: 5;
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 }
                                 ")),
                 tags$style("#mymap {height: calc(100vh - 90px) !important;}"),
                 tags$style("#select {
                    font-size:10px;
                    height:10px;
           }")
  ))

Akt <- c("Wandern", "Fahrrad fahren")

Akt_logos <- c(
  "https://icon-library.com/images/hiking-icon-png/hiking-icon-png-24.jpg",
  "https://icon-library.com/images/biking-icon/biking-icon-5.jpg"
)

# ================================================================================================= -
# User Interface ----
# ================================================================================================= -

drop_auth(rdstoken = "droptoken.rds")

ui <- fluidPage(
  
  tweaks,
  
  setBackgroundColor(color = "ghostwhite"),
  useShinydashboard(),

  titlePanel("Wähle Tour(en)"),

  sidebarLayout(

    sidebarPanel(

      fileInput("GPXfile",
                "Wähle .gpx file(s)",
                accept=c("text/xml", ".gpx"),
                multiple = TRUE),

      checkboxGroupInput(inputId = "activities",
                         label = "Wähle die Aktivität(en)",
                         choiceNames = mapply(Akt, Akt_logos, FUN = function(Akt, AktUrl){
                           tagList(
                             tags$img(src=AktUrl, width=20, height=15),
                             Akt
                           )
                         }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                         choiceValues = Akt,
                         selected = NULL,
                         inline = FALSE),
      
      dateRangeInput(inputId = "dateRange", label = "Wähle den Zeitraum", start = "2013-01-01", end = "2013-01-01",
                     min = "2013-01-01", max = "2013-01-01", format = "yyyy-mm-dd", startview = "year", weekstart = 0,
                     language = "de", separator = " bis ", width = NULL),

      tags$div(align = 'left',
               class = 'multicol',
               checkboxGroupInput(inputId  = "select",
                                  label    = "Wähle Tour(en)",
                                  choices  = NULL,
                                  selected = NULL,
                                  inline   = FALSE)),

      actionLink("selectall","Alles auswählen"),
      verbatimTextOutput("select")

    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Höhenprofil", plotOutput("Streckenprofil")),
        tabPanel("Karte", leafletOutput("mymap")),
        tabPanel("Statistiken",
                 column(width = 12, id = "main-panel",
                        valueBoxOutput("distbox", width = 6)),
                 column(width = 12, id = "main-panel",
                        valueBoxOutput("uphillbox", width = 6)),
                 column(width = 12, id = "main_panel",
                        plotlyOutput("progressplot")),
                 column(width = 12, id = "main_panel",
                        plotlyOutput("progressplot_hdiff"))))
      )
    )
  )



# ================================================================================================= -
# Server ----
# ================================================================================================= -

drop_auth(rdstoken = "droptoken.rds")

server <- function(input, output, session) {
  
  # reads raw .gpx files from selected files,
  # pre-processes .gpx files and exports to remote repository,
  # loads all available pre-processed data from remote repository
  # combines and newly read raw .gpx files with pre-processed data loaded from remote repository
  readGPX <- eventReactive(input$GPXfile, {
    
    # get paths of selected files
    FILE0 <- input$GPXfile
    if(is.null(FILE0)) return(NULL)
    # get input file path(s)
    FILE <- FILE0$datapath
    # get input file name(s)
    FILENAME <- FILE0$name
    # load GPS tracks
    
    # get prepared data from remote repository
    filesInfo <- drop_dir("AppData")
    filePaths <- filesInfo$path_display
    if(!is.null(filePaths)){
      print("Loading Data from Remote Repository. Please wait...")
      predat <- drop_read_csv(filePaths)
      predat <- predat %>% dplyr::select(-X) %>% 
        mutate(date = as.Date(date, "%Y-%m-%d"))
      pre_dates <- unique(predat$date)
    } else {
      predat <- NULL
      pre_dates <- NULL
    }
    # identify all selected dates from names of chosen files
    dates_process <- strsplit(FILENAME, "_") %>% lapply(., "[[", 1) %>% unlist()
    
    # redefine which files to read and process by comparing dates
    date_load_idx <- which(!as.character(dates_process) %in% as.character(pre_dates))
    if(!identical(date_load_idx, integer(0))){
      FILE <- FILE[date_load_idx]
      FILENAME <- FILENAME[date_load_idx]
    }
    
    # if not all data is available in pre-processed form already on remote repository, 
    # load raw .gpx files from disk and pre-process
    # export the updated pre-processed data to remote repository
    if(!identical(date_load_idx, integer(0))){
      
      print("Processing Raw .gpx Files. Please Wait...")
      
      data <- lapply(FILE, plotKML::readGPX)

      # get tour date(s)
      date <- basename(FILENAME) %>% strsplit(., "_") %>% 
        lapply(., "[[", 1) %>% unlist() %>% as.Date(format = "%Y-%m-%d")
      # reshape gps data
      geodat <- lapply(data, get_geodata)
      # add tour date
      for (i in 1:length(geodat)){
        geodat[[i]]$date <- date[i]
      }
      # check for duplicate dates and merge files, if necessary
      # identify dates with more than one track
      date <- list()
      for (i in 1:length(geodat)){
        date[[i]] <- geodat[[i]]$date[1]
      }
      duplicated_dates <- date[which(duplicated(date))]
      dateindex <- which(duplicated(date))
      # iterate over dates with more than one track
      # these tracks are merged to a single track
      for (d in duplicated_dates){ 
        ids <- which(unlist(date) %in% d)
        # total distance of first part
        final_dist1 <- max(geodat[[ids[1]]]$dist_tot, na.rm = TRUE)
        # add distance of first part to the distances in the second part
        geodat[[ids[2]]]$dist_tot <- geodat[[ids[2]]]$dist_tot + final_dist1
        # add grouping variable for the two separate parts to enable disjointed lines plotting
        geodat[[ids[1]]]$part = "A"
        geodat[[ids[2]]]$part = "B"
        # bind elements
        geodat[[ids[1]]] <- rbind(geodat[[ids[1]]], geodat[[ids[2]]])
      }
      geodat[c(dateindex[1], dateindex[2])] <- NULL
      # to single df
      alldat <- bind_rows(geodat)
      
      alldat <- alldat %>% mutate(ele = as.numeric(ele),
                                  time = as.factor(time),
                                  ele.p1 = as.numeric(ele.p1))
      
      # bind rows of previously and newly loaded data
      alldat <- bind_rows(predat, alldat)
      
      # update processed data on repository
      write.csv(alldat, 'AppData.csv', col.names = FALSE)
      drop_upload('AppData.csv', path = 'AppData')
    }
    else{
      # if all dates already represented in data from remote,
      # no reloading of raw .gpx files is required
      alldat <- predat
    }
    return(alldat)
  })
  
  # ================================================================================================= -
  
  text_message <- reactiveVal('')
  output$print_action <- renderText({text_message()})
  output$range <- renderText({input$dateRange})
  output$value <- renderText({input$select})
  
  observe({
    alldat <- readGPX()
    if(is.null(alldat)) return(NULL)
    choices <- unique(alldat$date) %>% as.Date(., format = "%Y-%m-%d")
    updateDateRangeInput(session, "dateRange", 
                      "Wähle den Zeitraum:",
                      min = min(choices),
                      max = max(choices))
    early <- input$dateRange[1]
    late <- input$dateRange[2]
    selected_dates <- choices[which(choices >= early & choices <= late)]
    
    if (length(selected_dates)>0){
      updateCheckboxGroupInput(session, "select","Wähle Tour(en)",
                               choices = selected_dates)
    }

    if(input$selectall == 0) return(NULL)
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session, "select","Wähle Tour(en)",
                               choices = selected_dates)
    }
    else
    {
      updateCheckboxGroupInput(session, "select",
                               label = "Wähle Tour(en)",
                               choices = selected_dates,
                               selected = selected_dates)
    }
    
  })
  
  output$dateRangeText <- renderText({dateRangeMonth$Month})
  
  output$mymap <- renderLeaflet({
    # alldat <- readGPX()
    # initiate object
    mymap <- leaflet() %>% 
      addTiles()
    # color vector
    # colors <- rainbow(n = length(input$select))
    # read input file
    alldat <- readGPX()
    
    print(head(alldat))
    
    if(is.null(alldat)) return(NULL)
    # add all selected tours
    if (length(input$select) > 0){
      for (i in 1:length(input$select)){
      
        # extract the track
        df <- alldat[alldat$date == as.Date(input$select[i], format = "%Y-%m-%d"), 
                     c("part", "lat", "lon", "activity")]
        
        # add color variable for plotting
        df$color <- ifelse(df$activity == "Fahrrad fahren", "blue", "green")

        # perform the activity selection
        if(length(input$activities) < 2){
          if(df$activity[1] != input$activities[1]){
            next
          }
        }
        
        # separate dfs for separate parts of a tour
        # required to plot separate polylines
        dfA <- df[df$part == "A", c("part", "lat", "lon", "color")]
        dfB <- df[df$part == "B", c("part", "lat", "lon", "color")]
        
        # dfAmultipoints <- st_multipoint(data.matrix(dfA[,c(3, 2)]))
        # dfAmultiline <- st_cast(dfAmultipoints, "MULTILINESTRING")
        # 
        # dfBmultipoints <- st_multipoint(data.matrix(dfB[,c(3, 2)]))
        # dfBmultiline <- st_cast(dfBmultipoints, "MULTILINESTRING")
        # 
        # dfAmultiline_simple <- rmapshaper::ms_simplify(dfBmultiline, keep = 0.05, keep_shapes = TRUE)
        # dfAmultiline_simple <- st_transform(dfAmultiline_simple, 4326)
        # 
        # dfBmultiline_simple <- rmapshaper::ms_simplify(dfBmultiline, keep = 0.05, keep_shapes = TRUE)
        # dfBmultiline_simple <- st_transform(dfBmultiline_simple, 4326)
        
        mymap <- mymap %>%
          addProviderTiles(provider = providers$Stamen.Terrain, layerId = 0) %>%
          #   addFeatures(data = dfAmultiline,
          #               color = 'blue',
          #               weight = 3) %>% 
          #   addFeatures(data = dfBmultiline,
          #               color = 'blue',
          #               weight = 3)
          addPolylines(data = dfA,
                       lng = ~lon, lat = ~lat,
                       col = unique(dfA$color),
                       layerId = i,
                       group = "base") %>%
          addPolylines(data = dfB,
                       lng = ~lon, lat = ~lat,
                       col = unique(dfA$color),
                       layerId = i,
                       group = "base")
      }
      # define map limits
      mymap <- mymap %>% clearBounds()
    }
  })
  
  # Observe the polylines
  observe({
    leafletProxy("mymap") %>% clearPopups() %>% clearGroup(group = "selected")
    event <- input$mymap_shape_click
    if (is.null(event))
      return(NULL)
    # selected data for the clicked polyline
    alldat <- readGPX()
    df <- alldat[alldat$date == as.Date(input$select[event$id], format = "%Y-%m-%d"),]
    # summary data to display on click
    date <- unique(df$date)
    dist <- max(df$dist_tot, na.rm = T)
    up <- round(max(df$cum.uphill, na.rm = T), 0)
    highest <- round(max(as.numeric(df$ele), na.rm = T), 0)
    # # graph
    # p <- profilplot(df)
    # plot(p)
    # Popup on click
    leafletProxy("mymap") %>%
      addPopups(
        event$lng, event$lat,
        paste(paste0("\uF4C5", date), "\n",
              paste0("\u2192", dist, "km"),
              paste0("\u2191", up, "m"),
              paste0("\u25B2", highest, "M.ü.M"),
              sep = " ")
      ) %>%
      addPolylines(data = df,
                   lng = ~lon,
                   lat = ~lat,
                   layerId = event$id,
                   group = "selected",
                   dashArray = 3,
                   color = "red", weight = 5, opacity = 1)
    # %>%
    #   addPolygons(data = df, popup = popupGraph(p, type = "svg"))
  })
  
  output$Streckenprofil <- renderPlot({
    alldat <- readGPX()
    choices <- unique(alldat$date) %>% as.Date(., format = "%Y-%m-%d")
    selected_dates <- choices[which(choices >= input$dateRange[0] & choices <= input$dateRange[1])]
    profilplot(alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"),])
  })
  
  output$distbox <- renderValueBox({
    alldat <- readGPX()
    # subset data according to selected dates
    df <- alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"), ]
    #subset data according to selected activities
    df <- df[df$activity %in% input$activities, ]
    # extract daily distance
    data_list <- split(df, df$date)
    daily_dist <- lapply(data_list, function(df) max(df$dist_tot, na.rm = TRUE))
    # sum up
    sum_dd <- daily_dist %>% unlist() %>% unname() %>% sum()
    valueBox(paste0(sum_dd, "km"), "Distanz", 
             icon = icon("arrow-alt-circle-right", lib = "font-awesome"), 
             width = 7.5, color = "green")
  })
  
  output$uphillbox <- renderValueBox({
    alldat <- readGPX()
    # subset data according to selected dates
    df <- alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"), ]
    #subset data according to selected activities
    df <- df[df$activity %in% input$activities, ]
    # extract daily uphill
    data_list <- split(df, df$date)
    daily_uphill <- lapply(data_list, function(d) max(d$cum.uphill, na.rm = TRUE))
    # sum up
    sum_dup <- daily_uphill %>% unlist() %>% unname() %>% sum() %>% round(., 0)
    valueBox(paste0(sum_dup, "m"), "Aufwärts", 
             icon = icon("arrow-alt-circle-up", lib = "font-awesome"), 
             width = 7.5, color = "red")
  })
  
  
  output$progressplot <- renderPlotly({
    alldat <- readGPX()
    alldat$date = as.Date(alldat$date, format = "%Y-%m-%d")
    # subset data according to selected dates
    df <- alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"), ]
    # subset data according to selected activities
    df <- df[df$activity %in% input$activities, ]
    newdf <- df %>% group_by(date) %>% top_n(1, dist_tot) %>% 
      slice(1) %>% ungroup()
    newdf <- newdf %>%
      mutate(year = lubridate::year(date) %>% as.factor())
    newdf <- newdf %>% group_by(year) %>% 
      mutate(dist_tot_all = cumsum(dist_tot))
    
    print(head(newdf))
    
    newdf <- newdf[, c("date", "year", "dist_tot", "dist_tot_all")]
    newdf$doy <- newdf$date
    lubridate::year(newdf$doy) <- 2000
    
    # extract daily distance
    axis_lim <- max(newdf$dist_tot_all) + 20
    p <- ggplot(newdf) +
      geom_step(aes(x = doy, y = dist_tot_all, group = year, col = year)) +
      geom_point(data = newdf[which.max(newdf$date),],
                 aes(x = doy, y = dist_tot_all), col = "red") +
      ylim(c(0, axis_lim)) +
      xlab("Monat") + ylab("Kumulierte Distanz (km)") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            plot.background = element_blank(),
            axis.line = element_line(colour = "grey50"))
    
    ggplotly(p)

  })
  
  output$progressplot_hdiff <- renderPlotly({
    alldat <- readGPX()
    alldat$date = as.Date(alldat$date, format = "%Y-%m-%d")
    # subset data according to selected dates
    df <- alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"), ]
    # subset data according to selected activities
    df <- df[df$activity %in% input$activities, ]
    newdf <- df %>% group_by(date) %>% top_n(1, cum.uphill) %>% 
      slice(1) %>% ungroup()
    newdf <- newdf %>%
      mutate(year = lubridate::year(date) %>% as.factor())
    newdf <- newdf %>% group_by(year) %>% 
      mutate(hdiff_tot_all = cumsum(cum.uphill))
    
    newdf <- newdf[, c("date", "year", "hdiff_tot_all")]
    
    print(newdf)
    
    newdf$doy <- newdf$date
    lubridate::year(newdf$doy) <- 2000
    
    # extract daily distance
    axis_lim <- max(newdf$hdiff_tot_all) + 20
    q <- ggplot(newdf) +
      geom_step(aes(x = doy, y = hdiff_tot_all, group = year, col = year)) +
      geom_point(data = newdf[which.max(newdf$date),],
                 aes(x = doy, y = hdiff_tot_all), col = "red") +
      ylim(c(0, axis_lim)) +
      xlab("Monat") + ylab("Kumulierter Höhenunterschied (m)") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            plot.background = element_blank(),
            axis.line = element_line(colour = "grey50"))
    
    ggplotly(q)
    
  })
  
}

# ================================================================================================= -

# Create Shiny app ----
shinyApp(ui = ui, server = server)

# ================================================================================================= -