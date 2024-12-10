#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#set up for the shiny app
# Load required libraries
library(shiny)
library(tmap)
library(dplyr)
library(sf)
library(stringr)  # For string manipulation
library(ggplot2)

# Read in well database CSV
welldatabase <- read.csv("data/welldatabase.csv")

# Ensure Long_dd and Lat_dd are numeric and remove invalid rows
welldatabase <- welldatabase %>%
  mutate(
    Long_dd = as.numeric(Long_dd),
    Lat_dd = as.numeric(Lat_dd)
  ) %>%
  filter(!is.na(Long_dd) & !is.na(Lat_dd)) %>%
  # Create new column 'NWIS_no' by extracting the site number from the 'USGS_NWIS' column
  mutate(
    NWIS_no = str_extract(USGS_NWIS, "(?<=site_no=)\\d+")
  )

# Convert data to an sf object
well_sf <- st_as_sf(welldatabase, coords = c("Long_dd", "Lat_dd"), crs = 4326)

# Set tmap mode to interactive
tmap_mode("view")

# Define UI
ui <- fluidPage(
  # App title
  titlePanel("NPS Wells Database and NWIS Groundwater Level Data"),
  
  # Informational text
  h5("In this app you can explore NPS wells and display any NWIS water level data associated with that well. 
     Click on individual sites to explore their attributes."),
  
  # Main layout
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Sidebar width for filters
      
      # Display number of wells in the database
      fluidRow(
        column(12, 
               h5("Number of Wells"),
               verbatimTextOutput("num_wells")%>% 
                 tagAppendAttributes(style = "font-size: 14px; font-family: Arial; color: #007bff; 
                         background-color: #f8f9fa; padding: 10px; border-radius: 5px;"),
        )
      ),
      
      # Add a horizontal line
      tags$hr(style = "border-top: 2px solid #000000;"),  # Thicker black line
      
      # Filters for Park, State, Status, and Type
      selectizeInput("selected_parks", "Filter by Park:", choices = NULL, multiple = TRUE),
      selectizeInput("selected_state", "Filter by State", choices = NULL, multiple = TRUE),
      selectizeInput("selected_status", "Filter by Well Status", choices = NULL, multiple = TRUE),
      selectizeInput("selected_type", "Filter by Well Type", choices = NULL, multiple = TRUE),
      
      # Checkbox for filtering by wells with NWIS data
      checkboxInput("filter_nwis", "Only show wells that have an NWIS Site #", value = FALSE)
    ),
    
    mainPanel(
      width = 9,  # Main panel width for map and NWIS site box
      
      # Row for the map
      tmapOutput("map"),
      
      # Row for NWIS Site No display
      fluidRow(
        column(12,
               h5("**Double click on a well location to update the NWIS Site #**"),
               h4("NWIS Site #"),
               verbatimTextOutput("selected_nwis")
        )
      ),
      
      # Add button to retrieve groundwater data
      fluidRow(
        column(12, 
               h5("Click to retrieve groundwater data for the selected NWIS Site #"),
               actionButton("get_data", "Retrieve GW Data")
        )
      ),
      
      #Summarize NWIS Data
      fluidRow(
        column(12, 
               h5("Groundwater Data Summary"),
               verbatimTextOutput("num_observations"),  # Number of observations
               verbatimTextOutput("period_of_record"),  # Period of record
               verbatimTextOutput("avg_depth")          # Average depth to water
        )
      ),
      
      #Button and plot of GW Levels
      fluidRow(
        column(12, 
               h5("Click to display groundwater level data"),
               actionButton("display_plot", "Display Groundwater Level Data"),  # Button for scatter plot
               plotOutput("gw_scatter")  # Placeholder for the scatter plot
        )
      ),
      
      # Button
      downloadButton("downloadData", "Download the Data!")
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # Dynamically generate unique values for filters
  updateSelectizeInput(session, "selected_parks", choices = unique(welldatabase$Park), server = TRUE)
  updateSelectizeInput(session, "selected_state", choices = unique(welldatabase$State), server = TRUE)
  updateSelectizeInput(session, "selected_status", choices = unique(welldatabase$Status), server = TRUE)
  updateSelectizeInput(session, "selected_type", choices = unique(welldatabase$Type), server = TRUE)
  
  # Reactive object for filtered well data
  well_react <- reactive({
    filtered_data <- well_sf
    
    if (!is.null(input$selected_parks) && length(input$selected_parks) > 0) {
      filtered_data <- filtered_data %>% filter(Park %in% input$selected_parks)
    }
    
    if (!is.null(input$selected_state) && length(input$selected_state) > 0) {
      filtered_data <- filtered_data %>% filter(State %in% input$selected_state)
    }
    
    if (!is.null(input$selected_status) && length(input$selected_status) > 0) {
      filtered_data <- filtered_data %>% filter(Status %in% input$selected_status)
    }
    
    if (!is.null(input$selected_type) && length(input$selected_type) > 0) {
      filtered_data <- filtered_data %>% filter(Type %in% input$selected_type)
    }
    
    if (input$filter_nwis) {
      filtered_data <- filtered_data %>% filter(!is.na(USGS_NWIS) & USGS_NWIS != "")
    }
    
    filtered_data
  })
  
  # Define permanent colors for each type
  type_colors <- c(
    "Supply" = "#084594",  # Dark Blue
    "Monitoring" = "#ffffbf",  # Light yellow
    "Unknown" = "#d73027", # Red
    "Geothermal" = "#005824",   # Green
    "Test" = "#969696", # Grey
    "Oil & Gas" = "#040405", # Black
    "Unknown" = "#feb24c" # Yellow
    )
  
  
  # Render the map based on the reactive dataset
  output$map <- renderTmap({
  tm_basemap("OpenStreetMap") +
    tm_add_legend(
        type = "fill",
        labels = names(type_colors),
        col = type_colors,
        title = "Well Type"
      ) +
    tm_shape(well_react()) +
    tm_dots(
      col = "Type",
      size = 0.1,
      palette = type_colors,
      legend.show = FALSE,
      popup.vars = c(
        "Park" = "Park",
        "State" = "State",
        "Status" = "Status",
        "Type" = "Type",
        "Depth" = "Depth_ft",
        "Constructed" = "Completion_Date",
        "NWIS Site No" = "NWIS_no"
      )
    ) 
})
  
  
  
  
  # Reactive value to store selected NWIS Site No
  selected_nwis <- reactiveVal("")
  
  # Handle map clicks to update the NWIS Site No
  observeEvent(input$map_click, {
    click_data <- input$map_click
    
    if (!is.null(click_data)) {
      # Create a clicked point from the map click coordinates
      clicked_point <- st_point(c(click_data$lng, click_data$lat)) %>% st_sfc(crs = 4326)
      
      # Find the nearest well to the clicked point
      filtered_wells <- well_react()
      if (nrow(filtered_wells) > 0) {
        nearest_index <- which.min(st_distance(clicked_point, filtered_wells))
        nearest_well <- filtered_wells[nearest_index, , drop = FALSE]
        
        # Ensure nearest_well is valid and non-empty
        if (nrow(nearest_well) > 0) {
          # Update the reactive values
          selected_nwis(nearest_well$NWIS_no)
        } else {
          selected_nwis("")  # Reset if no well found
        }
      } else {
        selected_nwis("")  # Reset if no filtered data exists
      }
    }
  })
  
  # Render the selected NWIS Site No
  output$selected_nwis <- renderText({
    nwis_no <- selected_nwis()
    if (!is.na(nwis_no) && nwis_no != "") {
      paste("Selected NWIS Site No:", nwis_no)
    } else {
      "No NWIS Site # selected"
    }
  })
  
  # Render statistics about the wells
  output$num_wells <- renderText({
    n_wells <- nrow(well_react())  # Get the number of wells
    paste(n_wells)
  })
  
  # Create reactive value to store GW_data
  gw_data <- reactiveVal(NULL)
  
  # Observe button click to fetch GW data
  observeEvent(input$get_data, {
    req(selected_nwis())  # Ensure a site is selected
    
    nwis_no <- selected_nwis()
    
    if (!is.na(nwis_no) && nwis_no != "") {
      tryCatch({
        # Attempt to retrieve groundwater data
        GW_data <- dataRetrieval::readNWISgwl(siteNumbers = nwis_no, parameterCd = "72019")
        
        if (nrow(GW_data) > 0) {
          # Store data in reactive value
          gw_data(GW_data)
          showNotification("Groundwater data retrieved successfully!", type = "message")
        } else {
          # Handle case where no data is returned
          gw_data(NULL)
          showNotification("No Groundwater Level Data found.", type = "warning")
        }
      }, error = function(e) {
        # Handle error cases
        gw_data(NULL)
        showNotification("Error retrieving groundwater data: Ensure site number is valid.", type = "error")
      })
    } else {
      showNotification("No NWIS Site # selected.", type = "warning")
    }
  })
  
  # Optionally render the retrieved data in the UI
  output$gw_table <- renderTable({
    req(gw_data())  # Ensure data is available
    gw_data()
  })
  
  # Add a table output (optional) to display GW data
  fluidRow(
    column(12, 
           h4("Groundwater Data"),
           tableOutput("gw_table")
    )
  )
  
  # Render the number of observations from GW_data
  output$num_observations <- renderText({
    data <- gw_data()  # Retrieve the reactive value
    
    if (is.null(data)) {
      "No Groundwater Level Data found"
    } else {
      num_obs <- nrow(data)  # Count the number of rows
      paste("Number of observations found:", num_obs)
    }
  })
  
  # Render the period of record
  output$period_of_record <- renderText({
    data <- gw_data()  # Retrieve the reactive value
    
    if (is.null(data) || !"lev_dt" %in% colnames(data)) {
      "Period of Record: Not available"
    } else {
      # Ensure lev_dt is of Date type
      data$lev_dt <- as.Date(data$lev_dt)
      
      min_date <- min(data$lev_dt, na.rm = TRUE)
      max_date <- max(data$lev_dt, na.rm = TRUE)
      
      if (is.na(min_date) || is.na(max_date)) {
        "Period of Record: Not available"
      } else {
        paste("Period of Record:", min_date, "to", max_date)
      }
    }
  })
  
  # Render the average depth to water
  output$avg_depth <- renderText({
    data <- gw_data()  # Retrieve the reactive value
    
    if (is.null(data) || !"lev_va" %in% colnames(data)) {
      "Average Depth to Water: Not available"
    } else {
      avg_depth <- mean(data$lev_va, na.rm = TRUE)  # Calculate average depth
      
      if (is.na(avg_depth)) {
        "Average Depth to Water: Not available"
      } else {
        paste("Average Depth to Water:", round(avg_depth, 2), "ft")
      }
    }
  })
  
  # Create a plot of GW Level Data
  # Render scatter plot for Groundwater Level Data
  output$gw_scatter <- renderPlot({
    req(input$display_plot)  # Ensure button is clicked
    req(gw_data())  # Ensure GW_data is available
    
    data <- gw_data()
    
    # Check if required columns are available
    if (!("lev_dt" %in% colnames(data)) || !("lev_va" %in% colnames(data))) {
      showNotification("Required data columns not found in GW_data.", type = "error")
      return(NULL)
    }
    
    # Create scatter plot
    ggplot(data, aes(x = lev_dt, y = lev_va)) +
      geom_point(color = "blue", alpha = 2) +
      labs(
        x = "Date",
        y = "Depth to Water (ft)",
        title = "Groundwater Levels Over Time"
      ) +
      scale_y_reverse() +  # Reverse the y-axis
      theme_minimal()
  })
  


# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("USGS_", selected_nwis(), "_gwlvls.csv", sep = "")
  },
  content = function(file) {
    write.csv(gw_data(), file, row.names = FALSE)
  }
)

}

# Run the application 
shinyApp(ui = ui, server = server)
