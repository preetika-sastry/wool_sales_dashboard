#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#---------------------------------------------------- LIBRARIES ------------------------------------------------------------

#library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(ggmap)
library(stringr)
library(plotly)
#library(DT)

#---------------------------------------------------- DATA ------------------------------------------------------------

museum_sales <- read.csv("geelongnationalwoolmuseumsales.csv")

#---------------------------------------------------- GEOCODING------------------------------------------------------------

geocoded_local <- read.csv("geocoded_local.csv")
output_int <- read.csv("geocoded_international.csv")

geocoded <- rbind(geocoded_local, output_int)

#----------------------------------------------------------------------------------------------------------------
datasets <- list(museum_sales,geocoded)
count=0
for (i in datasets){
            
            #Creating Local/International Column
            i$Location <- ifelse(i$postcode == 0, 'NA', ifelse(nchar(as.character(i$postcode)) == 4, 'Local', 'International'))
            
            #Converting date column to Date Type
            i$date <- as.Date(i$date, format = "%d/%m/%Y")
            
            #Converting total column to numeric
            i <- i %>% mutate(total = as.numeric(gsub("[$,]", "", total)))
            
            #Classifying Membership and Ticket Sales
            i <- i %>%
              mutate(Sales_Type = case_when(
                grepl("membership", Item.Description, ignore.case = TRUE) ~ "Membership Sales",
                substr(Item.Description, 1, 1) == "%" ~ "Ticket Sales",
                TRUE ~ Item.Description
              ))
            
            #Classifying Discounts
            i <- i %>%
              mutate(Sales_Type_2 = case_when(
                grepl("%", Sales_Type) ~ "Discount",
                TRUE ~ Sales_Type
              ))
            
            #Deleting irrelevant rows
            exclude_values <- c("Goods And Services Tax", "Cash", "Gst Free", "0pen Cash Drawer", "Visa", "Eftpos", "Mastercard", "American Express", "Cheque", "Rounding Button for % Discounts")
            i <- i %>%
              filter(!Sales_Type_2 %in% exclude_values)
            i <- i %>%
              filter(!(total == '$0.00 ' | grepl("\\(", total)))
            
            #Classifying Shop Sales
            i <- i %>%
              mutate(Sales_Type = case_when(
                !(Sales_Type_2 %in% c("Ticket Sales", "Discount", "Membership Sales")) ~ "Shop Sales",
                TRUE ~ Sales_Type_2
              ))
            i <- i %>% select(-Sales_Type_2)
            
            
            # OPTIONAL : DELETE DISCOUNT ROWS
            i <- i %>% filter(Sales_Type != "Discount")
            
            if(count==0){df<- i}
            else{geo <- i}
            count = count+1
}

#---------------------------------------------------- SETTINGS ------------------------------------------------------------


#colors_cats <- [membership(yellow): f9c74f, tickets(blue): 00afb9,shops (red): f07167]
#colors_locs <- [international (orange): #f8961e, local(green): #99d98c]
colors_cats <- c("Membership Sales" = "#f9c74f", 
                   "Ticket Sales" = "#00afb9", 
                   "Shop Sales" = "#f07167")
colors_locs <- c("International" = "#f8961e", 
                 "Local" = "#76c893")


#---------------------------------------------------- UI ------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "National Wool Museum Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Sales Distribution", tabName = "sales", icon = icon("chart-bar")),
      dateRangeInput("dateRange", 
                     label = "Select Date Range", 
                     start = min(df$date, na.rm = TRUE), 
                     end = max(df$date, na.rm = TRUE)),
      selectInput("salesType", 
                  label = "Choose Sales Type",
                  choices = c("All Sales Types", unique(df$Sales_Type)),
                  selected = "All Sales Types"), 
      selectInput("metric", 
                  label = "Select Metric",
                  choices = c("Number of Sales", "Revenue"),
                  selected = "Number of Sales")

    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      
      .small-box h3 {
    font-size: 20px;
    font-weight: 500;
    margin: 0px 0px 1px;
    white-space: nowrap;
    padding: 0px;
}
      .small-box .icon-large {
    position: absolute;
    top: auto;
    bottom: 5px;
    right: 5px;
    font-size: 30px;
    color: rgba(0, 0, 0, 0.15);
}
      
      #   .dataTable thead th {
      #   color: white;
      #   background-color: #001d3d; /* Set background color if needed */
      # }
      
        /* Dashboard Background Color */
        .content-wrapper, .main-header, .main-sidebar {
          background-color: #001d3d !important;
        }
        
element.style {
}
* {
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
    box-sizing: border-box;
}
user agent stylesheet
div {
    display: block;
}
.dropdown-menu {
    position: absolute;
    top: 100%;
    left: 0;
    z-index: 1000;
    display: none;
    float: left;
    min-width: 160px;
    padding: 5px 0;
    margin: 2px 0 0;
    font-size: 14px;
    text-align: left;
    list-style: none;
    background-color: #fff;
    background-clip: padding-box;
    border: 1px solid #ccc;
    border: 1px solid rgba(0,0,0,.15);
    border-radius: 4px;
    -webkit-box-shadow: 0 6px 12px rgba(0,0,0,.175);
    box-shadow: 0 6px 12px rgba(0,0,0,.175);
}
.datepicker {
    border-radius: 0.25rem;
    color: #003566 !important;
    direction: ltr;
}
.skin-blue .main-header .logo {
    background-color: #003566;
    color: #fff;
    border-bottom: 0 solid transparent;
}
        .main-sidebar{
        background-color: #003566 !important;
        color: black !important;
        }
        
        .skin-blue .main-header .navbar {
            background-color: #003566;
        }

        /* Graph Background Color */
        .box {
          background-color: #001d3d !important;
          border-color: #001d3d;  /* Remove border to make the background fill the box */
          outline: none;
          border: none !important;
        }
        
          .box-title {
          background-color: #001d3d !important;
          border-color: #003566;  /* Remove border to make the background fill the box */
          }
        
          .box-header {
          background-color: #001d3d !important;
          border-color: #001d3d;  /* Remove border to make the background fill the box */
        }

        /* Change text color for all text */
        body, .main-header, .content-wrapper, .box, .box-title, .box-body{
          color: white !important;
        }

        /* Adjust title size and make it bold */
        .box-title {
          font-size: 20px;
          font-weight: bold;
        }
p{
    margin: 0 0 0px;
}
      "))
    ),
    
    fluidRow(      # Total Sales Widget
      valueBoxOutput("totalSalesBox", width = 3),
      
      # Total Revenue Widget
      valueBoxOutput("totalRevenueBox", width = 3)),
    
    fluidRow(
      box(
        title = "Performance Across Categories",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        boxStyle = "border: none; padding: 0px;", 
        plotOutput("categoryCounts", height = 330)
      ),      
      box(
        title = "Performance Over Time",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        boxStyle = "border: none; padding: 0px;", 
        plotlyOutput("sales_over_time", height=330)
      ),
      box(
        title = "Local Vs International",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        boxStyle = "border: none; padding: 0px;", 
        plotOutput("locationCounts", height = 330)
      )
    ),
    
        fluidRow(
          box(
            title = "Top Performing Products",
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            boxStyle = "border: none; padding: 0px;", 
            plotOutput("topItems", height=350),
          ),
          # box(
          #   title = "Top Products",
          #   width = 3,
          #   status = "primary",
          #   solidHeader = TRUE,
          #   collapsible = TRUE,
          #   boxStyle = "border: none; padding: 0px;", 
          #   DT::DTOutput("topItemsTable",height = 200)
          # ),
          box(
            title = "Regional Distribution",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            boxStyle = "border: none; padding: 0px;", 
            selectInput("mapType", 
                        label = "Select Map Type",
                        choices = c("Local", "International"),
                        selected = "Local"),
            tags$div(
              style = "font-size: 12px; text-align: left; color: white; background-color: none;",
              "(Note: Shows only a random subset of the data to avoid slow processing speeds)"
            ),
            leafletOutput("sales_map", height = 300)
          ),
          box(
            title = "Top Suburbs and Countries",
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            boxStyle = "border: none; padding: 0px;", 
            plotOutput("topSuburbs", height=200),
            plotOutput("topCountries", height=200)
          )


        )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#--------------------------------------------MAIN FILTER--------------------------------------------

  # Reactive data based on selected Sales Type
  filtered_data <- reactive({
      if (input$salesType == "All Sales Types") {
        df %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
      } else {
        df %>% filter(Sales_Type == input$salesType) %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
      }
    })
  
  geo_filtered_data <- reactive({
    if (input$salesType == "All Sales Types") {
      geo %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    } else {
      geo %>% filter(Sales_Type == input$salesType) %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    }
  })
  
#----------------------------------------------------------------------------------------------------------------
  
  # Create a bar chart of sales_type distribution
  output$categoryCounts <- renderPlot({
    
    plot_data <- filtered_data() %>%
      group_by(Sales_Type) %>%
      summarise(value = if (input$metric == "Revenue") sum(total, na.rm = TRUE) else n())
    
    ggplot(plot_data, aes(x = reorder(Sales_Type, value), y = value, fill = Sales_Type)) +
      geom_bar(stat = "identity", width=0.7) +
      scale_fill_manual(values = colors_cats) +
      geom_text(aes(label = scales::comma(value, accuracy = 1)), vjust = -0.5, size = 4.5, color='white', fontface = "bold") +
      labs(title = if (input$metric == "Revenue") "Total Revenue ($)" else "Total Sales",
           x = "",
           y = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "white"),
        plot.background = element_rect(fill = "#001d3d", color = "transparent"),
        axis.text.x = element_text(size = 12),  # Make x-axis text bigger and bolder
        axis.text.y = element_blank(),                        # Make y-axis text bigger and bolder
        axis.title.x = element_blank(),                       # Make x-axis title bigger and bolder
        axis.title.y = element_blank(), 
        axis.text = element_text(color = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#001d3d", , color = "transparent"),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0)# Removes the legend
      ) 
  })
  
  output$locationCounts <- renderPlot({
    
    location_data <- reactive({
      if (input$salesType == "All Sales Types") {
        filtered_data() %>% filter(!(Location=='NA'))
      } else {
        filtered_data() %>% filter(Sales_Type == input$salesType & !(Location=='NA'))
      }
    })
    
    plot_data <- location_data() %>%
      group_by(Location) %>%
      summarise(value = if (input$metric == "Revenue") sum(total, na.rm = TRUE) else n())
    
    ggplot(plot_data, aes(x = reorder(Location, value), y = value, fill = Location)) +
      geom_bar(stat = "identity", width=0.7) +
      scale_fill_manual(values = colors_locs) +
      geom_text(aes(label = scales::comma(value, accuracy = 1)), vjust = -0.5, size = 4.5, color='white', fontface = "bold") +
      labs(title = if (input$metric == "Revenue") "Total Revenue ($)" else "Total Sales",
           x = "",
           y = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "white"),
        plot.background = element_rect(fill = "#001d3d", color = "transparent"),
        axis.text.x = element_text(size = 12),  # Make x-axis text bigger and bolder
        axis.text.y = element_blank(),                        # Make y-axis text bigger and bolder
        axis.title.x = element_blank(),                       # Make x-axis title bigger and bolder
        axis.title.y = element_blank(), 
        axis.text = element_text(color = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#001d3d", , color = "transparent"),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0)# Removes the legend
      ) 
  })
#-------------------------------------------------------STRUGGLE STREET-----------------------------------
filtered_data_MAP <- reactive({
  geo_filtered_data() %>%
      filter(Location == input$mapType)
  })
  aggregated_data <- reactive({
    filtered_data_MAP() %>%
      group_by(Post.code.description, lat, lon) %>%
      summarise(
        total_sales = n(),  # Count the number of sales
        total_revenue = sum(total, na.rm = TRUE),  # Sum the revenue
        .groups = 'drop'
      )
  })
  
  output$sales_map <- renderLeaflet({

    data <- aggregated_data()
    
    
    # Choose map based on location
    if (input$mapType == "Local") {
      # Map of Australia (using default for local)
      map_center <- c(-25, 133)  # Australia center
      map_zoom <- 4  # Zoom level for Australia
    } else {
      # World map if international
      map_center <- c(0, 0)  # Center of the world
      map_zoom <- 2  # Zoom out for world view
    }
    
        # Dynamic scaling for radius based on metric (Sales or Revenue)
    if(input$metric == "Number of Sales") {
      radius_values <- data$total_sales
    } else {
      radius_values <- data$total_revenue
    }
    
    # Calculate a scaling factor based on the metric's range
    scale_factor <- max(radius_values) / 50
    
    # Create map plot based on selected metric
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = map_center[2], lat = map_center[1], zoom = map_zoom) %>%
      
      # Add markers based on the selected metric
      addCircleMarkers(
        ~lon, ~lat, 
        radius = ~total_sales/10,  # Adjust marker size based on the metric
        color = ifelse(input$mapType=="Local","#06c449", "#f78902"),  # Custom marker color
        popup = ~paste(
          "Location: ",Post.code.description , "<br>", 
          "Total Sales: ", total_sales, "<br>", 
          "Total Revenue: $", total_revenue, "<br>", 
          ifelse(input$metric == "Number of Sales", paste("Sales Count: ", total_sales), paste("Total Revenue: $", total_revenue))
        ),
        label = ~paste("Location: ", Post.code.description, "Sales: ", total_sales, " - Revenue: $", total_revenue)
      )
    
  })
  
  
  output$sales_over_time <- renderPlotly({
    # Reactive data filtering based on Sales Type and Date Range
    plot_data <- filtered_data() %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2]) %>%
      group_by(date, Sales_Type) %>%
      summarise(value = if (input$metric == "Revenue") sum(total, na.rm = TRUE) else n(), .groups = 'drop') %>%
      mutate(date = as.Date(date))
    
    # Apply monthly aggregation
    plot_data <- plot_data %>%
      mutate(month = as.Date(format(date, "%Y-%m-01"))) %>%
      group_by(month, Sales_Type) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
    
    # Calculate date range in days
    date_range_days <- as.numeric(diff(range(plot_data$month)))
    
    # Determine appropriate date format based on range
    x_format <- if (date_range_days <= 31) {
      list(tickformat = "%d %b", dtick = "D1")  # Daily: "15 Jan"
    } else if (date_range_days <= 90) {
      list(tickformat = "%d %b", dtick = "D7")  # Weekly: "15 Jan"
    } else if (date_range_days <= 365) {
      list(tickformat = "%b %Y", dtick = "M1")  # Monthly: "Jan 2024"
    } else if (date_range_days <= 365 * 2) {
      list(tickformat = "%b %Y", dtick = "M3")  # Quarterly: "Jan 2024"
    } else {
      list(tickformat = "%Y", dtick = "M12")    # Yearly: "2024"
    }
    
    # Apply smoothing for each Sales_Type
    smoothed_data <- plot_data %>%
      group_by(Sales_Type) %>%
      arrange(month) %>%
      do({
        # Create more points between existing points for smoother interpolation
        data.frame(
          month = seq(min(.$month), max(.$month), by = "days"),
          value = stats::spline(x = as.numeric(.$month), 
                                y = .$value,
                                n = length(seq(min(.$month), max(.$month), by = "days")))$y
        )
      }) %>%
      ungroup()
    
    # Create the plotly visualization
    p <- plot_ly(smoothed_data, 
                 x = ~month, 
                 y = ~value, 
                 color = ~Sales_Type,
                 colors = colors_cats) %>%
      # Add lines with smoothing
      add_lines(
        line = list(
          shape = "spline",
          smoothing = 1.3,
          width = 3
        ),
        hoverinfo = "text",
        text = ~paste(
          "Date: ", format(month, "%B %d, %Y"),
          "<br>", Sales_Type,
          "<br>Total: ", scales::comma(round(value, 2))
        )
      ) %>%
      # Layout configuration
      layout(
        title = list(
          text = paste(""),
          font = list(color = "white", size = 16)
        ),
        xaxis = list(
          title = list(
            text = "", 
            font = list(color = "white", size = 12)
          ),
          tickfont = list(color = "white", size = 12),
          type = "",
          tickformat = x_format$tickformat,
          dtick = x_format$dtick,
          gridcolor = "transparent",
          showgrid = FALSE,
          rangeslider = list(visible = FALSE)
        ),
        yaxis = list(
          title = list(
            text = ifelse(input$metric == "Revenue", 
                          "Total Revenue ($)", 
                          "Total Sales"),
            font = list(color = "white", size = 12)
          ),
          tickfont = list(color = "white", size = 12),
          gridcolor = "transparent",
          showgrid = FALSE,
          tickformat = ",."
        ),
        paper_bgcolor = "#001d3d",
        plot_bgcolor = "#001d3d",
        showlegend = FALSE,
        legend = list(
          orientation = "h",
          y = -0.2,
          x = 0.5,
          xanchor = "center",
          font = list(color = "white"),
          bgcolor = "#001d3d"
        ),
        margin = list(t = 50, r = 20, b = 50, l = 70)
      ) %>%
      # Configuration options
      config(displayModeBar = TRUE,
             displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'zoom2d', 'pan2d', 'select2d', 'lasso2d', 
               'zoomIn2d', 'zoomOut2d', 'autoScale2d', 
               'resetScale2d', 'hoverClosestCartesian',
               'hoverCompareCartesian'
             ))
  })
  
  # Top 5 Countries
  output$topCountries <- renderPlot({
    
    # Filter data for international locations and group by country
    top_countries_data <- filtered_data() %>%
      filter(Location == "International") %>%
      group_by(Country = Post.code.description) %>%
      summarise(value = if (input$metric == "Revenue") sum(total, na.rm = TRUE) else n(), .groups = 'drop') %>%
      arrange(desc(value)) %>%
      slice_head(n = 5)  # Select top 5 countries
    
    ggplot(top_countries_data, aes(x = reorder(Country, value), y = value, fill = value)) +
      geom_bar(stat = "identity", width=0.5) +
      scale_fill_gradient(low = "#ffd000", high ="#ff7b00" ) +
      geom_text(aes(label = scales::comma(value, accuracy = 1)), hjust = 1, size = 4, color='black', fontface = "bold") +
      labs(title = if (input$metric == "Revenue") "Top 5 Countries by Revenue" else "Top 5 Countries by Sales",
           x = "",
           y = "") +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "white"),
        plot.background = element_rect(fill = "#001d3d", color = "transparent"),
        axis.text.y = element_text(size = 12, color = "white"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  })
  
  # Top 5 Suburbs
  output$topSuburbs <- renderPlot({
    
    # Filter data for local locations and group by suburb
    top_suburbs_data <- filtered_data() %>%
      filter(Location == "Local") %>%
      group_by(Suburb = Post.code.description) %>%
      summarise(value = if (input$metric == "Revenue") sum(total, na.rm = TRUE) else n(), .groups = 'drop') %>%
      arrange(desc(value)) %>%
      slice_head(n = 5)  # Select top 5 suburbs
    
    ggplot(top_suburbs_data, aes(x = reorder(Suburb, value), y = value, fill = value)) +
      geom_bar(stat = "identity", width=0.5) +
      scale_fill_gradient(low = "#d9ed92", high ="#52b69a" ) +
      geom_text(aes(label = scales::comma(value, accuracy = 1)), hjust = 1, size = 4, color='black', fontface = "bold") +
      labs(title = if (input$metric == "Revenue") "Top 5 Suburbs by Revenue" else "Top 5 Suburbs by Sales",
           x = "",
           y = "") +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "white"),
        plot.background = element_rect(fill = "#001d3d", color = "transparent"),
        axis.text.y = element_text(size = 12, color = "white"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  })
  
  output$topItems <- renderPlot({
    
    # Filter and group data by Item.Description and summarize based on selected metric
    top_items_data <- filtered_data() %>%
      group_by(Item.Description) %>%
      summarise(value = if (input$metric == "Revenue") sum(total, na.rm = TRUE) else n(), .groups = 'drop') %>%
      arrange(desc(value)) %>%
      slice_head(n = 10)  # Select top 5 items
    
    ggplot(top_items_data, aes(x = reorder(Item.Description, value), y = value, fill = value)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient(low = "#8bcef7", high = "#0273b7") +
      geom_text(aes(label = scales::comma(value, accuracy = 1)), hjust = 1, size = 4, color = 'white', fontface = "bold") +
      labs(title = if (input$metric == "Revenue") "Top 10 Items by Revenue" else "Top 5 Items by Sales",
           x = "",
           y = "", hjust = -10) +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "white"),
        plot.background = element_rect(fill = "#001d3d", color = "transparent"),
        axis.text.y = element_text(size = 12, color = "white"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  })
  
  
  # Top 10 Performing Items Table
  # Top 10 Performing Items Interactive Table
  # output$topItemsTable <- DT::renderDataTable({
  #   
  #   # Filter and summarize data for top items
  #   top_items_data <- filtered_data() %>%
  #     group_by(Item = Item.Description) %>%
  #     summarise(value = if (input$metric == "Revenue") sum(total, na.rm = TRUE) else n(), .groups = 'drop') %>%
  #     arrange(desc(value)) %>%
  #     slice_head(n = 10)  # Select top 10 items
  #   
  #   # Prepare the data for display in the table
  #   table_data <- top_items_data %>%
  #     mutate(
  #       Total = if (input$metric == "Revenue") paste0("$", scales::comma(value)) else scales::comma(value)
  #     ) %>%
  #     select(Item, Total)  # Only keep the Item and Total columns
  #   
  #   # Render interactive table with DT
  #   DT::datatable(
  #     table_data,
  #     rownames = FALSE,
  #     options = list(
  #       dom = 't',  # Show only the table body without filtering/search box
  #       pageLength = 10,
  #       autoWidth = FALSE,
  #       columnDefs = list(
  #         list(width = '80%', targets = 0),  # Adjust Item column width
  #         list(width = '20%', targets = 1)   # Adjust Metric column width
  #       )
  #     ),
  #     class = 'display compact cell-border stripe'  # Add styling classes
  #   ) %>%
  #     DT::formatStyle(
  #       columns = names(table_data),
  #       backgroundColor = '#001d3d',
  #       color = 'white',
  #       fontWeight = 'light',
  #     )
  # })
  # 

  
  output$totalSalesBox <- renderValueBox({
    # Compute total sales and percentage change
    current_sales <- nrow(filtered_data())
    past_sales <- nrow(filtered_data() %>% filter(date < (max(date) - 365)))
    percentage_change <- round(((current_sales - past_sales) / past_sales) * 100, 1)
    
    # Format the percentage change with color
    change_text <- if (percentage_change > 0) {
      paste0("<span style='color:#99d98c;'>(", percentage_change, "% ↑)</span>")
    } else {
      paste0("<span style='color:#ff7b00;'>(", percentage_change, "% ↓)</span>")
    }
    
    # Render valueBox
    valueBox(
      value = HTML(paste0(current_sales, " ", change_text)),
      subtitle = "Total Sales (% Change Since Last Year)",
      icon = icon("shopping-cart"),
      color = "blue"  # Matches theme color
    )
  })
  
  output$totalRevenueBox <- renderValueBox({
    # Compute total revenue and percentage change
    current_revenue <- sum(filtered_data()[["total"]], na.rm = TRUE)
    past_revenue <- sum(
      filtered_data() %>% filter(date < (max(filtered_data()$date) - 365)) %>% pull(total),
      na.rm = TRUE
    )
    percentage_change <- round(((current_revenue - past_revenue) / past_revenue) * 100, 1)
    
    # Format the percentage change with color
    change_text <- if (percentage_change > 0) {
      paste0("<span style='color:#99d98c;'>(", percentage_change, "% ↑)</span>")
    } else {
      paste0("<span style='color:#ff7b00;'>(", percentage_change, "% ↓)</span>")
    }
    
    # Render valueBox
    valueBox(
      value = HTML(paste0("$", scales::comma(current_revenue, accuracy = 1), " ", change_text)),
      subtitle = "Total Revenue (% Change Since Last Year)",
      icon = icon("dollar-sign"),
      color = "blue"  # Matches theme color
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
