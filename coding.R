# install packages
# library(devtools)
# install.packages("shiny")
# install.packages("rgdal")
# install.packages("leaflet")
# install.packages("dplyr")
# install_github('rCharts', 'ramnathv')
# install.packages("shinythemes")
# install.packages("shinyWidgets")
# install.packages("DT")
# install.packages("viridis")
# install.packages("echarts4r") # if install failed or require failed, try to remove the package and install again

# libraries
library(shiny)
library(rgdal)
library(leaflet)
library(rCharts)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(viridis)
library(echarts4r)

# read data files
denver_map <- rgdal::readOGR("./denver.geojson", "OGRGeoJSON")
crime <- read.csv("./crime_wrangling.csv")
nbhd <- read.csv("./data_wrangling.csv")

# create some choice lists
month_list <- list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6,
                   "July" = 7, "August" = 8, "September" = 9, "October" = 10,  "Novermber" = 11, "December" = 12)
nbhd_list <- nbhd[,'NBHD_NAME']
cate <- sort(unique(crime[,1]))

# creat shiny UI
ui <- shinyUI(navbarPage("Denver Crimes", id = "home", # create a navigation page
                         theme = shinytheme("flatly"), # set the theme 
                         tabPanel("WELCOME", # first panel
                                  setBackgroundImage(src = "https://i.ibb.co/BwBKcx0/denverwelcome.jpg"),
                                  fluidPage(style = "margin-top: 17%;text-align: center;", # add a picture as background
                                             actionButton('jumpTo2', 'Let\'s start the journey!')  # add an active buttun
                                  )
                         ),
                         tabPanel("NEIGHBORHOODS", value = "nbhd", # second panel
                                  fluidPage( # set up layouts
                                    fluidRow( 
                                      style='height:200px',
                                      column(3, 
                                             wellPanel(
                                               selectizeInput("select_nb", label = h5("Neighborhood: "), # create an input
                                                              choices = nbhd_list
                                               ),
                                               br(), # add an empty line
                                               selectInput("select_month", label = h5("Select a month: "), # create an input
                                                           choices = month_list
                                               ),
                                               style = "height: 280px;opacity: 0.9" # some settings
                                             )
                                      ),
                                      column(9, 
                                             wellPanel(
                                               h5(textOutput("month"), align = "center"), # add a line of text
                                               showOutput("BarChart", "nvd3"), # plot the bar chart
                                               style = "height: 280px;opacity: 0.9"
                                             )
                                      )   
                                    ),
                                    fluidRow(
                                      column(8,
                                             wellPanel(
                                               leafletOutput("map_nbhd", width = "100%", height = 580), # plot the leaflet map
                                               style = "padding: 0px;opacity: 0.95")
                                             ),
                                      column(4,
                                             wellPanel(
                                               br(),
                                               h3(textOutput("nb"), align = "center"),
                                               DT::dataTableOutput("table"), # plot the table
                                               style = "height: 580px;opacity: 0.9"))
                                    )
                                  )
                         ),
                         tabPanel("CRIME TYPE", value = "type", # third panel
                                  fluidPage(
                                    fluidRow(style='height:200px;',
                                             column(3,
                                                    wellPanel(
                                                      selectInput("select_nb2", label = h4("Neighborhood: "), # create an input
                                                                  choices = nbhd_list
                                                      ),
                                                      style = "height: 170px;opacity: 0.9"
                                                    )
                                             ),
                                             column(3,
                                                    wellPanel(
                                                      radioButtons("select_time", label = h4("Select a time period:"), # create an input
                                                                   choices = list("Morning: 06:00am - 12:00pm" = 1, 
                                                                                  "Afternoon: 12:00pm - 19:00pm" = 2, 
                                                                                  "Night: 19:00pm - 06:00am" = 3)
                                                      ),
                                                      style = "height: 170px;opacity: 0.9"
                                                    )
                                             ),
                                             column(6,
                                                    wellPanel(
                                                      h4(textOutput("hour_text"), align = "center"),
                                                      echarts4rOutput("LineChart", width = "100%", height = "300px"), # plot the stack line chart
                                                      style = "opacity: 0.9"
                                                    )
                                             )
                                    ),
                                    absolutePanel( # add an absolute panel to contain the map
                                      top = 270, left = 30, width = 675,
                                      draggable = FALSE,
                                      wellPanel(
                                        leafletOutput("map2", width = "100%", height = 700), # plot another map
                                        style = "height: 500px;padding: 0px;")
                                      ,
                                      style = "opacity: 0.9"
                                    ),
                                    absolutePanel(
                                      top = 490, right = 30, width = 675,
                                      draggable = FALSE,
                                      wellPanel(
                                        h4("Crime Type Occupency", align = "center"),
                                        echarts4rOutput("PieChart", width = "100%", height = "450px"), # plot the donut chart
                                        style = "height: 480px;padding: 0px;")
                                      ,
                                      style = "opacity: 0.9"
                                    )
                                  )
                         )
)
)

# server
server <- shinyServer(function(input, output, session) {
  
  # some text output to transfer the input back to UI
  observeEvent(input$jumpTo2, {
    updateTabsetPanel(session, "home",
                      selected = "nbhd")
  })
  
  output$month <- renderText({
    paste("Crime Records per day in ", names(month_list)[month_list == input$select_month])
  }
  )
  
  output$nb <- renderText({
    input$select_nb
  }
  )
  
  output$hour_text <- renderText({
    paste("Crime Time Distribution in ",input$select_nb2)
  }
  )
  
  # bar chart
  output$BarChart <- renderChart({
    # create a new data frame which show the data of input month and neighbourhood
    temp <- data.frame(table(crime[(crime$month==input$select_month)&(crime$NBHD_NAME==input$select_nb),'day']), input$select_month)
    # rename the data frame
    colnames(temp) <- c("Date", "Records", "Month")
    BarChart <- nPlot(
      Records ~ Date, 
      data=temp,
      type = "multiBarChart", 
      dom = "BarChart")
    BarChart$params$width <- 900 # plot width
    BarChart$params$height <- 220 # plot height
    BarChart$chart(color = "#! function(d){ return 'skyblue'} !#") # plot color
    BarChart$chart(margin = list(left = 60)) # expand the left margin to show the yaxis name
    BarChart$yAxis(axisLabel = "Number of Records", width = 60)
    BarChart$xAxis(axisLabel = "Day", width = 60)
    BarChart$chart(tooltipContent = "#! function(key, x, y, e){ 
                   return '<b>Date</b>: ' + x + '/' + e.point.Month + '<br>' + '<b>Records</b>:' + y} !#") # tooltip formatting
    BarChart$chart(showControls = FALSE) # close the control option
    BarChart
  }
  )
  
  # create a table
  output$table <- DT::renderDataTable({
    temp_table <- t(nbhd[nbhd$NBHD_NAME==input$select_nb,])
    rownames(temp_table) <- c("Neighborhood Name", "Total Population", "Property Price (per sq.m)", "Yearly Crime Records", 
                              "Median Earnings", "Bachelor or Higher Edu", "Median Age", "Total Households", 
                              "Poverty Percentage (%)")
    DT::datatable(temp_table, colnames = ' ', options = list(dom = 't'))
  }
  )
  
  # create a leaflet map
  output$map2 <- renderLeaflet({
    # use the category as factor
    color <- unique(crime[,1])
    # create colour pal
    wardpal <- colorFactor(viridis(9), color)
    temp_map <- crime[crime$NBHD_NAME==input$select_nb2&crime$timepe==input$select_time, ]
    leaflet(temp_map) %>%
      addTiles() %>%
      addCircleMarkers(~GEO_LON, ~GEO_LAT,
                       radius = 8, # radius of circle
                       color = ~wardpal(OFFENSE_CATEGORY_ID), 
                       fillOpacity = 0.6,
                       stroke = FALSE,
                       label= ~paste0("Type: ", as.character(OFFENSE_CATEGORY_ID), "\n","Neighborhood: ", as.character(NBHD_NAME))) %>%
      addLegend("bottomleft", pal = wardpal, values = ~OFFENSE_CATEGORY_ID, opacity = 0.8, title = "Crime Types")
  })
  
  # create a line chart (stack)
  output$LineChart <- renderEcharts4r({
    # locate the data
    temp_hour <- data.frame(table(crime[crime$NBHD_NAME==input$select_nb2,'hour']))
    colnames(temp_hour) <- c("Hour", "Records")
    temp_hour %>% 
      e_charts(Hour) %>% 
      e_area(Records) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
      function(params){return('Time: ' + params.value[0] + ':00'+ '<br />Records: ' + params.value[1])}")) %>% # using some js language to custom the tooltip
      e_color("steelblue") %>%
      e_format_x_axis(suffix = ": 00") %>% # make the xaxis to show like 7:00
      e_x_axis(name="Hour") %>%
      e_y_axis(name="Records") %>%
      e_legend(show=FALSE) 
  }
  )
  
  # create a leaflet map
  output$map_nbhd <- renderLeaflet({
    pal <- colorNumeric("YlGnBu", NULL)
    leaflet(denver_map) %>% # data is the geojson file
      addTiles() %>% 
      addPolygons(stroke = TRUE, smoothFactor = 0.3, 
                  fillColor = ~pal(nbhd$recording), fillOpacity = 0.7, 
                  color = "grey", 
                  weight = 1, 
                  label = ~paste0(nbhd_name, ": ", nbhd$recording),
                  highlightOptions = highlightOptions(color = "darkblue", weight = 2.5, fillOpacity = 0.3, bringToFront = TRUE)) %>%
      addLegend("bottomleft", pal = pal, values = ~nbhd$recording, opacity = 0.8, title = "Crime Records") %>% 
      setView(-104.891916,39.761944, zoom=10.5)
  }
  )
  
  # create a donut chart
  output$PieChart <- renderEcharts4r({
    # locate the data
    temp_cate <- data.frame(table(crime[crime$NBHD_NAME==input$select_nb2,'OFFENSE_CATEGORY_ID']))
    colnames(temp_cate) <- c("Category", "Records")
    temp_cate %>% 
      dplyr::mutate(model = Category) %>% 
      e_charts(model) %>% 
      e_pie(Records, radius = c("48%", "70%"))  %>%
      e_color(color = viridis(9)) %>%
      e_tooltip()
  })
  
}
)

shinyApp(ui, server)
