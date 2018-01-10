
library(shiny)

library(ggplot2)
library(plotly)
library(RColorBrewer)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(htmltools)
library(crosstalk)
library(shinydashboard)
library(lubridate)
library(shinyjs)
library(shinyBS)


appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"







dt.path <- file.path("data","total_crime_data_new_2")



data_c_ui <- as.data.table(readRDS(dt.path))
dts <- data_c_ui[YEAR == 2017 & new.type == "theft"]


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


vars <- seq(2003,2017)

ctype <- data_c_ui[,unique(new.type)]

base_view <- data_c_ui[sample(.N,1),.(lat,lng)]

top_neigh <- function(dtm,radio_inp){
  if ( radio_inp == "all"){
    n_neigh <- as.vector(dtm[,unique(NEIGHBOURHOOD)])
  }else
    {
  top_n <- dtm[,list("count"=sum(N)),NEIGHBOURHOOD][order(-count)]
  n_neigh <- as.vector(top_n[1:as.integer(radio_inp),NEIGHBOURHOOD])
  
    }
  n_neigh
}



header_1 <- dashboardHeader(title =  "Vancouver Crime Data Vizualization ")

sidebar_1<-  dashboardSidebar(
  sidebarMenu(
    
    menuItem("Vancouver Crime Map Visualization ",tabName = "ad",icon = icon("dashboard")
             #
             ),
    menuItem("Interactive Viz",tabName = "bdb",icon = icon("dashboard"))
  )
)

body_1 <- dashboardBody(
  tabItems(
    tabItem(tabName = "bdb",
            fluidRow(
              column(width=6,align="center",
                     bsPopover("pplot","just a title","showing trend",placement = "top"),
                     
                     
                     plotlyOutput("pplot") 
              ),
              column(width = 6,align = "center",
                     
                       plotlyOutput("trendplot")
                                           
                     )
              
            ),
            fluidRow(
              
              
              
              column(width =12 ,
                     
                     uiOutput("checkbox"),
                     
                     
                     align ="center",plotlyOutput("month_hist")) 
              
              
              
              
            )
                   
            
           
            ),
    
    tabItem(tabName = "ad",
            fluidRow(
              leafletOutput("map"),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 30, left = "auto", right = 20, bottom = 'auto',
                            width = 330, height = "auto",
              selectInput("Year", "Year", vars,selected = 2017),
              checkboxGroupInput("Type", "Type", ctype, selected = ctype[1])
              )
              
            ),
            fluidRow(
              
              tabBox(height = "250px",
                     tabPanel("TrendOverTheYear","Distribution in Month",plotlyOutput("scatterline")),
                     tabPanel("NeighbourhoodCrimeCount","Neighbourhoods and Crime Counts",plotlyOutput("histday"))
                
                
              ),
              tabBox(height = "250px",side = "right",
                     tabPanel("CrimeTime","Time of the day and Crime",plotlyOutput("nebarwatch")),
                     tabPanel("CrimeBlock","Locality of Crime",plotlyOutput("localitywatch")))
              #column(width = 6 ,plotlyOutput("scatterline"))
              
            )
            
            
            
            
            
            
            )
    
    
    
    
    ))







# Define server logic required to draw a histogram
server <- function(input,output,session){

  
  
  
  
  
  
  output$pplot <- renderPlotly({
    
    showModal(modalDialog(
      Title = "Select Part of the plot to show Trend ",

      "use lasso or box select to see Part of the trend in 3 separate plots "

    ))
    
    addPopover(session, "pplot", "Tip",
            content = 
            paste0("Overall Crime Trend from 2004 and 2017.
                   Use the controls on the plots to see the trends for a specific period "), trigger = 'click',
            placement = "bottom")
    
    
    dtm <- data_c_ui[,.N,.(YEAR,MONTH,new.type)]
    dtm <-  dtm[order(YEAR,MONTH)]
    dtm[,YEAR_MON := paste(YEAR,MONTH,sep = "-")][ ,index:= .I]
    
    
    #dtm <<- dtm 
    ##draw plotly axis 
    
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = sort(unique(dtm$index)),
      showgrid = FALSE,
      showline = FALSE,
      autorange = TRUE,
      showticklabels = TRUE,
      dtick = 20,
      ticks = "outside",
      tickangle = 0,
      title = "Over all Time Period"
    ) 
    
    
    
    plt <- plot_ly(dtm,x = ~YEAR_MON ,
                   y = ~N ,
                   color = ~new.type,
                   
                   text =  ~paste('Year:',YEAR,'Month:',MONTH, '<br>Count:',N),
                   type = "scatter" ,
                   mode = "lines+markers",
                   
                   source = "mapplot", dragmode = "lasso",key = ~paste(YEAR_MON,new.type,sep="-"))
    
    
    plt <- plt%>%layout(title = "Overall Crime trend of Different types",xaxis = ax,yaxis = list(title = "crime count"))
    plt
    
    
    
  })
  
  plot_points <- reactive({
    j <- event_data("plotly_selected",source = "mapplot")
    j
    
  })
  
  top_q <- reactive({
    
    dp <- input$chk
    #print(dp)
    dp
    
  })
  
  
  observeEvent(plot_points(),{
    
    
    m <- event_data("plotly_selected",source = "mapplot")
    
    ed <- str_split_fixed(m$key,fixed("-"),n = 3)
    #print(ed)
    yr <- ed[,1]
    mn <- ed[,2]
    type <- ed[,3]
    
    
    required_dt_for_new_map <- data_c_ui[YEAR %in% yr & MONTH %in% mn & new.type %in% type]
   
    
    
    observeEvent(top_q(),{ 
      radio_inp <- top_q()
      #print(radio_inp)
      #n_neighborhood <- ifelse(radio_inp=="all")
      dtm <- required_dt_for_new_map[,.N,.(NEIGHBOURHOOD,MONTH,DAY)]
      n_neighborhood <- top_neigh(dtm,radio_inp)
      
      print(n_neighborhood)
      #dtm <- dtm[order(-N)][1:n_neighborhood]
      dtm <- dtm [NEIGHBOURHOOD %in% n_neighborhood]
      
    output$month_hist <- renderPlotly({
      
      addPopover(session, "month_hist", "Tip",
                 content = 
                   paste0("The Top Neighbourhood in terms of Crime distribution grouped by each month during the selected time
                           ,move the slider to go from 1-Jan to 12-Dec.The left plot shows distribution across the 
                          days of the selected month across the Neighbourhoods"), trigger = 'click',
                 placement = "top")
      
      #n_neighborhood <- input$chk
      
      
      #print("print n_neigh", n_neighborhood)
      #dtm <- required_dt_for_new_map[,.N,.(NEIGHBOURHOOD,MONTH,DAY)]
      dtm_key <- crosstalk::SharedData$new(dtm, ~MONTH)
      
      
      withProgress(message = 'Making plot', value = 0, {
        # Number of times we'll go through the loop
        n <- 100
        
        for (i in 1:n) {
          
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing part", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.05)
        }
      })
      
      
      
      plt <- dtm[,.("N"=sum(N)),.(NEIGHBOURHOOD,MONTH)]%>% 
        plot_ly(x = ~ N ,
                y = ~ NEIGHBOURHOOD ,
                #color= ~ NEIGHBOURHOOD,
                clickinfo = "text",
                text = ~paste("Neighbourhood:",NEIGHBOURHOOD,"Crime count:",N),
                source = "click")%>%
                add_markers(
                  data =  dtm_key,
                  frame = ~ MONTH)%>%
                 
                  animation_opts(frame = 50, transition = 1, redraw = FALSE)
      
      
      plt%>%highlight("plotly_click")%>%layout(xaxis = list(title = "Crime Count"))
      
      
      
      plt2 <- plot_ly(dtm,
                      x = ~DAY,color = ~NEIGHBOURHOOD,
                      y = ~N,text = ~NEIGHBOURHOOD,size = ~N/1000)%>%
        add_markers(
          data = dtm_key,
          frame = ~ MONTH)

      subplot(plt, plt2, nrows = 1, widths = c(0.5, 0.5),titleX = T) %>%
        
        animation_opts(1000, redraw = FALSE) %>%
        layout(clickmode = "y", margin = list(l = 100)) %>%
        highlight("plotly_selected", color = "blue", opacityDim = 1, hoverinfo = "none")
      
      
      
      
     
      
      
    })
    })
    
    
    
    output$trendplot <- renderPlotly({
      isolate(input$chk)
      
      all_trend <- required_dt_for_new_map[,.N,.(YEAR,MONTH,TYPE)]
      all_trend <- all_trend[order(YEAR,MONTH)]
      all_trend[,YEAR_MON:=paste(YEAR,MONTH)][,index:= .I]
      all_trend_key <- crosstalk::SharedData$new(all_trend,~YEAR_MON)
      min_year <- required_dt_for_new_map[,min(YEAR)]
      max_year <- required_dt_for_new_map[,max(YEAR)]
      msg <- paste("creating the trend between:", min_year ," and", max_year)
      
      ax <- list(
        type = "category",
        categoryorder = "array",
        categoryarray = sort(unique(all_trend$YEAR)),
        showgrid = FALSE,
        showline = FALSE,
        autorange = TRUE,
        autotick = FALSE,
        dtick = 10,
        showticklabels = TRUE,
        ticks = "outside",
        tickangle = 0,
        title = 'Time Period'
      ) 
      
      withProgress(message = msg , value = 0, {
        # Number of times we'll go through the loop
        n <- 60
        
        for (i in 1:n) {
          
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing part", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.05)
        }
      })
      
      
      plt <- plot_ly(all_trend,x = ~YEAR_MON ,y = ~N ,
                     color = ~ TYPE,
                     text =  ~paste('Year:',YEAR,'Month:',MONTH, '<br>Count:',N),
                     type = "scatter" , 
                     mode = "lines+markers",
                     source = "trepplot", 
                     dragmode = "lasso",
                     key = ~paste(YEAR_MON,TYPE,sep="-"))
      
      
      plt <- plt%>%layout(title =
                          paste("crime trend between ",min_year ,"and ",max_year),
                          xaxis =ax,
                          yaxis = list(title ="crime count") )
      
      
      
      
      
    })
    
    output$checkbox <- renderUI({

      radioButtons("chk","Select Top Neighbourhoods based on crime",
                   choices = list("top 5" = 5,"top 10" =10, "all" ="all"),inline = TRUE)
    })

    
    
  })
  
  

  
  
  input.yr <- reactive({
    yr <- input$Year
    yr
  }
  )
  
  input.type <- reactive({
    type <- input$Type
    type

  })
  

  output$map <- renderLeaflet({
    
    showModal(modalDialog(
      Title = "Zoom into /out of the map to see trends specific to that area ",
      
      " Zoom into /out of the map to see trends specific to that area "
      
    ))
    
    
    
    m <- leaflet()%>%setView(lng = base_view$lng,lat = base_view$lat ,zoom = 11)
    m%>%addProviderTiles(providers$Esri.NatGeoWorldMap)
    
    
    
  }) 
  
  zipsInBounds <- reactive({
    req(input$map_bounds)
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    data_c_ui[(lat >= latRng[1] & lat<=latRng[2])& (lng >= lngRng[1]& lng <= lngRng[2])]
  
  })
  
  
 
  
  output$histday <- renderPlotly({
    # If no zipcodes are in view, don't plot
    #req(input$map_bounds)
    dts <- zipsInBounds()
    dts <- dts[YEAR == input.yr() & new.type %in% input.type()]
    #dts[,dayoftheweek:= {dt = as.Date(paste(YEAR,MONTH,DAY,sep="-")); wday(dt)}]
    mts <- dts[,.N,NEIGHBOURHOOD]
    mts%>%plot_ly(x = ~NEIGHBOURHOOD,y = ~N,type = "bar",source = "areahist",key= ~NEIGHBOURHOOD)%>%
      layout(xaxis =list(title=""),
             yaxis = list(title = "Crime Count") )
    
  })
  
  plot_area_events <- reactive({
    
    m <- event_data("plotly_hover",source = "areahist")
    m
    
  })
  
  
  observeEvent(plot_area_events(),{
    
    m <- event_data("plotly_hover",source = "areahist")
    dts <- zipsInBounds()
    nebar <- m$key
    data_for_nebar <- dts[NEIGHBOURHOOD %in% nebar]
    td <- data_for_nebar[,.N,timeoftheday]
    crime_100_block_count <- data_for_nebar[,.N,street_name][order(-N),
                                                        rbind(.SD[1:10],list(street_name="Others",
                                                                             N=sum(.SD[[2]][11:.N])))]
    
    crime_100_block_count[,street_name:=ifelse(is.na(street_name),"Unknown",street_name)]
    
    output$nebarwatch <- renderPlotly({
      
      p <- plot_ly(td,labels = ~timeoftheday,values = ~N,type = "pie",
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste('CrimeCount:', N),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE)%>%
        layout(title = paste('Crime count in different Time of the day in ',nebar),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
     
      })
    
    output$localitywatch <- renderPlotly({
      
      q <- plot_ly(crime_100_block_count,labels = ~street_name,values = ~N,type = "pie",
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   hoverinfo = 'text',
                   text = ~paste('CrimeCount:', N),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE)%>%
        layout(title = paste('Crime count in different Streets in the  ',nebar),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      
    })
      
    
    
    
    
    
  })
  
  

  
  
  
  
  output$scatterline <- renderPlotly({
    # If no zipcodes are in view, don't plot
    #req(input$map_bounds)
    
    dts <- zipsInBounds()
    dts <- dts[YEAR == input.yr() & new.type %in% input.type()]
    dts <- dts[,.N,.(MONTH,TYPE)]
    dts <- dts[order(MONTH)]
    dts[,index := .I]
    
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = sort(unique(dts$index)),
      showgrid = FALSE,
      showline = FALSE,
      autorange = TRUE,
      showticklabels = TRUE,
      dtick = 2,
      ticks = "outside",
      tickangle = 0,
      title = "Months "
    )
    
    
    
    plt <- dts %>% plot_ly(x = ~ month.abb[MONTH],y = ~N,color = ~TYPE,type = "scatter",mode = "lines+markers")
           
    
    plt%>%layout(xaxis = ax ,yaxis = list(title = "Crime Count") )
    
    
    
    
    
  })
  
  observe({
    yr <- input.yr()
    print(yr)
    type <- input.type()
    print(type)
    mdt <- zipsInBounds()
    fmap <- mdt[YEAR %in% yr & new.type %in% type]
    #fmap <- dts 
    print(fmap[,.N])
    
    
    colorData <- fmap[,unique(NEIGHBOURHOOD)]
    pal <- colorFactor("Accent", colorData)
    
    
    
    leafletProxy("map", data = fmap) %>%
      clearMarkerClusters()%>%
      addMarkers(~lng, ~ lat,clusterOptions = markerClusterOptions(),layerId = "cluster") 
  })
  

 
  
  
}





ui <- dashboardPage(header_1,sidebar_1,body_1)
shinyApp(ui, server)

