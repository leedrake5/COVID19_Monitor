library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)
library(maptools)

function(input, output, session) {
    
    
    # Leaflet bindings are a bit slow; for now we'll just sample to compensate
    set.seed(100)
    
    output$datevarui <- renderUI({
        dateInput("datevar", label="Select Date", value = max(covid_county_processed$date), min = min(covid_county_processed$date), max = max(covid_county_processed$date),
        format = "yyyy-mm-dd", startview = "month", weekstart = 0,
        language = "en", width = NULL)
    })
    
    covidCountyDate <- reactive({
        covid.frame <- as.data.frame(covid_county_processed)
        result <- covid.frame[covid.frame$date==input$datevar,]
        #result <- result[complete.cases(result),]
        result
    })
    
    covidMerge <- reactive({
        req(input$displayvar)
        data.frame(index=covidCountyDate()$index, Selected=as.vector(as.numeric(covidCountyDate()[,input$displayvar] )))
    })
    
    
    spatialObject <- reactive({
        covid_merge <- covidMerge()
        spatial_merge <- sp::merge(USA_counties, covid_merge, by="index", duplicateGeoms=TRUE, all.x=TRUE)
        #spatial_merge <- left_join(USA_counties, covid_merge, by = 'index')
        spatial_merge
    })
    
    myPal <- reactive({
        #req(input$datevar, input$displayvar)

        covid_date <- covidMerge()
        
        colorNumeric(palette = colorRamp(c('yellow', 'red')), domain = as.vector(as.numeric(covid_date$Selected)))
    })
    

  ## Interactive Map ###########################################
  
  chosenLat <- reactive({
      req(input$zipvar)
      na.omit(zips[zips$zip_code %in% input$zipvar, "latitude"])[1]
  })
  
  chosenLong <- reactive({
      req(input$zipvar)
      
      na.omit(zips[zips$zip_code %in% input$zipvar, "longitude"])[1]
  })
  

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(-74.00, 40.71, zoom = 6)
  })
  
  
  #output$chosenmap <- renderPlot({
      #leafletProxy("map", data = covidCountyDate()) %>%
      #clearShapes() %>%
        #addPolygons(data = USA, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3, fillColor = ~myPal()(covidCountyDate()[,input$displayvar]), popup = paste("Region: ", USA$NAME_2, "<br>", input$displayvar, ": ", round(covidCountyDate()[,input$displayvar], 2), "<br>")) %>%
        #addLegend(position = "bottomleft", pal = myPal(), values = covidCountyDate()[,input$displayvar], title = input$displayvar, opacity = 1)
  #})
  
  # This observer is responsible for maintaining the circles and legend,
   # according to the variables the user has chosen to map to color and size.
   observe({
       req(input$displayvar, input$datevar)
       #covid_date <- covidCountyDate()
       #covid_date <- covid_date[order(covid_date$state, covid_date$county),]
       mypal <- myPal()
       
       leafletProxy("map") %>%
       clearControls()
       
       USA <- spatialObject()
       color_range <- range(na.omit(as.numeric(USA$Selected)))
     #tryCatch(
     leafletProxy("map", data = USA) %>%
     addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3, fillColor = ~mypal(USA$Selected), opacity=USA$Selected/my.max(USA$Selected), popup = paste("Region: ", USA$NAME_2, "<br>", input$displayvar, ": ", round(USA$Selected, 2), "<br>")) %>%
           addLegend(position = "bottomleft", pal = mypal, values = na.omit(USA$Selected), title = input$displayvar, opacity = 1)#, error=function(e) NULL)
   })
  
  

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  dataInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(covid_county_merge_spatial[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(covid_county_merge_spatial,
      covid_county_merge_spatial$latitude >= latRng[1] & covid_county_merge_spatial$latitude <= latRng[2] &
        covid_county_merge_spatial$longitude >= lngRng[1] & covid_county_merge_spatial$longitude <= lngRng[2])
  })
  
  dataInBoundsSimple <- reactive({
      req(input$map_bounds)
    data <- dataInBounds()[,!names(dataInBounds()) %in% c("zip_code", "latitude", "longitude")]
    data[!duplicated(data),]
  })
  
  dataInBoundsPlot <- reactive({
      req(input$map_bounds)
    data <- dataInBoundsSimple()[,c("date", "cases", "deaths", "new_cases", "new_deaths", "cases_norm", "deaths_norm", "cases_pop", "deaths_pop", "new_cases_pop", "new_deaths_pop", "new_cases_7d_avg", "new_deaths_7d_avg")]
    data_modified <- data.table::setDT(data)[, lapply(.SD, mean), by=.(date), .SDcols=c("cases", "deaths", "new_cases", "new_deaths", "cases_norm", "deaths_norm", "cases_pop", "deaths_pop", "new_cases_pop", "new_deaths_pop", "new_cases_7d_avg", "new_deaths_7d_avg")]
    as.data.frame(data_modified)
  })
  
  # Precalculate the breaks we'll need for the two histograms

  output$cases_timeseries <- renderPlot({
      req(input$map_bounds)
    # If no zipcodes are in view, don't plot
    if (nrow(dataInBounds()) == 0)
      return(NULL)
      
    temp.frame <- data.frame(Date=dataInBoundsPlot()$date, Cases=dataInBoundsPlot()$new_cases_pop)
    tem.frame <- temp.frame[complete.cases(temp.frame),]
    #temp.frame <- na.omit(temp.frame)
    
    scatter <- ggplot(aes(Date, Cases), data=temp.frame) +
    stat_smooth() +
    geom_point(colour="blue", alpha=0.4) +
    #geom_line(aes(Date, TTR::DEMA(Cases, 7)), data=temp.frame) +
    #geom_line()
    theme_light(base_size=15) +
    scale_x_date("", date_breaks = "1 month", date_labels="%b") +
    scale_y_continuous("New Cases per 100,000", labels=scales::comma)
    
    scatter
  })
  
    output$deaths_timeseries <- renderPlot({
        req(input$map_bounds)
      # If no zipcodes are in view, don't plot
      if (nrow(dataInBoundsPlot()) == 0)
        return(NULL)
        
      temp.frame <- data.frame(Date=dataInBoundsPlot()$date, Deaths=dataInBoundsPlot()$new_deaths_pop)
      tem.frame <- temp.frame[complete.cases(temp.frame),]
      #temp.frame <- na.omit(temp.frame)
      
      scatter <- ggplot(aes(Date, Deaths), data=temp.frame) +
      stat_smooth() +
      geom_point(colour="blue", alpha=0.4) +
      #geom_line(aes(Date, TTR::DEMA(Deaths, 7)), data=temp.frame) +
      #geom_line()
      theme_light(base_size=15) +
      scale_x_date("", date_breaks = "1 month", date_labels="%b") +
      scale_y_continuous("New Deaths per 100,000", labels=scales::comma)
      
      scatter
    })
  
  observe({
      leafletProxy('map') %>%
        flyTo(lng = chosenLong(), lat=chosenLat(), zoom=9)

  })
  

  output$covidtable <- DT::renderDataTable({
      
      df <- dataInBoundsSimple()[dataInBoundsSimple()$data %in% input$datevar,]
      DT::datatable(df)


  })
  
  output$downloadcovidtable <- downloadHandler(
  filename = function() { paste0("covidregional", '.csv') },
  content = function(file
  ) {
      write.csv(dataInBoundsSimple(), file)
  }
  )
  
  
  
}
