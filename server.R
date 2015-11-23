# server.R
library(RColorBrewer)
library(leaflet)
library(maps)

mapdat <- readRDS("mapdat.rds")
mapdat[mapdat==""] <- NA
mapdat <- mapdat[complete.cases(mapdat), ]

mapdat <- mapdat[names(mapdat) %in% c("Business_Name1", "Location_Zip_Code", "city", 
                                      "long", "lat", "fillColor", "textout", "grade")]
mapdat$Business_Name1 <- gsub("^\\s+|\\s+$", "", mapdat$Business_Name1)

bnames <- sort(unique(mapdat$Business_Name1))
zips <- sort(unique(mapdat$Location_Zip_Code))
cities <- sort(unique(as.character(mapdat$city)))

FLmap <- map("state", "florida", boundary = FALSE, lty = 1, fill = TRUE, plot = FALSE)

shinyServer(function(input, output, session) {
    
    output$uiRest <- renderUI({
        selectizeInput("bname", "Select Restaurant (max = 1).",
                       choices=NULL, multiple=TRUE, 
                       options = list(maxItems = 1, placeholder = 'Select Restaurant (max = 1)'))
    })
   
    output$uiZip <- renderUI({
        selectizeInput("zip", "Select Zip Code (max = 5).",
                       choices=NULL, multiple=TRUE, 
                       options = list(maxItems = 5, placeholder = 'Select Zip Code (max = 5)'))
    })
    
    output$uiCity <- renderUI({
        selectizeInput("city", "Select City (max = 5).",
                       choices=NULL, multiple=TRUE, 
                       options = list(maxItems = 5, placeholder = 'Select City (max = 5)'))
    })
    
    updateSelectizeInput(session, 'bname', choices = bnames, server = TRUE)
    updateSelectizeInput(session, 'zip', choices = zips, server = TRUE)
    updateSelectizeInput(session, 'city', choices = cities, server = TRUE)
    
    output$Text <- renderText({ 
        if (input$plotlevel=="Business Name") {
            outtext <- paste("[ *", input$bname, sep = " ] ")
        } else if (input$plotlevel=='Zip Code') {
            outtext <- paste("[ *", input$zip, sep = " ] ")
        } else {
            outtext <- paste("[ *", input$city, sep = " ] ")
        }
    })   
    
    cenmap <- reactive({
        if (input$plotlevel=='Business Name') {
            se <- input$bname
            names(mapdat)
        } else if (input$plotlevel=='Zip Code') {
            se <- input$zip
        } else {
            se <- input$city
        }
        
        if (input$plotlevel=='Business Name') {
            cenzips <- subset(mapdat, Business_Name1==se)
        } else if (input$plotlevel=='Zip Code') {
            cenzips <- subset(mapdat, Location_Zip_Code==se)
        } else {
            cenzips <- subset(mapdat, city==se)
        }
        return(cenzips)
    })
        
    output$mymap <- renderLeaflet({
           leaflet() %>%
                addPolygons (data = FLmap, lng = ~x, lat = ~y, stroke = FALSE) %>%
                fitBounds(lng1=FLmap$range[1], lat1=FLmap$range[3], 
                         lng2=FLmap$range[2], lat2=FLmap$range[4]) %>%
                addProviderTiles("Stamen.TonerLite",
                                 options = providerTileOptions(noWrap = FALSE)) %>%
                addLegend("bottomleft", 
                         labels=sort(unique(mapdat$grade)), 
                         colors=rev(brewer.pal(5, 'RdYlGn')),
                         title="Health Grade")
    })
    
    observe({
        sv <- cenmap()
       
            map <- leafletProxy("mymap", data = sv) 
            map %>% clearMarkers()
            
            if(nrow(sv)>0) {
                map %>% addCircleMarkers(data = sv, lng = ~long, lat = ~lat, color = "black", 
                                         fillColor = ~fillColor, popup = ~textout,
                                         fill = TRUE, stroke = TRUE, weight = 1, radius = 10,
                                         fillOpacity = 0.7) %>%
                    fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
            } else {
                map %>% fitBounds(lng1=FLmap$range[1], lat1=FLmap$range[3], 
                                  lng2=FLmap$range[2], lat2=FLmap$range[4])
            }
        })
})
