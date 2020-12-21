library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rgdal)
library(raster)
library(plotly)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(viridis)
library(shinycssloaders)
library(sf)
library(shinymanager)


credentials <- data.frame(
  user = c("1", "gather"),
  password = c("1", "sanitation"),
  stringsAsFactors = FALSE)


source('server_modules.R')
source('functions.R')

#set the working directory
#setwd("C:/Users/someg/dev/gather/git/Sanitation_Risk_App/Shiny/Sanitaion_Risk_App/Sanitation_Risk_App/Shiny/")

#open the cua5 outline shapefile
CUA5 <- readOGR("Shapefiles/CUA5.shp",GDAL1_integer64_policy = TRUE, layer = 'CUA5')
CUA5 <- spTransform(CUA5, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#open the fokontanies dataset
fok <- readOGR("Shapefiles/fokontanies_max.shp", GDAL1_integer64_policy = TRUE, layer = 'fokontanies_max')
fok <- spTransform(fok, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Changing certain names where the french symbols have not translated properly
fok@data$Nom[fok@data$Nom == 'RiziÃƒÂ¨res'] <- 'Riziares'
fok@data$Nom[fok@data$Nom == 'Analamahitsy CitÃƒÂ©'] <- 'Analamahitsy Cite'

# CUA5 Risk
CUA5_Risk <- readOGR("Shapefiles/Final_SRI_1.shp",GDAL1_integer64_policy = TRUE, layer = "Final_SRI_1")
CUA5_Risk<- spTransform(CUA5_Risk, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Adding roads
roads <- readOGR("Shapefiles/roads.shp", GDAL1_integer64_policy = TRUE, layer = 'roads')
roads <- spTransform(roads, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Adding Elevation and slope Data
elevation <- raster('Shapefiles/elevation2.tif', package = 'raster')
slope <- raster('Shapefiles/slope.tif', package = 'raster')
pal <- colorNumeric(c("#a6611a", '#dfc27d', "#f5f5f5", "#a6dba0", '#008837'), values(elevation), na.color = "transparent")
pal_1 <- colorNumeric(c('#252525', '#636363', '#969696', '#cccccc', '#f7f7f7'), values(slope), na.color = "transparent")



#Validation Dataset
CUA5_valid <- readOGR("Shapefiles/SRI_grid_with_validation.shp", layer = 'SRI_grid_with_validation')
CUA5_valid <- spTransform(CUA5_valid, CRS("+proj=longlat +datum=WGS84 +no_defs"))
CUA5_valid@data$low_risk_1[is.na(CUA5_valid@data$low_risk_1)] <- 0
CUA5_valid@data$high_ris_1[is.na(CUA5_valid@data$high_ris_1)] <- 0

#changing the SRI to 2 decimal places!
fok@data[,'SRI1_max']=round(fok@data[,'SRI1_max'],2)
fok@data[,'SRI2_max']=round(fok@data[,'SRI2_max'],2)
fok@data[,'SRI3_max']=round(fok@data[,'SRI3_max'],2)

#create a rank data column
fok@data$rank_sri1 <- NA
fok@data$rank_sri1[order(-fok@data$SRI1_max)] <- 1:nrow(fok@data)

fok@data$rank_sri2 <- NA
fok@data$rank_sri2[order(-fok@data$SRI2_max)] <- 1:nrow(fok@data)

fok@data$rank_sri3 <- NA
fok@data$rank_sri3[order(-fok@data$SRI3_max)] <- 1:nrow(fok@data)


# Server ----
server <- function(input, output, session) {
  
  # Checking the user input against the saved credientials
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  # Setting up the .zip files for being downloaded
  
  datasetInput = reactive({
    switch(input$dataset,
           "Final_Risk" = Final_Risk,
           "CUA5-Roads" = CUA5-Roads,
           "CUA5" = CUA5)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Shapefiles/", input$dataset, ".zip", sep = "")
    },
    content = function(file) {
      writeOGR(dsn = filename, )
    }, 
    contentType = "application/zip"
  )

  # Bounding box filter (If the stats are being used)
  boundingBox <- reactive({
    if(!is.null(input$toilets_bounds)){
      Map <- in_bounding_box(df, df$latitude, df$longitude, input$toilets_bounds)
    } else {
      Map
    }
    
  })
  
  
  #create the symbology bins
  pallete <- c("#008837", "#a6dba0", "#f7f7f7", "#c2a5cf", "#7b3294")
  pallete_low <- c("#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", '#008837')
  pallete_high <- c("#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#7b3294")
  
  
  bins_sri1 <- c(0, 0.419, 0.525, 0.621, 0.744, 1)
  pal_sri1 <- colorBin(pallete, domain = fok@data$SRI1_max, bins = bins_sri1)
  
  bins_sri2 <- c(0, 0.7, 0.797, 0.836, 0.928, 1)
  pal_sri2 <- colorBin(pallete, domain = fok@data$SRI2_max, bins = bins_sri2)
  
  bins_sri3 <- c(0, 0.682, 0.75, 0.79, 0.909, 1)
  pal_sri3 <- colorBin(pallete, domain = fok@data$SRI3_max, bins = bins_sri3)
  
  pal_valid_low <- colorBin(pallete_low, domain = CUA5_valid@data$low_risk_1, bins = bins_sri3)
  
  bins_high <- c(1, 0.909, 0.6, 0.4, 0.2, 0)
  pal_valid_high <- colorBin(pallete_high, domain = CUA5_valid@data$high_ris_1, bins = bins_high)
  
  #Labels for the symbology
  sym_labels<- c("Lowest"," ","Medium"," ", "Highest")
  
  
  #This creates the labels for the pop ups on maps
  popup_sri1 <- paste(
    "<strong>Summary:</strong>",
    "<br><strong>Name: </strong>"
    , fok$Nom
    , "<br><strong>Max Risk: </strong>"
    , fok$SRI1_max
    , "<br><strong>Rank: </strong>"
    , fok$rank_sri1)
  
  popup_sri2 <- paste(
    "<strong>Name: </strong>"
    , fok$Nom
    ,"<br><strong>Max Risk: </strong>"
    , fok$SRI2_max
    , "<br><strong>Rank: </strong>"
    , fok$rank_sri2)
  
  
  popup_sri3 <- paste(
    "<strong>Name: </strong>"
    , fok$Nom
    ,"<br><strong>Max Risk: </strong>"
    , fok$SRI3_max
    , "<br><strong>Rank: </strong>"
    , fok$rank_sri3)
  
  
  
  #the data input for SRI1 tab
  output$SRI1 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      # addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
      #              opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
      #              fillColor = "Blue") %>% 
      addPolylines(data = roads, color = "Black", weight = 1, smoothFactor = 0.5,
                   opacity = 1, fillOpacity = 0.3,
                   group = "Roads") %>%
      addPolygons(data = fok, fillColor = ~pal_sri1(SRI1_max), weight = 1, smoothFactor = 0.5, #color = "#444444",
                  opacity = 0.2, fillOpacity = 0.7, popup = ~popup_sri1,
                  #label = HTML_labels(fok$SRI1_max, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Fokontany (Max Risk)") %>%
      addPolygons(data = CUA5_Risk, fillColor = ~pal_sri1(SRI1_), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$SRI1_, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Grid") %>%
      addPolygons(data = CUA5_valid, fillColor = ~pal_valid_low(low_risk_1), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_valid$low_risk_1, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.3,
                    bringToFront = TRUE),
                  group = "Areas perceived as best") %>%
      addPolygons(data = CUA5_valid, fillColor = ~pal_valid_high(high_ris_1), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_valid$high_ris_1, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Areas perceived as worst") %>%
      addRasterImage(elevation, colors = pal, group = "Elevation") %>%
      addRasterImage(slope, colors = pal_1, group = "Slope") %>%
      addLegend(values = values(slope),
                pal = pal_1,
                title = "Slope (degrees)",
                position = "bottomleft", 
                group = 'Slope') %>%
      addLegend(values = values(elevation),
                pal = pal,
                title = "Elevation (m)",
                position = "bottomleft", 
                group = 'Elevation') %>%
      addLegend(title = "Risk", 
                position = "bottomleft", 
                values = CUA5_Risk$SRI1_,
                pal = pal_sri1,
                bins = 5,
                labFormat = function(type, cuts, p){  # Here's the trick
                  paste0(sym_labels)}
      ) %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite Map"),
        overlayGroups = c("Fokontany (Max Risk)", "Grid", "Roads", "Areas perceived as best", "Areas perceived as worst", "Elevation", "Slope"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Grid") %>%
      hideGroup("Roads") %>%
      hideGroup("Areas perceived as best") %>%
      hideGroup("Areas perceived as worst") %>%
      hideGroup("Elevation")%>%
      hideGroup("Slope")
    
  })
  
  
  output$SRI2 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      # addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
      #              opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
      #              fillColor = "Blue") %>% 
      addPolylines(data = roads, color = "Black", weight = 1, smoothFactor = 0.5,
                   opacity = 1, fillOpacity = 0.3,
                   group = "Roads") %>%
      addPolygons(data = fok, fillColor = ~pal_sri2(SRI2_max), weight = 1, smoothFactor = 0.5, #color = "#444444",
                  opacity = 0.2, fillOpacity = 0.7, popup = ~popup_sri2,
                  #label = HTML_labels(fok$SRI2_max, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Fokontany (Max Risk)") %>%
      addPolygons(data = CUA5_Risk, fillColor = ~pal_sri2(SRI2_), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$SRI2_, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Grid") %>%
      addPolygons(data = CUA5_valid, fillColor = ~pal_valid_low(low_risk_1), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_valid$low_risk_1, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.3,
                    bringToFront = TRUE),
                  group = "Areas perceived as best") %>%
      addPolygons(data = CUA5_valid, fillColor = ~pal_valid_high(high_ris_1), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_valid$high_ris_1, text = ""),
                  highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  opacity = 0.7,
                  bringToFront = TRUE),
                  group = "Areas perceived as worst") %>%
      addLegend(title = "Risk", 
                position = "bottomleft", 
                values = CUA5_Risk@data$SRI2_,
                pal = pal_sri2,
                bins = 5,
                labFormat = function(type, cuts, p){  # Here's the trick
                  paste0(sym_labels)}
      ) %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite Map"),
        overlayGroups = c("Fokontany (Max Risk)", "Grid", "Roads", "Areas perceived as best", "Areas perceived as worst"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Grid") %>%
      hideGroup("Roads") %>%
      hideGroup("Areas perceived as best") %>%
      hideGroup("Areas perceived as worst")
  })
  
  output$SRI3 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      addPolylines(data = roads, color = "Black", weight = 1, smoothFactor = 0.5,
                   opacity = 1, fillOpacity = 0.3,
                   group = "Roads") %>%
      addPolygons(data = fok, fillColor = ~pal_sri3(SRI3_max), weight = 1, smoothFactor = 0.5, #color = "#444444",
                  opacity = 0.2, fillOpacity = 0.7, popup = ~popup_sri3,
                  #label = HTML_labels(fok$SRI2_max, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Fokontany (Max Risk)") %>%
      addPolygons(data = CUA5_Risk, fillColor = ~pal_sri3(SRI3_), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$SRI3_, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Grid") %>%
      addPolygons(data = CUA5_valid, fillColor = ~pal_valid_low(low_risk_1), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_valid$low_risk_1, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.3,
                    bringToFront = TRUE),
                  group = "Areas perceived as best") %>%
      addPolygons(data = CUA5_valid, fillColor = ~pal_valid_high(high_ris_1), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_valid$high_ris_1, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Areas perceived as worst") %>%
      addLegend(title = "Risk", 
                position = "bottomleft", 
                values = CUA5_Risk@data$SRI3_,
                pal = pal_sri3,
                bins = 5,
                labFormat = function(type, cuts, p){  # Here's the trick
                  paste0(sym_labels)}
      ) %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite Map"),
        overlayGroups = c("Fokontany (Max Risk)", "Grid", "Roads", "Areas perceived as best", "Areas perceived as worst"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Grid") %>%
      hideGroup("Roads") %>%
      hideGroup("Areas perceived as best") %>%
      hideGroup("Areas perceived as worst")
  })
  


  output$updates <- renderUI(HTML("<ul>
  <li>04/10/2019 - Added the final texts and toilets individual informations</li>
  <li>02/10/2019 - Change the sample toilets for a best adaptation to the new borders</li>
  <li> 02/10/2019 - Added a new CUA5 shapefile based on an official source</li>
  <li>11/09/2019 - Created the first interactive version</li>
                                  </ul>"))
  
  output$home <- renderUI(HTML("<p>This prototype visualises three versions of geospatial analysis on the same set of 13 indicators to predict the level of risk of uncontained faecal waste in the environment in the 5eme arrondissement in Antananarivo (CUA5). For more information, contact us at hello@gatherhub.org.</p>")
  
)}

# When ready to deploy, it will then ask if you want to update the original app domain. Click yes. 
#it will upload here >> https://sanitation-hub.shinyapps.io/shiny
# library(rsconnect)
# rsconnect::setAccountInfo(name='sanitation-hub', token='82EA28FA57A3CF8359DEAC9326DA0DDE', secret='UXNQCdSIUZB6Wfv7HhhiWf4Nqf+MEJ894mPJWC2s')
# rsconnect::deployApp('C:\\Users\\someg\\dev\\gather\\git\\Sanitation_Risk_App\\Shiny\\Sanitaion_Risk_App\\Sanitation_Risk_App\\Shiny\\', account = 'sanitation-hub')
