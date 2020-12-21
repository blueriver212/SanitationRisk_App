library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinymanager)


# This will log out the user after a period of inactivity
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

#This is where the logins go, probably worth hiding this section on the Github.
credientials <- data.frame(
  user = c("1", "gather"),
  password = c("1", "sanitation"),
  stringsAsFactors = FALSE)

#setwd("C:/Users/someg/dev/gather/git/Sanitation_Risk_App/Shiny/Sanitaion_Risk_App/Sanitation_Risk_App/Shiny/")


# User interface ----
ui <- secure_app(head_auth = tags$script(inactivity),
  tagList(tags$head(tags$script(type="text/javascript", src = "code.js")),
  navbarPage(title = "Sanitation Data Platform", id = "nav", theme = "style.css",
             tabPanel('Home', value = -1,
                      fluidRow( class = "updateTitle",
                                column(4, "Sanitation Data Platform: Geospatial Visualisations for three Sanitation Risk Indices for Antananarivo, Madagascar", div(style = "height:30px;"), offset = 4)
                      ),
                      fluidRow(class = "updateArea",
                               column(4, uiOutput(outputId = 'home'), offset = 4)
                      )),
             #this is in the www folder                        
             tabPanel("Data Sources", value = 5,
                      tags$iframe(class = 'leaflet-container', style="height:400px; width:100%; scrolling=yes", src="Datasets_sources.pdf")),
             tabPanel("SRI1", value = 0,
                      leafletOutput(outputId = "SRI1", height = 700) %>% withSpinner(type = 4),
                      absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                    draggable = F, top =140, left = "auto", right = 50, bottom = 20,
                                    width = 400, height = 600,
                                    style = "overflow-y: scroll;",
                                    #Stats on the side
                                    h1("Risk"),
                                    br(),
                                    p(id = "mainText", "It considers household density to be of equal importance to all the other indicators combined. This is to account for the additional risk that high household density poses in areas where sanitation facilities are poor (Hathi, et al. 2017)."),
                                    p(id = "mainText", "This index predicts a high level of uncontained faecal waste in the southwest, with another hotspot in the northwest of the study area. The northeast and east are generally predicted to have a low level of risk. This index generates values with few areas that have a significantly higher or lower risk than the others. "),
                                    p(id= "mainText", "SRI1=(Flood risk+Road density+Terrain movement+Households sharing toilet+Main drinking water source+Toilet type+ Toilet location+Population density+Children aged under 5 per household+Rent+Tax+Open defecation)*Household density"),
                                    downloadButton("Download", style="display: block; margin: 0 auto; width: 230px;color: black;"))
                      ),
             tabPanel("SRI2", value = 1,
                      leafletOutput(outputId = "SRI2", height = 700) %>% withSpinner(type = 4),
                      absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                    draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                    width = 400, height = 600,
                                    style = "overflow-y: scroll;",
                                    #Stats on the side
                                    h1("Risk"),
                                    br(),
                                    p(id = "mainText", "We see a much greater spread in areas that are predicted to have lower rates of uncontained faecal matter in the environment. Areas of high risk are largely located in the northeast and west of the study area, whereas the southeast is predicted to be at lower risk."), 
                                    p(id = "mainText", "Similar to SRI1, this index is limited by the assumption that all of the indicators are of equal importance to one another, limiting its accuracy. To improve this, we need to be able to weight each indicator according to its importance. "),
                                    p(id = "mainText", "SRI2= Average(Flood risk+Road density+Terrain movemen+Open defecation+ Households sharing toilet+Main drinking water source+Toilet type+Population density+Household density+Toilet location+ Children aged under 5 per household+Rent+Tax)"))

             ), tabPanel("SRI3", value = 2,
                         leafletOutput(outputId = "SRI3", height = 700) %>% withSpinner(type = 4),
                         absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                       draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                       width = 400, height = 600, 
                                       style = "overflow-y: scroll;",
                                       #Stats on the side
                                       h1("Risk"),
                                       br(),
                                       p(id = "mainText", "For SRI3, we utilised the AHP method to develop weights for each indicator according to how important it was in predicting the level of uncontained faecal waste in the environment. It is clear from the weightings that the experts we consulted viewed environmental indicators - flood risk and terrain movement - as the key indicators that predict the level of uncontained faecal waste in the environment."),
                                       p(id = "mainText", "This index shows hotspots in the northeast and northwest, and areas of low risk in the east. It correlates closely to flood risk dataset, as this was the indicator that was most influential by the experts and therefore has the highest weighting. This index gives the most even spread of values across all risk values. "),
                                       p(id = "mainText", "SRI3 = (Flood risk*0.17 + Terrain Movement*0.17 + Road density*0.12 + Household density*0.10 + Population density*0.10 + Rent*0.08 + Tax*0.07 + Main drinking water source*0.04 + Children aged under 5 per household*0.04 + Open Defecation*0.04 + Households sharing toilet*0.03 + Toilet type*0.02 + Toilet location*0.02)"))
                         ),
            # Adding a download bar
             tabPanel("Download Data", value = 5,
                      sidebarPanel(
                        sidebarPanel(
                          selectInput("dataset", "Choose a dataset:",
                                      choices = c("Final Risk", "CUA5 Roads", "CUA5 Outline")),
                          
                          # Button
                          downloadButton("downloadData", "Download")
                      ))),
            useShinyjs()
             )))

# When ready to deploy, it will then ask if you want to update the original app domain. Click yes. 
# it will upload here >> https://sanitationrisk.shinyapps.io/shiny
# library(rsconnect)
# rsconnect::setAccountInfo(name='sanitation-hub', token='82EA28FA57A3CF8359DEAC9326DA0DDE', secret='UXNQCdSIUZB6Wfv7HhhiWf4Nqf+MEJ894mPJWC2s')
# rsconnect::deployApp('C:\\Users\\someg\\dev\\gather\\git\\Sanitation_Risk_App\\Shiny\\Sanitaion_Risk_App\\Sanitation_Risk_App\\Shiny', account = 'sanitation-hub')
