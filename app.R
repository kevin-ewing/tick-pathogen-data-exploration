library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(rgdal)
library(shinythemes)
library(sf)
library(rgeos)
library(leaflet.minicharts)

########### Imports ########### 

sitePathogen<-read_csv("data/tick/wrangledPathogen.csv")

sitePathogen<-sitePathogen%>%
  mutate(Negative = 1-`Borrelia burgdorferi sensu lato`,
         Posative = `Borrelia burgdorferi sensu lato`)

domainPathogen<-read_csv("data/tick/domainPathogen.csv")
CDCdata<- read_csv('data/tick/LDcdcData.csv')

usCounties<-readOGR('data/location/county.json')
usDomains<-readOGR('data/location/NEONDomains')

#simplifying usCounties and turning into sf file

usCounties@data<-usCounties@data%>%
  mutate(STATE = as.double(STATE),
         COUNTY = as.double(COUNTY))

usCounties@data<-left_join(usCounties@data,
                      CDCdata,
                      by = c("STATE"="STCODE",
                             "COUNTY"="CTYCODE"))

usDomains@data<-usDomains@data%>%
  left_join(domainPathogen,
            by = c("DomainID" = "domainID"))

countyColors <- colorBin("YlGn",
                         domain = usCounties@data$Cases2001,
                         bins = c(0,0.01,0.05,0.1,0.25,0.5,1,2.5,5,20))


########### UI ########### 
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Tick Pathogens"),
  hr(),
  
  tabsetPanel(
    
    tabPanel("CDC vs. NEON", 
             br(),
             p("This page allows users to explore how NEON site tick-borne pathogen rates correlate
               to CDC human incident for tick-borne diseases data. Press on a NEON site to further explore
               its rate of Borrelia burgdorferi sensu lato. Note that this data represents the CDC's 2018
               sampling for tick-borne disease cases."),
             leafletOutput(outputId = "countyMap"),
             p("Select a site above to learn more about the rate of Borrelia burgdorferi sensu lato and prevalence of Borrelia burgdorferi sensu lato."),
             #plotOutput(outputId = "countyGraph"),
             br()
    ),
    
    tabPanel("Lyme Over Time",
             br(),
             p("This page allows users to explore how the CDC's incedence rate of Lyme disease changes over time.
               Data was only easily accessible from the years 2000-2018. Note that each state reports Lyme cases
               independently which might result in a skewed distribution as mentioned on the info tab."),
             sliderInput(inputId = "year",
                         label = "Select a Year", 
                         min = 2000, 
                         max = 2018,
                         value = 2018,
                         sep = ""),
             actionButton(inputId = "enterBot",
                          label = "Enter"),
             leafletOutput(outputId = "timeMap"),
             br()
    ),
    
    tabPanel("Other Pathogens", 
             br(),
             p("This final page allows users to explore the rates of other tick-borne pathogens as tested by
               NEON sites from 2014-2018. These sites are then grouped into thier domains to show the overall geographical
               distribution of tick-borne diseases."),
             selectInput(inputId = "Species",
                         label = "Choose a Species",
                         choices = c("IXOSCA","AMBAME"), 
                         selected='IXOSCA'), 
             selectInput(inputId = "pathogen",
                         label = "Select a Pathogen:",
                         choices = c("Anaplasma phagocytophilum","Babesia microti","Borrelia burgdorferi sensu lato","Borrelia mayonii","Borrelia miyamotoi","Borrelia sp.","Ehrlichia chaffeensis","Ehrlichia ewingii"),
                         selected = "Borrelia burgdorferi sensu lato"),
             actionButton(inputId = "submit",
                          label = 'Apply Changes'),
             br(),
             leafletOutput(outputId = "pathMap"),
             p("Hover over a domain above to learn more about the rate of the selected pathogen.")
             # plotOutput(outputId = "barplot"),
             # br(),
    ),
  
  tabPanel("More Info", 
           br(),
           p("This app allows users to look at both the NEON tick-borne pathogen 
               data and CDC human incidence data for tick-borne diseases. If you wish
               to explore how NEON tick-borne pathogen rates correlate to CDC incident data,
               select the first tab. The second tab compares these CDC incidence rates of tick-borne
               diseases over time. The final tab lets you explore other tick-borne pathogen rates
               across the different NEON domains that sampled for pathogens."),
           br(),
           p("Reports of Lyme disease are collected and verified by state and local health departments in accordance with their legal mandate and surveillance practices. 
               After removal of personal identifiers, selected information on cases is shared with CDC."),
           br(),
           p("Note: There exists limitations regarding the surveillance dataset obtained:"),
           p("1. Under-reporting and misclassification."),
           p("2. data depends on each state’s abilities to capture and classify cases, which might dependent on available budget."),
           p("3. Data are captured by county of residence, not county of exposure."),
           p("4. States may close their annual surveillance dataset at a different time than CDC."),
           p("5. Following its implementation in 1991, the national surveillance case definition for Lyme disease was modified in 1996, 2008, 2011, and again in 2017.")
  ))
  
)

server <- function(input, output, session) {
  
  
  ########### TAB 1 ########### 
  output$countyMap <- renderLeaflet({
    bins<-c(0,0.01,0.05,0.1,0.25,0.5,1,2.5,5,20)
    countyColors <- colorBin("YlGn",
                             domain = usCounties@data$Cases2018,
                             bins = bins)
    
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 1){
      colorAdditions <- paste0(colors, "; border-radius: 100%; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
      
      return(addLegend(map, title = "Ixodes Scapularis Density ",colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = "bottomleft"))
    }
    minichartColors = c("#707070","#4c76ad")
    
    
    
    usCounties%>%
      leaflet()%>%
      addPolygons(fillColor =~countyColors(Cases2018),
                  fillOpacity =~.8,
                  color = "black",
                  opacity = 1,
                  weight = .25)%>%
      setView(-80, 37.8, 4)%>%
      addTiles()%>%
      addLegend(position = "bottomright",
                pal = countyColors,
                values = usCounties@data$Cases2018,
                title = "CDC Lyme Disease Cases Per 1000 people")%>%
      addMinicharts(chartdata = sitePathogen[,c("Negative","Posative")],
                    type = "pie",
                    lat = sitePathogen$field_latitude,
                    lng = sitePathogen$field_longitude,
                    width = sitePathogen$mean_density*2000,
                    layerId = sitePathogen$siteID,
                    colorPalette= minichartColors,
                    opacity = 1,
                    popup = popupOptions(html = sprintf(
                      "<strong>%s</strong><br />Tick Density: %.2g <br/>Infected Percent: %.4g&#37",
                      sitePathogen$siteID, 
                      sitePathogen$mean_density,
                      sitePathogen$Posative*100) %>% lapply(htmltools::HTML)),
                    legend = FALSE)%>%
      addLegend("bottomleft",
                colors = minichartColors, 
                opacity = 1,
                labels = c("Negative", "Posative"),
                title = "NEON Site tests for Borrelia burgdorferi"
      )%>%
      addLegendCustom(colors = c("#707070", "#707070", "#707070"),
                      labels = c("0.035", "0.020", "0.005"), 
                      sizes = c(65, 38, 10))
 
  
    
    
    
    
     })
  
  
  ########### TAB 2 ########### 
  output$timeMap <- renderLeaflet({
    
    usCounties%>%
      leaflet()%>%
      addTiles()%>%
      setView(-80, 37.8, 4)%>%
      addLegend(pal = countyColors,
                values = usCounties@data$Cases2001,
                title = "CDC Lyme Disease Cases Per 1000 People")%>%
      addPolygons(fillColor =~countyColors(get(paste("Cases", 2018, sep = ""))),
                  fillOpacity =~.8,
                  color = "black",
                  opacity = 1,
                  weight = .25,
                  label = sprintf(
                    "<strong>%s %s, %s:</strong><br/>Cases per 1000 people: %g",
                    usCounties$NAME, 
                    usCounties$LSAD,
                    usCounties$Stname,
                    usCounties$Cases2018
                    ) %>% lapply(htmltools::HTML))
    
  })
   #usCounties[,paste("Cases", 2018, sep = "")]
  
  observe({
    input$enterBot
    
    leafletProxy(mapId = "timeMap", data = usCounties$data) %>%
      clearShapes()%>%
      addPolygons(data = usCounties,
                  fillColor =~countyColors(get(paste("Cases", isolate(input$year), sep = ""))),
                  fillOpacity =~.8,
                  color = "black",
                  opacity = 1,
                  weight = .25,
                  label = sprintf(
                    "<strong>%s %s, %s:</strong><br/>Cases per 1000 people: %g",
                    usCounties$NAME, 
                    usCounties$LSAD,
                    usCounties$Stname,
                    usCounties@data[,paste("Cases", isolate(input$year), sep = "")]
                    ) %>% lapply(htmltools::HTML))
    
  })
  
  
  
  ########### TAB 3 ########### 
  observeEvent(input$Species, {
    if(input$Species == "IXOSCA"){
      updateSelectInput(session,
                        inputId = "pathogen",
                        choices = c("Anaplasma phagocytophilum","Babesia microti","Borrelia burgdorferi sensu lato","Borrelia mayonii","Borrelia miyamotoi","Borrelia sp.","Ehrlichia chaffeensis","Ehrlichia ewingii"),
                        selected = "Borrelia burgdorferi sensu lato")
      
    }
    else if(input$Species == "AMBAME"){
      updateSelectInput(session,
                        inputId = "pathogen",
                        choices = c("Borrelia lonestari","Ehrlichia ewingii"),
                        selected = "Borrelia lonestari")
    }
  })
  
  output$pathMap <- renderLeaflet({
    input$submit
    
    bins <- c(0.0, 0.001, 0.002, 0.005, 0.01, 0.05, 0.1, 0.20, 0.25, 0.3)
    pathogenColors <- colorBin(palette = "YlGn",
                               domain = 0.3,
                               bins = bins)
    
    
    usDomains%>%
      leaflet()%>%
      setView(-80, 37.8, 4)%>%
      addTiles()%>%
      addPolygons(opacity = .15,
                  color = "black",
                  weight = 3,
                  fillOpacity = .6,
                  fillColor =~pathogenColors(get(isolate(input$pathogen))),
                  layerId = ~DomainID,
                  label = sprintf(
                    "<strong> <i>%s </i>rate:</strong><br/> Domain: %s <br/> Rate: %g",
                    isolate(input$pathogen), 
                    usDomains$DomainName,
                    usDomains@data[,isolate(input$pathogen)]) %>% lapply(htmltools::HTML))%>%
      addLegend(pal = pathogenColors,
                values =~pathogenColors(get(isolate(input$pathogen))),
                title = "Rate of selected pathogen",
                position = "bottomright")
    
    
  })
}

shinyApp(ui, server)