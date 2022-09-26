library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(geosphere)
library(rnaturalearth)
library(ggspatial)
library(elementalist)
library(ggbump)


#Collecting each year of Data
SARH_2015_16 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2015_16",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2016_17 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2016_17",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2017_18 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2017_18",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2018_19 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2018_19",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2019_20 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2019_20",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2020_21 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2020_21",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
#Combining All Togerher
SARH_2015_2021 <- rbind(SARH_2015_16,SARH_2016_17,SARH_2017_18,SARH_2018_19,SARH_2019_20,SARH_2020_21)
#Adding Base Locations
Caernarfon <- mutate(filter(SARH_2015_2021, base =="Caernarfon"), HeliType = "S92", BLat = 53.100824, BLong = -4.335326,callsign="936")
Humberside <- mutate(filter(SARH_2015_2021, base =="Humberside"), HeliType = "S92", BLat = 53.576608, BLong = -0.338628,callsign="912")
Inverness <- mutate(filter(SARH_2015_2021, base =="Inverness"), HeliType = "AW189", BLat = 57.538000, BLong = -4.047985,callsign="151")
LeeOnSolent <- mutate(filter(SARH_2015_2021, base =="Lee On Solent"), HeliType = "AW189", BLat = 50.811413, BLong = -1.208161,callsign="175")
Lydd <- mutate(filter(SARH_2015_2021, base =="Lydd"), HeliType = "AW189", BLat = 50.958359, BLong = 0.933960,callsign="163")
Newquay <- mutate(filter(SARH_2015_2021, base =="Newquay"), HeliType = "S92", BLat = 50.443431, BLong = -4.993911,callsign="924")
Prestwick <- mutate(filter(SARH_2015_2021, base =="Prestwick"), HeliType = "AW189", BLat = 55.511174, BLong = -4.581976,callsign="199")
StAthan <- mutate(filter(SARH_2015_2021, base =="St Athan"), HeliType = "AW189", BLat = 51.400839, BLong = -3.439028,callsign="187")
Stornoway <- mutate(filter(SARH_2015_2021, base =="Stornoway"), HeliType = "S92", BLat = 58.219136, BLong = -6.326690,callsign="948")
Sumburgh <- mutate(filter(SARH_2015_2021, base =="Sumburgh"), HeliType = "S92", BLat = 59.876695, BLong = -1.296892,callsign="900")
#Recombining
SARH_2015_2021 <- rbind(Caernarfon,Humberside,Inverness,LeeOnSolent,Lydd,Newquay,Prestwick,StAthan,Stornoway,Sumburgh)
#Adding Mission Distance
SARH_2015_2021 <- SARH_2015_2021 %>% rowwise()%>% mutate(distance=distm(c(BLong,BLat),c(longitude,latitude))/1000)
#Putting in date order
SARH_2015_2021 <- SARH_2015_2021%>%arrange(date)
#Renaming tasking outcome variables
SARH_2015_2021 <- SARH_2015_2021%>%mutate(tasking_outcome=ifelse({tasking_outcome=="Completed"|tasking_outcome=="Complete"|tasking_outcome=="Complete (Nothing Found)"|tasking_outcome=="Supported & Completed"},{"Completed"},{"Terminated"}))
#Renaming type of tasking 
SARH_2015_2021$type_of_tasking <- recode_factor(SARH_2015_2021$type_of_tasking, "Search (only)"="Search", "Aborted/Not Required"="Support", "Pre-arranged transfer"="Pre-arranged Transfer", 
                                                "Rescue/ Recovery"="Rescue/Recovery", "Search (Only)"="Search", "Search only"="Search", "Search Only"="Search")
#Creating Factors
SARH_2015_2021 <- SARH_2015_2021%>%mutate(tasking_outcome=as.factor(tasking_outcome),tasking_location=as.factor(tasking_location))



library(leaflet)
library(htmltools)
library(shiny)


base_options <- c("All",unique(SARH_2015_2021$base))
year_options <- c("All",unique(year(SARH_2015_2021$date)))


ui <- fluidPage(
  div(clas="outer",tags$head(includeCSS("style.css"))),
  leafletOutput("mymap", width = "100%", height = 1100),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto", 
                h2("Filters"),
                selectInput("base","Base",base_options, "Humberside"),
                selectInput("year","Year",year_options, "2015"),
                plotOutput("tasking_count"),
                plotOutput("tasking_type")),
  
)


server <- function(input,output,sessin){
  

  
  base_select <- reactive(ifelse({input$base == "All"},{map(c("base%in%c(base_options)"),rlang::parse_expr)},{map(c(sprintf("base=='%s'",input$base)), rlang::parse_expr)}))
  year_select <- reactive(ifelse({input$year == "All"},{map(c("year(date)%in%c(year_options)"),rlang::parse_expr)},{map(c(sprintf("year(date)=='%s'",input$year)), rlang::parse_expr)}))
  
  
  data_in_range <- reactive({    
    bounds <- input$mymap_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    as.data.frame(SARH_2015_2021)%>%filter(!!!base_select())%>%filter(!!!year_select())%>%
      filter(latitude >= latRng[1] & latitude <= latRng[2] & longitude >= lngRng[1] & longitude <= lngRng[2])
    
  })
  
  output$tasking_count <- renderPlot({
    data_in_range()%>%
      ggplot(aes(y=fct_rev(fct_infreq(base))))+
      geom_bar(fill = "#5D948C")+
      theme_classic()+
      labs(x="Base",y="Count",title="Tasking Count")
    
  })
  
  output$tasking_type <- renderPlot({
    data_in_range()%>%group_by(type_of_tasking)%>%count()%>%
      ggplot(mapping=aes(x="",y =n, fill= type_of_tasking))+
      geom_col()+
      scale_fill_brewer(palette="Paired")+
      coord_polar(theta = "y")+
      geom_label(aes(label = n), position = position_stack(vjust = 0.5), show.legend = FALSE)+
      theme_classic()%+replace%theme(axis.line = element_blank(), axis.text = element_blank(),axis.title = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))+
      labs(title = "Tasking Types", fill = "Type")
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    leaflet(as.data.frame(SARH_2015_2021)%>%filter(!!!base_select())%>%filter(!!!year_select()))%>%
      addTiles()%>%addMarkers(lng = ~longitude, lat = ~latitude, popup = ~paste(base, "<br>", type_of_tasking))
    
  })
}

shinyApp(ui,server)
