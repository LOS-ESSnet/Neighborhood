#' @author : François Sémécurbe, Nicolas Chareyre, Saïd Khadraoui, 
#'           Arnaud Degorre, Farida Marouchi

library(shiny)
library(SPARQL)
library(leaflet)
library(RColorBrewer)
library(geosphere)
library(opencage)
library(sf)
library(jsonlite)

options(encoding = "UTF-8")

points <- data.frame()
first <- TRUE

findCoordinate <- function(adresse){
  print(paste0("Dans findCoordinate"))
  query_adresse = opencage_forward(adresse
                                   , key = "d61aae4ec0d64491ab669fac6755a038"
                                   , countrycode = "FR")
  adresse_mercator = data.frame(lat = as.integer(query_adresse$results$annotations.Mercator.x[1])
                                , lng = as.integer(query_adresse$results$annotations.Mercator.y[1]))
  adresses_wgs84 =st_coordinates(st_transform(st_as_sf(adresse_mercator
                                                       , coords = c("lat","lng")
                                                       , crs = 54004), 4326))
  return(adresses_wgs84)
}

calcul_score <- function(data1,data2)
{
  dist = matrix(0,nrow(data1),nrow(data2))
  for (i in 1:nrow(data1))
  {
    for (j in 1:nrow(data2))
    {
      dist[i,j]=distGeo(c(data1[i,1],data1[i,2]),c(data2[j,1],data2[j,2]))
    }
  }
  minimum_mean = mean(apply(dist,1,min))
  score=(exp(-minimum_mean/1000))*100
  return(score)
}



querySiren <-  function(lat, long, dist, naf){ 
  
  endpoint_and_query <- read_json("endpoint_query.json", flatten=TRUE)
  endpoint = endpoint_and_query[[1]]
  query = endpoint_and_query[[2]]
  query <- sprintf(query , lat, long, paste0(dist, "km"), naf)
  
  resultBrut <- SPARQL(endpoint,query)
  dataBrut <- resultBrut$results
  if(nrow(dataBrut) > 0){
    dataBrut$print <- paste(dataBrut$numeroSiret, " - ", dataBrut$enseigne
                            , " - ", dataBrut$RS, " - ", dataBrut$apet)
    colnames(dataBrut) <- c("sirene", "latitude", "longitude", "apet", "numeroSiret"
                            , "enseigne", "RS", "print")
  }
  return(dataBrut)
}

ui <- fluidPage(
  
  titlePanel("Do you really know your neighborhood ?")
  , h4("Application made by François Sémécurbe, Nicolas Chareyre, Saïd Khadraoui, Arnaud Degorre and Farida Marouchi.")
  , h4("Source : Insee, Base Sirene des entreprises et de leurs établissements")
  , h4(" ")
  , sidebarLayout(
    sidebarPanel(
      textInput("adresse", "Adresse : ", value = "88 avenue Verdier, Montrouge")
      , selectInput("naf"
                    , "Choice :"
                    , choices = c("bakeries" = "1071C"
                                  ,"schools" = "8520Z"
                                  , "banks" = "6419Z" )
                    , selected = "bakeries")
      , sliderInput("rayon",
                    "Radius :",
                    min = 100,
                    max = 300,
                    value = 100)
      , actionButton("check", "Check !")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("verbatim")
      , leafletOutput("carte")
    )
  )
)

server <- function(input, output) {
  
  datasetInput <- eventReactive(input$check, {
    points
  }, ignoreNULL = FALSE)
  
  resetInput <- eventReactive(input$reset, {
    print("Clique bouton reset")
    TRUE
  }, ignoreNULL = FALSE)
  
  output$carte <- renderLeaflet({
    click <- input$carte_click
    coordinate <- findCoordinate(input$adresse)
    latitude <- coordinate[2]
    longitude <- coordinate[1]
    if(is.null(click)){
      previousClick <<- click
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik,
                         options = providerTileOptions(noWrap = TRUE))%>% 
        addCircles(lat = as.numeric(latitude)
                   , lng = as.numeric(longitude)
                   , radius = input$rayon)
    }
    else { 
      listePoints <- datasetInput()
      print(paste0("Nb points :", nrow(points)))
      print(paste0("Nb listePoints :", nrow(listePoints)))
      print(paste0("First : ", first))

      if(nrow(listePoints) == 0 && nrow(points) == 0 && !first){
        first <<- TRUE
        points <<- data.frame()
        previousClick <<- click
        leaflet() %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik,
                           options = providerTileOptions(noWrap = TRUE))%>%
          addCircles(lat = as.numeric(latitude)
                     , lng = as.numeric(longitude)
                     , radius = input$rayon)
      }
      else if(nrow(listePoints) == 0 || nrow(points) == 0){
          if(is.null(previousClick) || (previousClick$lat != click$lat)|| (previousClick$lng != click$lng)){
            points <<- rbind(points, list("latitude" =  as.numeric(click$lat)
                                          , "longitude" = as.numeric(click$lng)))
            colnames(points) <- c("latitude", "longitude")
          }
        previousClick <<- click
        first <<- FALSE
        leaflet(data = points) %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik,
                           options = providerTileOptions(noWrap = TRUE))%>% 
          addCircles(lat = as.numeric(latitude)
                     , lng = as.numeric(longitude)
                     , radius = input$rayon) %>%
          addMarkers(~longitude, ~latitude )
      } else {
        data <- querySiren(as.numeric(latitude)
                           , as.numeric(longitude)
                           , input$rayon/1000
                           , input$naf)
        if(nrow(data) > 0) data$color <- "green"
        points$color <- "red"
        print <- vector()
        for(i in 1:nrow(points)){
          print <- c(print, paste0("click ", i))
        } 
        points$print <- print
        if(nrow(data) > 0) {
          dataAll <- rbind(data[, c("latitude", "longitude", "print", "color")]
                         , points[, c("latitude", "longitude", "print", "color")]) 
        } else {
          dataAll <- points[, c("latitude", "longitude", "print", "color")]
        }
        points <<- data.frame()
        previousClick <<- click
        icons <- awesomeIcons(markerColor = dataAll$color)
        leaflet(dataAll) %>% addTiles() %>%
          addCircles(lat = as.numeric(latitude)
                     , lng = as.numeric(longitude)
                     , radius = input$rayon) %>%
          addAwesomeMarkers(~longitude, ~latitude
                            , icon = icons
                            , label = ~as.character(print))
      }
    }
  })
  
  output$verbatim <- renderText({
    listePoints <- datasetInput()
    coordinate <- findCoordinate(input$adresse)
    latitude <- coordinate[2]
    longitude <- coordinate[1]
    data <- querySiren(as.numeric(latitude)
                       , as.numeric(longitude)
                       , input$rayon/1000
                       , input$naf)
    if(nrow(listePoints) > 0){
      print(colnames(listePoints))
      print(colnames(data))
      score <- calcul_score(data1 = listePoints[,c("longitude", "latitude")]
                            , data2 = data[,c("longitude", "latitude")])
      print("Liste des points cliqués")
      print(listePoints)
      paste0("Score : "
             , format(score, digits = 3, nsmall = 0, decimal.mark = "," ) 
             , " / 100")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


