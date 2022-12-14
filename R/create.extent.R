#' Tool to draw more complex extent using leaflet
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @examples
#' #emprise<-create.extent(st_convex_hull(st_union(gps.all)))
#' @export

#

create.extent<-function(gps.extent) {

  # on ne doit selecionner que les no_trajet avec observation
library(shiny)
  library(leaflet.extras)
  runApp(list(
  # Define UI
  ui=fluidPage(
    leafletOutput("mymap",height=800)
    ,actionButton("quit", "Quit")
  )
,
  # Define server logic
  server=function(input, output,session) {

    output$mymap <- renderLeaflet(
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
        addPolygons(data= gps.extent, color = "green") %>%
        addDrawToolbar(
          targetGroup='draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
        addLayersControl(overlayGroups = c('draw'), options =
                           layersControlOptions(collapsed=FALSE))
    )



    observeEvent(input$quit, {
      if(input$quit > 0){
        stopApp(geojsonsf::geojson_sf(jsonify::to_json(feature, unbox = T)))
      }
    })
    observeEvent(input$mymap_draw_new_feature,{
      feature <<- input$mymap_draw_new_feature
    })

  }
))
  # Run the application

}
