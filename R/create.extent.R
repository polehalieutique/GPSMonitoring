#' Tool to draw more complex extent using leaflet
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @examples
#' \dontrun{
#' library(GPSMonitoring)
#' library(dplyr)
#' library(sf)
#' data(GPSdataset)
#'
#' GPSdataset %>% mutate(filename=paste(code_village,code_engin,code_pecheur,'.gpx',sep='_')) %>%
#' arrange (filename) %>% dplyr::distinct(code_village,code_engin,code_pecheur,filename) %>%
#'  dplyr::mutate(track_fid=row_number(),track_seg_id=track_fid) %>%
#'  inner_join(GPSdataset) %>%
#'  group_by(filename) %>% arrange (filename,date_heure) %>% dplyr::mutate(track_seg_point_id = row_number()) %>%
#'  dplyr::rename(time=date_heure) %>%
#'  st_as_sf(coords = c("longitude", "latitude"), crs = 4326,remove=FALSE)->gps.all
#' emprise<-create.extent(st_convex_hull(st_union(gps.all)))
#' }
#' @export

#

create.extent<-function(gps.extent) {

  # on ne doit selecionner que les no_trajet avec observation
  library(shiny)
  library(leaflet.extras)
  library(htmlwidgets)
  runApp(list(
    # Define UI
    ui=fluidPage(
      leafletOutput("mymap",height=800),
      verbatimTextOutput("out")
      ,actionButton("quit", "Quit")
    )
    ,
    # Define server logic
    server=function(input, output,session) {

      output$mymap <- renderLeaflet(
        leaflet() %>%
          addProviderTiles("Esri.WorldImagery")  %>%
          addPolygons(data= gps.extent, color = "green") %>%
          addDrawToolbar(
            targetGroup='draw',
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
          addLayersControl(overlayGroups = c('draw'), options =
                             layersControlOptions(collapsed=FALSE))%>%
          onRender(
            "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                }"
          )
      )

      output$out <- renderText({
        if(is.null(input$hover_coordinates)) {
          "Mouse outside of map"
        } else {
          paste0("Lat: ", input$hover_coordinates[1],
                 "\nLng: ", input$hover_coordinates[2])
        }
      })

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
