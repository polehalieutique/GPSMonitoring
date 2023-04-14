#' Propose a user interface to identified portion of the tracks that could be active one.
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @examples
#' \dontrun{
#' data(R2)
#' new_trajet29<-silicoTraj(dplyr::filter(R2,no_trajet==29),mode='speed')
#'
#' new_trajet29<-silicoTraj(dplyr::filter(R2,no_trajet==29),mode='map')
#' }
#' @export
#'

silicoTraj<-function(traj,mode='map') {

#  require(shiny)

  library(sf)
  library(tidyr)
  library(ggplot2)
  library(kableExtra)
  library(shiny)
  library(leaflet)


  inputinit<-st_drop_geometry(traj)
  inputtmp<-inputinit
  colours <- c("active" = "green", "UK" = "red")
  activity_plus<-''
x_range<-c(-999,-999)
y_range<-c(-999,-999)
  if (mode=='speed')
    {
    runApp(
    list(
      ui=fluidPage( tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
      }
      /* Change font of header text */
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-frame{height:1800px;}
      /* Make text visible on inputs */
      .shiny-input-container {
        color: white;
      }")),titlePanel(paste("In silico observed fishing activities using speed for traj ",unique(traj$no_trajet),sep='')),
                    sidebarLayout(
                      sidebarPanel(
                        plotOutput("plot", brush = "plot_brush"),
                        actionButton("do", "Validate points"),
                        actionButton("unvalidate", "UnValidate points"),
                        actionButton("carte", "Plot map"),
                        actionButton("quit", "Quit")


                      ),
                      mainPanel(

                       #plotOutput("plot2"),
                       #br(),
                       leafletOutput("endynamique")

                        #tableOutput("data")
                      )
                    )
      ),
      server=function(input,output,session) {

        R2_selectionne <- reactive({
          input$do
          # Add a little noise to the cars data so the points move
          retour<-brushedPoints(inputtmp, input$plot_brush)
          inputtmp %>% dplyr::inner_join(retour)
        })

        R2_selectionne <- reactive({
          input$unvalidate
          # Add a little noise to the cars data so the points move
          retour<-brushedPoints(inputtmp, input$plot_brush)
          inputtmp %>% dplyr::inner_join(retour)
        })



        output$plot <- renderPlot({
          #Sys.sleep(1)

          input$do
          input$unvalidate
          retour<-brushedPoints(inputtmp , input$plot_brush)


          if (activity_plus=='active' & dim(retour)[1]>0) {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }
          if (activity_plus=='UK'  & dim(retour)[1]>0) {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }

          ggplot(inputtmp, aes(x=id, y=dist,color=activity_plus)) + geom_point()+
            scale_color_manual(values = colours)
        })

        observeEvent(input$carte,{


          retour<-brushedPoints(inputtmp, input$plot_brush)

          R2_selectionne<-inputtmp %>% dplyr::inner_join(retour)


          if (activity_plus=='active') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }
          if (activity_plus=='UK') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }
          #output$plot2 <- renderPlot({
          #  ggplot(inputtmp)+geom_point(aes(x=longitude,y=latitude,color=activity_plus),size=1)+
          #    scale_color_manual(values = colours)

          #})
          output$endynamique<-renderLeaflet({
            traj %>% st_coordinates() %>% st_linestring() %>% st_zm(drop = T, what = "ZM")->ligne
            pal <- colorFactor(c("green", "red"), domain = c("active", "UK"))
            leaflet(inputtmp) %>%
              addProviderTiles("Esri.WorldImagery")  %>%
              addPolylines(data=ligne,weight = 1,col='orange') %>%
              addCircleMarkers(~longitude,~latitude,radius=1,color=~pal(activity_plus),popup = ~paste0(id,'-',date))
          })

        })



        observeEvent(input$do, {
          if(input$do > 0) {activity_plus <<- "active"}
        })
        observeEvent(input$unvalidate, {
          if(input$unvalidate > 0) {activity_plus <<- "UK"}
        })

        observeEvent(input$quit, {
          if(input$quit > 0){
            stopApp(inputtmp %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326,remove=FALSE))
          }
        })
      }
    ))
  }else
  {
    runApp(
      list(
        ui=fluidPage( tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
      }
      /* Change font of header text */
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-frame{height:1800px;}
      /* Make text visible on inputs */
      .shiny-input-container {
        color: white;
      }")),titlePanel(paste("In silico observed fishing activities using map ",unique(traj$no_trajet),sep='')),br(),

                        mainPanel(
                          plotOutput("plot", brush = "plot_brush"),br(),
                          actionButton("do", "Validate points"),
                          actionButton("unvalidate", "UnValidate points"),
                          actionButton("zoom", "Zoom"),
                          actionButton("unzoom", "UnZoom"),
                          actionButton("quit", "Quit"),br(),
                         # tableOutput("data")

                      )
        ),
        server=function(input,output,session) {

          output$plot <- renderPlot({
            input$do
            input$unvalidate
            input$zoom
            input$unzoom

            g1<-ggplot(inputtmp, aes(x=longitude, y=latitude,color=activity_plus)) + geom_point()+
                scale_color_manual(values = colours)

            if (x_range[1]==-999) {g1} else
            {
            g1+xlim(x_range)+ylim(y_range)

            }



          })


          observeEvent(input$undo, {
            inputtmp<<-inputinit%>% dplyr::mutate(lon = longitude,
                                           lat = latitude)
          })

          observeEvent(input$do, {

            activity_plus <<- "active"
            retour<-brushedPoints(inputtmp , input$plot_brush)
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus
          })
          observeEvent(input$unvalidate, {
            activity_plus <<- "UK"
            retour<-brushedPoints(inputtmp , input$plot_brush)

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus
          })

          observeEvent(input$zoom, {
            retour<-brushedPoints(inputtmp , input$plot_brush)
            x_range<<-c(min(retour$longitude),max(retour$longitude))
            y_range<<-c(min(retour$latitude),max(retour$latitude))
          })
          observeEvent(input$unzoom, {
            x_range<<-c(-999,-999)
            y_range<<-c(-999,-999)
          })

          observeEvent(input$quit, {
            if(input$quit > 0){
              stopApp(inputtmp %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326,remove=FALSE))
            }
          })

        }
      ))

  }

}
