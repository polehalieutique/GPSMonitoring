#' Propose a user interface to identified portion of the tracks that could be active one.
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @examples
#' #new_trajet29<-silicoTraj(filter(R2,no_trajet==29))
#' @export
#'

silicoTraj<-function(traj,mode='map') {

#  require(shiny)

#  library(sf)
#  library(tidyr)
#  library(ggplot2)
#  library(kableExtra)
  #load('/home/jerome/PESCAOS/Rpackages/GPSMonitoring/app/Traj_obs.Rdata')

  inputinit<-traj
  inputtmp<-inputinit

  activity_plus<-''

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
      }")),titlePanel("In silico observed fishing activities using speed"),
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("ui_species"),
                        plotOutput("plot", brush = "plot_brush"),
                        actionButton("do", "Validate points"),
                        actionButton("unvalidate", "UnValidate points"),
                        actionButton("undo", "Reset to initial values"),
                        actionButton("quit", "Quit")


                      ),
                      mainPanel(

                        plotOutput("plot2"),
                        #tableOutput("data")
                      )
                    )
      ),
      server=function(input,output,session) {

        output$plot <- renderPlot({
          Sys.sleep(1)
          input$do
          input$undo
          input$unvalidate
          retour<-brushedPoints(inputtmp %>% st_drop_geometry(), input$plot_brush)
          if (activity_plus=='active') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }
          if (activity_plus=='UK') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }

          ggplot(inputtmp %>% st_drop_geometry(), aes(x=id, y=dist,color=activity_plus)) + geom_point()
        })

        output$plot2 <- renderPlot({
          Sys.sleep(2)
          input$do
          input$undo
          input$unvalidate
          retour<-brushedPoints(inputtmp %>% st_drop_geometry(), input$plot_brush)
          R2_selectionne<-inputtmp %>% dplyr::inner_join(retour)
          bb<-st_bbox(R2_selectionne)
          bbinit<-st_bbox(inputinit)
          xrangeinit<-abs(bbinit[[1]]-bbinit[[3]])
          yrangeinit<-abs(bbinit[[2]]-bbinit[[4]])
          xrange<-abs(bb[[1]]-bb[[3]])/15
          yrange<-abs(bb[[2]]-bb[[4]])/15

          xrange<-min(xrange,xrangeinit)
          yrange<-min(yrange,yrangeinit)

          if (activity_plus=='active') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }
          if (activity_plus=='UK') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

          }

          ggplot(inputtmp)+geom_sf(aes(color=activity_plus),size=1)+geom_sf(data=R2_selectionne,shape=1,size=5)+
            xlim(bb[[1]]-xrange, bb[[3]]+xrange)+
            ylim(bb[[2]]-yrange, bb[[4]]+yrange)
        })

        output$data <- renderTable({

          input$do
          input$unvalidate
          input$reset
          retour<-brushedPoints(inputtmp %>% st_drop_geometry(), input$plot_brush)
          if (activity_plus=='active') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus
            activity_plus<<-''

          }
          if (activity_plus=='UK') {

            retour$activity_plus<-activity_plus
            inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus
            activity_plus<<-''
          }
          #retour
        })


        observeEvent(input$undo, {
          inputtmp<<-inputinit
        })

        observeEvent(input$do, {
          activity_plus <<- "active"
        })
        observeEvent(input$unvalidate, {
          activity_plus <<- "UK"
        })

        observeEvent(input$quit, {
          if(input$quit > 0){
            stopApp(inputtmp)
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
      }")),titlePanel("In silico observed fishing activities using map"),br(),

                        mainPanel(
                          plotOutput("plot", brush = "plot_brush"),br(),
                          actionButton("do", "Validate points"),
                          actionButton("unvalidate", "UnValidate points"),
                          actionButton("undo", "Reset to initial values"),
                          actionButton("quit", "Quit"),br(),
                         # tableOutput("data")

                      )
        ),
        server=function(input,output,session) {

          output$plot <- renderPlot({
            Sys.sleep(1)
            input$do
            input$undo
            input$unvalidate

            ggplot(inputtmp %>% st_drop_geometry(), aes(x=longitude, y=latitude,color=activity_plus)) + geom_point()
          })



          output$data <- renderTable({
            paste("<BR/>nous sommes ici ",input$do)
            input$unvalidate
            input$undo
            retour<-brushedPoints(inputtmp %>% st_drop_geometry(), input$plot_brush)
            if (activity_plus=='active') {

              retour$activity_plus<-activity_plus
              inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

            }
            if (activity_plus=='UK') {

              retour$activity_plus<-activity_plus
              inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

            }

            #retour
          })


          observeEvent(input$undo, {
            inputtmp<<-inputinit%>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                           lat = sf::st_coordinates(.)[,2])
          })

          observeEvent(input$do, {
            activity_plus <<- "active"
          })
          observeEvent(input$unvalidate, {
            activity_plus <<- "UK"
          })

          observeEvent(input$quit, {
            if(input$quit > 0){
              stopApp(inputtmp)
            }
          })

        }
      ))

  }

}
