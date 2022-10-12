toto<-silicoTraj(traj=R2_avec_obs_id_plus)
silicoTraj<-function(traj,no_trajet=NULL) {

require(shiny)

library(sf)
library(dplyr)
library(ggplot2)
library(kableExtra)
#load('/home/jerome/PESCAOS/Rpackages/GPSMonitoring/app/Traj_obs.Rdata')

inputinit<-traj
inputtmp<-inputinit

activity_plus<-''

toto<-shinyApp(

ui <- fluidPage( tags$style(HTML("
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
      }")),titlePanel("In silico observed fishing activities"),
                 sidebarLayout(
                   sidebarPanel(
                     uiOutput("ui_species"),
                     plotOutput("plot", brush = "plot_brush"),
                     actionButton("do", "Validate points"),
                     actionButton("undo", "Reset to initial values"),
                     actionButton("quit", "Quit")


                   ),
                   mainPanel(

                          plotOutput("plot2"),
                          tableOutput("data")
                   )
                 )
),
server <- function(input,output,session) {

  output$plot <- renderPlot({
    Sys.sleep(3)
    input$do
    input$undo
    ggplot(inputtmp %>% st_drop_geometry(), aes(x=id, y=dist,color=activity_plus)) + geom_point()
  })

  output$plot2 <- renderPlot({

    input$do
    input$undo
    retour<-brushedPoints(inputtmp %>% st_drop_geometry(), input$plot_brush)
    R2_selectionne<-inputtmp %>% inner_join(retour)
    bb<-st_bbox(R2_selectionne)
    bbinit<-st_bbox(inputinit)
    xrangeinit<-abs(bbinit[[1]]-bbinit[[3]])
    yrangeinit<-abs(bbinit[[2]]-bbinit[[4]])
    xrange<-abs(bb[[1]]-bb[[3]])/15
    yrange<-abs(bb[[2]]-bb[[4]])/15

xrange<-min(xrange,xrangeinit)
yrange<-min(yrange,yrangeinit)

ggplot(inputtmp)+geom_sf(aes(color=activity_plus),size=1)+geom_sf(data=R2_selectionne,shape=1,size=5)+
      xlim(bb[[1]]-xrange, bb[[3]]+xrange)+
      ylim(bb[[2]]-yrange, bb[[4]]+yrange)
  })

    output$data <- renderTable({
      paste("<BR/>nous sommes ici ",input$do)

    retour<-brushedPoints(inputtmp %>% st_drop_geometry(), input$plot_brush)
    if (activity_plus=='active') {

      retour$activity_plus<-activity_plus
      inputtmp[inputtmp$id %in% retour$id,]$activity_plus<<-activity_plus

    }

    retour
  })


    observeEvent(input$undo, {
      inputtmp<<-inputinit
    })

    observeEvent(input$do, {
      activity_plus <<- "active"
    })

    observeEvent(input$quit, {
      if(input$quit > 0){
        stopApp(inputtmp)
      }
    })

}
)
return(toto)
}
