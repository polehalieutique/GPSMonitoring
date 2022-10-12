#Define server function
server <- function(input, output) {
  dfStates <- readCensus()
  dfStates <- dfStates[dfStates$stateName != "District of Columbia",]
  dfStates$region <- state.region
  dfStates$stateName <- tolower(dfStates$stateName)
  dfStates$popChange <- dfStates$july11pop - dfStates$july10pop
  dfStates$percentChange <- dfStates$popChange/dfStates$july10pop * 100
  output$scatterplot <- renderPlot({
    ggplot(dfStates, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha)
  })
}


#read in the census data set
readCensus <- function(){
  urlToRead <-
    "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  #clean data set
  testFrame <- read.csv(url(urlToRead))
  testFrame <- testFrame[-1:-8,]
  testFrame <- testFrame[,1:5]
  testFrame$stateName <- testFrame[,1]
  testFrame <- testFrame[,-1]
  testFrame <- testFrame[-52:-58,]
  testFrame$stateName <- gsub("\\.","",testFrame$stateName)
  #convert columns to numbers
  testFrame$april10census <- gsub(",", "", testFrame$X)
  testFrame$april10base <- gsub(",", "", testFrame$X.1)
  testFrame$july10pop <- gsub(",", "", testFrame$X.2)
  testFrame$july11pop <- gsub(",", "", testFrame$X.3)
  testFrame$april10census <- as.numeric(gsub(" ", "", testFrame$april10census))
  testFrame$april10base <- as.numeric(gsub(" ", "", testFrame$april10base))
  testFrame$july10pop <- as.numeric(gsub(" ", "", testFrame$july10pop))
  testFrame$july11pop <- as.numeric(gsub(" ", "", testFrame$july11pop))
  testFrame <- testFrame[,-1:-4]
  #remove the row names
  rownames(testFrame) <- NULL
  return(testFrame)
}