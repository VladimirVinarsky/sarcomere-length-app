library(shiny)
library(vroom)
library(tidyverse)
  
ui <- fluidPage(
  titlePanel("Measure Sarcomere Length"),
  sidebarLayout(
    sidebarPanel(width = 3,
    "Upload files with sarcomere peaks",
    fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
    downloadButton("download")
    ),
    mainPanel(with=9,
      fluidRow(
      column(2, actionButton("start", "Start")),
      column(7, textOutput("name"))
      ),
      fluidRow(
      column(8, plotOutput("plot", click = "plot_click")),
      ),
    fluidRow(
      column(2, actionButton("reset", "Reset\nPeaks")),
      column(2, actionButton("previous", "Previous")),
      column(2, actionButton("forward", "Next")),
      column(2, actionButton("save", "Save")),
    ),
  fluidRow(
    tableOutput("results")
  )
    )
  )

)
server <- function(input, output, session) {
  
  # define the variables here
  file_index<-reactiveVal(integer())
  counter<-reactiveVal(integer())
  name<-reactiveVal(character())
  points<-reactiveVal(tibble())
  summary<-reactiveVal(tibble())
  peak_distance<-reactiveVal(numeric())
  df <-reactive(vroom::vroom(input$upload$datapath[[file_index()]],
                             col_names=c("x","y"),
                             skip=1))
  reset<-reactiveVal(integer())
  results<-reactiveVal(tibble())
  output$results<-renderTable(results()) 
  
  # Initialize the parameters (after clicking Start)
  observeEvent(input$start,{
    file_index(1)
    counter(0)
    name(input$upload$name[[file_index()]])
    points(tibble(Peak=integer(),X=numeric()))
    summary(tibble(dist=numeric(),peaks=integer()))
    results(tibble(Sample=character(), Dist=numeric(),Peaks=integer()))
    reset(0)
  })

  # Control of moving files forward/previous
  observeEvent(input$forward, {
    if (file_index()<nrow(input$upload)){
    file_index(file_index()+1)}else{}
  })
  observeEvent(input$previous, {
    if (file_index()>1){
    file_index(file_index()-1)}else{}
  })
  
  # keep the name updated by the file index
  observeEvent(file_index(),
               name(input$upload$name[[file_index()]]))
  
  # Counter of peaks (clicks)
  observeEvent(input$plot_click,{ counter(counter()+1) } )
  
  # Reset the counter of peaks (clicks) when start/reset/previous/forward 
  observeEvent(input$start,
               reset(reset()+1))
  observeEvent(input$reset,
               reset(reset()+1))
  observeEvent(input$previous,
               reset(reset()+1))
  observeEvent(input$forward,
               reset(reset()+1))
  
  observeEvent(req(reset()>0), {
    points(tibble(Peak=integer(),X=numeric()))
    summary(tibble(Dist=numeric(),Peaks=integer()))
    counter(0)
  })
  
  # Text Output File name
  output$name<-renderText(name()) 
  
  # Plot
  output$plot <- renderPlot({
    req(input$start)
    plot()}, res=96 )
  
  plot<-reactive({ ggplot(df(), aes(x, y)) + 
      geom_point()+
      geom_vline(xintercept = points()$X)
  })
  
  
  # List of peaks generated with clicking (not to be displayed)
  observeEvent(input$plot_click$x, {
    points(points() %>%
             add_row(Peak = as.integer(counter()), X = input$plot_click$x))
  })
  
  # Table with summary for each plot derived from points() (not to be displayed)
  observeEvent(req(counter()>=2),     # to start working only at two peaks
               summary(points() %>%
                         summarize(Dist = (X[which.max(Peak)] -X[which.min(Peak)])/(nrow(points())-1),
                                   Peaks = nrow(points())) %>% 
                         mutate(Sample=name()) %>% 
                                  select(Sample,Dist,Peaks) ) 
  )
  
 
  # Table of results (one summary for each file) gets updated after save
 output$results<-renderTable(results())
   
  observeEvent(input$save,{
    results(results() %>%  add_row(summary()))
  })
  
  
  # Button to download
  output$download <- downloadHandler(
    filename = function() {
      paste0( "sarcomere_results_export.csv")
    },
    content = function(file) {
      write_csv(results(), file)
    }
  )
  
}


shinyApp(ui, server)