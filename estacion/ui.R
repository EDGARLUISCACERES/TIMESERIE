ui <- shinyUI(function(req){ fluidPage(
  
  includeCSS("www/styles.css"),
  
  titlePanel("Evolución del parámetro en el tiempo"), 
  
  fluidRow(
    
    tags$head(
      tags$script(src = "dygraph-extra.js")
    ),
    
    column(9,
           
           tabsetPanel(type = "tabs",
                       
                       tabPanel("Variación temporal", br(), dygraphOutput("dygraph"), br(), includeMarkdown("textomarkdown.md")),
                       
                       tabPanel("Tabla de datos", br(), DT::dataTableOutput("table"))
                       
           ) # Cierra tabsetpanel
           
    ), # Cierra column(9, 
    
    column(3, 
           
           wellPanel(
             
             uiOutput("moreControls")
             
           ) # Cierra wellPanel
           
    ) # Cierra column(3,
    
  ) # Cierra fluidRow
  
)}) # Cierra fluidPage y shinyUI