ui <- fluidPage(

  tags$head(
    tags$script(src="wicket.js"),
    tags$script(src="js.js"),
    tags$style(".shiny-notification {position: fixed; top: 20% ;left: 40%} {size: width: 200px; height: 200px")
    
  ),
  

  br(),
  
  # Application title
  titlePanel(h1("InundatEd - A flood visualization resource (PRE-ALPHA Version)",align="centre"),windowTitle = "InundatEd"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("Cs") %>% withSpinner(color="#0dc5c1"),
      #actionButton("refresh1","Click to apply"),
      br(),
      br(),
      br(),
      br(),
      width=2
    ),
    mainPanel(
      fluidRow(
        column(
          12,leafletOutput("map") %>% withSpinner(color="#0dc5c1")) ### Navigation Map
        ),
      br(),
      fluidRow(
        column(
          4,
          textOutput("pointD")
        ),
      
        column(3,
               uiOutput("select")
        ),
          column(3, 
                 conditionalPanel(
                   condition='input.select == "Return Period"',
                   uiOutput("rpf")
                 ),
                 conditionalPanel(
                   condition='input.select == "Discharge"',
                   uiOutput("dis")
                   
                 )
                 
          ),
      
          column(2,
                 uiOutput("genF")
                 
          )
        
        
        
        
        ),
      
      br(),
      br(),
      fluidRow(
      
        column(12,
               
               uiOutput("tabs")
      )
      
    ),
  
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br()
  
)
)
)
