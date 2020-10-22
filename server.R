function(input,output,session) {
  ## Credit Text
  output$credit <- renderText({
    c <- "Created by the Spatial Lab, Wilfrid Laurier University."
    c
  })
  
  ## Disclaimer Text
  output$disclaimer <- renderText({
    d <- "Disclaimer: This is a PRE-ALPHA Version."
  })
  
  
  ## Catchment Selection
  output$Cs <- renderUI({
    radioButtons(inputId = "Cs",label = "Step 1: Please select a watershed",choices = c("Grand River Watershed", "Ottawa River Watershed"),selected="Grand River Watershed")
  })
  
  ## Empty Placeholder Leaflet Map N
  output$map <- renderLeaflet({
    
    leaflet() %>%
      hideGroup("polysG") %>%
      hideGroup("polysO")%>%
      addGeoJSONv3(geojson = catchGround,highlightOptions = highlightOptions(
        weight=0, stroke=T,color="red",
        fillOpacity=0.3, opacity =1,
        bringToFront=TRUE, sendToBack=TRUE), stroke = T, fill = T, color = "black", fillColor = "grey", opacity = 1, fillOpacity = 0.01, group = "polysG", weight = 1, layerId = 'cat_id')%>%
      addTiles(options = tileOptions(minZoom=8))
                 
  })
  
  ## Initialize placeholder catid variable and depth value
  vglob <<- reactiveValues(catidOld = -12,d=-100)
  
  ## Initialize flood generation status (false means flood raster not generated)
  status <<- reactiveValues(s = FALSE)

  
  
  
  
  ############################### WATERSHED SELECTION ################################################################
  
  observeEvent(input$Cs,{### Catchment selection action button, refreshes map and zooms to catchment extent
    ## Render map based on catchment selection
    
    withProgress(message = 'Loading catchment - please wait', 
                 min=0,max=20,detail=NULL,
                 
                 
                 {
                   for (i in 1:20) {
                     incProgress(1/20)
                   }
    
    if (input$Cs == "Grand River Watershed"){
      
      leafletProxy("map") %>%
        hideGroup("polysO") %>%
        showGroup("polysG") %>%
        fitBounds(lng1 =-80.9496,lat1 = 42.8507,lng2 = -79.49012,lat2 = 44.22597) %>%
        setMaxBounds(lng1 =-80.9496,lat1 = 42.8507,lng2 = -79.49012,lat2 = 44.22597) %>%
        setView(lng = -80.37882,lat=43.48224,zoom = 8) %>%
        #addTiles(options = tileOptions(minZoom=8)) %>%
        clearMarkers() %>%
        clearControls() %>%
        #clearImages()%>%
        clearGroup(group="dynamic") %>%
        clearGroup(group="legend") %>%
        clearGroup(group="addP")
      
    }
    ## Ottawa River Watershed
    else if (input$Cs == "Ottawa River Watershed"){
      leafletProxy("map") %>%
        hideGroup("polysG")%>%
        showGroup("polysO") %>%
        fitBounds(lng1 =-80.9496,lat1 = 42.8507,lng2 = -79.49012,lat2 = 44.22597) %>%
        setMaxBounds(lng1 =-80.9496,lat1 = 42.8507,lng2 = -79.49012,lat2 = 44.22597) %>%
        setView(lng = -80.37882,lat=43.48224,zoom = 8) %>%
        #addTiles(options = tileOptions(minZoom=8)) %>%
        clearMarkers() %>%
        clearControls() %>%
        clearImages()%>%
        #clearGroup(group="polys") %>%
        clearGroup(group="dynamic") %>%
        clearGroup(group="static") %>%
        #clearGroup(group="mask")
        clearGroup(group="legend") %>%
        clearGroup(group="addP")
    }
    
    vglob$catidOld <<- -12 ## re-initialize placeholder catid value
    status$s <- FALSE ## re-initialize flood generation status
    
    ## Hide flood generation UI elements
    jqui_hide('#select',effect = "fade")
    jqui_hide('#rpf',effect = "fade")
    jqui_hide('#dis',effect = "fade")
    jqui_hide('#genF',effect = "fade")
    #jqui_hide('#tabs',effect= "fade")
    
    
    ### Text based on catchment selection
    isolate(
    
    output$pointD <- renderText({
      if (input$Cs == "Grand River Watershed"){
      text <- paste0("Grand River Watershed selected. Please select a subcatchment by clicking on the map once loaded.")
      }
      # else if (input$Cs == "No Selection"){
      #   text <- paste0("No watershed selected. Please select Grand River Watershed or Ottawa River Watershed to view subcatchments.")
      # }
      else if (input$Cs == "Ottawa River Watershed"){
        text <- paste0("Data for the Ottawa River Watershed is not currently available.")
      }
      
    })
    )
                 })
  })
  
  
    ############################## SUBCATCHMENT SELECTION ################################
  
    observeEvent(input$map_geojson_click,{ ### User clicks on a subcatchment
      catidNew <- as.integer(input$map_geojson_click$properties$cat_id) 
        
      
      withProgress(message = 
                     ifelse(status$s == TRUE & catidNew == vglob$catidOld,"Extracting depth","Loading subcatchment - please wait"),
                      
                   min=0,max=20,detail=NULL,
                   
                   
                   {
                     for (i in 1:20) {
                       incProgress(1/20)
                     }
      
      
      ## read catis of clicked-on subcatchment
      
      print(catidNew) # for troubleshooting
      
      if (catidNew != vglob$catidOld) {
        
        ### Prepare to show flood generation UI elements
        jqui_show('#rpf',effect = "fade")
        jqui_show('#dis',effect = "fade")
        jqui_show('#select',effect = "fade")
        jqui_show('#genF',effect = "fade")
        
        
        ## Update text
        output$pointD <- renderText({
          
          text <- paste0("Subcatchment ", catidNew, " selected. No flood generated.")
        })
        
        leafletProxy("map") %>%
          setView(lng = input$map_geojson_click$lng, lat = input$map_geojson_click$lat, zoom = 12)
        
        ## Call Function F1 to generate discharge bounds, outline, stream network, and midpoint of selected subcatchment
        results1 <<- f1(catidNew,input$Cs)
        
        
        leafletProxy("map")%>%
        clearMarkers() %>%
        clearControls() %>%
        clearImages() %>%
        clearGroup(group = "static") %>%
        clearGroup(group = "dynamic") %>%
        setCatID(catidNew) %>%
        addGlGeojsonPolygons(data = results1$boundary, color = cbind(1, 0, 0), opacity = 1, group = "static",) %>%
        addGlGeojsonPolygons(data = results1$rivers, color = cbind(0, 0, 1), group = "static", opacity = 1)
       
        
        vglob$catidOld <<- catidNew  ## update global catid value
        status$s <<- FALSE ### update global flood generation status (flood has not yet been generated)
        
        
        
        ## Render/refresh flood generation UI elements 
          
        output$select <- renderUI({
          radioButtons("select","Please select a method",choices=c("Return Period","Discharge"))
        })
        
        output$rpf <- renderUI({
          req(input$select)
          selectInput("rpf",label="Select flood return period (years)",choices=RPl,selected=NULL,multiple=FALSE)
        })
        output$dis <- renderUI({
          req(input$select)
          sliderInput("dis","Select discharge Value (cubic m / second)",value=NA,min=round(results1$dmin,3),max=round(results1$dmax,3),format="####.###")
        })
        
        output$genF <- renderUI({
          req(input$select)
          actionButton("genF", "Click to apply settings and generate flood")
        })
        
        
        
        
      }
                   })
    })
    
    ############################################# FLOOD GENERATION ##################################################
    
    observeEvent(input$genF,{
      
      withProgress(message = 'Generating flood - please wait', 
                   min=0,max=20,detail=NULL,
                   
                   
                   {
                     for (i in 1:20) {
                       incProgress(1/20)
                     }             
      if (input$select=="Discharge"){
        ## Call Function F2 to generate flood raster based on discharge input
        results2 <<- f2(con,results1$catid,input$dis,input$Cs)
        
        df <- results2
        #df2 <- data.frame(df)
        cmin <- min(df$VALUE)
        cmax <- max(df$VALUE)
        cols <- value_rgbD(df,cmin,cmax) ## call function to generate colour palette
        pal <- colorNumeric("Reds",domain=c(df$VALUE,cmin,cmax),alpha=FALSE)
        
        
        leafletProxy("map") %>%
          clearImages() %>%
          clearControls()%>%
          clearMarkers() %>%
          clearGroup(group="dynamic") %>%
          clearGroup(group="legend") %>%
          clearGroup(group="addP") %>%
          addGlGeojsonPolygons(data=df,group="dynamic",color = cols) %>% 
          addLegend(pal=pal,values = df$VALUE,title="Flood Depth (m)",labFormat = labelFormat(transform = function(values) sort(values, decreasing = FALSE)),group="legend")
        
        
      }
      
      else if (input$select=="Return Period"){
        
        
        
        rps <- as.numeric(input$rpf)
        catid <- results1$catid
        ## Call Function F3 to generate return period-based flood raster
        results3 <<- f3(con,catid,rps,input$Cs)
        df <- results3
        cmin <- min(df$VALUE)
        print(cmin)
        cmax <- max(df$VALUE)
        print(cmax)
        
        cols <- value_rgbR(df,cmin,cmax) ## call function to generate colour palette (return period-based)

        pal <- colorNumeric("Reds",domain=c(df$VALUE,cmin,cmax),alpha=FALSE)

        leafletProxy("map") %>%
          clearGroup(group="dynamic") %>%
          clearGroup(group="legend") %>%
          addGlGeojsonPolygons(data=df,group="dynamic",color = cols) %>%
          clearImages() %>%
          clearControls()%>%
          clearMarkers() %>%
          addLegend(pal=pal,values = df$VALUE,title="Flood Depth (m)",labFormat = labelFormat(transform = function(values) sort(values, decreasing = FALSE)),group="legend")
        
      }
      
                   
      
      
      
      ### Show graph tabs (stochastic flood depth and Hazus damage)
      jqui_show('#tabs',effect= "fade")
      
      ## update global flood generation value to show that a flood raster has now been generated
      status$s <<- TRUE
      
      ## Update text to indicate a flood raster has been generated
      output$pointD <- renderText({
        
        text <- paste0("Flood generated. No point selected.")
      })
                   }) ###### end of progress bar for flood generation 
      
    })
    
    
    ######################### DEPTH CLICKING ###################################################
    
    
    observeEvent(input$map_click, {
        
        if (status$s == TRUE) { ## If TRUE, a flood raster has been generated and depth values are available
          
          # Extract coordinate information from click
          pclick <<- input$map_click
          plat <- pclick$lat
          plong <- pclick$lng
          
          
          ## Add clicked point to map
          leafletProxy("map") %>%
            clearGroup(group="addp") %>%
            addCircleMarkers(lng=plong,lat=plat,group="addp",color="#03F")
          
          ## Convert coordinates of clicked point to DGGID and get catid
          dggid <-dgGEO_to_SEQNUM(dggs,plong,plat)$seqnum
          catid <- pclick$id
          
          print(dggid) ## for troubleshooting
          
          
          
          if (input$select== "Discharge") { 
            chunk <- data.frame(results2) ## Get flood raster generated using discharge
            discharge <- input$dis
            
            ## For troubleshooting
            catid <- vglob$catidOld
            print(paste(catid, "has been clicked"))
            
            
            if ( dggid %in% chunk$DGGID){ ### Check if the location of the user's click is within the flood raster's extent to avoid errors
             
              ## Call Function F4 to extract depth value from discharge-based flood raster as clicked-on point
              res <- f4(dggid,catid,discharge,chunk,input$Cs)
              
              v <<- as.numeric(res$value) ## actual depth value
              
              
              if (v < 0){ ## error handling
                v <- 0
              }
              
              vglob$d <<- v ## update global depth value
              vround <- round(v,digits=3) ## round depth value
              
              df1 <- res$df1
              df2 <- res$df2
              
              
              ## Update text
              output$pointD <- renderText({
                
                text <- paste0("Depth at selected point: ",vround," metres")
              })
              
              
              ## Stochastic flood depth plot
              output$plot <- renderPlotly({
                plot <- plot_ly(type="scatter",mode='lines+markers')%>%
                  add_trace(x=df1$rp,y=df1$Depth,name='Reference')%>%
                  add_markers(x=df2$rp,y=df2$Depth,marker = list(color = 'red',symbol = 'point',size=10),name='Value from Click') %>%
                  #add_segments(x = df2$rp, xend = df2$rp, y = 0, yend = res$value) %>%
                  layout(xaxis =list(title = "Flood Return Period (years)") , yaxis = list(title = "Flood Depth (m)"),title="Stochastic Flood Depth")
                plot
              })
              
              
              
            }
            
            else {
              output$pointD <- renderText({
                
                text <- paste0("No depth value for selected point.")
              })
            }
          }
          
          else if (input$select=="Return Period"){
            
            chunk <- data.frame(results3) ## get return period - based flood raster
            
            ## for troubleshooting
            catid <- vglob$catidOld
            print(paste(catid, "has been clicked"))
            
            if (dggid %in% chunk$DGGID){ ### check if user's clicked location is within the return period-based flood raster
              
              rpI <- as.numeric(input$rpf)
              catid <<- vglob$catidOld
              
              # Call Function F5 to extract depth value from return period-based flood raster as clicked-on point
              res2 <- f5(dggid,catid,rpI,input$Cs)
              v <- as.numeric(res2$value)
              
              if (v < 0){ ## error handling
                v <- 0
              }
              vglob$d <<- v ## update global depth value
              vround <- round(v,digits=2)
              
              df1 <- res2$df1
              df2 <- res2$df2
              
              # df3 <- subset(df1,df1$Depth==v)
              # df3
              
              ## Update text
              output$pointD <- renderText({
                
                text <- paste0("Depth at selected point: ",vround," metres")
              })
              
              ## Stochastic flood depth plot
              output$plot <- renderPlotly({
                plot <- plot_ly(type="scatter",mode='lines+markers')%>%
                  add_trace(x=df1$RP,y=df1$VALUE,name='Reference')%>%
                  add_markers(x=df2$rp,y=df2$Depth,marker = list(color = 'red',symbol = 'point',size=10),name='Value from Click') %>%
                  #add_segments(x = df2$rp, xend = df2$rp, y = 0, yend = res2$value) %>%
                  layout(xaxis =list(title = "Flood Return Period (years)") , yaxis = list(title = "Flood Depth (m)"),title="Stochastic Flood Depth")
                plot
              })
              
              
              
              
              
            }
            
            else { ### For clicks outside of generated flood raster
              
              output$pointD <- renderText({
                
                text <- paste0("No depth value for selected point.")
              })
              
            }
            
          }
        }
                 
    }) ## End of Observe Event for map click input 
    
    
   
  ### Render UI tabs (stochastic flood depth and hazus depth-damage)
    
  output$tabs = renderUI({
    req(status$s == TRUE)
    tabsetPanel(tabPanel('About these tabs',br(),textOutput("explanation")),
                tabPanel("Stochastic Flood Depth",br(),plotlyOutput("plot")),
                tabPanel("Damage Estimation",br(),
                         fluidRow(
                           column(
                             4,uiOutput("seloccA")),
                           column(
                             4,uiOutput("seloccC")),
                           column(
                             4,actionButton("submitDam", "Click to calculate damage")
                           )
                           ),
                         br(),
                         fluidRow(
                           column(
                             12, plotlyOutput("DamageP")
                           )
                         ),
                         br(),
                         br(),
                         br()
 
                         )
                         
                )
  })
  
  
  
  #### Code relating to damages calculation
  
  output$explanation <- renderText({
    explanation <- "These tabs will help you to understand and interpret the map and values above. The first tab, Stochastic Flood Depth, places in context the liklihood of the flood depth occuring at your clicked-on point. The second tab, Damages, will show you the expected damages to a building of your choice based on the flood depth at your clicked-on point."
  })
  
  output$seloccA <- renderUI({
    selectInput("seloccA",label="1. Select the category which best describes your building of interest. Type to filter.",choices=unique(haz_fl_occ$Desc1_2),selected=NULL,multiple=F)
  })
  
  
  class <- reactive({
    row <- which(haz_fl_occ$Desc1_2 == input$seloccA)
    class <- haz_fl_occ[row,2]
    class
    class
  })
  
  flDep <- reactive({
    flDep <- subset(fl_dept,fl_dept$Occupancy == class())
    flDep
  })
  
  output$seloccC <- renderUI({
    selectInput("seloccC",label="2. Select the most applicable further description of your building of interest. Type to filter.",choices=unique(flDep()$Description),selected=NULL,multiple=F)
  })
  
  flDep2 <- reactive({
    flDep2 <- subset(flDep(),flDep()$Description == input$seloccC)
    flDep2
  })
  
  depth <- reactive({
    v <- as.numeric(vglob$d)
    v
  })
  
  PercDam <- reactive({
    if (depth() > 7.5){
      perc <- 100
    }
    else {
      perc <- (approx(flDep2()$Depthm,flDep2()$damage,depth(), method = "linear"))$y
    }
    perc
  })
  
  observeEvent(input$submitDam,{
  
  req(vglob$d != -100)
    
    output$DamageP <- renderPlotly({
      plot <- plot_ly(type="scatter",mode='lines+markers')%>%
        add_trace(x=flDep2()$Depthm,y=flDep2()$damage,name=flDep2()$Description)%>%
        add_markers(x=depth(),y=PercDam(),marker = list(color = 'red',symbol = 'point',size=10),name='Value from Click') %>%
        layout(xaxis =list(title = "Flood Depth (metres)" , yaxis = list(title = "Damage to building (%)"),title="HAZUS Flood Depth-Damage Graph"))
      plot
    })
    
  })
  
}
