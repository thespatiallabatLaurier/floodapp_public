Sys.setenv( ODBCINI="/usr/local/etc/odbc.ini")
Sys.setenv(NZ_ODBC_INI_PATH="/usr/local/etc/")
print(print( R.home()))
## Load Libraries
## Load Libraries
library(shinycssloaders)
library(shiny)
library(shinyjqui)
library(ggmap)
library(dggridR)
library(DBI)
library(dplyr)
library(sf)
#library(shinyBS)
library(leaflet)
#library(leaflet.opacity)
library(leafgl)
#library(mapview)
library(plyr)
#library(geosphere)
library(rgeos)
library(distr)
#library(sp)
#library(rgdal)
library(raster)
library(ggplot2)
library(plotly)
#library(RJSONIO)
library(ggmap)
#library(tmap)
library(tmaptools)
library(dbplyr)
#library(colourvalues)
library(grDevices)
library(hazus)
library(leaflet.extras)
#library(htmlwidgets)
#library(grDevices)
#library(ICSNP)
#library(wicket)
library(leaflet.extras)
library(colourvalues)



## Netezza Connection
con = dbConnect(RNetezza::Netezza(), dsn='NZSQL') ## Already modified for server
#con = dbConnect(RNetezza::Netezza(), dsn='NZSQL_A') ## Mute this when done editing
## Construct
dggs = dgconstruct(res=23, metric=TRUE, resround='nearest', pole_lat_deg = 37,pole_lon_deg =-178)

## Miscellaneous Variables
RPl <- c(1.25,1.5,2.0,2.33,5.0,10.0,25.0,50.0,100.0,200.0,500.0) 
RPy <- RPl %>% data.frame()
RPy$RPI <- c(1,2,3,4,5,6,7,8,9,10,11)
names(RPy) <- c("RP", "REPID")

## Load Netezza Tables

riverrp <- tbl(con,"RRP")
catch=tbl(con,"CATCHMENT")
gridf=tbl(con,"GF") %>% filter(RESOLUTION==23)
rivers <- tbl(con,"NETWORK")
hand=tbl(con,"HAND")
rp <- tbl(con,"REP")
stadis <- tbl(con,"STAGEDISCHARGE_GRAND")

## Split tables based on watershed for future use

riverrpG <- filter(riverrp,ZONE=="GrandRiverBasin")
riverrpO <- filter(riverrp,ZONE=="OttawaRiverBasin")

catchG <- filter(catch,ZONE=="GrandRiverBasin")
catchO <- filter(catch,ZONE=="OttawaRiverBasin")

riversG <- filter(rivers,ZONE=="GrandRiverBasin")
riversO <- filter(rivers, ZONE=="OttawaRiverBasin")

handG <- filter(hand,ZONE=="GrandRiverBasin")
handO <- filter(hand,ZONE=="OttawaRiverBasin")

rpG <- filter(rp,ZONE=="GrandRiverBasin")
rpO <- filter(rp,ZONE=="OttawaRiverBasin")

stadisG <- filter(stadis,ZONE=="GrandRiverBasin")
stadisO <- filter(stadis,ZONE=="OttawaRiverBasin")

## catchment polygon
#G <- readOGR("catch.shp") ## Grand River Watershed
#O <- readOGR("OttawaGCS.shp") ## Ottawa River Watershed
#m <- readOGR("mask.shp") ## Global mask
catchGround <- readLines("catchment.geojson") %>% paste(collapse = "\n")
#O <- readOGR("OttawaGCS.shp")
mask <- readLines("mask.geojson") %>% paste(collapse = "\n")


 
### Function F1: Get discharge bounds, outline, stream network, and midpoint of selected subcatchment

f1 <- function(catidNew,catchment){
        catid=catidNew
        
        # Remove selected subcatchment from Grand River watershed polygon
        Gnew <- NA
        # Gnew <- subset(G,G$cat_id != catid)
        
        if (catchment=="Grand River Watershed"){
        
        
        ### Get discharge bounds based on catchment id
        ###TODO: check for performance
        bounds <- riverrpG %>% filter(CAT_ID==catid)
        bounds <- as.data.frame(bounds)
        dmin <- mean(bounds$RP1_25,na.rm = TRUE)
        dmax <- mean(bounds$RP500,na.rm = TRUE)
        
        ### Get rasters for relevant catid
        ## Catchment polygon
        catchmentB <- filter(catchG,CATID==catid) %>% filter(BOUNDARY==1) %>% inner_join(.,gridf,by="DGGID") %>% mutate(.,WKT=inza..ST_AsText(GEOM)) %>% dplyr::select("WKT")  %>% collect() 
        catchmentB <- as.data.frame(catchmentB)
        # %>% st_as_sf(., wkt='WKT', crs = 4326)

        #catchmentB <- mutate(catchmentP,WKT=inza..ST_AsText(GEOM)) %>% filter(BOUNDARY==1)  %>% collect()
        
        #%>% dplyr::select("WKT")
        #catchmentB <- catchmentP %>% filter(BOUNDARY==1) ## Catchment boundary only
        
        
        ## River Network lines
        rivers2 <- filter(riversG,CATID==catid) %>% inner_join(.,gridf,by="DGGID") %>% mutate(.,WKT=inza..ST_AsText(GEOM)) %>% dplyr::select("WKT") %>% collect()
        rivers2 <- as.data.frame(rivers2)
        #  %>% st_as_sf(., wkt='WKT', crs = 4326)
        
        
        # Get approximate spatial mean of selected subcatchment to guide map zoom
        #i <- mean(catchmentB$I)
        #j <- mean(catchmentB$J)
        #try <- dgQ2DI_to_GEO(dggs, 5, in_i=i, in_j=j) ##in_quad = 5
        lon <- NA
        lat <-NA
        
       
        #bounds <- wkt_bounding(catchmentP$WKT,as_matrix=FALSE)
        
        list <- list("x"=lon, "y"=lat,"catid"=catid,"dmin"=dmin,"dmax"=dmax,"rivers"=rivers2,"boundary"=catchmentB,"Gnew"=Gnew)
        return(list)
        }
        
} ## End of F1


### Function F2: Generate flood raster based on discharge input

f2 <- function(con,catid,discharge,catchment){
        if (catchment == "Grand River Watershed"){
        #name=paste(catid,'.csv', sep = "")
        #Data=read.csv(name)
        Data <- stadisG %>% filter(CATID==catid) %>% as.data.frame()
        RPStage=approx(Data$DISCHARGE, Data$STAGE,discharge, method = "linear")$y 
        handf=filter(handG,VALUE<RPStage)
        catchc=filter(catchG,CATID==catid)
        
        handc=handf%>%inner_join(.,catchc,by="DGGID")%>%inner_join(.,gridf,by="DGGID")
        handc=mutate(handc,VALUE1=RPStage-VALUE,WKT=inza..ST_AsText(GEOM))%>%
            dplyr::select(DGGID,VALUE=VALUE1,WKT) %>%
            collect()
        #%>%
        
        print(3)
        #chunk=collect(handc)
        chunk <<- as.data.frame(handc)
        #names(chunk)[2]<-paste("vv")
        #names(chunk)[12]<-paste("VALUE") 
        #chunk <- chunk[,c(1,12,13)]
        
        #FIXME: We need to get rid of the extra columns
        #chunk = st_as_sf(chunk, wkt='WKT', crs = 4326)
        }
        
        return(chunk)
        
} ### End of f2

### Function F3: Generate flood raster based on return period input

f3 <- function (con,catid,rps,catchment){
        
        if (catchment == "Grand River Watershed"){
        rps <- which(RPl==rps)
        #catchc=filter(catch,CATID==catid)
        rp2 <- filter(rpG,REPID==rps)%>%filter(CATCHID==catid)
        #rp2=rp2%>%inner_join(.,catchc,by="DGGID")%>%inner_join(.,gridf,by="DGGID")
        rp2=rp2%>%inner_join(.,gridf,by="DGGID")
        rp2=mutate(rp2,WKT=inza..ST_AsText(GEOM)) %>% dplyr::select(DGGID,VALUE,WKT) %>% collect()
        #rp2= st_as_sf(rp2, wkt='WKT', crs = 4326)
        rp2 <- as.data.frame(rp2)
        }
        
        return(rp2)
        
}


### Function F4: Extract depth value from discharge-based flood raster as clicked-on point

f4 <- function (dggid,catid,discharge,chunk,catchment){
        
        if (catchment == "Grand River Watershed"){
        
        
        hex <- dggid
        rp3 <- filter(rpG,DGGID==hex) %>% collect()
        New <- subset(chunk, DGGID==hex)
        value <- New$VALUE
        rp3 <- arrange(rp3,REPID)
        rp3 <- inner_join(rp3,RPy,by="REPID")
        
        if (nrow(rp3) >=2){########## some hexagons in Waterloo catchment only have 1 rp, so I can't interpolate
                
                RetPer <- (approx(rp3$VALUE,rp3$RP,value, method = "linear"))$y 
        }
        else {
                RetPer <- rp3$VALUE
        }
        ####
        df1 <- data.frame("rp"=rp3$RP,"Depth"=rp3$VALUE) ## Depths for all return periods 
        
        df2 <- data.frame("rp"=RetPer,"Depth"=value) ## Depth for the clicked point, selected discharge only
        list <- list("value"=value,"df1"=df1,"df2"=df2)
        
        }
        return (list)
        
}

### Function F5: Extract depth value from return period-based flood raster as clicked-on point

f5 <- function(dggid,catid,rpI,catchment){
        
        if (catchment == "Grand River Watershed"){
        hex <- dggid

        rp4 <- filter(rpG,DGGID==hex) %>% collect()
        rp4 <- inner_join(rp4,RPy,by="REPID") %>% arrange(REPID)
        rowN <- which(rp4$RP==rpI)
        depth <- as.numeric(rp4[rowN,1])
        df2 <- data.frame("rp"=rpI,"Depth"=depth) ## Depth for the clicked point, selected RP only
        
        list <- list("value"=depth,"df1"=rp4,"df2"=df2)
        
        }
        
        
        return (list)
        
}

## Generate RGB palette for discharge-based flood raster depth values
value_rgbD=function(df,cmin,cmax){
        list=c(df$VALUE,cmin,cmax)
        cols = colour_values_rgb(list, palette="Reds",include_alpha = FALSE) / 255
        cols=cols[1:(dim(cols)[1]-2),]
        return(cols)
}

# Generate RGB palette for return period-based flood raster depth values
value_rgbR=function(df,cmin,cmax){
        list=c(df$VALUE,cmin,cmax)
        cols = colour_values_rgb(list, palette="Reds",include_alpha = FALSE) / 255
        cols=cols[1:(dim(cols)[1]-2),]
        return(cols)
}

## Get fl_dept (Hazus depth-damage)
fl_dept <- extract_hazus_functions()
haz_fl_occ <- haz_fl_occ
haz_fl_occ$Desc1_2 <- paste(haz_fl_occ$Occ_Desc1,haz_fl_occ$Occ_Desc2)
fl_dept$Depthm <- fl_dept$depth / 3.2808




addGlGeojsonPolygons = function(map,
                         data,
                         color = cbind(0, 0.2, 1),
                         fillColor = color,
                         opacity = 0.8,
                         fillOpacity = 0.6,
                         group = "glpolygons",
                         popup = NULL,
                         layerId = NULL,
                         src = FALSE,
                         ...) {
        
   
        ## currently leaflet.glify only supports single (fill)opacity!
        opacity = opacity[1]
        fillOpacity = fillOpacity[1]
        
        if (is.null(group)) group = deparse(substitute(data))
        #if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)
        #stopifnot(inherits(sf::st_geometry(data), c("sfc_POLYGON", "sfc_MULTIPOLYGON")))
        # if (inherits(sf::st_geometry(data), "sfc_MULTIPOLYGON"))
        #         stop("Can only handle POLYGONs, please cast your MULTIPOLYGON to POLYGON using sf::st_cast",
        #              call. = FALSE)
        
        #bounds = as.numeric(sf::st_bbox(data))
        
        # fillColor
        args <- list(...)
        palette = "viridis"
        if ("palette" %in% names(args)) {
                palette <- args$palette
                args$palette = NULL
        }
        fillColor <- leafgl:::makeColorMatrix(fillColor, data, palette = palette)
        if (ncol(fillColor) != 3) stop("only 3 column fillColor matrix supported so far")
        fillColor = as.data.frame(fillColor, stringsAsFactors = FALSE)
        colnames(fillColor) = c("r", "g", "b")
        
        # cols = jsonlite::toJSON(fillColor)
        cols = jsonify::to_json(fillColor, digits = 3)
        
        # # popup
        # if (is.null(popup)) {
        #         # geom = sf::st_transform(sf::st_geometry(data), crs = 4326)
        #         geom = sf::st_geometry(data)
        #         data = sf::st_sf(id = 1:length(geom), geometry = geom)
        # } else if (isTRUE(popup)) {
        #         data = data[, popup]
        # } else {
        #         htmldeps <- htmltools::htmlDependencies(popup)
        #         if (length(htmldeps) != 0) {
        #                 map$dependencies = c(
        #                         map$dependencies,
        #                         htmldeps
        #                 )
        #         }
        #         popup = makePopup(popup, data)
        #         popup = jsonify::to_json(popup)
        #         geom = sf::st_geometry(data)
        #         data = sf::st_sf(id = 1:length(geom), geometry = geom)
        # }
        
        # data
        # if (length(args) == 0) {
        #         geojsonsf_args = NULL
        # } else {
        #         geojsonsf_args = try(
        #                 match.arg(
        #                         names(args)
        #                         , names(as.list(args(geojsonsf::sf_geojson)))
        #                         , several.ok = TRUE
        #                 )
        #                 , silent = TRUE
        #         )
        #         if (inherits(geojsonsf_args, "try-error")) geojsonsf_args = NULL
        #         if (identical(geojsonsf_args, "sf")) geojsonsf_args = NULL
        # }
        # data = do.call(geojsonsf::sf_geojson, c(list(data), args[geojsonsf_args]))
        # data = geojsonsf::sf_geojson(data, ...)
        
        # dependencies
        map$dependencies = c(
                leafgl:::glifyDependencies()
                , map$dependencies
        )
        
        map = leaflet::invokeMethod(
                map
                , leaflet::getMapData(map)
                , 'addGlGeojsonPolygons'
                , data
                , cols
                , popup
                , fillOpacity
                , group
                , layerId
        )
        
       
        
}

addGeoJSONv3 = function(
    map, geojson, layerId = NULL, group = NULL,
    markerType = NULL, markerIcons = NULL,
    markerIconProperty = NULL, markerOptions = leaflet::markerOptions(),
    clusterOptions = NULL, clusterId = NULL,
    labelProperty = NULL, labelOptions = leaflet::labelOptions(),
    popupProperty = NULL, popupOptions = leaflet::popupOptions(),
    stroke = TRUE,
    color = "#03F",
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillColor = color,
    fillOpacity = 0.2,
    dashArray = NULL,
    smoothFactor = 1.0,
    noClip = FALSE,
    pathOptions = leaflet::pathOptions(),
    highlightOptions = NULL
) {
    
    leaflet.extras:::invokeJSAddMethod("addGeoJSONv3",
                      map, geojson, layerId, group,
                      markerType, markerIcons,
                      markerIconProperty, markerOptions,
                      clusterOptions, clusterId,
                      labelProperty, labelOptions, popupProperty, popupOptions,
                      stroke,
                      color,
                      weight,
                      opacity,
                      fill,
                      fillColor,
                      fillOpacity,
                      dashArray,
                      smoothFactor,
                      noClip,
                      pathOptions, highlightOptions)
}

setCatID = function(
    map, catid
) {
    
    leaflet.extras:::invokeJSAddMethod("setCatID",
                                       map, catid)
}
