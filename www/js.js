 // utility method to extract resetStyle from defaultStyle & highlightStyle
function getResetStyle(style, highlightStyle) {
  var resetStyle = {};
  if(!$.isEmptyObject(highlightStyle)) {
    $.each(highlightStyle, function (k, v) {
      if(k != 'bringToFront' && k != 'sendToBack'){
        if(style && style[k]) {
          resetStyle[k] = style[k];
        }
      }
    });
  }
  return resetStyle;
}
 
 LeafletWidget.methods.addGlGeojsonPolygons = function (data, cols, popup, opacity, group, layerId) {

   var map = this;

   var t0 = performance.now()
   fs = [];
   var wkt = new Wkt.Wkt();
   for (let index = 0; index < data.WKT.length; index++) {
    //  var geojson = Terraformer.WKT.parse(data.WKT[index]);
     
     
     wkt.read(data.WKT[index]);
     //var a = wkt.toJson()
  
     var f = {
      type: "Feature",
      geometry: wkt.toJson(),
      properties: {
        value: (data.VALUE)? data.VALUE[index] : 0
      }
    }
    fs.push(f);

   }

   var t1 = performance.now()
    console.log("Call to convert wkt to geojson for "+data.WKT.length +" objects in js took " + (t1 - t0) + " milliseconds.")


   var conv = {
    "type": "FeatureCollection",
    "crs": {
      "type": "name",
      "properties": {
        "name": "urn:ogc:def:crs:OGC:1.3:CRS84"
      }
    },
     features: fs
   }


   var clrs;
  if (cols.length === 1) {
    clrs = cols[0];
  } else {
    clrs = function(index, feature) { return cols[index]; };
  }


   //L.glify.latitudeFirst();
   var shapeslayer = L.glify.shapes({
     map: map,
     data: conv,
     className: group,
     color: clrs,
     opacity:1
   });

   map.layerManager.addLayer(shapeslayer.glLayer, 'glify', layerId, group);
 };
 
 
 
 LeafletWidget.methods.setCatID = function (catId){
   LeafletWidget.methods.catId=catId;
   console.log("catId is updated to "+catId)
 }
 
 function addGeoJSONLayer(
  widget,
  geojsonLayerFunction,
  layerId, group,
  setStyle,
  markerType, markerIcons,
  markerIconProperty, markerOptions, markerIconFunction,
  clusterOptions, clusterId,
  labelProperty, labelOptions, popupProperty, popupOptions,
  pathOptions, highlightOptions
) {
  var self = widget;

  // Initialize Clusering support if enabled.
  var clusterGroup = self.layerManager.getLayer('cluster', clusterId),
    cluster = clusterOptions !== null;
  if (cluster && !clusterGroup) {
    clusterGroup = L.markerClusterGroup.layerSupport(clusterOptions);
    if(clusterOptions.freezeAtZoom) {
      var freezeAtZoom = clusterOptions.freezeAtZoom;
      delete clusterOptions.freezeAtZoom;
      clusterGroup.freezeAtZoom(freezeAtZoom);
    }
    clusterGroup.clusterLayerStore = new LeafletWidget.ClusterLayerStore(clusterGroup);
  }
  var extraInfo = cluster ? { clusterId: clusterId } : {};
  var thisGroup = cluster ? null : group;

  // Initialize shape highlighting if enabled.
  var style = pathOptions;
  var highlightStyle = highlightOptions;
  var defaultStyle = getResetStyle(style, highlightStyle);

  function highlightFeature(e) {
    
   console.log("catId is "+LeafletWidget.methods.catId)
    if((e.sourceTarget.feature.properties.cat_id!=undefined) && (LeafletWidget.methods.catId !=undefined) && e.sourceTarget.feature.properties.cat_id==LeafletWidget.methods.catId){
      return;
    }
    
    var layer = e.target;
    layer.setStyle(highlightStyle);
    if(highlightStyle.bringToFront) {
      layer.bringToFront();
    }
  }
  function resetFeature(e){
    var layer = e.target;
    layer.setStyle(defaultStyle);
    if(highlightStyle.sendToBack) {
      layer.bringToBack();
    }
  }

  var globalStyle = $.extend({}, style);

  function styleFunction(feature) {
    return $.extend(globalStyle, feature.style || {},
      feature.properties.style || {});
  }

  function onEachFeatureFunction(feature, layer) {
    var featureExtraInfo = $.extend({
      featureId: feature.id,
      properties: feature.properties
    }, extraInfo || {});

    // create and bind popups if enabled.
    if (typeof popupProperty !== 'undefined' && popupProperty !== null) {
      if(typeof popupProperty == 'string') {
        if(!$.isEmptyObject(popupOptions)) {
          layer.bindPopup(feature.properties[popupProperty], popupOptions);
        } else {
          layer.bindPopup(feature.properties[popupProperty]);
        }
      } else if(typeof popupProperty == 'function') {
        if(!$.isEmptyObject(popupOptions)) {
          layer.bindPopup(popupProperty(feature), popupOptions);
        } else {
          layer.bindPopup(popupProperty(feature));
        }
      }
    }

    // create and bind labels if enabled.
    if (typeof labelProperty !== 'undefined' && labelProperty !== null) {
      if(typeof labelProperty == 'string') {
        if(!$.isEmptyObject(labelOptions)) {
          if(labelOptions.permanent) {
            layer.bindTooltip(feature.properties[labelProperty], labelOptions).showLabel();
          } else {
            layer.bindTooltip(feature.properties[labelProperty], labelOptions);
          }
        } else {
          layer.bindTooltip(feature.properties[labelProperty]);
        }
      } else if(typeof labelProperty == 'function') {
        if(!$.isEmptyObject(labelOptions)) {
          if(labelOptions.noHide) {
            layer.bindTooltip(labelProperty(feature), labelOptions).showLabel();
          } else {
            layer.bindTooltip(labelProperty(feature), labelOptions);
          }
        } else {
          layer.bindTooltip(labelProperty(feature));
        }
      }
    }

    // add EventListeners to highlight shapes on hover if enabled.
    if(!$.isEmptyObject(highlightStyle)) {
      layer.on({
        'mouseover': highlightFeature,
        'mouseout': resetFeature});
    }

    layer.on('click', LeafletWidget.methods.mouseHandler(self.id, layerId,
      thisGroup, 'geojson_click', featureExtraInfo), self);
    layer.on('mouseover', LeafletWidget.methods.mouseHandler(self.id, layerId,
      thisGroup, 'geojson_mouseover', featureExtraInfo), self);
    layer.on('mouseout', LeafletWidget.methods.mouseHandler(self.id, layerId,
      thisGroup, 'geojson_mouseout', featureExtraInfo), self);
  }

  // code for custom markers
  function pointToLayerFunction(feature, latlng) {
    var layer = null;
    if(markerType === 'circleMarker') {
      layer = L.circleMarker(latlng, markerOptions || {});
    } else {
      if (typeof markerIconProperty !== 'undefined' && markerIconProperty !== null) {
        if(typeof markerIconProperty == 'string') {
          layer = L.marker(latlng, $.extend({
            icon: markerIconFunction(markerIcons[feature.properties[markerIconProperty]])
          }, markerOptions || {}));
        } else if(typeof markerIconProperty == 'function') {
          layer = L.marker(latlng, $.extend({
            icon: markerIconFunction(markerIcons[markerIconProperty(feature)])
          }, markerOptions || {}));
        }
      } else {
        layer = L.marker(latlng, $.extend({
          icon: markerIconFunction(markerIcons)
        }, markerOptions || {}));
      }
    }

    return layer;
  }

  var geojsonOptions = {};
  if(setStyle){
    geojsonOptions.style = styleFunction;
  }
  geojsonOptions.onEachFeature = onEachFeatureFunction;

  if(markerType === 'circleMarker' || !$.isEmptyObject(markerIcons)) {
    geojsonOptions.pointToLayer = pointToLayerFunction;
  }

  var gjlayer = geojsonLayerFunction(geojsonOptions);

  if (cluster) {
    clusterGroup.clusterLayerStore.add(gjlayer);
    self.layerManager.addLayer(clusterGroup, 'cluster', clusterId, group);
  } else {
    self.layerManager.addLayer(gjlayer, 'geojson', layerId, thisGroup);
  }

}

 LeafletWidget.methods.addGeoJSONv3 = function(
  geojson, layerId, group,
  markerType, markerIcons,
  markerIconProperty, markerOptions, markerIconFunction,
  clusterOptions, clusterId,
  labelProperty, labelOptions, popupProperty, popupOptions,
  pathOptions, highlightOptions
) {
  var self = this;

    addGeoJSONLayer(
      self,
      function getGeoJSONLayer(geoJSONOptions){
        return L.geoJson(
          LeafletWidget.utils.getParsedGeoJSON(geojson), geoJSONOptions);
      },
      layerId, group,
      true,
      markerType, markerIcons,
      markerIconProperty, markerOptions, markerIconFunction,
      clusterOptions, clusterId,
      labelProperty, labelOptions, popupProperty, popupOptions,
      pathOptions, highlightOptions
    );
  
};
 