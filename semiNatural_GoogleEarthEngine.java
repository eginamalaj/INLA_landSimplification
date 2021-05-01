//
// Code to extract Land Cover area from AAFC data when overlaying a polygon
//
// Shapefile with all wetlands
var cd = ee.FeatureCollection("users/eginamalaj/2016CD_ag");
//print(cd);
//
// Select year
//
// Raster data from AAFC
var crop = ee.ImageCollection('AAFC/ACI')
              .filter(ee.Filter.date('2009'))
              .first();
//print(crop)
//Visualize
//Map.addLayer(crop);
//Map.setCenter(-103.6936428, 51.43638685, 5);
//Map.addLayer(cd, {color: 'red'})

// Function to calc area
var calculateClassArea = function(feature) {
    var areas = ee.Image.pixelArea().addBands(crop)
    .reduceRegion({
      reducer: ee.Reducer.sum().group({
      groupField: 1,
      groupName: 'class',
    }),
    geometry: feature.geometry(),
    scale: 30,
    maxPixels: 40e9
    })

    var classAreas = ee.List(areas.get('groups'))
    var classAreaLists = classAreas.map(function(item) {
      var areaDict = ee.Dictionary(item)
      var classNumber = ee.Number(
        areaDict.get('class')).format()
      var area = ee.Number(
        areaDict.get('sum')).divide(1e6) //1e4 area in ha /1e6 area in km2
      return ee.List([classNumber, area])
    })

    var result = ee.Dictionary(classAreaLists.flatten())
    var cdID = feature.get('CDUID')
    return ee.Feature(
      feature.geometry(),
      result.set('cdID', cdID))
}

var cdAreas = cd.map(calculateClassArea);
//print('LandCover CD Unit',cdAreas) // takes a minute to calc

var classes = ee.List(crop.get('landcover_class_values'));
var outputFields = ee.List(['cdID']).cat(classes).getInfo();
//print('fields', outputFields);

Export.table.toDrive({
    collection: cdAreas,
    description: 'land_use_calc_LandSimplification',
   folder: 'landuse',
    fileNamePrefix: 'landuseCDCensus2009',
    fileFormat: 'CSV',
    selectors: outputFields
    })
