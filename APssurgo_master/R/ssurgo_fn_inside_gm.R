test = FALSE
if(test){
  template=a
  label=SiteName
  raw.dir = "./RAW/SSURGO"
  extraction.dir = paste0("./EXTRACTIONS/",label, "/SSURGO")
  force.redo = FALSE
}


get_ssurgo <- function (template, label, raw.dir = "./RAW/SSURGO", 
                          extraction.dir = paste0("./EXTRACTIONS/",label, "/SSURGO"), 
                        force.redo = FALSE){
  
  raw.dir <- normalizePath(paste0(raw.dir, "/."), mustWork = FALSE)
  extraction.dir <- normalizePath(paste0(extraction.dir, "/."), 
                                  mustWork = FALSE)
  dir.create(raw.dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(extraction.dir, showWarnings = FALSE, recursive = TRUE)
  files <- list.files(extraction.dir)
  files <- files[grepl("csv", files)]
  files <- files[order(files)]
  files <- files[grepl(label, files)]
  if (!force.redo & length(files) > 0 & file.exists(paste(extraction.dir, "/", label, "_SSURGO_", "Mapunits.shp", sep = ""))) {
    SSURGOMapunits <- rgdal::readOGR(dsn = normalizePath(extraction.dir), 
                                     layer = paste0(label, "_SSURGO_", "Mapunits"), verbose = F)
    tables <- lapply(files, function(file) {
      suppressMessages(readr::read_csv(paste(normalizePath(extraction.dir), 
                                             "/", file, sep = ""), progress = F)) })
    names(tables) <- gsub(".csv", "", files)
    names(tables) <- gsub(paste0(label, "_SSURGO_"), "", 
                          names(tables))
    return(list(spatial = SSURGOMapunits, tabular = tables))
  }
  if (class(template) == "character") {
    q <- paste0("SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol IN (", 
                paste(paste0("'", template, "'"), collapse = ","), 
                ");")
    SSURGOAreas <- soilDB::SDA_query(q)
    template.poly <- template
  } else {
    if (class(template) %in% c("RasterLayer", "RasterStack", 
                               "RasterBrick")) {
      template.poly <- spdf_from_polygon(sp::spTransform(polygon_from_extent(template), 
                                                         sp::CRS("+proj=longlat +ellps=GRS80")))
    }
    else if (class(template) %in% c("SpatialPoints", "SpatialPointsDataFrame")) {
      suppressWarnings(template.poly <- raster::buffer(polygon_from_extent(template), 
                                                       width = 1e-06))
    } else {
      template.poly <- template
    }
    SSURGOAreas <- get_ssurgo_inventory(template = template.poly, 
                                        raw.dir = raw.dir)
    SSURGOAreas <- SSURGOAreas[SSURGOAreas$iscomplete != 
                                 0, ]
  }
  SSURGOData <- lapply(1:nrow(SSURGOAreas), function(i) {
    message("(Down)Loading SSURGO data for survey area ", 
            i, " of ", nrow(SSURGOAreas), ": ", as.character(SSURGOAreas$areasymbol[i]))
    get_ssurgo_study_area(template = template.poly, area = as.character(SSURGOAreas$areasymbol[i]), 
                          date = as.Date(SSURGOAreas$saverest[i], format = "%m/%d/%Y"), 
                          raw.dir = raw.dir)
  })
  SSURGOPolys <- lapply(SSURGOData, "[[", "spatial")
  message("Merging all SSURGO Map Unit polygons")
  SSURGOPolys <- do.call("rbind", SSURGOPolys)
  if (!is.null(template) & !is.character(template)) {
    message("Cropping all SSURGO Map Unit polygons to template")
    if (class(template) %in% c("SpatialPoints", "SpatialPointsDataFrame")) {
      SSURGOPolys <- sp::spTransform(template, sp::CRS(raster::projection(SSURGOPolys))) %over% 
        SSURGOPolys
      SSURGOPolys <- SpatialPointsDataFrame(template@coords, 
                                            data = SSURGOPolys, proj4string = sp::CRS(raster::projection(template)))
    }
    else {
      SSURGOPolys <- raster::crop(SSURGOPolys, sp::spTransform(template.poly, 
                                                               sp::CRS(raster::projection(SSURGOPolys))))
    }
  }
  SSURGOTables <- lapply(SSURGOData, "[[", "tabular")
  message("Merging all SSURGO data tables")
  tableNames <- unique(unlist(sapply(SSURGOTables, names)))
  tableNames <- tableNames[order(tableNames)]
  SSURGOTables <- lapply(tableNames, function(name) {
    tables <- lapply(SSURGOTables, "[[", name)
    tables <- do.call("rbind", tables)
    tables <- unique(tables)
    return(tables)
  })
  names(SSURGOTables) <- tableNames
  SSURGOTables <- extract_ssurgo_data(tables = SSURGOTables, 
                                      mapunits = as.character(unique(SSURGOPolys$MUKEY)))
  suppressWarnings(rgdal::writeOGR(SSURGOPolys, dsn = normalizePath(paste0(extraction.dir, 
                                                                           "/.")), layer = paste0(label, "_SSURGO_Mapunits"), driver = "ESRI Shapefile", 
                                   overwrite_layer = TRUE))
  junk <- lapply(names(SSURGOTables), function(tab) {
    readr::write_csv(SSURGOTables[[tab]], path = paste(extraction.dir, 
                                                       "/", label, "_SSURGO_", tab, ".csv", sep = ""))
  })
  return(list(spatial = SSURGOPolys, tabular = SSURGOTables))
}


get_ssurgo_inventory <- function (template = NULL, raw.dir){
  if (!is.null(template)) {
    if (class(template) %in% c("RasterLayer", "RasterStack", 
                               "RasterBrick")) {
      template <- sp::spTransform(polygon_from_extent(template), 
                                  sp::CRS("+proj=longlat +datum=WGS84"))
    }else {
      template <- sp::spTransform(template, sp::CRS("+proj=longlat +datum=WGS84"))
    }
    bounds <- polygon_from_extent(template)
    if ((raster::xmax(bounds) - raster::xmin(bounds)) > 1 | (raster::ymax(bounds) - raster::ymin(bounds)) > 1) {
      
      grid <- sp::GridTopology(cellcentre.offset = c(-179.5, -89.5), cellsize = c(1, 1), cells.dim = c(360, 180)) %>% 
        sp::SpatialGrid(proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
        as("SpatialPolygons")
      bounds <- rgeos::gIntersection(grid, bounds, byid = T)
    }
    n = NULL
    SSURGOAreas <- foreach::foreach(n = 1:length(bounds), .combine = rbind) %do% {
      #n=1
                                      bound <- sp::bbox(bounds[n])
                                      bound[1,2] = -97.98
                                      bound[2,1] = 49.49
                                      49.501502, -97.978725
                                      
                                      
                                      if (identical(bound[1, 1], bound[1, 2])) bound[1, 2] <- bound[1, 2] + 1e-04
                                      if (identical(bound[2, 1], bound[2, 2])) bound[2, 2] <- bound[2, 2] + 1e-04
                                      bbox.text <- paste(bound, collapse = ",")
                                      url <- paste("https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs?Service=WFS&Version=1.0.0&Request=GetFeature&Typename=SurveyAreaPoly&BBOX=", 
                                                   bbox.text, sep = "")
                                      temp.file <- paste0(tempdir(), "/soils.gml")
                                      opts <- list(verbose = F, noprogress = T, fresh_connect = TRUE)
                                      hand <- curl::new_handle()
                                      curl::handle_setopt(hand, .list = opts)
                                      tryCatch(
                                        status <- curl::curl_download(url, destfile = temp.file, handle = hand), 
                                        error = function(e) stop("Download of ", url, " failed!"))
                                      
                                      #C:\Users\germanm2\AppData\Local\Temp\RtmpiGrad7
                                      SSURGOAreas <- rgdal::readOGR(dsn = temp.file, layer = "surveyareapoly", 
                                                                    disambiguateFIDs = TRUE, stringsAsFactors = FALSE, 
                                                                    verbose = FALSE)
                                      raster::projection(SSURGOAreas) <- raster::projection(template)
                                      if (class(template) == "SpatialPointsDataFrame" & 
                                          length(template) == 1) {
                                        template <- polygon_from_extent(bounds, proj4string = raster::projection(template))
                                      }
                                      SSURGOAreas <- raster::crop(SSURGOAreas, sp::spTransform(template, 
                                                                                               sp::CRS(raster::projection(SSURGOAreas))))
                                      SSURGOAreas <- SSURGOAreas@data
                                      SSURGOAreas$saverest <- as.Date(SSURGOAreas$saverest, 
                                                                      format = "%b %d %Y")
                                      return(SSURGOAreas)
                                    }
    SSURGOAreas <- unique(SSURGOAreas)
  }
  else {
    tmpdir <- tempfile()
    if (!dir.create(tmpdir)) 
      stop("failed to create my temporary directory")
    file <- download_ssurgo_inventory(raw.dir = raw.dir)
    utils::unzip(file, exdir = tmpdir)
    SSURGOAreas <- rgdal::readOGR(normalizePath(tmpdir), 
                                  layer = "soilsa_a_nrcs", verbose = FALSE)@data
    unlink(tmpdir, recursive = TRUE)
  }
  if (0 %in% SSURGOAreas$iscomplete) {
    warning("Some of the soil surveys in your area are unavailable.\n\n            Soils and productivity data will have holes.\n\n            Missing areas:\n", 
            as.vector(SSURGOAreas[SSURGOAreas$iscomplete == 0, 
                                  ]$areasymbol), "\n\n\n            Continuing with processing available soils.\n\n")
  }
  return(SSURGOAreas)
}