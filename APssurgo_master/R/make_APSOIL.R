# SSURGO2APSIM Function

source("R/xmlCompile.R")
test = FALSE
if(test){
data = all_soils
area_threshold = 0.1
site_name = input_n$LOC_ID
path = "./APSIM_soils/"
coords = c(input_n$LAT,input_n$LONG)
label = ''
}


make_APSOIL <- function(data,
                         area_threshold,
                         site_name,
                         coords,
                         crops,
                         path = "",
                         label = "") {
  
  #message(paste0("Creating APSIM toolbox with soils that occupy an area greather than ",area_threshold*100,"% of the ",site_name)," site.")
  
  #if(!(area_threshold >= 0 & area_threshold <= 1)) stop("'area_threshold' must be between 0 and 1")
  
  #data$soils <- lapply(data$soils, function(x) {if(x[[1]] > area_threshold) return(x)})
  #data$soils <- data$soils[!sapply(data$soils, is.null)] 
  site_name = unique(all_soils$SiteName)[1]
  
  data <- all_soils[SiteName == site_name]
  
  
  soils <- list(Average_Soil =
                  list(area = mean(data$area_pct),
                       horizon = data[,-c('SiteName', 'X', 'Y', 'restriction')]))
  
  soils$Average_Soil$coords <- unique(data[,c('SiteName', 'X', 'Y')])
  

  out <- xmlCompile(data = soils, 
                    site_name = site_name, 
                    crops = c("Maize","Soybean"), 
                    coords = soils$Average_Soil$coords[,.(X,Y)])
  
  writeLines(saveXML(out), paste0(path,site_name,label,".xml"))
}

base_doc <- xmlParse('C:/Users/germa/Box Sync/APSIM Workshop 2018/day 1/day 1/Soils/SoilsISU2015.soils')
class(base_doc)
str(base_doc)
class(out)

library(XML)

doc = newXMLDoc()
root = newXMLNode("root", doc = doc)

# LOOP THROUGH 50 REQUESTS
lapply(seq(2), function(i) {
  # PARSE ALL CONTENT
  tmp <- out
  
  # APPEND FROM API XML ROOT
  addChildren(root, getNodeSet(tmp, '/apixmlroot'))
class(base_doc)
class(doc)

base_doc
parsed <- xmlTreeParse(base_doc, useInternalNodes=TRUE)

# SAVE TO FILE OR USE doc FOR FURTHER WORK 
saveXML(doc, file= paste(path,'soilsSYN.soils', sep = ''))

doc1 <- copy(out)
doc2children <- xml_children(out)

for (child in doc2children) {
  xml_add_child(doc1, child)
}
