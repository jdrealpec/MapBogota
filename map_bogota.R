library(maps)
library(ggmap)
library(RJSONIO)
library(geosphere) #medir distantica en el mapa
library(maptools)
library(rgdal) 
library(ggplot2)
library(sp)

Bogota = get_map(location = "bogota", zoom = 12)

ggmap(Bogota)

geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

geocodeAdddress("Calle 55a 19-11, Bogota")

gsub("e", "", group) ###corregir o reemplazar caracteres
miyi <- geocode("Calle 55a 19-11, bogota, colombia", output = "latlona", source = "google")
julian <- geocode("Carrera 24 45C-79, bogota, colombia", output = "latlona", source = "google")

ggmap(Bogota) + geom_point(data=julian, aes(lon, lat))

set <- c("Calle 55a 19-11, bogota, colombia","Carrera 24 45C-79, bogota, colombia")
add <- geocode(set, output = "latlona", source = "google")

counts = as.data.frame(table(round(add$lon,3),round(add$lat,3))) #COn varias mediciones

counts$lon = as.numeric(as.character(counts$Var1))
counts$lat = as.numeric(as.character(counts$Var2))

#Hotspot Map
ggmap(Bogota) + geom_point(data = counts, aes(lon, lat, color =  Freq), fill = "red") + scale_color_gradient(low="yellow", high="red")

#Heatmap
ggmap(Bogota) + geom_tile(data = counts, aes(lon, lat, alpha = Freq), fill = "red") 

Julian <- julian[1:2]
Miyi <- miyi[1:2]
distm(Julian, Miyi)


area <- readShapePoly("/SCat/SCat.shp")
area <- readShapePoly(file.choose())

ggmap(Bogota) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points)

crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
states=readShapePoly("SCat.shp",proj4string=crswgs84,verbose=TRUE)

shapefile <- readOGR("SCat/SCat.shp")
shapefile_df <- fortify(shapefile)

map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'white', size = .2)
print(map)

map_projected <- map +
  coord_map()

print(map_projected)
