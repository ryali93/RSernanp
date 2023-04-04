# Set work directory
setwd("E:/2020/unmsm/programacion_20201/sesion08")

# Cargar librerias
library(sf)
library(raster)
library(mapview)
library(dplyr)

# Leer datos shapefile
dep_raster <- shapefile("data/Departamentos.shp")
dep_sf <- st_read("data/Departamentos.shp")

class(dep_raster)
class(dep_sf)

# Entendiendo qué tipo de dato es la geometría de un objeto
a <- c(1,2,3)
b <- c("a","b","c")
c <- list(c(T,F), "b",matrix(1:9,3))

data.frame(a, b, c)
tibble(a, b, c)

# Ploteo
plot(dep_raster)
plot(st_geometry(dep_sf))


# Leer datos raster una sola banda
dem <- raster("data/raster/ASTGTM_S08W079_dem.tif")

# Leer datos raster multibanda
band_names <- list.files("data/raster/", pattern = "*[1-4].TIF", full.names = T)
st_bands <- stack(band_names)

# Analizando los metadatos de los objetos raster
dem
st_bands

# Extraer elementos del metadato
extent(dem)
ncell(dem)
nlayers(st_bands)
crs(dem)

# Leer valores de un objeto raster
mean(getValues(dem))

# Creando mapas rápidos con plot() y plotRGB()
library(cptcity)
plot(dem)
plot(dem, col = viridis::viridis(10))
plot(dem, col = cpt(pal = find_cpt("radar")[2], n=10))

st_new <- stack(st_bands[[4]], st_bands[[3]], st_bands[[2]])
plotRGB(st_new)


# Convertir csv a puntos - st_as_sf()
ter <- read.csv("data/earthquakes.csv")
plot(ter)
head(ter)
st_crs(32717) # UTM zona 17 sur
st_crs(32718) # UTM zona 18 sur
st_crs(32719) # UTM zona 19 sur
st_crs(4326) # Geograficas

terremotos <- st_as_sf(ter, coords = c("Longitude", "Latitude"), crs = st_crs(4326))
head(terremotos)

plot(st_geometry(terremotos))
mapview(st_geometry(terremotos))


# Crear buffer - st_buffer()
mapview(st_buffer(terremotos, 1))

# Extraer centroides - st_centroid()
plot(st_geometry(dep_sf))
dep_centroid <- st_centroid(dep_sf)
plot(st_geometry(dep_centroid), add=T)

# Crear contorno - st_bbox(), st_make_grid()
st_bbox(dep_sf)
plot(st_geometry(st_make_grid(dep_sf, n=1)), add=T)

# Unir objetos - st_union()
st_union(dep_sf)
plot(st_geometry(st_union(dep_sf)))

# Unir puntos - st_convex_hull()
dep_convex <- st_convex_hull(st_union(dep_centroid))
plot(st_geometry(dep_convex), add=T)


# Enmascaramiento de raster - mask() con as(pol, "Spatial")
plot(dem)
plot(dep_sf, add=T)
mapview(list(dem, dep_sf))

prov <- st_read("data/Provincias.shp")
mapview(list(dem, prov))

prov_gc <- prov %>%
  filter(PRONOM98 == "GRAN CHIMU")

mapview(list(dem, prov_gc))

prov_gc_sp <- as(prov_gc, "Spatial") # AS
dem_crop <- crop(dem, prov_gc_sp) # CROP

mascara <- mask(dem_crop, prov_gc_sp) # MASK
plot(mascara)
writeRaster(mascara, "data/raster/mascara.tif")

# Extracción de datos - extract()
mean_dem <- raster::extract(dem, prov_gc_sp, fun = mean)
mean_dem

max_min_dem <- function(dem_area, area){
  max_dem <- raster::extract(dem, prov_gc_sp, fun = max)
  min_dem <- raster::extract(dem, prov_gc_sp, fun = min)
  return(c(max_dem, min_dem))
}

mmdem <- max_min_dem(dem, prov_gc_sp)

# Algebra de mapas
# ndvi <- (NIR - B3) / (NIR + B3)
ndvi <- (st_bands[[4]] - st_bands[[3]]) / (st_bands[[4]] + st_bands[[3]])
plot(ndvi)

ndvi[ndvi>0.7] = 5
ndvi[ndvi<0.2] = 1
ndvi[ndvi>=0.2 & ndvi <=0.7] = 2
plot(ndvi)
