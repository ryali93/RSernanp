# Set work directory
# Cargar librerias
library(sf)
library(raster)
# library(tidyverse)
# library(mapview)
# library(cptcity)

# Leer datos shapefile
dep <- read_sf("data/file.RDS")

# Entendiendo qu? tipo de dato es la geometr?a de un objeto
a <- c(1,2,3)
b <- c("a","b","c")
c <- list(c(T,F),"b",matrix(1:9,3))
data.frame(a,b,c)
tibble(a,b,c)
is.list(dep)
class(dep)
typeof(dep)

# Ploteo
plot(st_geometry(dep))


# Leer datos raster una sola banda
dem <- raster("data/raster/ASTGTM_S08W079_dem.tif")

# Leer datos raster multibanda
band_names <- list.files("data/raster", full.names = T, pattern = "*[1-9].TIF")
multiband <- stack(band_names)

# Analizando los metadatos de los objetos raster
dem

# Extraer elementos del metadato
extent(dem)
ncell(dem)
res(dem)
nlayers(dem)
crs(dem)

# Leer valores de un objeto raster
getValues(dem)

# Creando mapas r?pidos con plot() y plotRGB()
plot(dem)
plot(dem, col=viridis::viridis(10))
plot(dem, col=cptcity::cpt(pal = find_cpt("radar")[1], n = 10))
dev.off()
plotRGB(stack(multiband[[4]],multiband[[2]],multiband[[1]]))


# Convertir csv a puntos - st_as_sf()
ter <- read.csv("data/earthquakes.csv")
head(ter)
columns
st_crs(32718)
terremotos <- st_as_sf(ter, coords = c("Latitude", "Longitude"), crs = st_crs(4326))
plot(st_geometry(terremotos))
terremotos <- st_as_sf(ter, coords = c("Longitude", "Latitude"), crs = st_crs(4326))
plot(st_geometry(terremotos))

# Crear buffer - st_buffer()
mapview(st_buffer(terremotos, 1))

# Extraer centroides - st_centroid()
dep <- read_sf("data/Departamentos.shp")
plot(st_geometry(dep))
dep_centroid <- st_centroid(dep)
plot(st_geometry(st_centroid(dep)), add=T)

# Crear contorno - st_bbox(), st_make_grid()
st_bbox(dep)
st_make_grid(dep, n=3)
plot(st_geometry(st_make_grid(dep, n=3)))

# Unir objetos - st_union()
st_union(dep)
plot(st_geometry(st_union(dep)))
head(dep)
dep %>% select(DEPNOM, geometry)

# Unir puntos - st_convex_hull()
dep_convex <- st_convex_hull(st_union(dep_centroid))
plot(st_geometry(dep_convex))

# Enmascaramiento de raster - mask() con as(pol, "Spatial")
prov <- st_read("data/Provincias.shp")
mapView(list(dem, dis, prov))
# GRAN CHIMU

prov_granchimu <- filter(prov, PRONOM98 == "GRAN CHIMU")
# Extracci?n de datos - extract()
prov_granchimu_sp <- st_transform(prov_granchimu, crs = crs(dem, asText = TRUE))
raster::extract(dem, as(prov_granchimu_sp, "Spatial"), fun=mean)

# Algebra de mapas
plot((multiband[[4]] - multiband[[3]])/(multiband[[4]] + multiband[[3]]))
