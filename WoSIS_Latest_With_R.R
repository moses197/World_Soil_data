## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warnings = FALSE, purl = FALSE)
options(warn=-1)


## ----load.package--------------------------------------------------------------------------------
# library(devtools)
# devtools::install_github("JoshOBrien/gdalUtilities")
library(gdalUtilities)      # wrappers for GDAL utility programs that could be
                        #  called from the command line, but here via `sf`
# devtools::install_github("gearslaboratory/gdalUtils")
library(gdalUtils)      # wrappers for GDAL utility programs that could be
                        #  called from the command line,
library(sf)             # spatial data types -- Simple Features
library(stars)          # Spatiotemporal Arrays, Raster and Vector Data Cubes
library(dplyr)          # tidyverse data manipulation functions
library(ggplot2)        # gpplot graphics
library(maps)           # optional -- for boundary polygons
library(mapdata)


## ------------------------------------------------------------------------------------------------
drivers <- sf::st_drivers()
# print(drivers)
ix <- grep("GPKG", drivers$name,  fixed=TRUE)
drivers[ix,]
ix <- grep("ESRI", drivers$name,  fixed=TRUE)
drivers[ix,]
ix <- grep("CSV", drivers$name,  fixed=TRUE)
drivers[ix,]


## ----specify.wfs.address-------------------------------------------------------------------------
wfs <- "WFS:https://maps.isric.org/mapserv?map=/map/wosis_latest.map"


## ----ogrinfo.wfs---------------------------------------------------------------------------------
(layers.info <- st_layers(wfs))


## ----ogrinfo.site--------------------------------------------------------------------------------
profiles.info <-
  gdalUtils::ogrinfo(wfs, layer = "ms:wosis_latest_profiles",
                     ro = TRUE, so = TRUE, q = TRUE)
cat(profiles.info, sep="\n")


## ----ogrinfo.eu.profiles.1-----------------------------------------------------------------------
central.eu.profiles.info <-
  gdalUtils::ogrinfo(wfs, ro=TRUE, so=TRUE, q=FALSE,
                     layer="ms:wosis_latest_profiles",
                     spat=c(6, 48, 8, 50),
                     verbose = FALSE)
head(central.eu.profiles.info, 8)


## ----ogrinfo.eu.profiles.2-----------------------------------------------------------------------
ix.f <- grep("Feature Count", central.eu.profiles.info)
central.eu.profiles.info[ix.f]
ix.e <- grep("Extent", central.eu.profiles.info)
central.eu.profiles.info[ix.e]
ix.g <- grep("GEOGCRS", central.eu.profiles.info)
cat(paste(central.eu.profiles.info[ix.g:(ix.g+17)], collapse="\n"))


## ----show.fields---------------------------------------------------------------------------------
ix.p <- grep("Geometry Column", central.eu.profiles.info)
n <- length(central.eu.profiles.info)
central.eu.profiles.info[ix.p+1:n]


## ----ogrinfo.india.profiles----------------------------------------------------------------------
india.profiles.info <-
  ogrinfo(wfs, ro=TRUE, so=TRUE, q=FALSE,
          layer="ms:wosis_latest_profiles",
          where="country_name='India'")


## ----ogrinfo.india.profiles.2--------------------------------------------------------------------
ix.f <- grep("Feature Count", india.profiles.info)
india.profiles.info[ix.f]
ix.e <- grep("Extent", india.profiles.info)
india.profiles.info[ix.e]


## ----ogrinfo.properties, warnings=-1-------------------------------------------------------------
property.info <- gdalUtils::ogrinfo(wfs, layer = "ms:wosis_latest_bdfi33",
                                    ro = TRUE, so = TRUE, q = FALSE)
cat(property.info, sep="\n")
ix.f <- grep("Feature Count", property.info)
property.info[ix.f]


## ----ogrinfo.bd.india----------------------------------------------------------------------------
bd.india.info <- gdalUtils::ogrinfo(wfs, layer="ms:wosis_latest_bdfi33",
                                    where="country_name='India' AND upper_depth > 100",
                                    ro=TRUE, so=TRUE, q=FALSE)
ix.f <- grep("Feature Count", bd.india.info)
bd.india.info[ix.f]
(n.records <- as.numeric(strsplit(bd.india.info[ix.f],
                                  split=": ", fixed=TRUE)[[1]][2]))


## ----setup.local.dir-----------------------------------------------------------------------------
wosis.dir.name <- "./wosis_latest"
if (!file.exists(wosis.dir.name)) dir.create(wosis.dir.name)


## ----download.profile.gpkg-----------------------------------------------------------------------
layer.name <- "wosis_latest_profiles"
(dst.target.name <- paste0(wosis.dir.name,"/", layer.name, ".gpkg"))
if (!file.exists(dst.target.name)) { 
system.time(
  gdalUtilities::ogr2ogr(src=wfs,
          dst=dst.target.name,
          layer=layer.name,
          f = "GPKG",
          overwrite=TRUE,
          skipfailures=TRUE)
)
}
file.info(dst.target.name)$size/1024/1024


## ----read.profile.gpkg---------------------------------------------------------------------------
profiles.gpkg <- st_read(dst.target.name)
class(profiles.gpkg)
dim(profiles.gpkg)
names(profiles.gpkg)


## ------------------------------------------------------------------------------------------------
head(sort(table(profiles.gpkg$country_name), decreasing=TRUE))


## ----map.profiles.gpkg, fig.width=12, fig.height=12----------------------------------------------
ggplot(data=profiles.gpkg[(profiles.gpkg$country_name=="Brazil"), ]) +
  aes(col=dataset_id) +
  geom_sf(shape=21, size=0.8, fill="black")


## ----download.profiles---------------------------------------------------------------------------
layer.name <- "ms_wosis_latest_profiles"
(dst.target.name <- paste0(wosis.dir.name,"/", layer.name, ".shp"))
if (!file.exists(dst.target.name)) { 
system.time(
  gdalUtilities::ogr2ogr(src=wfs,
          dst=wosis.dir.name,
          layer=layer.name,
          f = "ESRI Shapefile",
          overwrite=TRUE,
          skipfailures=TRUE)
)
}
file.info(dst.target.name)$size/1024/1024


## ----download.profiles.india---------------------------------------------------------------------
wosis.dir.name.india <- "./wosis_latest/india"
if (!file.exists(wosis.dir.name.india)) dir.create(wosis.dir.name.india)
layer.name <- "ms_wosis_latest_profiles"
(dst.target.name <- paste0(wosis.dir.name,"/", layer.name, ".shp"))
if (!file.exists(dst.target.name)) { 
  system.time(
  gdalUtilities::ogr2ogr(src=wfs,
          dst=wosis.dir.name.india,
          layer=layer.name,
          f = "ESRI Shapefile",
          where="country_name='India'",
          overwrite=TRUE,
          skipfailures=TRUE)
)
}
file.info(dst.target.name)$size/1024/1024


## ----import.profiles-----------------------------------------------------------------------------
layer.name <- "ms_wosis_latest_profiles"
profiles <- sf::st_read(dsn=wosis.dir.name, layer=layer.name,
                stringsAsFactors = FALSE)
class(profiles)
dim(profiles)
names(profiles)
head(profiles)


## ----import.profiles.india-----------------------------------------------------------------------
profiles.india <- sf::st_read(dsn=wosis.dir.name.india, layer=layer.name,
                stringsAsFactors = FALSE)
dim(profiles.india)
names(profiles.india)
head(profiles.india)


## ----download.bd---------------------------------------------------------------------------------
layer.name <- "ms_wosis_latest_bdfi33"
(dst.target.name <- paste0(wosis.dir.name,"/", layer.name, ".shp"))
if (!file.exists(dst.target.name)) { 
  system.time(
  gdalUtilities::ogr2ogr(src=wfs,
          dst=wosis.dir.name,
          layer=layer.name,
          f = "ESRI Shapefile",
          overwrite=TRUE,
          skipfailures=TRUE)
)
}
file.info(dst.target.name)$size/1024/1024


## ----download.bd.india---------------------------------------------------------------------------
layer.name <- "ms_wosis_latest_bdfi33"
(dst.target.name <- paste0(wosis.dir.name.india,"/", layer.name, ".shp"))
if (!file.exists(dst.target.name)) { 
  system.time(
    gdalUtilities::ogr2ogr(src=wfs,
                           dst=wosis.dir.name.india,
                           layer=layer.name,
                           f = "ESRI Shapefile",
                           where="country_name='India'",
                           overwrite=TRUE,
                           skipfailures=TRUE)
  )
}
file.info(dst.target.name)$size/1024/1024


## ----import.bd-----------------------------------------------------------------------------------
# here strings are just that, not to be interpreted as R factors
layer.name <- "ms_wosis_latest_bdfi33"
bd33 <- st_read(dsn=wosis.dir.name, layer=layer.name,
                stringsAsFactors = FALSE)
class(bd33)
names(bd33)
head(bd33)


## ----head.bd-------------------------------------------------------------------------------------
head(bd33$bdfi33_val)


## ----example.bd----------------------------------------------------------------------------------
bd33[75:80, c("profile_id","upper_dept","lower_dept","bdfi33_val","bdfi33_v_1")]


## ----import.bd.india, out.width='0.7\\linewidth'-------------------------------------------------
layer.name <- "ms_wosis_latest_bdfi33"
bd33.india <- st_read(dsn=wosis.dir.name.india, layer=layer.name,
                stringsAsFactors = FALSE)
class(bd33.india)
dim(bd33.india)
(profile.id.india <- unique(bd33.india$profile_id))
names(bd33.india)
hist(bd33.india$bdfi33_v_1, main="Bulk density, soil layers in India", 
     xlab="g cm^{-3}")
rug(bd33.india$bdfi33_v_1)


## ----select.bd33.30cm----------------------------------------------------------------------------
bd30cm <- bd33.india %>%
  select(profile_id, upper_dept, lower_dept, bdfi33_v_1) %>%
  filter((lower_dept > 30) ) %>%
  group_by(profile_id) %>%    # may be more than one layer deeper than this
  arrange(upper_dept) %>%     # select the shallowest
  filter(row_number() == 1) %>%
  ungroup()
glimpse(bd30cm)


## ----join.profile.bd33---------------------------------------------------------------------------
india.pts <- profiles.india %>%
  select(profile_id, geometry) 
bd30cm.j <- 
  left_join(india.pts, st_drop_geometry(bd30cm), by="profile_id")
print(bd30cm.j)


## ------------------------------------------------------------------------------------------------
IN <- map_data(map = 'world',
               region = 'India')
# remove named subregions, i.e., the islands
ix <- which(is.na(IN$subregion))
IN <- IN[ix,]
ggplot() +
  geom_path(aes(x = long, y = lat), data=IN) +
  geom_sf(data = bd30cm, aes(col = bdfi33_v_1)) +
  labs(title = "Bulk density at 30 cm depth", col = expression(paste(g," ", cm)^-3))


## ----download.profiles.csv-----------------------------------------------------------------------
wosis.dir.name.ceu <- "./wosis_latest/central_europe"
if (!file.exists(wosis.dir.name.ceu)) dir.create(wosis.dir.name.ceu)
src.layer.name <- "ms:wosis_latest_profiles"
dst.layer.name <- "wosis_latest_profiles_ceu"
(dst.target.name <- paste0(wosis.dir.name.ceu,"/",dst.layer.name,".csv"))
if (!file.exists(dst.target.name)) {
gdalUtilities::ogr2ogr(src=wfs, 
        dst=dst.target.name,
        layer=src.layer.name,
        f="CSV",
        spat=c(6, 48, 8, 50),
        overwrite=TRUE)
}
round(file.info(dst.target.name)$size/1024,1)


## ----import.profiles.csv-------------------------------------------------------------------------
layer.name <- "wosis_latest_profiles_ceu"
system.time(
  profiles.ceu <- read.csv(paste0(wosis.dir.name.ceu, "/",layer.name,".csv"),
                stringsAsFactors = FALSE)
)
names(profiles.ceu)


## ----download.bd.csv-----------------------------------------------------------------------------
src.layer.name <- "wosis_latest_bdfi33"
dst.layer.name <- "wosis_latest_bdfi33"
(dst.target.name <- paste0(wosis.dir.name,"/",dst.layer.name,".csv"))
if (!file.exists(dst.target.name)) { 
gdalUtilities::ogr2ogr(src=wfs,
        dst=dst.target.name,
        layer=src.layer.name,
        f="CSV",
        overwrite=TRUE)
}
file.info(dst.target.name)$size/1024/1024


## ----import.bd.csv-------------------------------------------------------------------------------
layer.name <- "wosis_latest_bdfi33"
system.time(
  bd33.pts <- read.csv(file=paste0(wosis.dir.name,"/",layer.name,".csv"),
                stringsAsFactors = FALSE)
)
dim(bd33.pts)
names(bd33.pts)


## ----coordinates.profiles------------------------------------------------------------------------
profiles.ceu <- st_as_sf(profiles.ceu, 
                         coords = c("longitude", "latitude"),
                         crs = 4326)
class(profiles.ceu)


## ----profiles.wrb--------------------------------------------------------------------------------
table(profiles.ceu$cwrb_reference_soil_group)


## ----map.profiles.wrb----------------------------------------------------------------------------
ggplot(data=profiles.ceu) +
  aes(col=cwrb_reference_soil_group) +
  geom_sf()


## ----load.aqp------------------------------------------------------------------------------------
require(data.table)
require(aqp)            # Algorithms for Quantitative Pedology


## ----bd.aqp--------------------------------------------------------------------------------------
ds.aqp <- st_drop_geometry(bd33)
depths(ds.aqp) <- profile_id ~ upper_dept + lower_dept
is(ds.aqp)
slotNames(ds.aqp)
str(ds.aqp@site)
str(ds.aqp@horizons)
head(ds.aqp@site)
head(ds.aqp@horizons[c(2,5,6,7,9)],12)


## ----plot.bd.spc,fig.width=12, fig.height=8------------------------------------------------------
plotSPC(ds.aqp[1:24,], name="layer_name", color='bdfi33_v_1')

