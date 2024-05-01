## ----------------------------------------------------------------------------------------------------------------------
library(rgdal)          # interface to GDAL Geographic Data Abstraction Language
library(gdalUtils)      # some useful utilities for GDAL
library(readr)          # tidyverse functions to read files
library(sf, warn.conflicts = FALSE)             # Simple Features spatial data
library(sp)             # spatial data types in R
library(dplyr, warn.conflicts = FALSE)          # another way to handle tabular data
library(dbplyr, warn.conflicts = FALSE)    # databases from dplyr
library(DBI)            # R database interface
library(RSQLite)        # R interface to SQLite databses


#

## ----download.snapshot.2019--------------------------------------------------------------------------------------------
wosis.dir.name <- "./wosis2019"
if (!file.exists(wosis.dir.name)) dir.create(wosis.dir.name)
zip.file.name <- "WoSIS_2019_September.zip"
snapshot.zip <- paste0("https://files.isric.org/public/wosis_snapshot/", zip.file.name)
target.zip <- paste0(wosis.dir.name, "/", zip.file.name)
if (!file.exists(target.zip)) {
  download.file(snapshot.zip, destfile=target.zip)
}


## ----------------------------------------------------------------------------------------------------------------------
if (!file.exists(paste0(wosis.dir.name, "/wosis_201909.gpkg"))) {
  system.time(unzip(target.zip, exdir=wosis.dir.name, junkpaths=TRUE))  
}
list.files(wosis.dir.name)


## ----------------------------------------------------------------------------------------------------------------------
profiles <- read_tsv(paste0(wosis.dir.name, "/wosis_201909_profiles.tsv"))
dim(profiles)
names(profiles)


## ----------------------------------------------------------------------------------------------------------------------
length(unique(profiles$country_name))
head(table(profiles$country_name))
length(unique(profiles$dataset_id))
head(table(profiles$dataset_id))


## ----------------------------------------------------------------------------------------------------------------------
table(profiles$cstx_order_name)
table(profiles$cwrb_reference_soil_group)
table(profiles$cfao_major_group)
sum(is.na(profiles$cfao_major_group))


## ----no.class.prof-----------------------------------------------------------------------------------------------------
round(100*(length(intersect(which(is.na(profiles$cfao_major_group)), 
          intersect(which(is.na(profiles$cwrb_reference_soil_group)),
          which(is.na(profiles$cstx_order_name))))))/dim(profiles)[1],1)


## ----------------------------------------------------------------------------------------------------------------------
table(profiles$geom_accuracy)


## ----------------------------------------------------------------------------------------------------------------------
profiles.sp <- data.frame(profiles)
coordinates(profiles.sp) <- c("longitude", "latitude")
proj4string(profiles.sp) <- CRS("+init=epsg:4326")
str(profiles.sp)


## ----------------------------------------------------------------------------------------------------------------------
dim(profiles.hi <- profiles %>% 
      dplyr::filter(country_id=="NL") %>%
      dplyr::filter(geom_accuracy < 1/3600))
coordinates(profiles.hi) <- c("longitude", "latitude")
proj4string(profiles.hi) <- CRS("+init=epsg:4326")
spplot(profiles.hi, zcol="geom_accuracy", key.space="right")


## ----------------------------------------------------------------------------------------------------------------------
profiles %>% select(profile_id, country_id, longitude, latitude, geom_accuracy, dsds)


## ----------------------------------------------------------------------------------------------------------------------
attributes <- read.table(paste0(wosis.dir.name, "/wosis_201909_attributes.tsv"),
                        header=TRUE,
                        sep="\t",
                        stringsAsFactors=FALSE)
str(attributes)


## ----------------------------------------------------------------------------------------------------------------------
attributes[, c("code", "type", "attribute", "unit")]
table(attributes$type)


## ----------------------------------------------------------------------------------------------------------------------
attributes[, c("code", "profiles", "layers")]


## ----------------------------------------------------------------------------------------------------------------------
attributes[1:5, c("code",  "description")]


## ----------------------------------------------------------------------------------------------------------------------
attributes[1:5, c("code", "attribute", "accuracy")]


## ----------------------------------------------------------------------------------------------------------------------
ix <- grep("Phosphorus", attributes$attribute)
attributes[ix, c("code", "attribute", "profiles", "layers", "unit", "accuracy")]
attributes[ix, "description"]


## ----------------------------------------------------------------------------------------------------------------------
physical <- readr::read_tsv(paste0(wosis.dir.name, "/wosis_201909_layers_physical.tsv"),
  col_types="iiddclcdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccc")
dim(physical)


## ----------------------------------------------------------------------------------------------------------------------
names(physical)


## ----------------------------------------------------------------------------------------------------------------------
(.clay.fields <- which(substr(names(physical), 1, 4)=="clay"))
data.frame(physical[1, .clay.fields])
(.clay.values.fields <- which(substr(names(physical), 1, 10)=="clay_value"))
data.frame(physical[,1:5], .clay.values.fields)[1:12,]


## ----------------------------------------------------------------------------------------------------------------------
(clay.values <- physical %>% select(contains("clay")))
length(clay.methods <- unique(clay.values$clay_method))
head(clay.methods)
length(clay.method.hydrometer.ix <- grep("hydrometer", clay.methods))
clay.methods[clay.method.hydrometer.ix][1:3]


## ----------------------------------------------------------------------------------------------------------------------
(clay.values <- physical %>% select(profile_id:layer_name, clay_value_avg))
summary(clay.values)
hist(clay.values$clay_value_avg, breaks=seq(0,100, by=2),
     xlab="Clay concentration, %", main="")


## ----------------------------------------------------------------------------------------------------------------------
chemical <- readr::read_tsv(paste0(wosis.dir.name, "/wosis_201909_layers_chemical.tsv"),
                            col_types="iiddclcdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccc")
dim(chemical)
# spec(chemical)
names(chemical)


## ----------------------------------------------------------------------------------------------------------------------
total.P <- chemical %>% 
   dplyr::filter(!is.na(phptot_value_avg)) %>% 
   select(profile_id:layer_name, phptot_value:phptot_licence)
summary(total.P)


## ----india.profiles----------------------------------------------------------------------------------------------------
(profiles.india <- dplyr::filter(profiles, country_name=="India") %>%
    select(profile_id, longitude, latitude))


## ----left.join---------------------------------------------------------------------------------------------------------
(layers.india <- left_join(profiles.india, physical) %>% 
      select(profile_id, upper_depth:layer_name,sand_value_avg, silt_value_avg, clay_value_avg))


## ----to.df-------------------------------------------------------------------------------------------------------------
layers.india <- as.data.frame(layers.india)


## ----read.gpkg---------------------------------------------------------------------------------------------------------
source <- paste0(wosis.dir.name, "/", "wosis_201909.gpkg")
(gpkg <- DBI::dbConnect(RSQLite::SQLite(), source))


## ----list.tables-------------------------------------------------------------------------------------------------------
DBI::dbListTables(gpkg)


## ----geom.column-------------------------------------------------------------------------------------------------------
dplyr::tbl(gpkg, "gpkg_geometry_columns")


## ----profiles.table----------------------------------------------------------------------------------------------------
dplyr::tbl(gpkg, "wosis_201909_profiles")


## ----sf.layers---------------------------------------------------------------------------------------------------------
st_layers(dsn=source)
wosis.sf <- st_read(source, stringsAsFactors=FALSE,
                    fid_column_name="profile_id")
class(wosis.sf)
dim(wosis.sf)
names(wosis.sf)


## ----sf.geom-----------------------------------------------------------------------------------------------------------
class(wosis.sf$geom)
str(wosis.sf$geom)
st_bbox(wosis.sf$geom)
st_crs(wosis.sf$geom)
head(wosis.sf$geom, 4)


## ----sf.row------------------------------------------------------------------------------------------------------------
wosis.sf[1024,]


## ----sf.profile--------------------------------------------------------------------------------------------------------
wosis.sf[which(wosis.sf$profile_id==45820),]


## ----sf.attrs----------------------------------------------------------------------------------------------------------
length(unique(wosis.sf$country_name))
head(table(wosis.sf$country_name))
length(unique(wosis.sf$dataset_id))
head(table(wosis.sf$dataset_id))


## ----plot-st,fig.width=9, fig.height=6---------------------------------------------------------------------------------
plot(wosis.sf["cstx_order_name"],
     xlim=c(-78, -74), ylim=c(42, 44),
     pch=20,
     key.length=1, # make the legend wide enough to show all classes
     main="Soil Taxonomy Order")
grid()


## ----subset.profiles---------------------------------------------------------------------------------------------------
(wosis.sf.india <- wosis.sf %>% dplyr::filter(country_name=="India"))
table(wosis.sf.india$dataset_id)
wosis.sf.india %>% count(cstx_order_name)


## ----read.as.sp--------------------------------------------------------------------------------------------------------
ogrInfo(dsn=source)
wosis.sp <- readOGR(dsn=source,
                stringsAsFactors = TRUE)
class(wosis.sp)
bbox(wosis.sp)
proj4string(wosis.sp)
dim(wosis.sp)
summary(wosis.sp)
names(wosis.sp@data)


## ----summarize.profiles------------------------------------------------------------------------------------------------
unique(wosis.sp$cwrb_reference_soil_group)
summary(is.na(wosis.sp$cwrb_reference_soil_group))
table(wosis.sp$cwrb_reference_soil_group)


## ----spplot.orders, height=9, width=5----------------------------------------------------------------------------------
spplot(wosis.sp, zcol="cwrb_reference_soil_group",
     xlim=c(4, 8), ylim=c(50, 54),
     pch=20, key.space="right",
     main="WRB RSG")


## ----class.chem.phys---------------------------------------------------------------------------------------------------
class(dplyr::tbl(gpkg, "wosis_201909_layers_chemical"))
class(dplyr::tbl(gpkg, "wosis_201909_layers_physical"))


## ----read.chem---------------------------------------------------------------------------------------------------------
wosis.chemical <- dplyr::tbl(gpkg, "wosis_201909_layers_chemical")
(wosis.chemical$ops$vars)


## ----load.aqp----------------------------------------------------------------------------------------------------------
library(aqp)            # Algorithms for Quantitative Pedology


## ----make.spc----------------------------------------------------------------------------------------------------------
ds.aqp <- as.data.frame(layers.india)
depths(ds.aqp) <- profile_id ~ upper_depth + lower_depth
is(ds.aqp)
slotNames(ds.aqp)
str(ds.aqp@site)
str(ds.aqp@horizons)


## ----plotspc, fig.width=12, fig.height=8-------------------------------------------------------------------------------
ds.aqp[100,]
plotSPC(ds.aqp[100:124,], name="layer_name", color='clay_value_avg')

