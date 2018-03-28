rm(list = ls())
setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\Census Data')
#setwd('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/Census Data')
library(rgdal)
library(foreign)
library(data.table)
library(raster)
library(rgeos)
library(maptools)
library(spatstat)
library(spdep)

ni_crs <- CRS('+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1 +x_0=200000 +y_0=250000 +ellps=airy +towgs84=482.5,-130.6,564.6,-1.042,-0.214,-0.631,8.15 +units=m +no_defs ')

settlements <- readOGR('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS', 'sdl')
# settlements <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS', 'sdl')
settlements <- spTransform(settlements, CRS('+proj=longlat +datum=WGS84'))
soadata <- readOGR('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS', 'SOA2011'
                   , stringsAsFactors = F)
# soadata <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS', 'SOA2011'
#                    , stringsAsFactors = F)
# soadata@proj4string <- CRS('+proj=longlat +datum=WGS84')
soadata <- spTransform(soadata, CRS('+proj=longlat +datum=WGS84'))
soadata@data$id <- seq(1:nrow(soadata@data))
names(soadata@data)[1] <- 'SOA Code'


########## MERGING IN CENSUS DATA
# Population: Count of population
pop <- fread('population2001.csv')
soadata@data <- merge(soadata@data, pop[, c(2, 3, 9), with = F], by = 'SOA Code', all.x = T)
# Industry: % of population employed in agriculture, forestry, hunting, or fishing
industry <- fread('industry2001.csv')
soadata@data <- merge(soadata@data, industry[, c(2, 17), with = F], by = 'SOA Code', all.x = T)
# Unemployment: % of population long-term unemployed
nsecemp <- fread('nsecemployment2001.csv')
soadata@data <- merge(soadata@data, nsecemp[, c(2, 24), with = F], by = 'SOA Code', all.x = T)
# Hours worked: Average hours/week worked by male population
hours <- fread('hoursworked2001.csv')
soadata@data <- merge(soadata@data, hours[, c(2, 17), with = F], by = 'SOA Code', all.x = T)
# Religious background: % of population from Catholic or Christian (non-Catholic) background
relig <- fread('religionbackground2001.csv')
soadata@data <- merge(soadata@data, relig[, c(2, 8, 9), with = F], by = 'SOA Code', all.x = T)


## Organizing data
soadata@data <- soadata@data[order(soadata$id), ]
soadata <- soadata[, -3]
names(soadata@data) <- c('SOACODE', 'SOALAB', 'TOTPOP', 'POPDENS', 'PCTFARM'
                         , 'LTUNEMP', 'WKHRS_M', 'PCTCATH', 'PCTPROT')


########## NEW VARIABLE: % OF EACH SOA URBANIZED (overlap with settlement > 1000 people)
soadata@data$PCTURBN <- 0.00
for(soa in soadata@data$SOACODE){
  this_soa <- soadata[soadata@data$SOACODE %in% soa, ]
  soa_urban <- try({intersect(this_soa, settlements)}, silent = T)
  if(
    class(soa_urban) != 'try-error' & class(soa_urban) != 'NULL'
  ){
    soadata@data$PCTURBN[soadata@data$SOACODE %in% soa] <- gArea(soa_urban) / gArea(this_soa)
  }
}
soadata@data$PCTURBN[soadata@data$PCTURBN > 1.0] <- 1.0


########## NEW VARIABLES: DUMMY INDICATORS FOR BELFAST/DERRY
belfast <- settlements[settlements@data$BAND_NAME %in% 'BELFAST METROPOLITAN URBAN AREA', ]
derry <- settlements[settlements@data$BAND_NAME %in% 'DERRY URBAN AREA', ]
soadata@data$BELFAST <- 0L
soadata@data$DERRY <- 0L
for(soa in soadata@data$SOACODE){
  this_soa <- soadata[soadata@data$SOACODE %in% soa, ]
  belfast_over <- over(this_soa, belfast)
  derry_over <- over(this_soa, derry)
  
  if(
    !is.na(belfast_over)
  ){
    soadata@data$BELFAST[soadata@data$SOACODE %in% soa] <- 1L
  }
  
  if(
    !is.na(derry_over)
  ){
    soadata@data$DERRY[soadata@data$SOACODE %in% soa] <- 1L
  }
}


########## RESHAPING: AGGREGATE SOAS INTO WARDS
ward_ids <- fread('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS\\NI_wards.csv')
ward_ids <- ward_ids[, list(SOA2001, WARD1992)]
setnames(ward_ids, c('SOACODE', 'WARDCODE'))
soadata@data$temp_id <- seq(1, nrow(soadata@data))
soadata@data <- merge(soadata@data, ward_ids, by = 'SOACODE', all.x = T, sort = F)

soadata_data <- data.table(soadata@data)



ward_data <- ddply(soadata_data, .(WARDCODE)
      , function(x) data.frame(
        TOTPOP = sum(x$TOTPOP)
        , POPDENS = weighted.mean(x$POPDENS, x$TOTPOP)
        , PCTFARM = weighted.mean(x$PCTFARM, x$TOTPOP)
        , LTUNEMP = weighted.mean(x$LTUNEMP, x$TOTPOP)
        , WKHRS_M = weighted.mean(x$WKHRS_M, x$TOTPOP)
        , PCTCATH = weighted.mean(x$PCTCATH, x$TOTPOP)
        , PCTPROT = weighted.mean(x$PCTPROT, x$TOTPOP)
        , PCTURBN = mean(x$PCTURBN)
        , BELFAST = max(x$BELFAST)
        , DERRY = max(x$DERRY)
        ))
ward_data <- data.table(ward_data)


for(ward in unique(soadata@data$WARDCODE)){
  soadata@data[soadata@data$WARDCODE %in% ward, -c(1:2, 13:14)] <- ward_data[WARDCODE %in% ward, -1, with = F]
}


soadata_attributes <- soadata@data
soadata_attributes <- soadata_attributes[!duplicated(soadata_attributes$WARDCODE), ]
row.names(soadata_attributes) <- soadata_attributes$WARDCODE

soadata_agg <- unionSpatialPolygons(soadata, soadata$WARDCODE)
soadata_agg <- SpatialPolygonsDataFrame(soadata_agg, soadata_attributes)



########## NEW VARIABLE: BORDER WARDS
#ireland <- getData('GADM', country = 'IRL', level = 0)
# ireland <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS', 'IRL_adm0')
ireland <- readOGR('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS', 'IRL_adm0')
ireland@proj4string <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
ireland <- spTransform(ireland, ni_crs)
ireland@data <- soadata_agg@data[1, ]
ireland@data[1, ] <- rep(0, ncol(ireland@data))
row.names(ireland) <- '577'
soadata_dist <- spTransform(soadata_agg, ni_crs)
row.names(soadata_dist) <- as.character(seq(1, nrow(soadata_dist)))


soadata_dist <- spRbind(soadata_dist, ireland)
borders <- poly2nb(soadata_dist, snap = 500)

soadata_agg@data$BORDER <- 0
soadata_agg@data$BORDER[c(borders[577][[1]])] <- 1
spplot(soadata_agg, z = 'BORDER')




writeOGR(soadata_agg
#          , dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
         , dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
         , layer = 'SOA_agg_2001census'
         , driver = 'ESRI Shapefile'
         , overwrite_layer = T)



deaths <- fread('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\MergedDeaths_20141229.csv')
# deaths <- fread('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/MergedDeaths_20141229.csv')
deaths <- deaths[complete.cases(cbind(deaths$longitude, deaths$latitude)),]
geodeaths <- SpatialPointsDataFrame(cbind(deaths$longitude, deaths$latitude), data = deaths)
proj4string(geodeaths) <- CRS(proj4string(soadata_agg))

writeOGR(geodeaths
#          , dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
         , dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
         , layer = 'geodeaths'
         , driver = 'ESRI Shapefile'
         , overwrite_layer = T)


bases <- fread('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\BaseLocations_20141230.csv')
# bases <- fread('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/BaseLocations_20141230.csv')
bases <- bases[complete.cases(cbind(bases$Longitude, bases$Latitude)),]
geobases <- SpatialPointsDataFrame(cbind(bases$Longitude, bases$Latitude), data = bases)
proj4string(geobases) <- CRS(proj4string(soadata_agg))

writeOGR(geobases
#          , dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
         , dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
         , layer = 'geobases'
         , driver = 'ESRI Shapefile'
         , overwrite_layer = T)

