
# load libraries
library(plyr)
library(data.table)
library(sp)
library(sf)
library(raster)
library(maptools)
library(leaflet)
library(tmap)
library(rgdal)
library(units)
library(stringr)
library(tidyverse)
library(purrr)
library(DescTools)

# Import public sewers####
setwd("G:\\Desktop\\STW PDaS") #set WD
public <- st_read("old stuff/Assets-Asset_Base-Output-Infilled_Asset_Base_GIS.shp")
public_ <- public
public_$Current_year <- as.numeric(format(Sys.Date(), "%Y"))
public_$Age <- (public_$Current_year - (as.numeric(as.character(public_$Bndd_Yr)))); public_$Current_year <- NULL; public_$Bndd_Yr <- NULL
str(public_)
public_x <- data.table(public_[,c(1,2,3,7,6,8,18,19,20,21,22)])
public_asset <- st_as_sf(public_x, crs = 27700)
str(public_asset)
public_asset_df <- st_sf(public_asset)

# read and combine year 1-5 pipe data####
cctv_survey4 <- st_read("PDaS AMP6 Yr4 CORRECT Joined Data (all except PDaS WS53UP) at 03052019/CCTV survey.shp")
pipe4 <- st_read("PDaS AMP6 Yr4 CORRECT Joined Data (all except PDaS WS53UP) at 03052019/Pipe.shp")
cctv_survey3 <- st_read("PDaS AMP6 Yr3 CORRECT Joined Data/CCTV survey.shp")
pipe3 <- st_read("PDaS AMP6 Yr3 CORRECT Joined Data/Pipe.shp")
cctv_survey1 <- st_read("PDaS AMP6 Yr1 JOINED DATA v3/CCTV survey.shp")
pipe1 <- st_read("PDaS AMP6 Yr1 JOINED DATA v3/Pipe.shp")
cctv_survey_yr345 <- st_read("PDaS AMP5 Yr3&Yr4&Yr5(1site) JOINED DATA - butNOTcleansed/CCTV survey.shp")
pipe_yr345 <- st_read("PDaS AMP5 Yr3&Yr4&Yr5(1site) JOINED DATA - butNOTcleansed/Pipe.shp")
cctv_survey_yr2 <- st_read("PDaS AMP6 Yr2 JOINED DATA complete (NOTE some now FullyCleansed&UPLOADED to GISSTdb)/CCTV survey.shp")
pipe_yr2 <- st_read("PDaS AMP6 Yr2 JOINED DATA complete (NOTE some now FullyCleansed&UPLOADED to GISSTdb)/Pipe.shp")
pipe_all <- st_as_sf(rbind.fill(pipe1,pipe_yr2,pipe3,pipe4,pipe_yr345))
nrow(pipe_all)
pipe_x <- pipe_all[!duplicated(pipe_all$id),]
nrow(pipe_x)
cctv_survey_all <- st_as_sf(rbind.fill(cctv_survey1,cctv_survey_yr2,cctv_survey3,cctv_survey4,cctv_survey_yr345))
nrow(cctv_survey_all)
cctv_survey_x <- cctv_survey_all[!duplicated(cctv_survey_all$id),]
nrow(cctv_survey_x)

#read,clean and combine blockage data########
blockage <- data.table(read.csv("new blockages data/blockage_data_R.csv", header=TRUE, stringsAsFactors=FALSE))
blockage$IRF.Grid.Ref.X[blockage$IRF.Grid.Ref.X == "3869+3"] <- "386903"
summary(blockage)
colnames(blockage)[colnames(blockage) == "IRF.Grid.Ref.X"] <- "longitude"
colnames(blockage)[colnames(blockage) == "IRF.Grid.Ref.Y"] <- "latitude"
blockage$longitude <-str_pad(blockage$longitude, 6, side = "right", pad = "0")
blockage$latitude <-str_pad(blockage$latitude, 6, side = "right", pad = "0")
blockage$longitude <- as.numeric(blockage$longitude)
blockage$latitude <- as.numeric(blockage$latitude)
blockage$No..of.Incidents <- as.numeric(blockage$No..of.Incidents)
blockage_ <- as.data.table(blockage[,c("IRF.Blockage.Cause","IRF.Incident.date..post.PDaS.",
                                       "IRF.Transferred.Asset","IRF.Cause.Asset.Type","longitude", "latitude", "No..of.Incidents")])
blockage1 <- data.table(read.csv("new blockages data/Total Blockages_v2.csv", header=TRUE, stringsAsFactors=FALSE))
colnames(blockage1)[colnames(blockage1) == "CAUSE"] <- "IRF.Blockage.Cause"
colnames(blockage1)[colnames(blockage1) == "CATEGORY"] <- "IRF.Other.Blockage.Cause"
colnames(blockage1)[colnames(blockage1) == "INCDNTDATE"] <- "IRF.Incident.date..post.PDaS."
colnames(blockage1)[colnames(blockage1) == "TRANSASSET"] <- "IRF.Transferred.Asset"
colnames(blockage1)[colnames(blockage1) == "ASSETTYPE"] <- "IRF.Cause.Asset.Type"
colnames(blockage1)[colnames(blockage1) == "Grid.Ref.X"] <- "longitude"
colnames(blockage1)[colnames(blockage1) == "Grid.Ref.Y"] <- "latitude"
blockage1$longitude <-str_pad(blockage1$longitude, 6, side = "right", pad = "0")
blockage1$latitude <-str_pad(blockage1$latitude, 6, side = "right", pad = "0")
blockage1$longitude <- as.numeric(blockage1$longitude)
blockage1$latitude <- as.numeric(blockage1$latitude)
blockage1$No..of.Incidents <- 1
blockage2 <- blockage1 %>% filter((IRF.Transferred.Asset == "Y") & (IRF.Cause.Asset.Type %in% c("Combined sewer", "Foul sewer", "Surface water pipe", "Former S24 sewer")))
blockage2 <- as.data.table(blockage2[,c("IRF.Blockage.Cause","IRF.Incident.date..post.PDaS.",
                                        "IRF.Transferred.Asset","IRF.Cause.Asset.Type","longitude", "latitude", "No..of.Incidents")])
blockage2018 <- as.data.table(read.csv("new blockages data/2018.19 Cut.csv", header=TRUE, stringsAsFactors=FALSE))
colnames(blockage2018)[colnames(blockage2018) == "Blockage.Cause"] <- "IRF.Blockage.Cause"
colnames(blockage2018)[colnames(blockage2018) == "Incident.Date"] <- "IRF.Incident.date..post.PDaS."
colnames(blockage2018)[colnames(blockage2018) == "Transferred.Asset"] <- "IRF.Transferred.Asset"
colnames(blockage2018)[colnames(blockage2018) == "Asset.Type"] <- "IRF.Cause.Asset.Type"
colnames(blockage2018)[colnames(blockage2018) == "Grid.Ref.X"] <- "longitude"
colnames(blockage2018)[colnames(blockage2018) == "Grid.Ref.Y"] <- "latitude"
blockage2018$longitude <-str_pad(blockage2018$longitude, 6, side = "right", pad = "0")
blockage2018$latitude <-str_pad(blockage2018$latitude, 6, side = "right", pad = "0")
blockage2018$longitude <- as.numeric(blockage2018$longitude)
blockage2018$latitude <- as.numeric(blockage2018$latitude)
blockage2018$No..of.Incidents <- 1
blockage2018 <- na.omit(blockage2018)
blockage2018$IRF.Incident.date..post.PDaS. <- as.Date(blockage2018$IRF.Incident.date..post.PDaS. - 2,origin = "1900-01-01") #convert Date integer to date
blockage2018$IRF.Incident.date..post.PDaS. <- format(strptime(blockage2018$IRF.Incident.date..post.PDaS.,"%Y-%m-%d"),"%d/%m/%Y")
blockage2018x <- blockage2018 %>% filter((IRF.Transferred.Asset == "Y") & (IRF.Cause.Asset.Type %in% c("Combined sewer", "Foul sewer", "Surface water pipe", "Former S24 sewer")))
blockage2018x <- as.data.table(blockage2018x[,c("IRF.Blockage.Cause", "IRF.Incident.date..post.PDaS.",
                                                "IRF.Transferred.Asset","IRF.Cause.Asset.Type","longitude", "latitude", "No..of.Incidents")])
str(blockage_)
str(blockage2)
str(blockage2018x)
blockage_all <- rbind(blockage_, blockage2, blockage2018x)
str(blockage_all)
blockage_all$IRF.Blockage.Cause <- tolower(blockage_all$IRF.Blockage.Cause)
nrow(blockage_all)
summary(blockage_all$IRF.Incident.date..post.PDaS.)
blockage_x <- blockage_all[!duplicated(blockage_all),]
nrow(blockage_x)
blockage_sf <- st_as_sf(blockage_x, coords = c("longitude", "latitude"), crs = 27700)
blockage_sf$rowguid <- seq.int(nrow(blockage_sf))
st_write(blockage_sf, "markdown/blockage_sf.csv", layer_options = "GEOMETRY=AS_WKT")
sd_blockage <- tm_basemap("Esri.WorldStreetMap") + tm_shape(blockage_sf) + tm_symbols(size = 0.00000005, col = "IRF.Blockage.Cause") + tm_style("beaver")
pipex <- as(pipe_x, "Spatial")

# find the closest pipes to blockages####
## assign rules (median to material, type and filter), diameter <= 225mm)
FindNearest <- function(x, y, y.name = "y") {
  # Accepts two sf objects (x and y) and determines the nearest feature in y for
  # every feature in x.
  #
  # Args:
  #   x: An sf object containing the features for which you want find the
  #      nearest features in y
  #   y: An sf object containing features to be assigned to x
  #   y.name: Characters prepended to the y features in the returned datatable
  #
  # Returns:
  #   A datatable containing all rows from x with the corresponding nearest
  #   feature from y. A column representing the distance between the features is
  #   also included. Note that this object contains no geometry.

  #browser()

  # Determine CRSs
  message(paste0("x Coordinate reference system is ESPG: ", st_crs(x)$epsg))
  message(paste0("y Coordinate reference system is ESPG: ", st_crs(y)$epsg))

  # Transform y CRS to x CRS if required
  if (st_crs(x) != st_crs(y)) {
    message(paste0(
      "Transforming y coordinate reference system to ESPG: ",
      st_crs(x)$epsg
    ))
    y <- st_transform(y, st_crs(x))
  }

  # Compute distance matrix
  dist.matrix <- st_distance(x, y)

  # Select y features which are shortest distance
  nearest.rows <- apply(dist.matrix, 1, which.min)
  # Determine shortest distances
  nearest.distance <-
    dist.matrix[cbind(seq(nearest.rows), nearest.rows)]

  # Report distance units
  distance.units <- deparse_unit(nearest.distance)
  message(paste0("Distance unit is: ", distance.units))

  # Build data table of nearest features
  nearest.features <- y[nearest.rows,]
  nearest.features$distance <- nearest.distance
  nearest.features$incidents <- x$No..of.Incidents
  nearest.features$rowguid <- x$rowguid
  # Remove geometries
  st_geometry(x) <- NULL
  st_geometry(nearest.features) <- NULL

  # Prepend names to y columns
  names(nearest.features) <- paste0(y.name, ".", names(nearest.features))

  # Bind datatables and return
  #output <- cbind(x, nearest.features)
  output <- nearest.features
  return(output)
}
pipe_x1 <- pipe_x[,c("id","pipe_mat","ds_height", "systemtyp","length")]
colnames(pipe_x1)[colnames(pipe_x1) == "ds_height"] <- "pipe_diam"
colnames(pipe_x1)[colnames(pipe_x1) == "systemtyp"] <- "pipe_typ"
summary(pipe_x1)
pipe_x11 <- pipe_x1
pipe_x11$pipe_diam[which(is.na(pipe_x11$pipe_diam))] <- median(pipe_x11$pipe_diam, na.rm = TRUE)
pipe_x11$pipe_diam[pipe_x11$pipe_diam <= 50] <- 50
pipe_x11$pipe_mat[which(is.na(pipe_x11$pipe_mat))] <- "VC"
pipe_x11$pipe_typ[which(is.na(pipe_x11$pipe_typ))] <- "F"
pipe_x11$pipe_typ <- as.character(pipe_x11$pipe_typ)
pipe_x2 <- pipe_x11 %>% dplyr::filter(pipe_diam <= 225)
summary(pipe_x1) #156000 x 6..
summary(pipe_x2) #151000 x 6..
str(pipe_x2)
str(blockage_sf) #100518 x 6
pipe_sf <- st_as_sf(pipe_x2)
st_crs(pipe_sf) <- 27700
## map pipes to nearest public sewer and add public sewer's age and county
str(pipe_sf)
str(public_asset_df)
pipe_asset <- st_join(pipe_sf,public_asset_df , join = st_nearest_feature, left = TRUE)
pipe_asset2 <- pipe_asset[,c("id","pipe_mat","pipe_diam", "pipe_typ","length","County" ,"Age")]
summary(pipe_asset2)
st_crs(pipe_asset2) <- 27700
## filter nearest pipes (from above) to not more than 89 yrs
pipe_x2 <- merge(x=as.data.table(pipe_x2),y=pipe_asset2,by=c("id"))
pipe_x2 <- pipe_x2[,c(1:6,11:12)]
pipe_x2 <- pipe_x2 %>% filter(Age <= 89)
str(pipe_x2) #128965 x 8
keep_pipe_x2 <- pipe_x2[,c(1:8)]
pipe_sf <- st_as_sf(pipe_x2)
st_crs(pipe_sf) <- 27700
st_write(pipe_sf, "markdown/pipe_sf.csv", layer_options = "GEOMETRY=AS_WKT")
pipe_sf <- st_read("markdown/pipe_sf.csv")


# plot assets removed due to derived age
assets_removed <- pipe_sf[,c("WKT","id")]
pipe_x_1 <- pipe_x[,1]
assets_removed <- pipe_x_1 %>%  anti_join(as.data.table(assets_removed), by = "id")
assets_removed <- st_sf(assets_removed, crs = 27700)
tmap_mode("view")
qtm(assets_removed, basemap = "Esri.WorldStreetMap") + tm_style("beaver") #excluded assets
qtm(pipe_sf, basemap = "Esri.WorldStreetMap") + tm_style("albatross") #mapped assets

sum(pipe_x$length)


### Plot to show county
#tmap_mode("view")
#qtm(pipe_asset2, lines.col = "County", basemap = "Esri.WorldStreetMap")
my_split <- rep(1:50,each = 1, length = nrow(blockage_sf))
my_split2 <- mutate(blockage_sf, my_split = my_split)
my_split3 <- nest(my_split2, - my_split)
library(tictoc)
tic()
#models <- map(my_split3$data, ~ FindNearest(., pipe_sf)) #lengthy processing time - 6 hrs
toc()
nearest_blockage <- rbindlist(models, use.names = TRUE, fill = TRUE)
write.csv(nearest_blockage, "new blockages data/nearest_blockage10062019.csv")
nearest_blockage <- read.csv("new blockages data/nearest_blockage10062019.csv")
str(nearest_blockage)
summary(nearest_blockage)
nearest_blockage$y.distance <- as.numeric(nearest_blockage$y.distance)
# nearest blockage with 10m distance to pipe ####
## rename the nearest blockage columns
nearest_blockage1 <- nearest_blockage[order(nearest_blockage$y.distance),]
nearest_blockage1$X <- NULL
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.id"] <- "id"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.pipe_mat.x"] <- "pipe_mat"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.length.x"] <- "length"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.distance"] <- "distance"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.incidents"] <- "incidents"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.pipe_diam.x"] <- "pipe_diam"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.pipe_typ.x"] <- "pipe_typ"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.rowguid"] <- "rowguid"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.Age"] <- "Age"
colnames(nearest_blockage1)[colnames(nearest_blockage1) == "y.County"] <- "County"
nearest_blockage1 <- nearest_blockage1[,c("rowguid","id","pipe_mat","pipe_diam", "pipe_typ","length","distance","incidents")]
nearest_blockage1$pipe_diam[which(is.na(nearest_blockage1$pipe_diam))] <- median(nearest_blockage1$pipe_diam, na.rm = TRUE)
nearest_blockage1$pipe_diam[nearest_blockage1$pipe_diam <= 50] <- 50
nearest_blockage1$pipe_mat[which(is.na(nearest_blockage1$pipe_mat))] <- "VC"
nearest_blockage1$pipe_typ <- as.character(nearest_blockage1$pipe_typ)
nearest_blockage1$pipe_typ[which(is.na(nearest_blockage1$pipe_typ))] <- "C"
nearest_blockage1 <- nearest_blockage1 %>% dplyr::filter(pipe_diam <= 225)
nearest_blockage1$pipe_typ <- replace(nearest_blockage1$pipe_typ, nearest_blockage1$pipe_typ != "S", "C") #change pipe type to C or S
summary(nearest_blockage1)
write.csv(nearest_blockage1, "markdown/nearest_blockage1.csv")
st_write(pipe_sf, "markdown/total_pipes.csv",layer_options = "GEOMETRY=AS_XY")
nearest_blockage_10m <- nearest_blockage1 %>% filter(distance <= 10)
tail(nearest_blockage_10m)
str(nearest_blockage_10m)


## add county and age to all blocked pipes with 10m tolerance
nearest_blockage_10m_age1 <- merge(x=nearest_blockage_10m,y=pipe_sf,by=c("id"))
nearest_blockage_10m_age1 <- nearest_blockage_10m_age1[,c("id","rowguid","pipe_mat","pipe_diam","pipe_typ",
                                                          "length","distance","incidents", "WKT","County","Age", "geometry")]
### add GIS point of blockages to the above using rowguid from blockages_sf
nearest_blockage_10m_age2 <- merge(x=nearest_blockage_10m_age1,y=blockage_sf,by=c("rowguid"))
x10m_age_pipegeom <- nearest_blockage_10m_age2[,c("id","rowguid","pipe_mat","pipe_diam","pipe_typ",
                                                          "length","incidents","distance","County","Age", "geometry.x")]
nearest_blockage_10m_age <- nearest_blockage_10m_age2[,c("id","rowguid","pipe_mat","pipe_diam","pipe_typ",
                                                          "length","distance","incidents","County","Age", "geometry.y")]
# export to markdown
st_write(nearest_blockage_10m_age, "markdown/nearest_blockage_10m_age.csv", layer_options = "GEOMETRY=AS_WKT")
nearest_blockage_10m_age <- st_read("markdown/nearest_blockage_10m_age.csv", stringsAsFactors=FALSE)
nearest_blockage_10m_age$rowguid <- as.integer(nearest_blockage_10m_age$rowguid)
nearest_blockage_10m_age$pipe_diam <- as.integer(nearest_blockage_10m_age$pipe_diam)
nearest_blockage_10m_age$incidents <- as.integer(nearest_blockage_10m_age$incidents)
nearest_blockage_10m_age$Age <- as.integer(nearest_blockage_10m_age$Age)
nearest_blockage_10m_age$length <- as.integer(nearest_blockage_10m_age$length)
summary(nearest_blockage_10m_age)
nearest_blockage_10m_age <- st_sf(nearest_blockage_10m_age)
st_crs(nearest_blockage_10m_age) <- 27700
x10m_age_pipegeom <- st_sf(x10m_age_pipegeom)
st_crs(x10m_age_pipegeom) <- 27700
## plot blockages and pipes within 10m of each other
tmap_mode("view")
qtm(x10m_age_pipegeom, basemap = "Esri.WorldStreetMap") + tm_style("beaver") + tm_shape(nearest_blockage_10m_age) +
  tm_dots(size = 0.01, col = "Age") + tm_basemap("Esri.WorldStreetMap")

#plot blockages within 10m and higher
blockages_sf_age <- nearest_blockage1 %>% mutate(tolerance = ifelse(distance <= 10, "within 10m", "more than 10m"))
blockages_sf_age <- merge(x=blockages_sf_age,y=blockage_sf,by=c("rowguid"))
blockages_sf_age <- st_sf(blockages_sf_age)
st_crs(blockages_sf_age) <- 27700
st_write(blockages_sf_age, "markdown/blockages_sf_age.csv", layer_options = "GEOMETRY=AS_WKT")
#blockages_sf_age <- st_read("markdown/blockages_sf_age.csv")
tm_shape(blockages_sf_age) + tm_style("beaver") + tm_dots(size = 0.01, col = "tolerance") + tm_basemap("Esri.WorldStreetMap")
blockages_sf_age1 <- blockages_sf_age %>%  filter(tolerance == "more than 10m")
blockages_sf_age2 <- blockages_sf_age %>%  filter(tolerance == "within 10m")
tm_shape(blockages_sf_age1) + tm_style("beaver") + tm_dots(size = 0.01, col = "tolerance") + tm_basemap("Esri.WorldStreetMap")
tm_shape(blockages_sf_age2) + tm_style("grey") + tm_dots(size = 0.01, col = "tolerance") + tm_basemap("Esri.WorldStreetMap")


# calculate failure rate for each material & Diameter #####
x_10m <- nearest_blockage_10m_age
colnames(x_10m)[colnames(x_10m) == "pipe_mat.x"] <- "pipe_mat"
colnames(x_10m)[colnames(x_10m) == "pipe_diam.x"] <- "pipe_diam"
colnames(x_10m)[colnames(x_10m) == "pipe_typ.x"] <- "pipe_typ"
colnames(x_10m)[colnames(x_10m) == "length.x"] <- "length"
str(x_10m)
str(pipe_x2)
sum(x_10m$incidents)
sum(pipe_x2$length)
pipex222 <- pipe_x2
pipe_x2 <- pipex222
pipe_x2 <- as.data.table(pipe_x2)
x_10m <- as.data.table(x_10m)
pipe_x2$pipe_mat <- as.character(pipe_x2$pipe_mat)
x_10m$pipe_mat <- as.character(x_10m$pipe_mat)
x_10m$pipe_typ <- as.character(x_10m$pipe_typ)
## band unbanded materials
unband_band <- read.csv("Assets - Rulesets - Material Banding Table v2.csv")
x10m <- x_10m
unband_band$MaterialTable_UnbandedMaterial <- as.character(unband_band$MaterialTable_UnbandedMaterial)
x10m$pipe_mat <- as.character(x10m$pipe_mat)
pipe_x2$pipe_mat <- as.character(pipe_x2$pipe_mat)
x10m$pipe_typ <- replace(x10m$pipe_typ, x10m$pipe_typ != "S", "C") #change pipe type to C or S
pipe_x2$pipe_typ <- replace(pipe_x2$pipe_typ, pipe_x2$pipe_typ != "S", "C") #change pipe type to C or S
x10m$pipe_mat <- replace(x10m$pipe_mat, x10m$pipe_mat == "vc", "VC")
pipe_x2$pipe_mat <- replace(pipe_x2$pipe_mat, pipe_x2$pipe_mat == "vc", "VC")
x10m$pipe_mat <- replace(x10m$pipe_mat, x10m$pipe_mat == "V C", "VC")
pipe_x2$pipe_mat <- replace(pipe_x2$pipe_mat, pipe_x2$pipe_mat == "V C", "VC")
unique(pipe_x2$pipe_mat)
unique(x10m$pipe_mat)
### create banding for blocked pipes and all pipes
bandedx_10m <- x10m %>%
  left_join(unique(unband_band[,c(1,2)]),by = c("pipe_mat"="MaterialTable_UnbandedMaterial")) %>%
  dplyr::mutate(pipe_mat2 = (pipe_mat = as.character(MaterialTable_BandedMaterial)))
bandedpipe_x2 <- pipe_x2 %>%
  left_join(unique(unband_band[,c(1,2)]),by = c("pipe_mat"="MaterialTable_UnbandedMaterial")) %>%
  dplyr::mutate(pipe_mat2= (pipe_mat = as.character(MaterialTable_BandedMaterial)))
bandedx_10m$pipe_mat <- NULL
bandedpipe_x2$pipe_mat <- NULL
bandedpipe_x2$pipe_mat.x <- NULL
bandedpipe_x2$pipe_typ.x <- NULL
bandedx_10m$MaterialTable_BandedMaterial <- NULL
bandedpipe_x2$MaterialTable_BandedMaterial <- NULL
colnames(bandedpipe_x2)[colnames(bandedpipe_x2) == "pipe_diam.x"] <- "pipe_diam"
colnames(bandedpipe_x2)[colnames(bandedpipe_x2) == "length.x"] <- "length"
colnames(bandedpipe_x2)[colnames(bandedpipe_x2) == "geometry.x"] <- "geometry"
colnames(bandedx_10m)[colnames(bandedx_10m) == "pipe_mat2"] <- "pipe_mat"
colnames(bandedpipe_x2)[colnames(bandedpipe_x2) == "pipe_mat2"] <- "pipe_mat"
bandedx_10m$pipe_mat <- as.character(bandedx_10m$pipe_mat)
bandedx_10m$pipe_mat[bandedx_10m$pipe_mat==""]<-NA
bandedx_10m$pipe_mat[which(is.na(bandedx_10m$pipe_mat))] <- "Others"
bandedpipe_x2$pipe_mat <- as.character(bandedpipe_x2$pipe_mat)
bandedpipe_x2$pipe_mat[bandedpipe_x2$pipe_mat==""]<-NA
bandedpipe_x2$pipe_mat[which(is.na(bandedpipe_x2$pipe_mat))] <- "Others"
bandedx_10m$geometry <- NULL
bandedpipe_x2$geometry <- NULL
bandedx_10m$pipe_typ <- replace(bandedx_10m$pipe_typ, bandedx_10m$pipe_typ != "S", "C") #change pipe type to C or S
bandedpipe_x2$pipe_typ <- replace(bandedpipe_x2$pipe_typ, bandedpipe_x2$pipe_typ != "S", "C") #change pipe type to C or S
str(bandedx_10m)
str(bandedpipe_x2)
unique(bandedx_10m$pipe_mat)
unique(bandedpipe_x2$pipe_mat)
write.csv(bandedpipe_x2, "markdown/bandedpipe_x2.csv")
write.csv(bandedx_10m, "markdown/bandedx_10m.csv")
total_pipex2_length <- bandedpipe_x2 %>%
  dplyr::select(pipe_mat, pipe_diam, pipe_typ,length) %>%
  dplyr::group_by(pipe_mat,pipe_diam, pipe_typ) %>%
  dplyr::summarise(sum.l = sum(length))
total_x10m_length <- bandedx_10m %>%
  dplyr::select(pipe_mat, pipe_diam, pipe_typ,length,incidents) %>%
  dplyr::group_by(pipe_mat,pipe_diam, pipe_typ,incidents) %>%
  dplyr::summarise(sum.l = sum(length))

# find diameter rates####
find_greater_equals <- function(d,e,t,m,y){
  #d - upper bound diameter
  #e - lower bound diameter
  #t - pipe type
  #m - pipe material
  #y - blockage data
  bandedpipe_x2 <- as.data.table(bandedpipe_x2)
  y <- as.data.table(y)
  bandedpipe_x2$pipe_mat <- as.character(bandedpipe_x2$pipe_mat)
  y$pipe_mat <- as.character(y$pipe_mat)
  v <- bandedpipe_x2 %>%
    dplyr::filter((pipe_diam <= d & pipe_mat == m & pipe_typ == t) & (pipe_diam > e & pipe_mat == m & pipe_typ == t)) %>%
    dplyr::group_by(pipe_mat) %>%
    dplyr::summarise(sum.l = sum(length))
  z <- y %>%
    dplyr::filter((pipe_diam <= d & pipe_mat == m & pipe_typ == t) & (pipe_diam > e & pipe_mat == m & pipe_typ == t)) %>%
    dplyr::group_by(pipe_mat) %>%
    dplyr::summarise(incidents = sum(incidents)) %>%
    left_join(v,by = c("pipe_mat")) %>%
    mutate(Failure.rate = (incidents/(sum.l/1000))/4)
  print(z)
  paste0(v$sum.l, " is the sum of length")
}
find_greater_equals_1 <- function(d,e,t,m,y){
  #d - upper bound diameter
  #e - lower bound diameter
  #t - pipe type
  #m - pipe material
  #y - blockage data
  bandedpipe_x2 <- as.data.table(bandedpipe_x2)
  y <- as.data.table(y)
  bandedpipe_x2$pipe_mat <- as.character(bandedpipe_x2$pipe_mat)
  y$pipe_mat <- as.character(y$pipe_mat)
  v <- bandedpipe_x2 %>%
    dplyr::filter((pipe_diam <= d & pipe_mat == m & pipe_typ == t) & (pipe_diam > e & pipe_mat == m & pipe_typ == t)) %>%
    dplyr::group_by(pipe_mat) %>%
    dplyr::summarise(sum.l = sum(length))
  z <- y %>%
    dplyr::filter((pipe_diam <= d & pipe_mat == m & pipe_typ == t) & (pipe_diam > e & pipe_mat == m & pipe_typ == t)) %>%
    dplyr::group_by(pipe_mat) %>%
    dplyr::summarise(incidents = sum(incidents)) %>%
    left_join(v,by = c("pipe_mat")) %>%
    mutate(Failure.rate = (incidents/(sum.l/1000))/4) %>%
    mutate(Diameter = paste((e+0.1),d,sep = "-")) %>%
    dplyr::select(pipe_mat, Diameter,incidents,sum.l , Failure.rate) %>%
    dplyr::rename("TotalLength" = sum.l) %>%
    dplyr::rename("Material" = pipe_mat) %>%
    dplyr::rename("FailureRate" = Failure.rate)
  print(z)
}

# Number of blockages per county
#blockages_county <- #nearest_blockage1_geom %>%
#dplyr::group_by(County) %>%
#  dplyr::summarise(sum.incidents = sum(incidents), sum.l = sum(length.x)) %>%
#  mutate(Failure.rate = (sum.incidents/(sum.l/1000))/4) %>%
#  arrange(sum.incidents)
bandedpipelength <- bandedpipe_x2 %>%
  dplyr::group_by(County) %>%
  dplyr::summarise(sum.l = sum(length))
blockages_10m_county <- x10m %>%
  dplyr::group_by(County) %>%
  dplyr::summarise(sum.incidents = sum(incidents)) %>%
  dplyr::left_join(bandedpipelength, by = "County") %>%
  mutate(Failure.rate = (sum.incidents/(sum.l/1000))/4) %>%
  arrange(sum.incidents)

find_greater_equals(75,0,"C","VC",bandedx_10m)
find_greater_equals(75,0,"S","VC",bandedx_10m)
find_greater_equals(100,75,"C","VC",bandedx_10m)
find_greater_equals(100,75,"S","VC",bandedx_10m)
find_greater_equals(150,100,"C","VC",bandedx_10m)
find_greater_equals(150,100,"S","VC",bandedx_10m)
find_greater_equals(225,150,"C","VC",bandedx_10m)
find_greater_equals(225,150,"S","VC",bandedx_10m)

find_greater_equals(75,0,"C","PVC",bandedx_10m)
find_greater_equals(75,0,"S","PVC",bandedx_10m)
find_greater_equals(100,75,"C","PVC",bandedx_10m)
find_greater_equals(100,75,"S","PVC",bandedx_10m)
find_greater_equals(150,100,"C","PVC",bandedx_10m)
find_greater_equals(150,100,"S","PVC",bandedx_10m)
find_greater_equals(225,150,"C","PVC",bandedx_10m)
find_greater_equals(225,150,"S","PVC",bandedx_10m)

find_greater_equals(75,0,"C","Others",bandedx_10m)
find_greater_equals(75,0,"S","Others",bandedx_10m)
find_greater_equals(100,75,"C","Others",bandedx_10m)
find_greater_equals(100,75,"S","Others",bandedx_10m)
find_greater_equals(150,100,"C","Others",bandedx_10m)
find_greater_equals(150,100,"S","Others",bandedx_10m)
find_greater_equals(225,150,"C","Others",bandedx_10m)
find_greater_equals(225,150,"S","Others",bandedx_10m)

find_greater_equals(75,0,"C","CI",bandedx_10m)
find_greater_equals(75,0,"S","CI",bandedx_10m)
find_greater_equals(100,75,"C","CI",bandedx_10m)
find_greater_equals(100,75,"S","CI",bandedx_10m)
find_greater_equals(150,100,"C","CI",bandedx_10m)
find_greater_equals(150,100,"S","CI",bandedx_10m)
find_greater_equals(225,150,"C","CI",bandedx_10m)
find_greater_equals(225,150,"S","CI",bandedx_10m)

find_greater_equals(75,0,"C","BR",bandedx_10m)
find_greater_equals(75,0,"S","BR",bandedx_10m)
find_greater_equals(100,75,"C","BR",bandedx_10m)
find_greater_equals(100,75,"S","BR",bandedx_10m)
find_greater_equals(150,100,"C","BR",bandedx_10m)
find_greater_equals(150,100,"S","BR",bandedx_10m)
find_greater_equals(225,150,"C","BR",bandedx_10m)
find_greater_equals(225,150,"S","BR",bandedx_10m)

find_greater_equals(75,0,"C","CO",bandedx_10m)
find_greater_equals(75,0,"S","CO",bandedx_10m)
find_greater_equals(100,75,"C","CO",bandedx_10m)
find_greater_equals(100,75,"S","CO",bandedx_10m)
find_greater_equals(150,100,"C","CO",bandedx_10m)
find_greater_equals(150,100,"S","CO",bandedx_10m)
find_greater_equals(225,150,"C","CO",bandedx_10m)
find_greater_equals(225,150,"S","CO",bandedx_10m)

## unique diameter rates
a1 <- find_greater_equals_1(225,0,"C","CO",bandedx_10m)
a2 <- find_greater_equals_1(100,0,"C","PVC",bandedx_10m)
a3 <- find_greater_equals_1(225,100,"C","PVC",bandedx_10m)
a4 <- find_greater_equals_1(100,0,"C","VC",bandedx_10m)
a5 <- find_greater_equals_1(225,100,"C","VC",bandedx_10m)
a6 <- find_greater_equals_1(225,100,"C","CI",bandedx_10m)
a7 <- find_greater_equals_1(100,0,"C","Others",bandedx_10m)
a8 <- find_greater_equals_1(225,100,"C","Others",bandedx_10m)
a_all <- rbind.data.frame(a1,a2,a3,a4,a5,a6,a7,a8)
a_all

# surface water sewer plot for CO, VC, PVC
b1 <- find_greater_equals_1(150,100,"S","CO",bandedx_10m)
b2 <- find_greater_equals_1(100,75,"S","PVC",bandedx_10m)
b3 <- find_greater_equals_1(150,100,"S","PVC",bandedx_10m)
b4 <- find_greater_equals_1(225,150,"S","PVC",bandedx_10m)
b5 <- find_greater_equals_1(75,0,"S","VC",bandedx_10m)
b6 <- find_greater_equals_1(100,75,"S","VC",bandedx_10m)
b7 <- find_greater_equals_1(150,100,"S","VC",bandedx_10m)
b8 <- find_greater_equals_1(225,150,"S","VC",bandedx_10m)
b_all <- rbind.data.frame(b1,b2,b3,b4,b5,b6,b7,b8)

b_all2 <- bandedpipe_x2 %>%
  dplyr::filter(!is.na(pipe_mat)) %>%
  dplyr::filter(pipe_typ == "C") %>%
  dplyr::group_by(pipe_mat, pipe_diam) %>%
  dplyr::summarise(sum.l = sum(length))
z <- bandedx_10m %>%
  dplyr::filter(pipe_typ == "C") %>%
  dplyr::group_by(pipe_mat, pipe_diam) %>%
  dplyr::summarise(incidents = sum(incidents)) %>%
  left_join(b_all2,by = c("pipe_mat")) %>%
  mutate(Failure.rate = (incidents/(sum.l/1000))/4)
z1 <- z %>% dplyr::filter((pipe_diam.x == pipe_diam.y))
z2 <- z %>% dplyr::filter((pipe_mat == "CO") |
              (pipe_mat == "VC") | (pipe_mat == "PVC")) %>%
           dplyr::filter((pipe_diam.x == pipe_diam.y))

ggplot(z1[,c(1,2,6)]) +
  geom_point(aes(x = pipe_diam.x, y = Failure.rate, col = pipe_mat)) +
  geom_line(aes(x = pipe_diam.x, y = Failure.rate, col = pipe_mat)) +
  labs(title = "Diameter Rates", y = "Blockage Rate (blockages/km/year)", x = "Diameter") +
  theme(legend.position = "bottom")
ggplot(z2[,c(1,2,6)]) +
         geom_point(aes(x = pipe_diam.x, y = Failure.rate, col = pipe_mat)) +
         geom_line(aes(x = pipe_diam.x, y = Failure.rate, col = pipe_mat)) +
         labs(title = "Diameter Rates", y = "Blockage Rate (blockages/km/year)", x = "Diameter") +
         theme(legend.position = "bottom")


# generate age coefficients####
pipe_x22 <- pipe_x2
pipe_x22 <- pipe_x22[,c(1:8)]
pipe_x22$Age <- as.numeric(as.character(pipe_x22$Age))
pipe_x22 <- pipe_x22 %>%  dplyr::rename("pipe_mat" = pipe_mat.x) %>%
  dplyr::rename("pipe_diam" = pipe_diam.x) %>%
  dplyr::rename("pipe_typ" = pipe_typ.x) %>%
  dplyr::rename("length" = length.x)
pipe_x22$pipe_typ <- replace(pipe_x22$pipe_typ, pipe_x22$pipe_typ != "S", "C")
pipe_x22 <- pipe_x22 %>%  dplyr::filter(pipe_typ == "C")
nearest_blockage1$pipe_typ <- replace(nearest_blockage1$pipe_typ, nearest_blockage1$pipe_typ != "S", "C")
nearest_blockage1 <- nearest_blockage1 %>%  dplyr::filter(pipe_typ == "C")
nearest_blockage_age <- merge(x=nearest_blockage1, y=pipe_x22,by=c("id"))
agecoef_pipelength <- bandedpipe_x2 %>%
  dplyr::filter(pipe_typ == "C") %>%
  dplyr::group_by(Age) %>%
  dplyr::summarise(sum.l = sum(length))
nearest_blockage_age1 <- nearest_blockage_age %>%
  dplyr::group_by(Age) %>%
  dplyr::summarise(sum.incidents = sum(incidents)) %>%
  dplyr::left_join(agecoef_pipelength, by = "Age") %>%
  mutate(Failure.rate = (sum.incidents/(sum.l/1000))/4) %>%
  arrange(Age)
## best line of fit
### cluster ages <40 yrs
FR_age5.1 <- nearest_blockage_age1
FR_age5.2 <- FR_age5.1[1:4,-c(4)] %>% mutate(Age = max(Age), sum.incidents = sum(sum.incidents), sum.l = sum(sum.l)) %>%  unique()
FR_age5.3 <- FR_age5.1[-c(1:4),-c(4)]
FR_age5 <- rbind(FR_age5.2,FR_age5.3)
FR_age5x <- FR_age5 %>%
  mutate(ProportionOfIncidents = sum.incidents/sum(sum.incidents)) %>%
  mutate(weighted.length = sum.l/sum(sum.l)) %>%
  mutate(new.Failure.rate = (ProportionOfIncidents/(weighted.length)))
#FR_age5x <- FR_age5x[-c(1),]
plot3 <- ggplot(FR_age5x, aes(x = Age, y = new.Failure.rate)) +
  geom_jitter(aes(size = ProportionOfIncidents)) +
  geom_smooth(aes(weight = ProportionOfIncidents), method = "lm") +
  labs(title = "Deterioration Relationship", y = "Age Factor", x = "Age")
plot3

lm_age5 <- lm(FR_age5x$new.Failure.rate ~ FR_age5x$Age, weights = FR_age5x$ProportionOfIncidents)
summary(lm_age5)
mco = coef(summary(lm_age5))
mco[, 1]
predict(lm_age5)

#outlier determination
quantile(FR_age5x$new.Failure.rate, .75) + 1.5 * (IQR(FR_age5x$new.Failure.rate)) #Q3+1.5IQR,Q1 - 1.5IQR


# Diameter rates
## RAW materials
pipe_x22 <- as.data.frame(pipe_x2)
pipe_x22 <- pipe_x22[,c(1:8)]
pipe_x22$Age <- as.numeric(as.character(pipe_x22$Age))
pipe_x22 <- pipe_x22 %>%  dplyr::rename("pipe_mat" = pipe_mat.x) %>%
  dplyr::rename("pipe_diam" = pipe_diam.x) %>%
  dplyr::rename("pipe_typ" = pipe_typ.x) %>%
  dplyr::rename("pipe_length" = length.x)
pipe_x22$pipe_mat <- as.character(pipe_x22$pipe_mat)
nearest_blockage_age <- merge(x=nearest_blockage1, y=pipe_x22,by=c("id"))
nearest_blockage_age$pipe_mat.x <- as.character(nearest_blockage_age$pipe_mat.x)
pipe_x22$pipe_typ <- replace(pipe_x22$pipe_typ, pipe_x22$pipe_typ != "S", "C") #change pipe type to C or S
unbandedpipelength <- pipe_x22 %>%
  dplyr::filter(pipe_typ == "C") %>%
  dplyr::group_by(pipe_mat,pipe_diam) %>%
  dplyr::summarise(sum.l = sum(pipe_length))
nearest_blockage_age3 <- as.data.frame(nearest_blockage_age) %>%
  dplyr::group_by(pipe_mat.x,pipe_diam.x,pipe_typ.x) %>%
  dplyr::summarise(sum.incidents = sum(incidents)) %>%
  dplyr::left_join(as.data.frame(unbandedpipelength), by = c("pipe_mat.x"="pipe_mat",
              "pipe_diam.x"="pipe_diam")) %>%
  mutate(Failure.rate = (sum.incidents/(sum.l/1000))/4)
FR_age8 <- as.data.table(nearest_blockage_age3)
FR_age8x <- FR_age8[-c(1),]
FR_age9 <- FR_age8x %>%
  dplyr::filter(pipe_typ.x == "C") %>%
  mutate(ProportionOfIncidents = sum.incidents/sum(sum.incidents)) %>%
  mutate(weighted.length = sum.l/sum(sum.l)) %>%
  mutate(new.Failure.rate = (ProportionOfIncidents/(weighted.length)))
plot98 <- ggplot(FR_age9) +
  geom_point(aes(x = pipe_diam.x, y = new.Failure.rate, col = pipe_mat.x)) +
  geom_line(aes(x = pipe_diam.x, y = new.Failure.rate, col = pipe_mat.x)) +
  labs(title = "Diameter Rates (Unbanded)", y = "Blockage Rate (blockages/km/year)", x = "Diameter") +
  theme(legend.position = "bottom")
plot98

#grouped materials
bandedpipe_x2$pipe_typ <- replace(bandedpipe_x2$pipe_typ, bandedpipe_x2$pipe_typ != "S", "C") #change pipe type to C or S
nearest_blockage_age$pipe_typ.x <- replace(nearest_blockage_age$pipe_typ.x, nearest_blockage_age$pipe_typ.x != "S", "C") #change pipe type to C or S
bandedpipelength <- as.data.frame(bandedpipe_x2) %>%
  dplyr::filter(pipe_typ == "C") %>%
  dplyr::group_by(pipe_mat,pipe_diam) %>%
  dplyr::summarise(sum.l = sum(length))
bandedpipelength <- as.data.frame(bandedpipelength)
banded_nearest_blockages <- nearest_blockage_age %>%
  left_join(unique(unband_band[,c(1,2)]),by = c("pipe_mat.x"="MaterialTable_UnbandedMaterial")) %>%
  dplyr::mutate(pipe_mat2 = (pipe_mat = as.character(MaterialTable_BandedMaterial)))
banded_nearest_blockages$pipe_mat.x<- NULL
banded_nearest_blockages$MaterialTable_BandedMaterial <- NULL
colnames(banded_nearest_blockages)[colnames(banded_nearest_blockages) == "pipe_mat2"] <- "pipe_mat"
banded_nearest_blockages$pipe_mat <- as.character(banded_nearest_blockages$pipe_mat)
banded_nearest_blockages$pipe_mat[banded_nearest_blockages$pipe_mat==""]<-NA
banded_nearest_blockages$pipe_mat[which(is.na(banded_nearest_blockages$pipe_mat))] <- "Others"
banded_nearest_blockages1 <- as.data.frame(banded_nearest_blockages) %>%
  dplyr::group_by(pipe_mat,pipe_diam.x,pipe_typ.x) %>%
  dplyr::summarise(sum.incidents = sum(incidents)) %>%
  dplyr::left_join(as.data.frame(bandedpipelength), by = c("pipe_mat"="pipe_mat",
              "pipe_diam.x"="pipe_diam")) %>%
  mutate(Failure.rate = (sum.incidents/(sum.l/1000))/4)
banded_nearest_blockages1 <- as.data.frame(banded_nearest_blockages1)
FR_age6 <- as.data.table(banded_nearest_blockages1)
FR_age7 <- FR_age6 %>%
  dplyr::filter(pipe_typ.x == "C") %>%
  mutate(ProportionOfIncidents = sum.incidents/sum(sum.incidents)) %>%
  mutate(weighted.length = sum.l/sum(sum.l)) %>%
  mutate(new.Failure.rate = (ProportionOfIncidents/(weighted.length)))
plot99 <- ggplot(FR_age7) +
  geom_point(aes(x = pipe_diam.x, y = new.Failure.rate, col = pipe_mat)) +
  geom_line(aes(x = pipe_diam.x, y = new.Failure.rate, col = pipe_mat)) +
  labs(title = "Diameter Rates (Banded)", y = "Blockage Rate (blockages/km/year)", x = "Diameter") +
  theme(legend.position = "bottom")
plot99
