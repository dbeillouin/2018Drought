library(dplyr)
library(eurostat)
library(sf)
## Linking to GEOS 3.6.2, GDAL 2.2.3, PROJ 4.9.3
library(tmap)

# Download attribute data from Eurostat
sp_data <- eurostat::get_eurostat("tgs00026",
                                  time_format = "raw",
                                  stringsAsFactors = FALSE) %>% 
  # subset to have only a single row per geo
  dplyr::filter(time == 2010, nchar(geo) == 4) %>% 
  # categorise
  dplyr::mutate(income = cut_to_classes(values, n = 5))
## Table tgs00026 cached at /tmp/RtmpNhcuBI/eurostat/tgs00026_raw_code_FF.rds
# Download geospatial data from GISCO
geodata <- get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
                                   nuts_level = 1,
                                   year = 2013)

t1 <- get_eurostat("tsdtr420") %>% filter(geo %in% c("UK", "FR", "PL", "ES", "PT"))
## Joining, by = "geo"
RES2<-RES
#RES2 %<>% filter(Country=="GE")
RES2 %<>% filter(Crop=="wheat_winter")
RES2 %<>% filter(Year=="2016")
RES2 %<>% group_by(Country,Crop, NUTS3) %>%summarise_all(funs(mean), na.rm=TRUE)
names(RES2)[3]<- "NUTS_NAME"
RES2 %<>% select(c("NUTS_NAME","anomaly_spline_norm"))
RES2 %<>% filter(!is.na(NUTS_NAME))
RES2 %<>% filter(!NUTS_NAME=="")


geodata$NUTS_NAME<-tolower(geodata$NUTS_NAME)
geodata$NUTS_NAME<-gsub("(.*),.*", "\\1", geodata$NUTS_NAME)
RES2$NUTS_NAME<-tolower(RES2$NUTS_NAME)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}
geodata$NUTS_NAME<-Unaccent(geodata$NUTS_NAME)
geodata$NUTS_NAME <- gsub("-", "_", geodata$NUTS_NAME)
geodata$NUTS_NAME <- gsub(" ", "_", geodata$NUTS_NAME)

#setdiff(geodata$NUTS_NAME, RES2$NUTS_NAME)
setdiff(RES2$NUTS_NAME, geodata$NUTS_NAME)

geodata %>%   filter(grepl("aachen",NUTS_NAME))


listNUTS2<- geodata %>%  select("id","NUTS_NAME","CNTR_CODE")

map_data <- inner_join(geodata, RES2)
#map_data <- left_join( RES2, geodata)


#Code_geo_time  <-  read.delim("~/Documents/2018Drought/Raw_data_yield/Code_NUTS2.csv")
#RES2  <- RES %>%   left_join(., Code_geo_time, by=c("NUTS2"))


map_data$class<-cut(map_data$anomaly_spline_norm, 10)
map_data %<>% filter(!is.na(anomaly_spline_norm))
                     
map1 <- tmap::tm_shape(geodata) +
  tmap::tm_fill("lightgrey") +
  tmap::tm_shape(map_data, xlim=-10) +
  tmap::tm_grid() +
  tmap::tm_polygons("class", title = "spline anomalies",  
                    palette = "Oranges")

print(map1)
