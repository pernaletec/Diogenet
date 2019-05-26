
library(ggplot2)
library(tidyverse)
library(pleiades)
library(sf)
library(devtools)
library(georeference)
library(geosphere)
library(leaflet)
library(magrittr)
library(shiny)
library(sp)
library(htmlwidgets)


install_github("editio/georeference")

read_georef = FALSE

# select nodes with attribute Place from newNodes.csv
# create new dataframe with places only = locations

nodes = read.csv(file="old_Nodes.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
places <- nodes$Name[nodes$Groups=="Place"]

##############################
# Reading data from pleiades #
##############################

# A key here is to use "%hellenistic-republican%". Different periods are combined with commas in the data base
# hence it is necessary to check for the string as part of the "timePeriodsKeys" string
gz <- as.data.frame(pleiades::pl_search('SELECT * FROM names WHERE "timePeriodsKeys" LIKE "%hellenistic-republican%"')) 
itsct = sapply(places, function(i) grepl(i, gz$nameTransliterated))
numRepPlaces = apply(itsct, 2, FUN = function(x) length(x[x == TRUE]))
det_pleiades = apply(itsct, 2, FUN = function(x) return(c(length(x[x == TRUE]),which(x == TRUE))))

###################
# Organizing data #
###################

lack_data = c()
avail_data = data.frame(name = c(0), lat = c(0), lon = c(0), source = c(0))
j = 1
h = 1
for (k in 1:length(places)) {
  if (det_pleiades[[k]][1] == 0) {
    lack_data[j] = as.character(places[as.integer(k)])
    j = j+1
  } else {
    for (w in 1:det_pleiades[[k]][1]){
      #browser()
      pos_gz = det_pleiades[[k]][w+1]
      avail_data[h,1]=as.character(places[as.integer(k)])
      avail_data[h,2]=gz$reprLat[pos_gz]
      avail_data[h,3]=gz$reprLong[pos_gz]
      avail_data[h,4]= as.character("pleiades")
      h = h+1
    }
  }
}

avail_data = na.omit(avail_data)

#################################
# Reading data from Jacobo file #
#################################

# Look up places that are in the dataframe 'locations' in gz and add lon to them
# write_csv(locations, "data/locations.csv")

given_loc = read.csv("locations.csv", stringsAsFactors = FALSE)
given_loc_ = data.frame(name = given_loc$name, 
                        lat=given_loc$lat, 
                        lon = given_loc$lon, 
                        source=rep("Jacobo", length(given_loc$name)))

# Combining all data 

avail_data = rbind(avail_data, given_loc_)

############################
# Reading data from georef #
############################

if (read_georef){
  res_georef = list()
  res_georef_df  = data.frame = data.frame(name = c(0), lat = c(0), lon = c(0), source = c(0))
  for (j in 1:length(places)) res_georef[[j]] = georeference::georef(places[j], source = "pelagios")  
  avail_georef <- which(sapply(res_georef, function(x) if (dim(x)[2] == 3) return(FALSE) else return(TRUE)))
  res_georef = res_georef[avail_georef]
  for (j in 1:length((res_georef))) {
    res_georef_df[j,1]=as.character(res_georef[[j]]$name)
    res_georef_df[j,2]=res_georef[[j]]$lat
    res_georef_df[j,3]=res_georef[[j]]$lon
    res_georef_df[j,4]=as.character("georef")
  }
  
  # Combining all data 
  # These are the available places
  avail_data = rbind(avail_data, res_georef_df)
  avail_data = avail_data[order(avail_data$name),]
}

itsct_ii = lapply(places, function(i) any(grepl(i, avail_data$name)))
final_avail = as.logical(as.numeric(itsct_ii))

# These are the available places!
places_available = places[which(final_avail)]
# These are the places that are not available in pleiades nor in georef nor in Jacobo's loc file
places_unavailable = places[-which(final_avail)]

##########################
# Selecting travels data #
##########################

## 1. create new table from newNodes and newEdges.
## 2. identify nodes that are in both relations "is from" and "travelled to"

edges = read.csv(file="old_Edges.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

# Origin of phylosophers 
names_origin <- edges$Source[which(edges$Relation == "is from")]
origin_places <- edges$Target[which(edges$Relation == "is from")]

# Validations!!!!
# Do places as Target in "is from" have identified localization??
id_knw_orig = which(origin_places %in% places_available)
# Known places
knw_origin_loc = origin_places[id_knw_orig]
# Unknown places
unk_origin_loc = origin_places[-id_knw_orig]
# Phylosophers with known origin
knw_origin_phy = names_origin[id_knw_orig]
# Phylosophers with unknown origin
unk_origin_phy = names_origin[-id_knw_orig]

# Travelers!
names_traveler <- edges$Source[which(edges$Relation == "traveled to")]
traveler_target <- edges$Target[which(edges$Relation == "traveled to")]

# Validations!!!!
# Which travelers are from known origin??
id_knw_loc_orig_trav = which(names_traveler %in% knw_origin_phy)
# Every traveler is from a known loc place?
isTRUE(length(id_knw_loc_orig_trav) == length(names_traveler)) 
# How many traveler's origin need to be identified?
dif_trav = length(names_traveler) - length(id_knw_loc_orig_trav)
# Do every destination have a known location?
id_knw_loc_trav_dest = which(traveler_target %in% places_available)
# Joint condition "known location origin phylosopher" + "known location traveler destrination"
intsect_condition = intersect(id_knw_loc_orig_trav,id_knw_loc_trav_dest)

# Travelers with joint condition 
knw_all_names_trav = names_traveler[intsect_condition]
# Destinations with joint condition
knw_all_travl_trgt = traveler_target[intsect_condition]


## 3. turn it into something like this:
##   source   target  name
##   Athens   Egypt   Pythagoras

travel_edges = data.frame(source = rep("", length(knw_all_names_trav)), 
                          target = knw_all_travl_trgt, 
                          name=knw_all_names_trav,
                          lat_source=rep("", length(knw_all_names_trav)), 
                          lon_source = rep("", length(knw_all_names_trav)),
                          lat_target=rep("", length(knw_all_names_trav)), 
                          lon_target = rep("", length(knw_all_names_trav)),
                          stringsAsFactors = FALSE)


all_places = sort(unique(c(travel_edges$source, travel_edges$target)), decreasing = FALSE)
## After getting all the places for which there IS at least one location identified 
## there was a "manual" search using the shiny app developed
all_places_full_data = read.csv(file = "locations_data.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

for (k in 1:length(travel_edges$source)) {
  print(paste0(k, " ", knw_origin_loc[which(knw_origin_phy == travel_edges$name[k])]))
  print(paste0(k, " ", travel_edges$name[k]))
  
  travel_edges$source[k] = (knw_origin_loc[which(knw_origin_phy == travel_edges$name[k])])
  travel_edges$lat_source[k] = (all_places_full_data$lat[which(all_places_full_data$name == travel_edges$source[k])])
  travel_edges$lon_source[k] = (all_places_full_data$lon[which(all_places_full_data$name == travel_edges$source[k])])
  travel_edges$lat_target[k] = (all_places_full_data$lat[which(all_places_full_data$name == travel_edges$target[k])])
  travel_edges$lon_target[k] = (all_places_full_data$lon[which(all_places_full_data$name == travel_edges$target[k])])
  }


###########################################
#########                     #############  
########     LEAFLET MAP       ############
#########                     #############
###########################################


avail_data_tb = as_tibble(travel_edges)

node_count = c(travel_edges$source, travel_edges$target)
node_count = as.data.frame(table(node_count))

all_places_full_data$degree = node_count$Freq[which(node_count$node_count == all_places_full_data$name)]

tcu_map = "https://api.tiles.mapbox.com/v3/{id}/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiaXNhd255dSIsImEiOiJBWEh1dUZZIn0.SiiexWxHHESIegSmW8wedQ"

map_attr = "Map data &copy<a href='http://openstreetmap.org'>OpenStreetMap</a> contributors <a href='http://creativecommons.org/licenses/by-sa/2.0/'>CC-BY-SA</a> Imagery © <a href='http://mapbox.com'>Mapbox</a>"

m <- leaflet(avail_data_tb) %>% 
  addTiles(urlTemplate = tcu_map, attribution = map_attr, options = list(maxZoom = 10, 
                                                                         id = 'isawnyu.map-knmctlkh',
                                                                         accessToken =  'pk.eyJ1IjoiaXNhd255dSIsImEiOiJBWEh1dUZZIn0.SiiexWxHHESIegSmW8wedQ'))%>%
  setView(lng= 24.92, lat = 35.255, zoom = 5) 

  for(i in 1:length(travel_edges$source)) {
    arc <- gcIntermediate( p1 = c(as.numeric(travel_edges$lon_source[i]), as.numeric(travel_edges$lat_source[i])),
                           p2 = c(as.numeric(travel_edges$lon_target[i]), as.numeric(travel_edges$lat_target[i])),
                           n=100, addStartEnd=TRUE )
    m <- addPolylines(m, 
                      data=arc, 
                      color="black", 
                      weight=1,
                      stroke = TRUE,
                      smoothFactor = 5,
                      fillOpacity = 0.75, popup = paste0("<p><center><b>", travel_edges$name[i], 
                                                         "</b><br/><small><i>From: </i>", 
                                                          travel_edges$source[i],
                                                          "<br/><i>To: </i>",
                                                          travel_edges$target[i], 
                                                          "<br/></small></center></p>")
                      )
  }

  m <- addCircleMarkers(map = m, all_places_full_data$lon, 
                        all_places_full_data$lat, 
                        popup=paste0("<p><center><b>", all_places_full_data$name, 
                                     "</b><br/><small><i>Lat: </i>", 
                                     all_places_full_data$lat,
                                     "<br/><i>Long: </i>",
                                     all_places_full_data$lon, 
                                     "<br/></small></center></p>"),
                        radius=5+0.5*all_places_full_data$degree+log(all_places_full_data$degree), 
                        color="#ff0d00", 
                        stroke =FALSE, 
                        fillOpacity = 0.75)
  
m

saveWidget(m, file = "map.html", selfcontained = TRUE)






