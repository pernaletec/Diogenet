
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


#install_github("editio/georeference")

read_georef = FALSE

# select nodes with attribute Place from newNodes.csv
# create new dataframe with places only = locations

nodes = read.csv(file="new_Nodes.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
black_list = read.csv(file="travels_blacklist.csv", header = FALSE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)


places <- nodes$Name[nodes$Groups=="Place"]

all_places_full_data = read.csv(file = "locations_data.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
places_available = places[places %in% all_places_full_data$name]

##########################
# Selecting travels data #
##########################

## 1. create new table from newNodes and newEdges.
## 2. identify nodes that are in both relations "is from" and "travelled to"

edges = read.csv(file="new_Edges.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

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
# Travelers in black list
trav_in_BL = names_traveler %in% black_list
names_traveler = names_traveler[!trav_in_BL]

# Traveler target in black list  
traveler_target <- edges$Target[which(edges$Relation == "traveled to")]
traveler_target = traveler_target[!trav_in_BL]

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

################################################################################
## In the following lines a table will be built for nodes in "travelled to"   ##
## but not in "is from". A column will be added to show weather or not these  ##
## places have an identified location                                         ##
################################################################################

# nodes in "travelled to" but not in "is from"
id_travelers_in_is_from = which(names_traveler %in% names_origin)
travelers_not_in_is_from = unique(names_traveler[-id_travelers_in_is_from])
write.table(x = travelers_not_in_is_from, file = "travelers_not_in_is_from.txt", fileEncoding = "UTF-8")

travelers_names = unique(names_traveler)

full_travel_edges = list(name = travelers_names,
                         from = rep("", length(travelers_names)),
                         to = rep("", length(travelers_names)))

traveler_source = function (x) {
  id = which(names_origin %in% x) 
  return(origin_places[id])
  }

traveler_destiny = function (x) {
  id = which(names_traveler %in% x) 
  return(traveler_target[id])
  }

full_travel_edges$from = sapply(full_travel_edges$name, traveler_source)
full_travel_edges$to = sapply(full_travel_edges$name, traveler_destiny)

# Table with all sources and detinations for each node (...only travelers)
write.csv(x = as.matrix(full_travel_edges$from), file = paste0("full_travel_edges_from.csv"), fileEncoding = "UTF-8")
write.csv(x = as.matrix(full_travel_edges$to), file = paste0("full_travel_edges_to.csv"), fileEncoding = "UTF-8")

################################################################################


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

## After getting all the places for which there IS at least one location identified 
## there was a "manual" search using the shiny app developed
#all_places_full_data = read.csv(file = "locations_data.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

# Some Phylosophers are identified with more that one "is from" Relation
# Keep that in mind!

for (k in 1:length(travel_edges$source)) {
  # print(paste0(k, " ", knw_origin_loc[which(knw_origin_phy == travel_edges$name[k])]))
  # print(paste0(k, " ", travel_edges$name[k]))
  travel_edges$source[k] = (knw_origin_loc[which(knw_origin_phy == travel_edges$name[k])])
  travel_edges$lat_source[k] = (all_places_full_data$lat[which(all_places_full_data$name == travel_edges$source[k])])
  travel_edges$lon_source[k] = (all_places_full_data$lon[which(all_places_full_data$name == travel_edges$source[k])])
  travel_edges$lat_target[k] = (all_places_full_data$lat[which(all_places_full_data$name == travel_edges$target[k])])
  travel_edges$lon_target[k] = (all_places_full_data$lon[which(all_places_full_data$name == travel_edges$target[k])])
  }

all_places = sort(unique(c(travel_edges$source, travel_edges$target)), decreasing = FALSE)
# These are the nodes in the graph
all_places = data.frame(places = all_places)

###########################################
#########                     #############  
########    GRAPH CREATION     ############
#########                     #############
###########################################

try(graph <- igraph::graph_from_data_frame(d = travel_edges[,1:2], directed=FALSE, vertices = all_places$places), silent = TRUE)
plot(graph)

###########################################
#########                     #############  
########     LEAFLET MAP       ############
#########                     #############
###########################################


avail_data_tb = as_tibble(travel_edges)

node_count = c(travel_edges$source, travel_edges$target)
node_count = as.data.frame(table(node_count))

all_places_full_data = all_places_full_data[(all_places_full_data$name %in% all_places$places),]

all_places_full_data$degree = node_count$Freq[node_count$node_count %in% all_places_full_data$name]

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

# saveWidget(m, file = "map.html", selfcontained = TRUE)
