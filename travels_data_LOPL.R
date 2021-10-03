library(tidyverse)
library(devtools)
library(geosphere)
library(leaflet)
library(shiny)
library(DT)
library(viridis)
library(igraph)

# LOPL = Life_of_Pythagoras_lamblichus
# Select Nodes with attributes places from dataset Life of Pythagoras lamblichus
# create dataframe with places only

#read nodes file
nodes_LOPL <- read.csv(file="new_Nodes_Life_of_Pythagoras_Iamblichus.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
# read edges file
edges_LOPL <- read.csv("new_Edges_Life_of_Pythagoras_Iamblichus.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
#filter nodes by places
places_LOPL <- nodes_LOPL$Name[nodes_LOPL$Groups == "Place"]
#read location data
all_places_full_data <- read.csv(file = "locations_data.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
# select all places in nodes_LOPL that are in all_place_full_data
places_availables_LOPL <- places_LOPL[places_LOPL %in% all_places_full_data$name]

####################################################################################
###################### Selecting travels data ######################################
########################################################################################################name################################################################

## 1. Create new table from NewNodes and newEdges.
## 2. identify nodes that are in both relation "is from" and "travelled to"

# Origin of phylosophers
names_origin_LOPL <- edges_LOPL$Source[which(edges_LOPL$Relation == "is from")]
origin_places_LOPL <- edges_LOPL$Target[which(edges_LOPL$Relation == "is from")]

# Validations for origin place
# Do places as Target in "is from" have identified localization??
id_knw_origi_LOPL <-which(origin_places_LOPL %in% places_availables_LOPL)
# Nnown Places
knw_origin_loc_LOPL <-origin_places_LOPL[id_knw_origi_LOPL]
# Unknown Places
uknw_origin_loc_LOPL <- origin_places_LOPL[-id_knw_origi_LOPL]
# Phylosophers with known origin
know_origin_phy_LOPL <- names_origin_LOPL[id_knw_origi_LOPL]
know_origin_loc_LOPL
# Phylosophers with unknown origin
uknow_origin_phy_LOPL <- names_origin_LOPL[-id_knw_origi_LOPL]

# travelers source
names_traveler_LOPL <- edges_LOPL$Source[which(edges_LOPL$Relation == "traveled to")]
# Traveler target 
traveler_target_LOPL <- edges_LOPL$Target[which(edges_LOPL$Relation == "traveled to")]

# validations for travelers
# which traveler are from known origin?
id_knw_loc_orig_trav_LOPL <- which(names_traveler_LOPL %in% know_origin_phy_LOPL)
# Is every traveler from a known localization place?
isTRUE(length(id_knw_loc_orig_trav_LOPL) == length(names_traveler_LOPL))
# How many traveler's origin need to be identified?
dif_trav_LOPL <- length(names_traveler_LOPL) - length(id_knw_origi_LOPL)
# Do ebery destination have a known location?
id_knw_loc_trav_dest_LOPL <- which(traveler_target_LOPL %in% places_availables_LOPL)
# Joint condition "known location origin phylosopher" + "known location traveler destination"
intsect_condition_LOPL <- intersect(id_knw_loc_orig_trav_LOPL,id_knw_loc_trav_dest_LOPL)

# traveler with joint condition
knw_all_names_trav_LOPL <- names_traveler_LOPL[intsect_condition_LOPL]
# Destinations with joint condition
knw_all_travl_trgt_LOPL <- traveler_target_LOPL[intsect_condition_LOPL]

## 3. Create a dataframe with the follow form:
## source target name
# Athens Egypt Pythagoras

travel_edges_LOPL <- data.frame(source = rep(NA, length(knw_all_names_trav_LOPL)),
                                target = knw_all_travl_trgt_LOPL,
                                name = knw_all_names_trav_LOPL,
                                lat_source = rep("", length(knw_all_names_trav_LOPL)),
                                lon_source = rep("", length(knw_all_names_trav_LOPL)),
                                lat_target = rep("", length(knw_all_names_trav_LOPL)),
                                lon_target = rep("", length(knw_all_names_trav_LOPL)),
                                stringsAsFactors = FALSE)
travel_edges_LOPL
know_origin_phy_LOPL
know_origin_phy_LOPL <- rep(know_origin_phy_LOPL,3)
for (k in 1:length(travel_edges_LOPL$source)){
  travel_edges_LOPL$source[k] <- (knw_origin_loc_LOPL[which(know_origin_phy_LOPL == travel_edges_LOPL$name[k])])
  travel_edges_LOPL$lat_source[k] <- (all_places_full_data$lat[which(all_places_full_data$name == travel_edges_LOPL$source[k])])
  travel_edges_LOPL$lon_source[k] <- (all_places_full_data$lon[which(all_places_full_data$name == travel_edges_LOPL$source[k])])
  travel_edges_LOPL$lat_target[k] <- (all_places_full_data$lat[which(all_places_full_data$name == travel_edges_LOPL$target[k])])
  travel_edges_LOPL$lon_target[k] <- (all_places_full_data$lon[which(all_places_full_data$name == travel_edges_LOPL$target[k])])
}

all_places_LOPL <- sort(c(travel_edges_LOPL$source, travel_edges_LOPL$target), decreasing = FALSE)
all_places_LOPL <- data.frame(places=all_places_LOPL)
write.csv(x = as.matrix(travel_edges_LOPL), file = paste0("travel_edges_LOPL.csv"), fileEncoding = "UTF-8")