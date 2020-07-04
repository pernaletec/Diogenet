---
output:
  pdf_document: default
  html_document: default
---
# Diogenet

Intellectual Networks in Ancient Greece

## About

Diogenet is a dataset and a series of web applications for investigating the impact of social networks on the emergence and development of ancient Greek philosophy. 

In contrast to most approaches to the intellectual history of classical antiquity, we do not only focus on philosophers but also on the social ties that linked them. 

In addition, we do not limit ourselves to studying relations among intellectuals, but also consider family members, romantic partners, friends, and benefactors. 

We think that moving away from the idea that philosophical and scientific fields are closed systems allows a better understanding of the social processes that underlie the construction of knowledge in the ancient world.

## What is in this repository?

The description of the datasets and applications, includind **RMarkdown** web deployments, is given below. Note that separate scripts used in the development process, and NOT necessarilly called at execution time, are also included. 

## Datasets

|Data set|Description|
|--------|--------|
|`new_Edges.csv`|Edges list|
|`new_Nodes.csv`|Nodes list|
|`old_Edges.csv`|Raw edges list|
|`old_Nodes.csv`|Raw nodes list|
|`travels_blacklist.csv`|Persons involved in nonsense travels (i.e Homer travels to Hades)|
|`all_places_graph.csv`|All places with identified location. Created at running time|
|`travel_edges_graph.csv`|All edges with locations for source and target. Created at running time|
|`full_travels_edges_from.csv`|All information about sources (Created in `travels_map.R`)|
|`full_travels_edges_to.csv`|All information destinations (Created in `travels_map.R`)|
|`locations.csv`|Raw information about locations|
|`locations_data.csv`|Ordered information about locations|

The script `formatToIgraph.R` converts old nodes and edges into new nodes and edges.

## Scripts

|Data set|Description|
|------|--------|
|`travels_dahsboard.Rmd` |Main script for **Diogenet's Map**. It is based on R chunks embedded in a markdown file|
|`diogenet_dahsboard.Rmd`|Main script Horus, Intellectual Networks in Ancient Greece.  based on R chunks embedded in a markdow file|
|`nets_ancient.Rmd`|Seed version of Horus! Just a reference|
|`source_data.R`|Test if every edge is composed by valid nodes|
|`formatToIgraph.R`|Converts raw nodes and edges files into new and clean files|
|`vnwGraphsToHTML.R`|Creates visnetwork graphs and then export them to HTML|

In addition there are separate shiny scripts for each type of graph general network, the local network and the communities network. For each type of graph there is an implementation for `igraph` and for `visnetwork`.  

|Shiny app|Description|
|------|--------|
|`community_igp` |Community graph using `igraph`|
|`community_vnw` |Community graph using `visnetwork`|
|`egograph_igp` |Local view graph using `igraph`|
|`egograph_vnw` |Local view graph using `visnetwork`|
|`network_igp` |Full graph using `igraph`|
|`network_vnw` |Full graph using `visnetwork`|
