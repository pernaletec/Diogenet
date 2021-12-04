# **Diogenet**

Intellectual Networks in Ancient Greece

[https://diogenet.ucsd.edu/](https://diogenet.ucsd.edu/)

## About

Diogenet is a dataset and a series of web applications for investigating the impact of social networks on the emergence and development of ancient Greek philosophy. 

In contrast to most approaches to the intellectual history of classical antiquity, we do not only focus on philosophers but also on the social ties that linked them. 

In addition, we do not limit ourselves to studying relations among intellectuals, but also consider family members, romantic partners, friends, and benefactors. 

We think that moving away from the idea that philosophical and scientific fields are closed systems allows a better understanding of the social processes that underlie the construction of knowledge in the ancient world.

## What is in this repository?

The description of the datasets and applications, includind **RMarkdown** web deployments, is given below. Note that separate scripts used in the development process are also included. Not all scripts are called called at execution time. 

## Datasets

|Data set|Description|
|--------|--------|
|`new_Edges.csv`|Edges list for Diogenes Laertius dataset|
|`new_Nodes.csv`|Nodes list for Diogenes Laertius dataset|
|`new_Edges_Life_of_Pythagoras_Iamblichus.csv`| Edges list for Life of Pythagoras Iamblichus dataset|
|`new_Nodes_Life_of_Pythagoras_Iamblichus.csv`| Nodes list for Life of Pythagoras Iamblichus dataset|
|`travels_blacklist.csv`|Persons involved in nonsense travels (i.e Homer travels to Hades)|
|`all_places_graph.csv`|All places with identified location. Created at running time|
|`travel_edges_graph.csv`|All edges with locations for source and target. Created at running time|
|`full_travels_edges_from.csv`|All information about sources (Created in `travels_map.R`)|
|`full_travels_edges_to.csv`|All information destinations (Created in `travels_map.R`)|
|`locations.csv`|Raw information about locations|
|`locations_data.csv`|Ordered information about locations|
|`travelers_not_in_is_from.txt`|Travelers with no origin place identified. Not considered in the map|

## Scripts

|Script|Description|
|------|--------|
|`travels_dahsboard.Rmd` |Main script for **Dataset's Map**. It is based on R chunks embedded in a Markdown file|
|`diogenet_dahsboard.Rmd`|Main script for **Horus, Intellectual Networks in Ancient Greece**.  It is based on R chunks embedded in a Markdown file|


## Dependencies

### `travels_dashboard.Rmd`

`flexdashboard 0.5.1.1`
`tidyverse 1.2.1`
`devtools 2.2.1`
`leaflet 2.0.2`
`shiny 1.4.0`
`DT 0.9`
`geosphere 1.5-10`
`viridis 0.5.1`
`igraph 1.2.4.1`
`visNetwork 2.0.8`

### `diogenet_dashboard.Rmd`

`flexdashboard 0.5.1.1`
`shiny 1.4.0`
`DT 0.9`
`rmarkdown 1.16`
`knitr 1.25`
`rsconnect 0.8.15`
`igraph 1.2.4.1`
`stringi 1.4.3`
`tidyverse 1.2.1`
`visNetwork 2.0.8`
`viridis 0.5.1`
`heatmaply 0.16.0`
