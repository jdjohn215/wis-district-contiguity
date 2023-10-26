library(tidyverse)
library(sf)
library(igraph)
library(spdep)
library(tmap)

# the original block shapefile
block.sf <- st_read("data/WI_BLOCKS_withWater_2020_TIGER_PL94171.geojson")

# keep blocks for specific district, with or without unassigned water
subset_district <- function(district, data = block.sf, with_water = FALSE){
  if(with_water == FALSE){
    data %>%
      filter(ASM == district)
  } else if(with_water == TRUE){
    data %>%
      filter(ASM %in% c(district, "ZZZ"))
  }
}

# identify component of membership for each block in a given district
get_district_components <- function(district, data = block.sf, with_water = FALSE){
  district.adj <- subset_district(district, data = data, with_water = with_water) %>%
    tmaptools::get_neighbours()
  igraph::graph_from_adj_list(district.adj) %>%
    components()
}

# summary statistics for each component in a given district
summarize_components <- function(district, data = block.sf, with_water = FALSE){
  subset_district(district, data = data, with_water = with_water) %>%
    mutate(component = get_district_components(district = district,
                                               data = data, with_water = with_water)[[1]]) %>%
    st_drop_geometry() %>%
    filter(ASM != "ZZZ") %>%
    mutate(component = as.numeric(as.factor(component))) %>%
    group_by(component) %>%
    summarise(blocks = n(),
              populated_blocks = sum(pop > 0),
              population = sum(pop)) %>%
    mutate(district = district)
}

# all the components in each district
all.dist.components <- map_df(unique(block.sf$ASM), summarize_components, with_water = TRUE, .progress = T)

# summary statistics for each district
district.summary <- all.dist.components %>%
  group_by(district) %>%
  summarise(extra_components = sum(component > 1),
            blocks_disconnected = sum(blocks[component > 1]),
            populated_blocks_disconnected = sum(populated_blocks[component > 1]),
            disconnected_population = sum(population[component > 1]))

# save tables
all.dist.components %>%
  arrange(district, component) %>%
  select(district, component, blocks, populated_blocks, population) %>%
  write_csv("tables/AllAssemblyDistrictComponents.csv")
write_csv(district.summary, "tables/AssemblyDistrictComponentsSummary.csv")

# this function visualizes the adjacency graph for a given assembly district
plot_district_adj <- function(district, data = block.sf, with_water = FALSE){
  district.blocks <- subset_district(district = district, data = data,
                                     with_water = with_water) %>%
    mutate(component = get_district_components(district = district, 
                                               data = data, with_water = with_water)[[1]]) %>%
    filter(ASM != "ZZZ") %>%
    mutate(component = as.factor(as.numeric(as.factor(component))))
  
  nb.district <- poly2nb(district.blocks)
  centers <- st_centroid(st_geometry(district.blocks))
  nb.sf.district <- nb2lines(nb.district, coords = st_centroid(st_geometry(district.blocks)), as_sf = T)
  
  if(max(as.numeric(district.blocks$component)) > 1){
    extra.components <- district.blocks %>%
      filter(as.numeric(component) > 1) %>%
      group_by(component) %>%
      summarise()
    tm_shape(district.blocks) +
      tm_polygons(col = "component", palette = "Dark2") +
      tm_shape(nb.sf.district) +
      tm_lines() +
      tm_shape(centers) +
      tm_dots() +
      tm_shape(extra.components) +
      tm_symbols(size = 1.5, col = "component", shape = 1, legend.col.show = FALSE,
                 palette = "Dark2", border.lwd = 1.5) +
      tm_layout(main.title = paste0("Adjacency graph of AD-", district),
                frame = FALSE, bg.color = "ghostwhite")
  } else{
    tm_shape(district.blocks) +
      tm_polygons(col = "component") +
      tm_shape(nb.sf.district) +
      tm_lines() +
      tm_shape(centers) +
      tm_dots() +
      tm_layout(main.title = paste0("Adjacency graph of AD-", district),
                frame = FALSE, bg.color = "ghostwhite")
  }
  
}

# plot_district_adj("074")
# plot_district_adj("074", with_water = TRUE)

# create and save an adjacency graph
build_and_save_adj_graph <- function(district){
  tm.plot <- plot_district_adj(district, with_water = TRUE)
  tmap_save(tm.plot, paste0("adjacency-maps/assembly/", district, ".svg"))
}

# build and save ALL district adjacency graphs
map(setdiff(unique(block.sf$ASM), "ZZZ"), build_and_save_adj_graph, .progress = T)
