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
  district.blocks <- subset_district(district, data = data, with_water = with_water)
  
  district.adj <- district.blocks %>%
    tmaptools::get_neighbours()
  
  district.components <- igraph::graph_from_adj_list(district.adj) %>%
    components()
  
  district.blocks %>%
    mutate(component = district.components$membership) %>%
    filter(ASM != "ZZZ") %>%
    # ensure that components are numbered sequential, beginning with 1
    mutate(component = as.numeric(as.factor(component)))
}

# all district components, excludes unassigned ZZZ blocks
all.district.components <- map_df(setdiff(unique(block.sf$ASM), "ZZZ"),
                                  get_district_components,
                                  with_water = TRUE, .progress = T)

# summary statistics for each component in a given district
district.component.summary <- all.district.components %>%
  st_drop_geometry() %>%
  group_by(district = ASM, component) %>%
  summarise(blocks = n(),
            populated_blocks = sum(pop > 0),
            population = sum(pop)) %>%
  ungroup() %>%
  arrange(district, component)

# summary statistics for each district
district.summary <- district.component.summary %>%
  group_by(district) %>%
  summarise(extra_components = sum(component > 1),
            blocks_disconnected = sum(blocks[component > 1]),
            populated_blocks_disconnected = sum(populated_blocks[component > 1]),
            disconnected_population = sum(population[component > 1]))

# save tables
district.component.summary %>%
  arrange(district, component) %>%
  select(district, component, blocks, populated_blocks, population) %>%
  write_csv("tables/AllAssemblyDistrictComponents.csv")
write_csv(district.summary, "tables/AssemblyDistrictComponentsSummary.csv")

# save all block-district-component assignments
all.district.components %>%
  # add the ZZZ blocks, they have NA component
  bind_rows(block.sf %>% filter(ASM == "ZZZ")) %>%
  st_drop_geometry() %>%
  select(GEOID20, ASM, component) %>% 
  write_csv("tables/blocks-with-assembly-components.csv")

################################################################################
# this function visualizes the adjacency graph for a given assembly district
#   automatically uses water connections
plot_district_adj <- function(district, data = block.sf){
  district.blocks <- all.district.components %>%
    filter(ASM == district) %>%
    mutate(component = as.factor(component))
  
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
  tm.plot <- plot_district_adj(district)
  tmap_save(tm.plot, paste0("adjacency-maps/assembly/", district, ".jpg"))
}

# build and save ALL district adjacency graphs
map(setdiff(unique(block.sf$ASM), "ZZZ"), build_and_save_adj_graph, .progress = T)
