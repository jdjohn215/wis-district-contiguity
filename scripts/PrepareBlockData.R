rm(list = ls())

library(tidyverse)
library(sf)

# https://www2.census.gov/geo/tiger/TIGER2020PL/STATE/55_WISCONSIN/55/tl_2020_55_tabblock20.zip
blocks <- st_read("~/Downloads/tl_2020_55_tabblock20/tl_2020_55_tabblock20.shp")

# get block population
block.pop <- tidycensus::get_decennial("block", variables = "P1_001N",
                                       year = 2020, sumfile = "pl",
                                       state = "WI")
# block district assignments
# https://www.census.gov/geographies/mapping-files/2023/dec/rdo/2022-state-legislative-bef.html
block.asm <- read_csv("data/55_WI_SLDL22.txt", col_types = "cc") %>%
  select(GEOID20 = GEOID, ASM = SLDLST)
block.sen <- read_csv("data/55_WI_SLDU22.txt", col_types = "cc") %>%
  select(GEOID20 = GEOID, SEN = SLDUST)

# block municipality (MCD) codes and names
blocks.to.mcds <- left_join(
  read_delim("data/BlockAssign_ST55_WI_MCD.txt", delim = "|",
             col_types = "ccc"),
  read_delim("data/NAMES_ST55_WI_MCD.txt", delim = "|",
             col_types = "ccccc")
) %>%
  select(GEOID20 = BLOCKID, mcd_fips = COUSUBFP, mcd_name = NAMELSAD)

# combine all the data sources
blocks.with.districts <- blocks %>%
  select(GEOID20) %>%
  left_join(block.pop %>% select(GEOID20 = GEOID, pop = value)) %>%
  left_join(blocks.to.mcds) %>%
  left_join(block.asm) %>%
  left_join(block.sen) %>%
  select(GEOID20, mcd_name, mcd_fips, ASM, SEN, pop)

st_write(blocks.with.districts, "data/WI_BLOCKS_withWater_2020_TIGER_PL94171.geojson",
         delete_dsn = T)