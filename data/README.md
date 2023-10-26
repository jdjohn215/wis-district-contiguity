This folder contains the original source data files.

* WI_BLOCKS_withWater_2020_TIGER_PL94171.geojson

This file is the original block GIS file from the Census Bureau, with additional fields added for total population, municipality codes and names, and state legislative districts. The file is too large to be hosted on Github (465MB), but you can download it from [Dropbox here](https://www.dropbox.com/scl/fi/rwsyuf1nujiittxhjequv/WI_BLOCKS_withWater_2020_TIGER_PL94171.geojson?rlkey=b8zbu4txzutb4w002fok3u1of&dl=1).

For more details, see `scripts/PrepareBlockData.R`.

* 55_WI_SLDL22.txt and 55_WI_SLDU22.txt

These files contain the state legislative district assignments for each Wisconsin census block. They are [downloaded from here](https://www.census.gov/geographies/mapping-files/2023/dec/rdo/2022-state-legislative-bef.html).

* BlockAssign_ST55_WI_MCD.txt and NAMES_ST55_WI_MCD.txt

These files contain the minor civil division (MCD) codes and names for each census block in Wisconsin at the time of the 2020 census. MCDs are equivalent to municipalities (within counties) in Wisconsin. See [here to download](https://www.census.gov/geographies/reference-files/time-series/geo/block-assignment-files.html).