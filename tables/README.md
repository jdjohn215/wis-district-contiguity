# blocks-with-*-components.csv

These files contain the district *and* component assignments for each census block in Wisconsin. They contain the following fields:

* GEOID20 - the block FIPS code
* SEN or ASM - either the Senate or Assembly district number. "ZZZ" if this block is unassigned water.
* component - a number indicating which component of the district this block falls into. A value of "1" indicates the primary component. If all values are "1" this district is contiguous.

# All*DistrictComponents.csv

These tables contain summary statistics for each component of a district. They contain the following fields:

* district - either the Assembly or Senate district number
* component - a number indicating which component of the district this block falls into. A value of "1" indicates the primary component. If "1" is the only component, then this district is contiguous.
* blocks - the number of census blocks in this component
* populated_blocks - the number of census blocks in this component which had a non-zero population in the 2020 census.
* population - the total population in this component

# *DistrictComponentsSummary.csv

These tables include contiguity-related summary statistics for entire districts. They include the following fields:

* district - either the Assembly or Senate district number
* extra_components - 0 if the district is fully contiguous, otherwise an integer indicating the number of disconnected components
* blocks_disconnected - the total number of census blocks outside of the district's primary component
* populated_blocks_disconnected - the total number of populated census blocks outside of the district's primary component
* disconnected_population - the total population count of all census blocks outside of the district's primary component

**Please Note:** the Census Bureau's adoption of new [differential privacy techniques](https://www.census.gov/programs-surveys/decennial-census/decade/2020/planning-management/process/disclosure-avoidance.html) in the 2020 census means that the population counts in individual census blocks are untrustworthy.