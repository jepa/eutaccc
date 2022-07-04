# Data Read me

## spp_dbem_data

This data set represents the maximum catch potential (MCP) of each species by sub area, division, year, esm and rcp. The structure is a s follows:

### Structure

- `sub_area`: ICES sub area
- `division`: ICES division
- `year`: time step from 1951 to 2100
- `taxon`: taxon number id. See the main script for a relational table between taxon and species name
- `esm`: Earth System Model. Runs currently done with ESMs from the CMIP5. Will eventually update to CMIP6
- `rcp`: Climate change scenario. `rcp26` representing a low emission climate change scenario and `rcp85` a high emission.

## temporal_chng

This data set is a processed version of the `spp_dbem_data`. It shows the average across ESMs (plus/min sd) percentage change in MCP of each species by sub_area in two time periods: med (2040-2060) and end (2080-2100) of the current century relative to historic values (1970's). The structure is a s follows:

### Structure

- `sub_area`: ICES sub area
- `taxon`: taxon number id. See the main script for a relational table between taxon and species name
- `rcp`: Climate change scenario. `rcp26` representing a low emission climate change scenario and `rcp85` a high emission.
- `time_period`: Future time periods `AA_med` represents the average of 2040-2060 and `AAA_end` the average of 2080-2100
- `per_chg_mean`: The average change in MCP of the time period across ESMs relative to the historical time period
- `per_chg_sd`: The standar deviation of the ESMs' average


