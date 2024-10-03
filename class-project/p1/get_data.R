## query and clean the FIA data
## FIADB version: 2024-04-15
## Created by Xiuyu Cao on Oct 2, 2024
library(tidyverse)


## -------------------------------- Query FIADB -------------------------------- ##
db.file <- 'data/proj/FIADB'  # path to the .db file

con <- RSQLite::dbConnect(RSQLite::SQLite(), db.file)
trees <- RSQLite::dbGetQuery(con, read_file('class-project/p1/query_trees.sql'))
RSQLite::dbDisconnect(con)

saveRDS(trees, 'data/proj/raw_trees.rds')  # write data


## ------------------------------ Preprocess Data ------------------------------ ##
genus.list <- c('Ailanthus', 'Robinia',  # MI invasive tree species
                'Acer', 'Pinus', 'Quercus')  # MI native trees

# get FIA plots with the invasive trees on record
plots <- readRDS('data/proj/raw_trees.rds') %>%
  filter(GENUS %in% c('Ailanthus', 'Robinia')) %>%
  filter(!is.na(DSTRBCD1)) %>%
  select(PLT_CN, LAT, LON) %>%
  distinct()

# get all the tree records in the plots
trees.clean <- readRDS('data/proj/raw_trees.rds') %>%
  filter(GENUS %in% genus.list) %>%
  right_join(plots, by = 'PLT_CN') %>%
  filter(!is.na(DSTRBCD1)) %>%
  select(-LAT.y, -LON.y) %>%
  rename(LAT = LAT.x, LON = LON.x)

# write data
saveRDS(trees.clean, 'data/proj/input_trees.rds')


## ----------------------------- Data Visualization ---------------------------- ##
library(sf)
library(ggplot2)


## ----------------------------- Map
MI <- st_read('data/proj/raw_mi_counties/Counties_(v17a).shp') %>%  # MI county shapefile
  st_transform(4269)  # FIA data is in NAD83

ggplot() +
  geom_sf(data = MI, fill=NA) +
  geom_point(data = plots, aes(x = LON, y = LAT), color = 'red', size=.1) +
  theme_minimal()







