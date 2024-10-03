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
saveRDS(plots, 'data/proj/input_plots.rds')
saveRDS(trees.clean, 'data/proj/input_trees.rds')


## ----------------------------- Data Visualization ---------------------------- ##
library(sf)
library(ggplot2)
library(ggpubr)


plots <- readRDS('data/proj/input_plots.rds')
trees <- readRDS('data/proj/input_trees.rds')


## ----------------------------- figure 2 (a) - Map of the plots
MI <- st_read('data/proj/raw_mi_counties/Counties_(v17a).shp') %>%  # MI county shapefile
  st_transform(5070)  # FIA data is in NAD83

plots.sf <- st_as_sf(plots, coords = c('LON', 'LAT'), crs = 4269) %>%
  st_transform(5070)

fig_2a <- ggplot() +
  geom_sf(data = MI, fill=NA) +
  geom_sf(data = plots.sf, color = 'orangered3', size = 2, shape=17) +
  theme_minimal() +
  theme(axis.text=element_text(size=12))


## ----------------------------- figure 2 (b) - genus count
# histogram of the trees$GENUS
fig_2b <- ggplot(trees, aes(x = GENUS)) +
  geom_bar(fill = 'forestgreen') +
  theme_minimal() +
  labs(x = "", y = "Count of Trees") +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_blank())


## ----------------------------- figure 2 (c) - biomass boxplot
biomass <- trees %>%
  mutate(bio_total=DRYBIO_STEM + DRYBIO_STEM_BARK +DRYBIO_BRANCH + DRYBIO_FOLIAGE)

# plot the box plot of bio_total, group by GENUS
fig_2c <- ggplot(biomass, aes(x = GENUS, y = bio_total)) +
  geom_boxplot(fill = 'forestgreen') +
  theme_minimal() +
  labs(x = "Genus", y = "Total Biomass (lbs)") +
  scale_y_continuous(limits = c(0, 900)) + 
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, face='italic'))


## ----------------------------- figure 2
fig_2bc <- ggarrange(fig_2b, fig_2c, ncol = 1, nrow = 2, heights = c(1, 1),
                     labels = c("b", "c"))

fig2 <- ggarrange(fig_2a, fig_2bc, ncol = 2, widths = c(1, 1.5),
                  labels = c("a", ""))

ggsave('class-project/figs/p1_fig2.png', fig2, width = 12, height = 7)


