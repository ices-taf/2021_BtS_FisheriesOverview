
library(icesTAF)
taf.library(icesFO)
library(sf)
library(ggplot2)
library(dplyr)

mkdir("report")

# set values for automatic naming of files:
year_cap = "2020"
ecoreg = "BtS"

##########
#Load data
##########

ices_areas <-
  sf::st_read("bootstrap/data/ICES_areas/areas.csv",
              options = "GEOM_POSSIBLE_NAMES=WKT", crs = 4326)
ices_areas <- dplyr::select(ices_areas, -WKT)

ecoregion <-
  sf::st_read("bootstrap/data/ICES_ecoregions/ecoregion.csv",
              options = "GEOM_POSSIBLE_NAMES=WKT", crs = 4326)
ecoregion <- dplyr::select(ecoregion, -WKT)

###############
##Ecoregion map
###############

plot_ecoregion_map(ecoregion, ices_areas)
ggplot2::ggsave(paste0(year_cap, "_", ecoreg, "_FO_Figure1.png"), path = "report", width = 170, height = 200, units = "mm", dpi = 300)


