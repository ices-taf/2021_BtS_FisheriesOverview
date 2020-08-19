library(icesTAF)
taf.library(icesFO)

areas <- icesFO::load_areas("Baltic Sea")

sf::st_write(areas, "areas.csv", layer_options = "GEOMETRY=AS_WKT")
