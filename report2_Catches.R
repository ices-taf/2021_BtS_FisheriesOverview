
library(icesTAF)
taf.library(icesFO)
library(sf)
library(ggplot2)
library(dplyr)


##########
#Load data
##########

catch_dat <- read.taf("data/catch_dat.csv")


#################################################
##1: ICES nominal catches and historical catches#
#################################################

#~~~~~~~~~~~~~~~#
# By common name
#~~~~~~~~~~~~~~~#
#Plot
plot_catch_trends(catch_dat, type = "COMMON_NAME", line_count = 5, plot_type = "line")
ggplot2::ggsave(paste0(year_cap, "_", ecoreg,"_FO_Catches_species.png"), path = "report/", width = 170, height = 100.5, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COMMON_NAME", line_count = 5, plot_type = "line", return_data = TRUE)
write.taf(dat, paste0(year_cap, "_", ecoreg,"_FO_Catches_species.csv"), dir = "report")


#~~~~~~~~~~~~~~~#
# By country
#~~~~~~~~~~~~~~~#
#Plot
plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 9, plot_type = "area")
ggplot2::ggsave(paste0(year_cap, "_", ecoreg,"_FO_Catches_country.png"), path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 9, plot_type = "area", return_data = TRUE)
write.taf(dat, file= paste0(year_cap, "_", ecoreg,"_FO_Catches_country.csv"), dir = "report")

#~~~~~~~~~~~~~~~#
# By guild
#~~~~~~~~~~~~~~~#
# I remove Crustacean and Elasmobranch because they were not there last year and
# create a new line "other" which is almost zero

catch_dat2 <- dplyr::filter(catch_dat, GUILD != "Crustacean")
catch_dat2 <- dplyr::filter(catch_dat2, GUILD != "Elasmobranch")

        #Plot
plot_catch_trends(catch_dat2, type = "GUILD", line_count = 4, plot_type = "line")
ggplot2::ggsave(paste0(year_cap, "_", ecoreg,"_FO_Catches_guild.png"), path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

        #data
dat <- plot_catch_trends(catch_dat, type = "GUILD", line_count = 4, plot_type = "line", return_data = TRUE)
write.taf(dat, file= paste0(year_cap, "_", ecoreg,"_FO_Catches_guild.csv"), dir = "report")

