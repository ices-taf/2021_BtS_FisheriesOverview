# All plots and data outputs are produced here

library(icesTAF)
taf.library(icesFO)
library(sf)
library(ggplot2)
library(dplyr)

mkdir("report")

##########
#Load data
##########

catch_dat <- read.taf("data/catch_dat.csv")

# stock information
sid <- read.taf("bootstrap/data/ICES_StockInformation/sid.csv")

frmt_effort <- read.taf("data/frmt_effort.csv")
frmt_landings <- read.taf("data/frmt_landings.csv")
trends <- read.taf("model/trends.csv")
catch_current <- read.taf("model/catch_current.csv")
catch_trends <- read.taf("model/catch_trends.csv")

clean_status <- read.taf("data/clean_status.csv")

ices_areas <-
  sf::st_read("bootstrap/data/ICES_areas/areas.csv",
              options = "GEOM_POSSIBLE_NAMES=WKT", crs = 4326)
ices_areas <- dplyr::select(ices_areas, -WKT)

ecoregion <-
  sf::st_read("bootstrap/data/ICES_ecoregions/ecoregion.csv",
              options = "GEOM_POSSIBLE_NAMES=WKT", crs = 4326)
ecoregion <- dplyr::select(ecoregion, -WKT)

# read vms fishing effort
effort <-
  sf::st_read("bootstrap/data/ICES_vms_effort_map/vms_effort.csv",
               options = "GEOM_POSSIBLE_NAMES=wkt", crs = 4326)
effort <- dplyr::select(effort, -WKT)

# read vms swept area ratio
sar <-
  sf::st_read("bootstrap/data/ICES_vms_sar_map/vms_sar.csv",
               options = "GEOM_POSSIBLE_NAMES=wkt", crs = 4326)
sar <- dplyr::select(sar, -WKT)

###############
##Ecoregion map
###############

plot_ecoregion_map(ecoregion, ices_areas)
ggplot2::ggsave("2019_BtS_FO_Figure1.png", path = "report", width = 170, height = 200, units = "mm", dpi = 300)


#################################################
##1: ICES nominal catches and historical catches#
#################################################

#~~~~~~~~~~~~~~~#
# By common name
#~~~~~~~~~~~~~~~#
#Plot
plot_catch_trends(catch_dat, type = "COMMON_NAME", line_count = 5, plot_type = "line")
ggplot2::ggsave("2019_BtS_FO_Figure5.png", path = "report/", width = 170, height = 100.5, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COMMON_NAME", line_count = 5, plot_type = "line", return_data = TRUE)
write.taf(dat, "2019_BtS_FO_Figure5.csv", dir = "report")


#~~~~~~~~~~~~~~~#
# By country
#~~~~~~~~~~~~~~~#
#Plot
plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 9, plot_type = "area")
ggplot2::ggsave("2019_BtS_FO_Figure2.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 9, plot_type = "area", return_data = TRUE)
write.taf(dat, file= "2019_BtS_FO_Figure2.csv", dir = "report")

#~~~~~~~~~~~~~~~#
# By guild
#~~~~~~~~~~~~~~~#
# I remove Crustacean and Elasmobranch because they were not there last year and
# create a new line "other" which is almost zero

catch_dat2 <- dplyr::filter(catch_dat, GUILD != "Crustacean")
catch_dat2 <- dplyr::filter(catch_dat2, GUILD != "Elasmobranch")

        #Plot
plot_catch_trends(catch_dat2, type = "GUILD", line_count = 4, plot_type = "line")
ggplot2::ggsave("2019_BtS_FO_Figure4.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

        #data
dat <- plot_catch_trends(catch_dat, type = "GUILD", line_count = 4, plot_type = "line", return_data = TRUE)
write.taf(dat, file= "2019_BtS_FO_Figure4.csv", dir = "report")

################################
## 2: STECF effort and landings#
################################

#~~~~~~~~~~~~~~~#
# Effort by country
#~~~~~~~~~~~~~~~#
        #Plot
plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = FALSE)
frmt_effort <- dplyr::filter(frmt_effort, COUNTRY %in% c("Sweden", "Poland", "Germany", "Denmark", "Lithuania","Latvia"))
plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = FALSE)
ggplot2::ggsave("2019_BtS_FO_Figure3.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)
        #data
dat <- plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= "2019_BtS_FO_Figure3.csv", dir = "report")


#~~~~~~~~~~~~~~~#
#Effort by gear
#~~~~~~~~~~~~~~~#
        #Plot
plot_stecf(frmt_effort,type = "effort", variable= "GEAR", "2019","August", 9, "15-23")
ggplot2::ggsave("2019_BtS_FO_Figure8.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

        #data
dat<-plot_stecf(frmt_effort,type = "effort", variable= "GEAR", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= "B2019_BtS_FO_Figure8.csv", dir = "report")

#~~~~~~~~~~~~~~~#
#Landings by country
#~~~~~~~~~~~~~~~#
        #Plot
plot_stecf(frmt_landings,type = "landings", variable= "GEAR", "2019","August", 9, "15-23")
ggplot2::ggsave("2019_BtS_FO_Figure6.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)
        #dat
dat <- plot_stecf(frmt_landings, type = "landings", variable="landings", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= "2019_BtS_FO_Figure6.csv", dir = "report")

###########
## 3: SAG #
###########

#~~~~~~~~~~~~~~~#
# A. Trends by guild
#~~~~~~~~~~~~~~~#
# 1. Demersal
#~~~~~~~~~~~
plot_stock_trends(trends, guild="demersal", cap_year = 2019, cap_month = "August", return_data = FALSE)
ggplot2::ggsave("2019_BtS_FO_Figure12b.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

dat <- plot_stock_trends(trends, guild="demersal", cap_year = 2019, cap_month = "August", return_data = TRUE)
write.taf(dat, file ="2019_BtS_FO_Figure12b.csv", dir = "report")

# 2. Pelagic
#~~~~~~~~~~~
plot_stock_trends(trends, guild="pelagic", cap_year = 2019, cap_month = "August", return_data = FALSE)
ggplot2::ggsave("2019_BtS_FO_Figure12c.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

dat <- plot_stock_trends(trends, guild="pelagic", cap_year = 2018, cap_month = "November", return_data = TRUE)
write.taf(dat,file ="2019_BtS_FO_Figure12c.csv", dir = "report")

# 3. Benthic
#~~~~~~~~~~~
plot_stock_trends(trends, guild="benthic", cap_year = 2019, cap_month = "August",return_data = FALSE )
ggplot2::ggsave("2019_BtS_FO_Figure12a.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

dat <- plot_stock_trends(trends, guild="benthic", cap_year = 2018, cap_month = "November", return_data = TRUE)
write.taf(dat, file ="2019_BtS_FO_Figure12a.csv", dir = "report" )


#~~~~~~~~~~~~~~~~~~~~~~~~~#
# Ecosystem Overviews plot
#~~~~~~~~~~~~~~~~~~~~~~~~~#
guild <- read.taf("model/guild.csv")

# For this EO, they need separate plots with all info

guild2 <- guild %>% filter(Metric == "F_FMSY")
plot_guild_trends(guild, cap_year = 2019, cap_month = "October",return_data = FALSE )
ggplot2::ggsave("2019_BtS_EO_GuildTrends.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)
guild2 <- guild2 %>% filter(FisheriesGuild != "MEAN")
plot_guild_trends(guild2, cap_year = 2019, cap_month = "November",return_data = FALSE )
ggplot2::ggsave("2019_BtS_EO_GuildTrends_noMEAN_F.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

guild2 <- guild %>% filter(Metric == "SSB_MSYBtrigger")
guild3 <- guild2 %>% dplyr::filter(FisheriesGuild != "MEAN")
plot_guild_trends(guild3, cap_year = 2019, cap_month = "November",return_data = FALSE )
ggplot2::ggsave("2019_BtS_EO_GuildTrends_short_noMEAN_SSB.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)


dat <- plot_guild_trends(guild, cap_year = 2019, cap_month = "October",return_data = TRUE)
write.taf(dat, file ="2019_BtS_EO_GuildTrends.csv", dir = "report", quote = TRUE)

dat <- trends[,1:2]
dat <- unique(dat)
dat <- dat %>% filter(StockKeyLabel != "MEAN")
dat2 <- sid %>% select(c(StockKeyLabel, StockKeyDescription))
dat <- left_join(dat,dat2)
write.taf(dat, file ="2019_BtS_EO_SpeciesGuild_list.csv", dir = "report", quote = TRUE)

#~~~~~~~~~~~~~~~#
# B.Current catches
#~~~~~~~~~~~~~~~#

# 1. Demersal
#~~~~~~~~~~~
bar <- plot_CLD_bar(catch_current, guild = "demersal", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "demersal", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2019_BtS_FO_Figure13_demersal.csv", dir = "report" )

kobe <- plot_kobe(catch_current, guild = "demersal", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = FALSE)
#kobe_dat is just like bar_dat with one less variable
#kobe_dat <- plot_kobe(catch_current, guild = "Demersal", caption = T, cap_year = 2018, cap_month = "November", return_data = TRUE)

png("report/2019_BtS_FO_Figure13_demersal.png",
    width = 131.32,
    height = 88.9,
    units = "mm",
    res = 300)
p1_plot<-gridExtra::grid.arrange(kobe,
                                 bar, ncol = 2,
                                 respect = TRUE, top = "demersal")
dev.off()

# 2. Pelagic
#~~~~~~~~~~~
bar <- plot_CLD_bar(catch_current, guild = "pelagic", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "pelagic", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2019_BtS_FO_Figure13_pelagic.csv", dir = "report")

kobe <- plot_kobe(catch_current, guild = "pelagic", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = FALSE)
png("report/2019_BtS_FO_Figure13_pelagic.png",
    width = 131.32,
    height = 88.9,
    units = "mm",
    res = 300)
p1_plot<-gridExtra::grid.arrange(kobe,
                                 bar, ncol = 2,
                                 respect = TRUE, top = "pelagic")
dev.off()


# 3. Benthic
#~~~~~~~~~~~
bar <- plot_CLD_bar(catch_current, guild = "benthic", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "benthic", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2019_BtS_FO_Figure13_benthic.csv", dir = "report" )

kobe <- plot_kobe(catch_current, guild = "benthic", caption = TRUE, cap_year = 2018, cap_month = "August", return_data = FALSE)
png("report/2019_BtS_FO_Figure13_benthic.png",
    width = 131.32,
    height = 88.9,
    units = "mm",
    res = 300)
p1_plot<-gridExtra::grid.arrange(kobe,
                                 bar, ncol = 2,
                                 respect = TRUE, top = "benthic")
dev.off()


# 4. All
#~~~~~~~~~~~
bar <- plot_CLD_bar(catch_current, guild = "All", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "All", caption = TRUE, cap_year = 2019, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2019_BtS_FO_Figure13_All.csv", dir = "report" )

kobe <- plot_kobe(catch_current, guild = "All", caption = TRUE, cap_year = 2018, cap_month = "August", return_data = FALSE)
png("report/2019_BtS_FO_Figure13_All.png",
    width = 131.32,
    height = 88.9,
    units = "mm",
    res = 300)
p1_plot<-gridExtra::grid.arrange(kobe,
                                 bar, ncol = 2,
                                 respect = TRUE, top = "All stocks")
dev.off()


#~~~~~~~~~~~~~~~#
# C. Discards
#~~~~~~~~~~~~~~~#
discardsA <- plot_discard_trends(catch_trends, 2019, cap_year = 2019, cap_month = "August")

dat <- plot_discard_trends(catch_trends, 2019, cap_year = 2019, cap_month = "August", return_data = TRUE)
write.taf(dat, file ="2019_BtS_FO_Figure7_trends.csv", dir = "report" )

#Need to change order?
discardsB <- plot_discard_current(catch_trends, 2019, cap_year = 2019, cap_month = "August")

dat <- plot_discard_current(catch_trends, 2019, cap_year = 2018, cap_month = "August", return_data = TRUE)
write.taf(dat, file ="2019_BtS_FO_Figure7_current.csv", dir = "report" )

png("report/2019_BtS_FO_Figure7.png",
    width = 131.32,
    height = 88.9,
    units = "mm",
    res = 300)
p1_plot <- gridExtra::grid.arrange(discardsA,
                                   discardsB, ncol = 2,
                                   respect = TRUE)
dev.off()

#~~~~~~~~~~~~~~~#
#D. ICES pies
#~~~~~~~~~~~~~~~#

plot_status_prop_pies(clean_status, "August", "2019")
ggplot2::ggsave("2019_BtS_FO_Figure10.png", path = "report/", width = 178, height = 178, units = "mm", dpi = 300)

dat <- plot_status_prop_pies(clean_status, "November", "2018", return_data = TRUE)
write.taf(dat, file= "2019_BtS_FO_Figure10.csv", dir = "report")

#~~~~~~~~~~~~~~~#
#E. GES pies
#~~~~~~~~~~~~~~~#

#Need to change order and fix numbers
plot_GES_pies(clean_status, catch_current, "August", "2019")
ggplot2::ggsave("2019_BtS_FO_Figure11.png", path = "report", width = 178, height = 178, units = "mm", dpi = 300)

dat <- plot_GES_pies(clean_status, catch_current, "November", "2018", return_data = TRUE)
write.taf(dat, file = "2019_BtS_FO_Figure11.csv", dir = "report")

#~~~~~~~~~~~~~~~#
#F. ANNEX TABLE
#~~~~~~~~~~~~~~~#


dat <- format_annex_table(clean_status, 2019)

write.taf(dat, file = "2019_BtS_FO_annex_table.csv", dir = "report")

# This annex table has to be edited by hand,
# For SBL and GES only one values is reported,
# the one in PA for SBL and the one in MSY for GES


###########
## 3: VMS #
###########

#~~~~~~~~~~~~~~~#
# A. Effort map
#~~~~~~~~~~~~~~~#

gears <- c("Static", "Midwater", "Otter", "Demersal seine")

effort <-
    effort %>%
      dplyr::filter(fishing_category_FO %in% gears) %>%
      dplyr::mutate(
        fishing_category_FO =
          dplyr::recode(fishing_category_FO,
            Static = "Static gears",
            Midwater = "Pelagic trawls and seines",
            Otter = "Bottom otter trawls",
            `Demersal seine` = "Bottom seines"),
          mw_fishinghours = as.numeric(mw_fishinghours)
        ) %>%
      filter(!is.na(mw_fishinghours))

# write layer
write_layer <- function(dat, fname) {
  sf::write_sf(dat, paste0("report/", fname, ".shp"))
  files <- dir("report", pattern = fname, full = TRUE)
  files <- files[tools::file_ext(files) != "png"]
  zip(paste0("report/", fname, ".zip"), files, extras = "-j")
  file.remove(files)
}
write_layer(effort, "2019_BtS_FO_Figure9")

# save plot
plot_effort_map(effort, ecoregion) +
  ggtitle("Average MW Fishing hours 2015-2018")

ggplot2::ggsave("2019_BtS_FO_Figure9.png", path = "report", width = 170, height = 200, units = "mm", dpi = 300)

#~~~~~~~~~~~~~~~#
# A. Swept area map
#~~~~~~~~~~~~~~~#

# write layer
write_layer(sar, "2019_BtS_FO_Figure17")

plot_sar_map(sar, ecoregion, what = "surface") +
  ggtitle("Average surface swept area ratio 2015-2018")

ggplot2::ggsave("2019_BtS_FO_Figure17a.png", path = "report", width = 170, height = 200, units = "mm", dpi = 300)

plot_sar_map(sar, ecoregion, what = "subsurface")+
  ggtitle("Average subsurface swept area ratio 2015-2018")

ggplot2::ggsave("2019_BtS_FO_Figure17b.png", path = "report", width = 170, height = 200, units = "mm", dpi = 300)

