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
  sf::st_read("vms_effort.csv",
               options = "GEOM_POSSIBLE_NAMES=wkt", crs = 4326)
effort <- dplyr::select(effort, -WKT)

# read vms swept area ratio
sar <-
  sf::st_read("vms_sar.csv",
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
ggplot2::ggsave("2020_BtS_FO_Catches_species.png", path = "report/", width = 170, height = 100.5, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COMMON_NAME", line_count = 5, plot_type = "line", return_data = TRUE)
write.taf(dat, "2020_BtS_FO_Catches_species.csv", dir = "report")


#~~~~~~~~~~~~~~~#
# By country
#~~~~~~~~~~~~~~~#
#Plot
plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 9, plot_type = "area")
ggplot2::ggsave("2020_BtS_FO_Catches_country.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 9, plot_type = "area", return_data = TRUE)
write.taf(dat, file= "2020_BtS_FO_Catches_country.csv", dir = "report")

#~~~~~~~~~~~~~~~#
# By guild
#~~~~~~~~~~~~~~~#
# I remove Crustacean and Elasmobranch because they were not there last year and
# create a new line "other" which is almost zero

catch_dat2 <- dplyr::filter(catch_dat, GUILD != "Crustacean")
catch_dat2 <- dplyr::filter(catch_dat2, GUILD != "Elasmobranch")

        #Plot
plot_catch_trends(catch_dat2, type = "GUILD", line_count = 4, plot_type = "line")
ggplot2::ggsave("2020_BtS_FO_Catches_guild.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

        #data
dat <- plot_catch_trends(catch_dat, type = "GUILD", line_count = 4, plot_type = "line", return_data = TRUE)
write.taf(dat, file= "2020_BtS_FO_Catches_guild.csv", dir = "report")

################################
## 2: STECF effort and landings#
################################

#~~~~~~~~~~~~~~~#
# Effort by country
#~~~~~~~~~~~~~~~#
plot_stecf <- function(x, type, variable = NULL, cap_year, cap_month, line_count, stecf_report, return_data = FALSE) {
        
        if(type == "effort"){
                if(variable=="COUNTRY"){
                        dat <- dplyr::rename_(effort_BtS, "type_var" ="country.name",
                                              "VALUE" = "total.kW.days.at.sea")}
                if(variable=="GEAR"){
                        dat <- dplyr::rename_(x, "type_var" ="GEAR",
                                              "VALUE" = "EFFORT")
                }
                Label <- "Nominal effort (1000 kW days at sea)"
        }
        if(type == "landings"){
                dat <- dplyr::rename(landings_BtS, "type_var" ="gear_class",
                                     "VALUE" = "total.live.weight.landed..tonnes.")
                Label <- "Landings (thousand tonnes)"
        }
        
        dat$type_var <- as.factor(dat$type_var)
        
        dat$VALUE <- as.numeric(dat$VALUE)
        Plot <- dplyr::group_by(dat,type_var)
        Plot <- dplyr::summarise(Plot,typeTotal = sum(VALUE, na.rm = TRUE))
        Plot <- dplyr::arrange(Plot,-typeTotal)        
        Plot <- dplyr::filter(Plot, typeTotal >= 1)
        Plot <- dplyr::mutate(Plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal))) 
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dplyr::left_join(dat, Plot)
        # dat <- dat[complete.cases(dat), ]
        dat <- dplyr::mutate(dat, type_var = replace(type_var, RANK > 7, "other"))
        dat <- dplyr::group_by(dat,type_var, year) 
        dat <- dplyr::summarise(dat, typeTotal = sum(VALUE, na.rm = TRUE))
        dat <- dplyr::filter(dat,!is.na(year))
        
        dat <- rbind(dat[!dat$type_var == "other",],
                     dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF %s. Accessed %s/%s.",
                             "19-11",
                             "August",
                             "2020")
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = Label,
                                 caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(7 + 1)
        
        order <- dplyr::group_by(dat, type_var)
        order <- dplyr::summarise(order, total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order, -total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                               levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        if(type == "effort"){
                dat$typeTotal <- dat$typeTotal/1000
        }
        
        pl <- ggplot2::ggplot(dplyr::ungroup(dat), ggplot2::aes(x = year, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(dat$year, na.rm = TRUE),
                                                         max(dat$year, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = 2018, y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(dat$year, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               panel.border = ggplot2::element_blank(),
                               strip.background = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6),
                               axis.line = ggplot2::element_blank())
        
        
        pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                      alpha = .9, position = "identity")
        dat2 <- dplyr::filter(dat,year == max(year, na.rm = TRUE))
        pl <- pl + ggrepel::geom_label_repel(data = dat2 ,
                                             ggplot2::aes(label = type_var,
                                                          fill = type_var),
                                             nudge_x = 3,
                                             label.size = 0.2,
                                             segment.size = 0.25,
                                             size = 2,
                                             color = 'white',
                                             force = 3,
                                             segment.color = 'grey60')
        
        
        if(return_data == T){
                dat
        }else{
                pl
        }
}




        #Plot
plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = FALSE)
frmt_effort <- dplyr::filter(frmt_effort, COUNTRY %in% c("Sweden", "Poland", "Germany", "Denmark", "Lithuania","Latvia"))
plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = FALSE)
ggplot2::ggsave("2020_BtS_FO_STECF_effortCountry.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)
        #data
dat <- plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= "2020_BtS_FO_STECF_effortCountry.csv", dir = "report")


#~~~~~~~~~~~~~~~#
#Effort by gear
#~~~~~~~~~~~~~~~#
plot_stecf <- function(x, type, variable = NULL, cap_year, cap_month, line_count, stecf_report, return_data = FALSE) {
        
        if(type == "effort"){
                if(variable=="COUNTRY"){
                        dat <- dplyr::rename_(effort_BtS, "type_var" ="country.name",
                                              "VALUE" = "total.kW.days.at.sea")}
                if(variable=="GEAR"){
                        dat <- dplyr::rename_(effort_BtS, "type_var" ="gear_class",
                                              "VALUE" = "total.kW.days.at.sea")
                }
                Label <- "Nominal effort (1000 kW days at sea)"
        }
        if(type == "landings"){
                dat <- dplyr::rename(landings_BtS, "type_var" ="gear_class",
                                     "VALUE" = "total.live.weight.landed..tonnes.")
                Label <- "Landings (thousand tonnes)"
        }
        
        dat$type_var <- as.factor(dat$type_var)
        
        dat$VALUE <- as.numeric(dat$VALUE)
        Plot <- dplyr::group_by(dat,type_var)
        Plot <- dplyr::summarise(Plot,typeTotal = sum(VALUE, na.rm = TRUE))
        Plot <- dplyr::arrange(Plot,-typeTotal)        
        Plot <- dplyr::filter(Plot, typeTotal >= 1)
        Plot <- dplyr::mutate(Plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal))) 
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dplyr::left_join(dat, Plot)
        # dat <- dat[complete.cases(dat), ]
        dat <- dplyr::mutate(dat, type_var = replace(type_var, RANK > 7, "other"))
        dat <- dplyr::group_by(dat,type_var, year) 
        dat <- dplyr::summarise(dat, typeTotal = sum(VALUE, na.rm = TRUE))
        dat <- dplyr::filter(dat,!is.na(year))
        
        dat <- rbind(dat[!dat$type_var == "other",],
                     dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF %s. Accessed %s/%s.",
                             "19-11",
                             "August",
                             "2020")
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = Label,
                                 caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(7 + 1)
        
        order <- dplyr::group_by(dat, type_var)
        order <- dplyr::summarise(order, total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order, -total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                               levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        if(type == "effort"){
                dat$typeTotal <- dat$typeTotal/1000
        }
        
        pl <- ggplot2::ggplot(dplyr::ungroup(dat), ggplot2::aes(x = year, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(dat$year, na.rm = TRUE),
                                                         max(dat$year, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = 2018, y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(dat$year, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               panel.border = ggplot2::element_blank(),
                               strip.background = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6),
                               axis.line = ggplot2::element_blank())
        
        
        pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                      alpha = .9, position = "identity")
        dat2 <- dplyr::filter(dat,year == max(year, na.rm = TRUE))
        pl <- pl + ggrepel::geom_label_repel(data = dat2 ,
                                             ggplot2::aes(label = type_var,
                                                          fill = type_var),
                                             nudge_x = 3,
                                             label.size = 0.2,
                                             segment.size = 0.25,
                                             size = 2,
                                             color = 'white',
                                             force = 3,
                                             segment.color = 'grey60')
        
        
        if(return_data == T){
                dat
        }else{
                pl
        }
}





        #Plot
plot_stecf(frmt_effort,type = "effort", variable= "GEAR", "2019","August", 9, "15-23")
ggplot2::ggsave("2020_BtS_FO_STECF_effortGear.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

        #data
dat<-plot_stecf(frmt_effort,type = "effort", variable= "GEAR", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= "2020_BtS_FO_STECF_effortGear.csv", dir = "report")

#~~~~~~~~~~~~~~~#
#Landings by country
#~~~~~~~~~~~~~~~#
plot_stecf <- function(x, type, variable = NULL, cap_year, cap_month, line_count, stecf_report, return_data = FALSE) {
        
        if(type == "effort"){
                if(variable=="COUNTRY"){
                        dat <- dplyr::rename_(x, "type_var" ="COUNTRY",
                                              "VALUE" = "EFFORT")}
                if(variable=="GEAR"){
                        dat <- dplyr::rename_(x, "type_var" ="GEAR",
                                              "VALUE" = "EFFORT")
                }
                Label <- "Nominal effort (1000 kW days at sea)"
        }
        if(type == "landings"){
                dat <- dplyr::rename(landings_BtS, "type_var" ="gear_class",
                                      "VALUE" = "total.live.weight.landed..tonnes.")
                Label <- "Landings (thousand tonnes)"
        }
        
        dat$type_var <- as.factor(dat$type_var)
        
        dat$VALUE <- as.numeric(dat$VALUE)
        Plot <- dplyr::group_by(dat,type_var)
        Plot <- dplyr::summarise(Plot,typeTotal = sum(VALUE, na.rm = TRUE))
        Plot <- dplyr::arrange(Plot,-typeTotal)        
        Plot <- dplyr::filter(Plot, typeTotal >= 1)
        Plot <- dplyr::mutate(Plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal))) 
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dplyr::left_join(dat, Plot)
        # dat <- dat[complete.cases(dat), ]
        dat <- dplyr::mutate(dat, type_var = replace(type_var, RANK > 6, "other"))
        dat <- dplyr::group_by(dat,type_var, year) 
        dat <- dplyr::summarise(dat, typeTotal = sum(VALUE, na.rm = TRUE))
        dat <- dplyr::filter(dat,!is.na(year))
        
        dat <- rbind(dat[!dat$type_var == "other",],
                     dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF %s. Accessed %s/%s.",
                             "19-11",
                             "August",
                             "2020")
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = Label,
                                 caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(5 + 1)
        
        order <- dplyr::group_by(dat, type_var)
        order <- dplyr::summarise(order, total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order, -total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                               levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        if(type == "effort"){
                dat$typeTotal <- dat$typeTotal/1000
        }
        
        pl <- ggplot2::ggplot(dplyr::ungroup(dat), ggplot2::aes(x = year, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(dat$year, na.rm = TRUE),
                                                         max(dat$year, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = 2018, y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(dat$year, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               panel.border = ggplot2::element_blank(),
                               strip.background = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6),
                               axis.line = ggplot2::element_blank())
        
        
        pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                      alpha = .9, position = "identity")
        dat2 <- dplyr::filter(dat,year == max(year, na.rm = TRUE))
        pl <- pl + ggrepel::geom_label_repel(data = dat2 ,
                                             ggplot2::aes(label = type_var,
                                                          fill = type_var),
                                             nudge_x = 3,
                                             label.size = 0.2,
                                             segment.size = 0.25,
                                             size = 2,
                                             color = 'white',
                                             force = 3,
                                             segment.color = 'grey60')
        
        
        if(return_data == T){
                dat
        }else{
                pl
        }
}



        #Plot
plot_stecf(frmt_landings,type = "landings", variable= "GEAR", "2019","August", 9, "15-23")
ggplot2::ggsave("2020_BtS_FO_STECF_landings.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)
        #dat
dat <- plot_stecf(frmt_landings, type = "landings", variable="landings", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= "2020_BtS_FO_STECF_landings.csv", dir = "report")

###########
## 3: SAG #
###########

#~~~~~~~~~~~~~~~#
# A. Trends by guild
#~~~~~~~~~~~~~~~#
# 1. Demersal
#~~~~~~~~~~~
plot_stock_trends(trends, guild="demersal", cap_year = 2020, cap_month = "August", return_data = FALSE)
ggplot2::ggsave("2020_BtS_FO_SAG_Trends_demersal.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

dat <- plot_stock_trends(trends, guild="demersal", cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(dat, file ="2020_BtS_FO_SAG_Trends_demersal.csv", dir = "report")

# 2. Pelagic
#~~~~~~~~~~~
plot_stock_trends(trends, guild="pelagic", cap_year = 2020, cap_month = "August", return_data = FALSE)
ggplot2::ggsave("2020_BtS_FO_SAG_Trends_pelagic.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

dat <- plot_stock_trends(trends, guild="pelagic", cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(dat,file ="2020_BtS_FO_SAG_Trends_pelagic.csv", dir = "report")

# 3. Benthic
#~~~~~~~~~~~
plot_stock_trends(trends, guild="benthic", cap_year = 2020, cap_month = "August",return_data = FALSE )
ggplot2::ggsave("2020_BtS_FO_SAG_Trends_benthic.png", path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

dat <- plot_stock_trends(trends, guild="benthic", cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(dat, file ="2020_BtS_FO_SAG_Trends_benthic.csv", dir = "report" )


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
bar <- plot_CLD_bar(catch_current, guild = "demersal", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "demersal", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2020_BtS_FO_SAG_Current_demersal.csv", dir = "report" )

kobe <- plot_kobe(catch_current, guild = "demersal", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)
#kobe_dat is just like bar_dat with one less variable
#kobe_dat <- plot_kobe(catch_current, guild = "Demersal", caption = T, cap_year = 2018, cap_month = "November", return_data = TRUE)

png("report/2020_BtS_FO_SAG_Current_demersal.png",
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
bar <- plot_CLD_bar(catch_current, guild = "pelagic", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "pelagic", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2020_BtS_FO_SAG_Current_pelagic.csv", dir = "report")

kobe <- plot_kobe(catch_current, guild = "pelagic", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)
png("report/2020_BtS_FO_SAG_Current_pelagic.png",
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
catch_current$Status[which(catch_current$StockKeyLabel == "sol.27.20-24")] <- "GREEN"

bar <- plot_CLD_bar(catch_current, guild = "benthic", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "benthic", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2020_BtS_FO_SAG_Current_benthic.csv", dir = "report" )

kobe <- plot_kobe(catch_current, guild = "benthic", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)
png("report/2020_BtS_FO_SAG_Current_benthic.png",
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
bar <- plot_CLD_bar(catch_current, guild = "All", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)

bar_dat <- plot_CLD_bar(catch_current, guild = "All", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(bar_dat, file ="2020_BtS_FO_SAG_Current_All.csv", dir = "report" )

kobe <- plot_kobe(catch_current, guild = "All", caption = TRUE, cap_year = 2020, cap_month = "August", return_data = FALSE)
png("report/2020_BtS_FO_SAG_Current_All.png",
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
discardsA <- plot_discard_trends(catch_trends, 2020, cap_year = 2020, cap_month = "August")

dat <- plot_discard_trends(catch_trends, 2020, cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(dat, file ="2020_BtS_FO_SAG_Discards_trends.csv", dir = "report" )

catch_trends2 <- catch_trends %>% filter(discards > 0)
discardsB <- plot_discard_current(catch_trends2, 2020,position_letter = "b)", cap_year = 2020, cap_month = "August", caption = FALSE)

discardsC <- plot_discard_current(catch_trends, 2020,position_letter = "c)", cap_year = 2020, cap_month = "August")

#Need to change order?
dat <- plot_discard_current(catch_trends, 2020, cap_year = 2020, cap_month = "August", return_data = TRUE)
write.taf(dat, file ="2020_BtS_FO_SAG_Discards_current.csv", dir = "report" )

cowplot::plot_grid(discardsA, discardsB, discardsC, align = "h", nrow = 1, rel_widths = 1, rel_heights = 1)
ggplot2::ggsave("2020_BtS_FO_SAG_Discards.png", path = "report/", width = 220.32, height = 88.9, units = "mm", dpi = 300)


#~~~~~~~~~~~~~~~#
#D. ICES pies
#~~~~~~~~~~~~~~~#

plot_status_prop_pies(clean_status, "August", "2020")
ggplot2::ggsave("2020_BtS_FO_SAG_ICESpies.png", path = "report/", width = 178, height = 178, units = "mm", dpi = 300)

dat <- plot_status_prop_pies(clean_status, "August", "2020", return_data = TRUE)
write.taf(dat, file= "2020_BtS_FO_SAG_ICESpies.csv", dir = "report")

#~~~~~~~~~~~~~~~#
#E. GES pies
#~~~~~~~~~~~~~~~#

#Need to change order and fix numbers
plot_GES_pies(clean_status, catch_current, "August", "2020")
ggplot2::ggsave("2020_BtS_FO_SAG_GESpies.png", path = "report", width = 178, height = 178, units = "mm", dpi = 300)

dat <- plot_GES_pies(clean_status, catch_current, "August", "2020", return_data = TRUE)
write.taf(dat, file = "2020_BtS_FO_SAG_GESpies.csv", dir = "report")

#~~~~~~~~~~~~~~~#
#F. ANNEX TABLE
#~~~~~~~~~~~~~~~#


dat <- format_annex_table(clean_status, 2020)

write.taf(dat, file = "2020_BtS_FO_SAG_annex_table.csv", dir = "report")

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
write_layer(effort, "2020_BtS_FO_VMS_effort")

# save plot
plot_effort_map(effort, ecoregion) +
  ggtitle("Average MW Fishing hours 2016-2019")

ggplot2::ggsave("2020_BtS_FO_VMS_effort.png", path = "report", width = 170, height = 200, units = "mm", dpi = 300)

#~~~~~~~~~~~~~~~#
# A. Swept area map
#~~~~~~~~~~~~~~~#

# write layer
write_layer(sar, "2020_BtS_FO_VMS_sar")

plot_sar_map(sar, ecoregion, what = "surface") +
  ggtitle("Average surface swept area ratio 2016-2019")

ggplot2::ggsave("2020_BtS_FO_VMS_sarA.png", path = "report", width = 170, height = 200, units = "mm", dpi = 300)

plot_sar_map(sar, ecoregion, what = "subsurface")+
  ggtitle("Average subsurface swept area ratio 2016-2019")

ggplot2::ggsave("2020_BtS_FO_VMS_sarB.png", path = "report", width = 170, height = 200, units = "mm", dpi = 300)

