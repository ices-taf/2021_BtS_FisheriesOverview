
# Initial formatting of the data

library(icesTAF)
taf.library(icesFO)
library(dplyr)

mkdir("data")

# load species list
species_list <- read.taf("bootstrap/data/FAO_ASFIS_species/species_list.csv")
sid <- read.taf("bootstrap/data/ICES_StockInformation/sid.csv")
effort$sub.region <- tolower(effort$sub.region)
unique(effort$sub.region)


# 1: ICES official cath statistics

hist <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_historical_catches.csv")
official <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_2006_2017_catches.csv")
prelim <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_preliminary_catches.csv")

catch_dat <-
  format_catches(2020, "Baltic Sea",
    hist, official, prelim, species_list, sid)

write.taf(catch_dat, dir = "data", quote = TRUE)


# 2: STECF effort and landings

effort <- read.taf("bootstrap/initial/data/Effort-by-country.csv", check.names = TRUE)
names(effort)
effort$sub.region <- tolower(effort$sub.region)
unique(effort$sub.region)
effort_BtS <- dplyr::filter(effort, grepl("27.3.b|27.3.c.22|27.3.d.24|27.3.d.25|27.3.d.28.2|
                                          27.3.d.29|27.3.d.26|27.3.d.27|27.3.b.23|27.3.d.28.1|27.3.d.30|27.3.d.31|27.3.d.32", sub.region))





landings1 <- read.taf("bootstrap/initial/data/Catches-by-country-2018.csv", check.names = TRUE)
landings2 <- read.taf("bootstrap/initial/data/Catches-by-country-2017.csv", check.names = TRUE)
landings3 <- read.taf("bootstrap/initial/data/Catches-by-country-2016.csv", check.names = TRUE)
landings4 <- read.taf("bootstrap/initial/data/Catches-by-country-2015.csv", check.names = TRUE)
landings <- rbind(landings1, landings2, landings3, landings4)
names(landings)
landings$sub.region <- tolower(landings$sub.region)
landings_BtS <- dplyr::filter(landings, grepl("27.3.b|27.3.c.22|27.3.d.24|27.3.d.25|27.3.d.28.2|
                                          27.3.d.29|27.3.d.26|27.3.d.27|27.3.b.23|27.3.d.28.1|27.3.d.30|27.3.d.31|27.3.d.32", sub.region))

# need to group gears, Sarah help.
unique(landings_BtS$gear.type)
unique(effort_BtS$gear.type)

landings_BtS <- dplyr::mutate(landings_BtS, gear_class = case_when(
        grepl("TBB", gear.type) ~ "Beam trawl",
        grepl("DRB|DRH", gear.type) ~ "Dredge",
        grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN", gear.type) ~ "Static/Gill net/LL",
        grepl("OTT|OTB|PTB|SSC|SB", gear.type) ~ "Otter trawl/seine",
        grepl("PTM|OTM|PS", gear.type) ~ "Pelagic trawl/seine",
        grepl("FPO", gear.type) ~ "Pots",
        grepl("NK", gear.type) ~ "other",
        is.na(gear.type) ~ "other",
        TRUE ~ "other"
)
)

effort_BtS <- dplyr::mutate(effort_BtS, gear_class = case_when(
        grepl("TBB", gear.type) ~ "Beam trawl",
        grepl("DRB|DRH", gear.type) ~ "Dredge",
        grepl("GNS|GND|GTN|LHP|LLS|FPN|GTR|FYK|LLD|SDN", gear.type) ~ "Static/Gill net/LL",
        grepl("OTT|OTB|PTB|SSC|SB", gear.type) ~ "Otter trawl/seine",
        grepl("PTM|OTM|PS", gear.type) ~ "Pelagic trawl/seine",
        grepl("FPO", gear.type) ~ "Pots",
        grepl("NK", gear.type) ~ "other",
        is.na(gear.type) ~ "other",
        TRUE ~ "other"
)
)


# effort <- read.taf("bootstrap/data/STECF_effort.csv", check.names = TRUE)
# 
# landings <- read.taf("bootstrap/initial/data/STECF_landings.csv", check.names = TRUE)
# 
# frmt_effort <- format_stecf_effort(effort)
# frmt_landings <- format_stecf_landings(landings)
# 
# write.taf(frmt_effort, dir = "data", quote = TRUE)
# write.taf(frmt_landings, dir = "data")


# 3: SAG
sag_sum <- read.taf("bootstrap/data/SAG_data/SAG_summary.csv")
sag_refpts <- read.taf("bootstrap/data/SAG_data/SAG_refpts.csv")
sag_status <- read.taf("bootstrap/data/SAG_data/SAG_status.csv")

clean_sag <- format_sag(sag_sum, sag_refpts, 2020, "Baltic")
clean_status <- format_sag_status(sag_status, 2020, "Baltic Sea")

# remove some stocks
Baltic_Out_stocks <-  c("sal.27.32", "sal.27.22-31", "ele.2737.nea", "trs.27.22-32", "her.27.30", "her.27.31")
clean_sag <- dplyr::filter(clean_sag, !StockKeyLabel %in% Baltic_Out_stocks)
clean_status <- dplyr::filter(clean_status, !StockKeyLabel %in% Baltic_Out_stocks)

write.taf(clean_sag, dir = "data")
write.taf(clean_status, dir = "data", quote = TRUE)
