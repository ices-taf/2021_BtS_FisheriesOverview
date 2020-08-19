# wd: bootstrap/data/ICES_nominal_catches

library(icesTAF)
taf.library(icesFO)

hist <- load_historical_catches()
write.taf(hist, file = "ICES_historical_catches.csv", quote = TRUE)

official <- load_official_catches()
write.taf(official, file = "ICES_2006_2017_catches.csv", quote = TRUE)

preliminary <- load_preliminary_catches(2018)
write.taf(preliminary, file = "ICES_preliminary_catches.csv", quote = TRUE)
