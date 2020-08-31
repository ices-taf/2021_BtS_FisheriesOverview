
library(icesTAF)
taf.library(icesFO)

sid <- load_sid(2020)

write.taf(sid, quote = TRUE)
