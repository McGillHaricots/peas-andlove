
setwd("/Users/jennylin/files/McGill/Thesis/QUGENE/qug_engine/NEW")

library(dplyr)
library(openxlsx)

map <- read.xlsx("qug-input-final.xlsx", sheet="Map")
position <- 1:1047

sorted <- cbind(position, map) %>% select(1,3,7)

getqtl <- sorted %>% filter(Type=="1")

qtl <- unlist(getqtl$position)

### 4,7,15,23,26,30,32,49,107,110,111,214,281,298,306,309,448,478,484,493,513,536,629,632,633,634,635,636,649,661,665,675,676,720,722,922,1026,1035

label <- unlist(map$LocusName)

setwd("/Volumes/Seagate drive/NEW/training")

write.table(label, file="Training-label", sep=",", row.names=FALSE, col.names=FALSE)

check <- read.table(file="Training-label", sep=",")
