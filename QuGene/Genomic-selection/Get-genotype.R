
strategy="/Volumes/Seagate drive/NEW/training15par"

setwd(strategy)


library("readr")
library("dplyr")
library("stringr")
library("openxlsx")
library("tidyverse")


popfiles0 <- list.files(path=strategy, pattern="*000.pop")

lapply(X=popfiles0, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle0", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})



cycle <- read.csv(file="cycle0", sep=",", header=FALSE)
rows <- rep(seq_len(nrow(cycle)/2), each=2)

hap <- rowsum(cycle, group=rows, reorder=FALSE)
hap[hap == 2] <- -1
hap[hap == 3] <- 0
hap[hap == 4] <- 1

#write.table(hap, file=paste0(folder, "/", basename(file), "GS"), sep=",", row.names=FALSE, col.names=TRUE)

### get the following from the writing-label.R file	
label <- unlist(read.table(file="Training-label", sep=","))

qtl <- c(4,7,15,23,26,30,32,49,107,110,111,214,281,298,306,309,448,478,484,493,513,536,629,632,633,634,635,636,649,661,665,675,676,720,722,922,1026,1035)
###

colnames(hap) <- label
removed <- hap %>% subset(select=-qtl)

write.table(removed, file="TRN-genotype", sep="\t", row.names=FALSE, col.names=TRUE)



	
