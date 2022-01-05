
folder="/home/jenny713/projects/def-haricots/jenny713/70run/bulk/allcycles" # change to your working directory in compute canada

setwd(folder)

library("dplyr")

############################################################################## 
### THIS CODE WILL CONVERT THE POP FILES TO 0, 1, 2 NOTATION FOR GAPIT PROGRAM

cyclelist <- list.files(path=folder, pattern="cycle")   

for (file in cyclelist) {

	cycle <- read.csv(file, sep=",", header=FALSE)
	rows <- rep(seq_len(nrow(cycle)/2), each=2)

	hap <- rowsum(cycle, group=rows, reorder=FALSE)
	hap[hap == 2] <- 0
	hap[hap == 3] <- 1
	hap[hap == 4] <- 2

	write.table(hap, file=paste0(folder, "/", basename(file), "GAP"), sep=",", row.names=FALSE, col.names=TRUE)
}

#############################################################################################  
### THIS IS AN ALTERNATIVE TO CONVERT THE POP FILES TO -1, 0, 1 NOTATION FOR rrBLUP IF NEEDED
cyclelist <- list.files(path=folder, pattern="cycle")   

for (file in cyclelist) {

	cycle <- read.csv(file, sep=",", header=FALSE)
	rows <- rep(seq_len(nrow(cycle)/2), each=2)

	hap <- rowsum(cycle, group=rows, reorder=FALSE)
	hap[hap == 2] <- -1
	hap[hap == 3] <- 0
	hap[hap == 4] <- 1

	write.table(hap, file=paste0(folder, "/", basename(file), "GS"), sep=",", row.names=FALSE, col.names=TRUE)
}

