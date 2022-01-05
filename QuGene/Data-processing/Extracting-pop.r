
strategy="/home/jenny713/projects/def-haricots/jenny713/100run/bulk" # set to your own directory in compute canada where your pop files are

setwd(strategy)

### load your libraries 
library("readr")
library("dplyr")
library("stringr")
library("openxlsx")
library("tidyverse")

### summarizes the pop files for up to 50 cycles. If less than 50, just delete as needed.

popfiles1 <- list.files(path=strategy, pattern="*001.pop")

lapply(X=popfiles1, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle1", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles2 <- list.files(path=strategy, pattern="*002.pop")

lapply(X=popfiles2, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle2", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles3 <- list.files(path=strategy, pattern="*003.pop")

lapply(X=popfiles3, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle3", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles4 <- list.files(path=strategy, pattern="*004.pop")

lapply(X=popfiles4, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle4", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles5 <- list.files(path=strategy, pattern="*005.pop")

lapply(X=popfiles5, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle5", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles6 <- list.files(path=strategy, pattern="*006.pop")

lapply(X=popfiles6, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle6", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles7 <- list.files(path=strategy, pattern="*007.pop")

lapply(X=popfiles7, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle7", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles8 <- list.files(path=strategy, pattern="*008.pop")

lapply(X=popfiles8, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle8", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles9 <- list.files(path=strategy, pattern="*009.pop")

lapply(X=popfiles9, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle9", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles10 <- list.files(path=strategy, pattern="*010.pop")

lapply(X=popfiles10, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle10", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles11 <- list.files(path=strategy, pattern="*011.pop")

lapply(X=popfiles11, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle11", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles12 <- list.files(path=strategy, pattern="*012.pop")

lapply(X=popfiles12, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle12", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles13 <- list.files(path=strategy, pattern="*013.pop")

lapply(X=popfiles13, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle13", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles14 <- list.files(path=strategy, pattern="*014.pop")

lapply(X=popfiles14, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle14", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles15 <- list.files(path=strategy, pattern="*015.pop")

lapply(X=popfiles15, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle15", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles16 <- list.files(path=strategy, pattern="*016.pop")

lapply(X=popfiles16, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle16", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles17 <- list.files(path=strategy, pattern="*017.pop")

lapply(X=popfiles17, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle17", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles18 <- list.files(path=strategy, pattern="*018.pop")

lapply(X=popfiles18, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle18", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles19 <- list.files(path=strategy, pattern="*019.pop")

lapply(X=popfiles19, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle19", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles20 <- list.files(path=strategy, pattern="*020.pop")

lapply(X=popfiles20, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle20", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles21 <- list.files(path=strategy, pattern="*021.pop")

lapply(X=popfiles21, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle21", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles22 <- list.files(path=strategy, pattern="*022.pop")

lapply(X=popfiles22, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle22", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles23 <- list.files(path=strategy, pattern="*023.pop")

lapply(X=popfiles23, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle23", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles24 <- list.files(path=strategy, pattern="*024.pop")

lapply(X=popfiles24, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle24", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles25 <- list.files(path=strategy, pattern="*025.pop")

lapply(X=popfiles25, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle25", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles26 <- list.files(path=strategy, pattern="*026.pop")

lapply(X=popfiles26, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle26", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles27 <- list.files(path=strategy, pattern="*027.pop")

lapply(X=popfiles27, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle27", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})

popfiles28 <- list.files(path=strategy, pattern="*028.pop")

lapply(X=popfiles28, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle28", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles29 <- list.files(path=strategy, pattern="*029.pop")

lapply(X=popfiles29, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle29", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles30 <- list.files(path=strategy, pattern="*030.pop")

lapply(X=popfiles30, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle30", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles31 <- list.files(path=strategy, pattern="*031.pop")

lapply(X=popfiles31, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle31", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles32 <- list.files(path=strategy, pattern="*032.pop")

lapply(X=popfiles32, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle32", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles33 <- list.files(path=strategy, pattern="*033.pop")

lapply(X=popfiles33, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle33", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles34 <- list.files(path=strategy, pattern="*034.pop")

lapply(X=popfiles34, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle34", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles35 <- list.files(path=strategy, pattern="*035.pop")

lapply(X=popfiles35, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle35", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles36 <- list.files(path=strategy, pattern="*036.pop")

lapply(X=popfiles36, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle36", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles37 <- list.files(path=strategy, pattern="*037.pop")

lapply(X=popfiles37, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle37", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles38 <- list.files(path=strategy, pattern="*038.pop")

lapply(X=popfiles38, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle38", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles39 <- list.files(path=strategy, pattern="*039.pop")

lapply(X=popfiles39, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle39", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles40 <- list.files(path=strategy, pattern="*040.pop")

lapply(X=popfiles40, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle40", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles41 <- list.files(path=strategy, pattern="*041.pop")

lapply(X=popfiles41, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle41", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles42 <- list.files(path=strategy, pattern="*042.pop")

lapply(X=popfiles42, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle42", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles43 <- list.files(path=strategy, pattern="*043.pop")

lapply(X=popfiles43, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle43", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles44 <- list.files(path=strategy, pattern="*044.pop")

lapply(X=popfiles44, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle44", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles45 <- list.files(path=strategy, pattern="*045.pop")

lapply(X=popfiles45, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle45", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles46 <- list.files(path=strategy, pattern="*046.pop")

lapply(X=popfiles46, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle46", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles47 <- list.files(path=strategy, pattern="*047.pop")

lapply(X=popfiles47, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle47", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles48 <- list.files(path=strategy, pattern="*048.pop")

lapply(X=popfiles48, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle48", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles49 <- list.files(path=strategy, pattern="*049.pop")

lapply(X=popfiles49, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle49", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})


popfiles50 <- list.files(path=strategy, pattern="*050.pop")

lapply(X=popfiles50, function(path) {
    pop <- read.table(path, blank.lines.skip=TRUE, 
        colClasses="character", skip=15, fill=TRUE)
    for (i in 1:dim(pop)[1]) {
        pop.data <- data.frame(matrix(unlist(str_split(slice(pop, i:i),pattern="")), 
            nrow=1, byrow=T), stringsAsFactors=FALSE)
    write.table(pop.data, file="cycle50", 
        append= TRUE, quote=FALSE, sep=,",", 
        row.names=FALSE, col.names=FALSE)
    }
})
