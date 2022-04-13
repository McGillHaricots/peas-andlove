###This is for extracting one cycle from the QuLinePlus output##

##read in pop file and skip the QuGene parameters at the beginning of the file##
srnmhaplo <- read.csv("SRNMdata_geno.pop", skip=15)

##SRNMdata_geno.pop renamed from "..001_001_001_001.pop"###

##eliminate the spaces between chromosomes##
srnmhaplo1 <- as.data.frame(apply(srnmhaplo, 2, function(x)gsub("\\s+", "", x)))


##add spaces between values##
addspace <- function(x) {
  gsub("([1])([1])", "\\1 \\2", x)
  }
srnmhaplo2 <- data.frame(lapply(srnmhaplo1, addspace))

addspace <- function(x) {
  gsub("([1])([2])", "\\1 \\2", x)
}
srnmhaplo3 <- data.frame(lapply(srnmhaplo2, addspace))

addspace <- function(x) {
  gsub("([2])([2])", "\\1 \\2", x)
}
srnmhaplo4 <- data.frame(lapply(srnmhaplo3, addspace))

addspace <- function(x) {
  gsub("([2])([1])", "\\1 \\2", x)
}
srnmhaplo5 <- data.frame(lapply(srnmhaplo4, addspace))

addspace <- function(x) {
  gsub("([1])([1])", "\\1 \\2", x)
}
srnmhaplo6 <- data.frame(lapply(srnmhaplo5, addspace))

addspace <- function(x) {
  gsub("([2])([2])", "\\1 \\2", x)
}
srnmhaplo7 <- data.frame(lapply(srnmhaplo6, addspace))
