## Calculate segment based kinship matrix ##
## the kinship matrix will be used for other optisel functions ##

##requires one file for each chromosome and a marker map##

##all individuals all markers##
geno <- read_xlsx("optiselgenoSR.xlsx")

##map must be in the below format##

map <- read_xlsx("phaseolusmap.xlsx")
colnames(map)<- c("Name", "Chr","site", "cM","M")
map <- map[,-c(3,5)]
           

##separate whole geno table by chromosome##

chromo1 <- geno[1:105,]
chromo2 <- geno[106:261,]
chromo3 <- geno[262:366,]
chromo4 <- geno[367:464,]
chromo5 <- geno[465:540,]
chromo6 <- geno[541:628,]
chromo7 <- geno[629:719,]
chromo8 <- geno[720:838,]
chromo9 <- geno[840:905,]
chromo10 <- geno[906:976,]
chromo11 <- geno[977:1047,]

##write .phased tables##

write.table(chromo1,"SR.Chr1.phased")
write.table(chromo2,"SR.Chr2.phased")
write.table(chromo3,"SR.Chr3.phased")
write.table(chromo4,"SR.Chr4.phased")
write.table(chromo5,"SR.Chr5.phased")
write.table(chromo6,"SR.Chr6.phased")
write.table(chromo7,"SR.Chr7.phased")
write.table(chromo8,"SR.Chr8.phased")
write.table(chromo9,"SR.Chr9.phased")
write.table(chromo10,"SR.Chr10.phased")
write.table(chromo11,"SR.Chr11.phased")

##read .phased tables into a list##
files <- paste0("SR.Chr", 1:11, ".phased")

##perform sedIBD function, save as ped (pedigree matrix) ##
ped <- segIBD(files, map, minSNP=20, minL=1.0,unitP="cM", unitL="cM")
