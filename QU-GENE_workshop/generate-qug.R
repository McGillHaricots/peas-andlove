
folder="C:/Users/vhvlo/OneDrive/Documents/JennyLin/QU-GENE-Workshop"
setwd(folder)
library(openxlsx)
source("QUGENE_UI_v4.r")

myfile = "example.xlsx"   ###change this

Diagnostic<-read.xlsx(myfile, sheet = "Diagnostic")
Environment<-read.xlsx(myfile, sheet = "Environment")
Error<-read.xlsx(myfile, sheet = "Error")
LocusEffect<-read.xlsx(myfile, sheet = "LocusEffect")
Map<-read.xlsx(myfile, sheet = "Map")
MarkerEffect<-read.xlsx(myfile, sheet = "MarkerEffect")
Population<-read.xlsx(myfile, sheet = "Population")
Trait<-read.xlsx(myfile, sheet = "Trait")
Type1Pop<-read.xlsx(myfile, sheet = "Type1Pop")
Type2Pop<-read.xlsx(myfile, sheet = "Type2Pop")
Type3Pop<-read.xlsx(myfile, sheet = "Type3Pop")
Epistasis<-read.xlsx(myfile, sheet = "Epistasis")

write.csv(Diagnostic,file = "Diagnostic.csv",row.names = F)
write.csv(Environment,file="Environment.csv",row.names = F)
write.csv(Error,file="Error.csv",row.names = F)
write.csv(LocusEffect,file="LocusEffect.csv",row.names = F)
write.csv(Map,file="Map.csv",row.names = F)
write.csv(MarkerEffect,file="MarkerEffect.csv",row.names = F)
write.csv(Population,file="Population.csv",row.names = F)
write.csv(Trait,file="Trait.csv",row.names = F)
write.csv(Type1Pop,file="Type1Pop.csv",row.names = F)
write.csv(Type2Pop,file="Type2Pop.csv",row.names = F)
write.csv(Type3Pop,file="Type3Pop.csv",row.names = F)
write.csv(Epistasis,file="Epistasis.csv",row.names = F)

nModel=10
randomSeed=10
qugFile=substr(paste0(myfile),1,nchar(myfile)-5)
GEfile=substr(paste0(myfile),1,nchar(myfile)-5)
mapFUN="Kosambi"
RF=F
mapFun="Kosambi"
env.csv="Environment.csv"
trait.csv="Trait.csv"
traitError.csv="Error.csv"
map.csv="Map.csv"
geneEff.csv="LocusEffect.csv"
markerEff.csv="MarkerEffect.csv"
popInfo.csv="Population.csv"
popType1.csv="Type1Pop.csv"
popType2.csv=NULL
popType3.csv=NULL
diagnostic.csv="Diagnostic.csv"
epistasis.csv=NULL
qug.generator(folder,randomSeed,qugFile,GEfile,RF=F,mapFUN,nModel,
		env.csv,trait.csv,traitError.csv,map.csv,geneEff.csv,markerEff.csv,
		popInfo.csv,popType1.csv,popType2.csv,popType3.csv,diagnostic.csv,epistasis.csv)



