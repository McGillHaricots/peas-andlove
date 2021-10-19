##########################################################################
## This is a modification of source code used for RShiny QUGENE_UI
## Changes: 
## 1. Add checking for the number of locus in map and effects
## 2. Re-order arguments
## 3. Remove "print(paste(".pop file are created at", wFolder))"
## 4. Add Type2 and Type 3 population
## 5. Add nModel in the argument
#########################################################################



###########################################################################
## Convert cM to RF
## map
## mapFUN = Haldane or Kosambi
##########################################################################
cMtoRF=function(map,mapFUN)
{
 map.dist=matrix(NA,nrow(map),2)
 map.dist[1,1]=0
 for(i in 2:nrow(map))
  if(map$Chr[i]==map$Chr[i-1])
   map.dist[i,1]=(map$cM[i]-map$cM[i-1])/100 else
    map.dist[i,1]=0
 if(mapFUN=="Haldane")
  map.dist[,2]=round(0.5*(1-exp(-2*map.dist[,1])),4) else
    map.dist[,2]=round((1-exp(-4*map.dist[,1]))/(2*(1+exp(-4*map.dist[,1]))),4)
 map$RF=map.dist[,2]
 map$RF[!duplicated(map$Chr)]=0.5
 map
}


######################################################################
## Generate .qug fiole from .xlsx template
## randomSeed	  = random seed
## qugFile        = name of qugFile, no extention
## GEfile         = name for .ges file, no extention
## RF		  = logical, if TRUE RF was provided in the map
## mapFUN         = map function to convert cM to RF
## nModel	  = number of model for randm effects. If NULL, nModel=1
## env.csv	  = csv file contains environments
## trait.csv      = csv file contains trait list
## traitError.csv = csv file contains trait error
## map.csv        = csv file contains map information 
## geneEff.csv    = csv file contains gene effects
## markereEff.csv = csv file contains marker effects
## popInfo.csv    = csv file contains population info
## popType1.csv   = csv file contains info for population type 1
## popType2.csv   = csv file contains info for population type 2
## popType3.csv   = csv file contains info for population type 3
## diagnostic.csv = csv file contains diagnostic
## epistasis.csv  = csv file contains epistatic network
#####################################################################
qug.generator=function(folder,randomSeed,qugFile,GEfile,RF=F,mapFUN,nModel=NULL,
		       env.csv,trait.csv,traitError.csv,map.csv,geneEff.csv,markerEff.csv,
  		       popInfo.csv,popType1.csv,popType2.csv,popType3.csv,diagnostic.csv,
		       epistasis.csv)
{
 ## Set working folder
    setwd(folder)

 ## qug
    qugFile = paste(qugFile,".qug",sep="")

 ## Read input from each worksheet
    env=read.csv(env.csv,row.names=NULL,header=T, stringsAsFactors=F)
    trait=read.csv(trait.csv,row.names=NULL,header=T, stringsAsFactors=F)
    traitError=read.csv(traitError.csv,row.names=NULL,header=T, stringsAsFactors=F)
    popInfo=read.csv(popInfo.csv,row.names=NULL,header=T, stringsAsFactors=F)
    diagnostic=read.csv(diagnostic.csv,row.names=NULL,header=T, stringsAsFactors=F)
    map.cM=read.csv(map.csv,row.names=NULL,header=T, stringsAsFactors=F)
    geneEff=read.csv(geneEff.csv,row.names=NULL,header=T, stringsAsFactors=F)

    ## Read population data
    if(is.null(popType1.csv)==FALSE)    
     popType1=read.csv(popType1.csv,row.names=NULL,header=T, stringsAsFactors=F) else
      popType1=NULL

    if(is.null(popType2.csv)==FALSE)
     popType2=read.csv(popType2.csv,row.names=NULL,header=T, stringsAsFactors=F) else
      popType2=NULL

    if(is.null(popType3.csv)==FALSE)
     popType3=read.csv(popType3.csv,row.names=NULL,header=T, stringsAsFactors=F) else
      popType3=NULL

 ## Convert cM to RF
    if(RF==TRUE)
     map=map.cM else
      map=cMtoRF(map.cM,mapFUN)
 
 ## Read marker effects
    if(is.null(markerEff.csv)==FALSE)
     markerEff=read.csv(markerEff.csv,row.names=NULL,header=T, stringsAsFactors=F)   else
      markerEff=NULL
 
 ## Read epistasis network
    if(is.null(epistasis.csv)==FALSE)
    {
     epiNetwork=read.csv(epistasis.csv,row.names=NULL,header=T, stringsAsFactors=F)
     ## Replaced epstasis network name in locus effect with epistasis network number
     geneEff$EffectType[geneEff$GPType==2]=epiNetwork$Number[match(geneEff$EffectType[geneEff$GPType==2],epiNetwork$Name)]
    } else
    epiNetwork=NULL

 ## Check for map and gene effects
    nLocus.map=length(unique(map$LocusName))
    nGene=length(unique(geneEff$LocusName))
    if(is.null(markerEff)==FALSE)
    nMarker=length(unique(markerEff$LocusName)) else
       nMarker=0
    nLocus.eff=nGene+nMarker
  
  if(nLocus.map!=nLocus.eff)
   stop("No. of locus in Map and Effect are not the same. Please check.") else
  {
   ## General information
   nEnv=nrow(env)
   nTrt=nrow(trait)
   nGenes=nrow(map)
   if(is.null(nModel))
    nModel=1

   version= "*** Version 2.2.01 - 1st June 2002 ***"
   write(version,qugFile)
   write(c("","! *** GE file name ***"),qugFile,append=T)
   write(c(GEfile,""),qugFile,append=T)
   write.table(c(nModel,randomSeed,nGenes,nEnv,nTrt),qugFile,row.names=F,quote=F,col.names=F,append=T)
   if(is.null(epiNetwork))
    write.table(t(c(1,1,1,0,0,0,1)),qugFile,row.names=F,quote=F,col.names=F,append=T) else
     write.table(t(c(1,1,1,0,1,0,1)),qugFile,row.names=F,quote=F,col.names=F,append=T)

   ## Environment information
   write(c("","! *** Environment Type Information ***"),qugFile,append=T)
   for(i in 1:nrow(env))
    write.table(t(env[i,]),qugFile,row.names=F,quote=F,col.names=F,append=T)

   ## Trait information
   write(c("","! *** Trait Information ***"),qugFile,append=T)
   for(i in 1:nrow(trait))
   {
    trt.name=trait$TraitName[i]
    write.table(t(trait[i,c(1:2)]),qugFile,row.names=F,quote=F,col.names=F,append=T)
    write.table(trait[i,c(3:ncol(trait))],qugFile,row.names=F,quote=F,col.names=F,append=T)
    error.trait=traitError[traitError$Name==trt.name,]
    if(length(error.trait$EnvName)>1)
    {
     write.table(error.trait[,c(3:ncol(error.trait))],
	qugFile,row.names=F,quote=F,col.names=F,append=T)
    } else
    {
     error.env=t(matrix(error.trait[1,c(3:ncol(error.trait))],3,nEnv))
     write.table(error.env,qugFile,row.names=F,quote=F,col.names=F,append=T)
    }
    write("",qugFile,append=T)
   }
  }
  ## Gene and marker infomation
  write(c("","! *** Gene Information ***"),qugFile,append=T)
  for(i in 1:nrow(map))
  {
   write(i,qugFile,append=T)
   gene.name=map$LocusName[i]
   write(gene.name,qugFile,append=T)
   if(map$Type[i]==1)
   {
    gene.eff=geneEff[geneEff$LocusName==gene.name,]
    NT=unique(gene.eff$TraitName)
    write.table(t(unlist(c(map[i,c("Chr","RF","NoAllele")],length(NT)))),
		qugFile,row.names=F,quote=F,col.names=F,append=T)
    for(j in 1:length(NT))
    {
     trait.name=NT[j]
     if(length(gene.eff$EnvName[gene.eff$TraitName==trait.name])==1 && gene.eff$EnvName[gene.eff$TraitName==trait.name]==0)
     {
      gene.trait=cbind(c(1:nEnv),t(matrix(gene.eff[j,c(4:ncol(gene.eff))],length(c(4:ncol(gene.eff))),nEnv)))
      if(unique(gene.trait[,2])==2)
       gene.trait[,3]=match(gene.trait[,3],epiNetwork$Name)
      geneEff.trt=matrix(NA,nEnv,5)
      geneEff.trt[1,5]=trait$No[trait$TraitName==trait.name]
     } else
     {
      gene.trait=gene.eff[gene.eff$TraitName==trait.name,c(3:ncol(gene.eff))]
      geneEff.trt=matrix(NA,nrow(gene.trait),5)
      geneEff.trt[1,5]=trait$No[trait$TraitName==trait.name]
     }
     geneEff.fin=cbind(geneEff.trt,gene.trait)
     geneEff.fin[is.na(geneEff.fin)]=" "
     write.table(geneEff.fin,qugFile,row.names=F,quote=F,col.names=F,append=T)
     }
    } else
    {
     gene.eff=markerEff[markerEff$LocusName==gene.name,]
     write.table(t(unlist(c(map[i,c("Chr","RF","NoAllele")],0))),
		qugFile,row.names=F,quote=F,col.names=F,append=T)
     geneEff.trt=matrix("",1,6)
     geneEff.trt[1,5]=0
     gene.trait=gene.eff[,c(2:ncol(gene.eff))]
     gene.trait[,is.na(gene.trait)]=""
     write.table(cbind(geneEff.trt,gene.trait),qugFile,row.names=F,quote=F,col.names=F,append=T)
    }
   write("",qugFile,append=T)
  }
 
  ## Epistasis network
  if(is.null(epiNetwork)==FALSE)
  {
  write(c("","! *** Epistasis Network ***"),qugFile,append=T)
  for(i in 1:nrow(epiNetwork))
  {
   write.table(t(epiNetwork[i,c(1:3)]),qugFile,row.names=F,quote=F,col.names=F,append=T)
   if(epiNetwork$Fitness[i]==0)
   {
    epiVal=epiNetwork[i,c(4:ncol(epiNetwork))]
    epiVal=epiVal[!is.na(epiVal)]
    write.table(t(epiVal),qugFile,row.names=F,quote=F,col.names=F,append=T)
   }
    write("",qugFile,append=T)
  }
 }

  ## Population information  
  write(c("","! *** Population Information (*.pop) ***"),qugFile,append=T)
  write(nrow(popInfo),qugFile,append=T)
  error.pop=match(1,popInfo$ErrorUse)

  if(length(error.pop)==1 && !is.na(error.pop))
  {
   write(error.pop,qugFile,append=T)
   write("",qugFile,append=T)
  } else
  stop("There is no/multiple populations used for error. Please check.")

  # Write information for each populations
  for(i in 1:nrow(popInfo))
  {
   write(i,qugFile,append=T)
   write.table(t(popInfo[i,c(1:4)]),qugFile,row.names=F,quote=F,col.names=F,append=T,na="")
   if(popInfo$Type[i]==1)
   {
    poptype1=popType1[popType1$PopName==popInfo$PopName[i],]
    write.table(poptype1[,c(2:ncol(popType1))],qugFile,row.names=F,quote=F,col.names=F,append=T,na="")
   }
   if(popInfo$Type[i]==2)
   {
    poptype2=popType2[popType2$PopName==popInfo$PopName[i],]
    write(randomSeed,qugFile,append=T)
    write.table(poptype2[,c(2:ncol(popType2))],qugFile,row.names=F,quote=F,col.names=F,append=T,na="")
   }
   if(popInfo$Type[i]==3)
   {
    poptype3.data=popType3[popType3$PopName==popInfo$PopName[i],]
    index=unique(poptype3.data$Individual)
    poptype3=poptype3.data[,-c(1:2)]
    # Check no of locus in population data = no. locus in map
    if(ncol(poptype3)!=nrow(map))
     stop("No of locus is not the same as defined in the map. Please check.")
    # Check no. individual = population size in population info
    if(length(index)!=popInfo$Size[i])
     stop("Population size is smaller/bigger than defined. Please check")
    if(ncol(poptype3)==nrow(map) && length(index)==popInfo$Size[i])
    {
     poptype3=poptype3[,map$LocusName]	## Order locus based on map order
     ## Check whether the data if haploid of diploid
     if(nrow(poptype3)!=(2*length(index)) && nrow(poptype3)!=length(index))
       stop("Population size is smaller/bigger than defined. Please check")
     # For haploid data
     if(nrow(poptype3)==length(index))
     {
      poptype3=poptype3[rep(row.names(poptype3),2),]
      poptype3=poptype3[order(dimnames(poptype3)[[1]]),]
     }
     for(k in 1:length(index))
     {
      write(k,qugFile,append=T)
      write(index[k],qugFile,append=T)
      write.table(poptype3[c((2*k-1):(2*k)),],qugFile,col.names=F,row.names=F,quote=F,append=T)
      write("",qugFile,append=T)
     }
    }
   }
   write("",qugFile,append=T)
  }

  ## Diagnostic information
  write(c("","! *** Diagnostic Information (*.dgn) ***"),qugFile,append=T)
  write.table(diagnostic,qugFile,row.names=F,quote=F,col.names=F,append=T)
}



############################################################################
## Run QuGene.exe
## wfolder  = foder where input and output are saved
## qFolder  = path of QuGene.exe
## qugFile  = name of .qug file
############################################################################
run.QuGene <- function(wFolder,qFolder,qugFile)
{
 setwd(wFolder)
 exe=paste(qFolder,"QUGENE.exe",sep="/")
 qug=paste(wFolder,paste(qugFile,".qug",sep=""),sep="/")

 write(paste(exe,qug,sep=" "),"QUGENE.bat")
 shell("QUGENE.bat",intern=FALSE,wait=TRUE)
 print(paste(".ges file is created at", wFolder))
}


############################################################################
## Run QuLine.exe
## wfolder    = foder where input and output are saved
## qFolder    = path of QuGene.exe
## gesFile    = name of .ges file
## popFile    = name of .pop file
## qmpFile    = name of .qmp file
## outFile    = name of output files
## nModel     = no. of model
############################################################################
run.QuLine <- function(wFolder,qFolder,gesFile,popFile,qmpFile,outFile,nModel)
{
 setwd(wFolder)
 exe=paste(qFolder,"QuLine.exe",sep="/")
 mio=paste(wFolder,"QuLine.mio",sep="/")
 write.table(c(gesFile,popFile,qmpFile,outFile),"QuLine.mio",col.names=F,row.names=F,quote=F)
 write(paste(exe,mio,sep=" "),"QuLine.bat")

 ## Check version of popFile
 pop=readLines(popFile)
 pop[1]="*** Version 2.2.04  - 27th June 2003 ***"
 write(pop,popFile)

 ## Changed no of model in .ges
 ges=readLines(gesFile)
 nm=grep("number of model",ges)+1
 ges[nm]=sub("1",nModel,ges[nm])
 write(ges,gesFile)

 shell("QuLine.bat",intern=FALSE,wait=TRUE)
 print(paste("Output files are created at", wFolder))
}


####################################################################################
## Create .pop file
## Folder  = folder where .pop will be created
## popData = haplotype data
## popName = name of .pop file
####################################################################################
write.InitPop=function(wFolder,popData,popName)
{
 setwd(wFolder)
 popFile=paste(popName,".pop",sep="")
 write(c("*** Version 2.2.04  - 27th June 2003 ***",""),popFile)
 write(c(1,""),popFile,append=T)
 write(c("PopInit",""),popFile,append=T)
 write(ncol(popData),popFile,append=T)
 
 nAllele=apply(popData,2,max)
 nAllele=nAllele[nAllele!=""]

 write.table(matrix(nAllele,1,ncol(popData)),popFile,col.names=F,row.names=F,quote=F,append=T)
 write(c("",(nrow(popData)/3),""),popFile,append=T)
 write.table(popData,popFile,col.names=F,row.names=F,quote=F,append=T)
}


####################################################################################
## Format .txt into .pop
## folder  = working directory
## popFile = csv file contains genotype, the 1st 3 column = chr, gene name, cM
## haploid = logical, if TRUE genotype in haploid form, else in diploid form
####################################################################################
csv.to.pop=function(wFolder,popFile,popName,haploid=T)
{
 setwd(wFolder)
 popData=read.table(popFile,header=T,row.names=NULL,stringsAsFactors=F,sep=",")
 popData=popData[,-c(1:3)]
 popData=t(popData)

 data=cbind(c(1:nrow(popData)),popData)
 fill=matrix("",nrow(data),ncol(data))
 fill[,1]=c(1:nrow(data))
 dimnames(fill)[[2]]=dimnames(data)[[2]]

 if(haploid==TRUE)
  pop.data=rbind(data,data,fill) else
   pop.data=rbind(data[seq(1,ncol(data),2)],data[seq(2,ncol(data),2)],fill) 

 pop.data=pop.data[order(pop.data[,1]),]
 pop.data=pop.data[,-1]
 write.InitPop(wFolder,pop.data,popName)
 print(paste(".pop file are created at", wFolder))
}



