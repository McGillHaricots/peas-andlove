
# set your working directory 
setwd("/Volumes/Seagate drive/NEW/100parents/50run-real/original files")

### load your libraries
library("data.table")
library("dplyr")

### PLEASE NOTE: THE RAW POU FILES ARE MASSIVE (THINK 25GB), SO THIS CODE WILL READ AND FILTER PARTS OF THE FILE AND THEM COMBINE THEM
### IN ADDITION, THIS IS FOR 100 PARENTS, WHICH IS THE LARGEST, MEANING SOME OF THIS CODE MAY NEED TO BE MODIFIED FOR YOUR PURPOSES

#############################
#MODIFYING POU
############################


### Filter the POU files and write csv files
### DAYS TO FLOWERING

#trait = "DF"
trait = "WM"
#trait = "SY"

if (trait == "DF") {
	myselection <- c(2,3,4,6,13,14)
	}else if (trait == "WM"){
		myselection <- c(2,3,4,6,10,11)
		}else{
		    myselection <- c(2,3,4,6,7,8)
		    }

one <- fread(file=paste0("z",trait,"1.pou"), header=FALSE, skip=2, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))


two1 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=2, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two2 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=4000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two3 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=8000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two4 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=12000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two5 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=16000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two6 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=20000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two7 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=24000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two8 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=28000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two9 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=32000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two10 <- fread(file=paste0("z",trait,"2.pou"), header=FALSE, skip=36000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))

twofirst <- rbind(two1,two2,two3,two4,two5,two6,two7,two8,two9,two10)
twofirst$V2 <- twofirst$V2 + 2

two11 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=2, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two12 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=4000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two13 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=8000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two14 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=12000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two15 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=16000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two16 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=20000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two17 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=24000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two18 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=28000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two19 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=32000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
two20 <- fread(file=paste0("z",trait,"3.pou"), header=FALSE, skip=36000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))


twosecond <- rbind(two11,two12,two13,two14,two15,two16,two17,two18,two19,two20)
twosecond$V2 <- twosecond$V2 + 3


three1 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=2, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three2 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=4000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three3 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=8000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three4 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=12000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three5 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=16000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three6 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=20000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three7 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=24000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three8 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=28000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three9 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=32000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three10 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=36000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three11 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=40000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three12 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=44000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three13 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=48000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three14 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=52000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three15 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=56000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three16 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=60000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three17 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=64000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three18 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=68000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
three19 <- fread(file=paste0("z",trait,"4.pou"), header=FALSE, skip=72000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))


three <- rbind(three1,three2,three3,three4,three5,three6,three7,three8,three9,three10,three11,three12,three13,three14,three15,three16,three17,three18,three19)
three$V2 <- three$V2 + 4



four1 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=2, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four2 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=4000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four3 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=8000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four4 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=12000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four5 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=16000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four6 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=20000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four7 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=24000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four8 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=28000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four9 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=32000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four10 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=36000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four11 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=40000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four12 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=44000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four13 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=48000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four14 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=52000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four15 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=56000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four16 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=60000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four17 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=64000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four18 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=68000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
four19 <- fread(file=paste0("z",trait,"5.pou"), header=FALSE, skip=72000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))

four <- rbind(four1,four2,four3,four4,four5,four6,four7,four8,four9,four10,four11,four12,four13,four14,four15,four16,four17,four18,four19)

four$V2 <- four$V2 + 6


five1 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=2, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five2 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=4000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five3 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=8000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five4 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=12000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five5 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=16000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five6 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=20000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five7 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=24000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five8 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=28000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five9 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=32000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five10 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=36000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five11 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=40000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five12 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=44000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five13 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=48000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five14 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=52000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five15 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=56000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five16 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=60000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five17 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=64000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five18 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=68000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))
five19 <- fread(file=paste0("z",trait,"6.pou"), header=FALSE, skip=72000002, nrows=4000000) %>% filter(V5=="Field") %>% select(all_of(myselection))

five <- rbind(five1,five2,five3,five4,five5,five6,five7,five8,five9,five10,five11,five12,five13,five14,five15,five16,five17,five18,five19)

five$V2 <- five$V2 + 8

all <- rbind(one, twofirst, twosecond, three, four, five)
colnames(all) <- c("Strategy","Run","Cycle","Individual","Genotypic","Phenotypic")
write.csv(all, file=paste0("pou-",trait),row.names=FALSE)

