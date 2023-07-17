# read and recode data

data = read_xlsx("PolymorphicSNPs2.xlsx")

data[data=="AA"] <- "0"
data[data=="BB"] <- "2"
data[data=="AB"] <- NA #remove hets
data[data=="--"] <- NA #remove no calls
data=data[,-2]
data=na.omit(data)

# define parents
C11223 = t(data[,2])
C11269 = t(data[,3])
C11212 = t(data[,4])
C14504 = t(data[,5])
C17031 = t(data[,6])

snpNames = t(data[,1])

# combine parents for each cross
C1 = as.data.frame(rbind(snpNames,C11223,C11269))
C3 = as.data.frame(rbind(snpNames,C14504,C11269))
C2 = as.data.frame(rbind(snpNames,C11212,C11269))
C10 = as.data.frame(rbind(snpNames,C11212,C11223))
C5 = as.data.frame(rbind(snpNames,C17031,C11223))
C6 = as.data.frame(rbind(snpNames,C17031,C11269))
C7 = as.data.frame(rbind(snpNames,C17031,C14504))
C8 = as.data.frame(rbind(snpNames,C11212,C14504))
C9 = as.data.frame(rbind(snpNames,C17031,C11212))
C4 = as.data.frame(rbind(snpNames,C11223,C14504))

#remove SNPs where alleles are the same for both parents

C1unique = C1 %>%  select(where(~ n_distinct(.) > 2))
C2unique = C2 %>%  select(where(~ n_distinct(.) > 2))
C3unique = C3 %>%  select(where(~ n_distinct(.) > 2))
C4unique = C4 %>%  select(where(~ n_distinct(.) > 2))
C5unique = C5 %>%  select(where(~ n_distinct(.) > 2))
C6unique = C6 %>%  select(where(~ n_distinct(.) > 2))
C7unique = C7 %>%  select(where(~ n_distinct(.) > 2))
C8unique = C8 %>%  select(where(~ n_distinct(.) > 2))
C9unique = C9 %>%  select(where(~ n_distinct(.) > 2))
C10unique = C10 %>%  select(where(~ n_distinct(.) > 2))

#first row is a list of SNP names - remove them for comparision

C1snps = t(as.data.frame(C1unique[1,]))
C2snps = t(as.data.frame(C2unique[1,]))
C3snps = t(as.data.frame(C3unique[1,]))
C4snps = t(as.data.frame(C4unique[1,]))
C5snps = t(as.data.frame(C5unique[1,]))
C6snps = t(as.data.frame(C6unique[1,]))
C7snps = t(as.data.frame(C7unique[1,]))
C8snps = t(as.data.frame(C8unique[1,]))
C9snps = t(as.data.frame(C9unique[1,]))
C10snps = t(as.data.frame(C10unique[1,]))

#must add colnames for intersect function
colnames(C1snps) = "snps"
colnames(C2snps) = "snps"
colnames(C3snps) = "snps"
colnames(C4snps) = "snps"
colnames(C5snps) = "snps"
colnames(C6snps) = "snps"
colnames(C7snps) = "snps"
colnames(C8snps) = "snps"
colnames(C9snps) = "snps"
colnames(C10snps) = "snps"

#create a group with the intersection of whichever cross you want
Group1 = Reduce(intersect, list(C10snps,C4snps))
