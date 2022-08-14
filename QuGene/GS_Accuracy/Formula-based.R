setwd("/Volumes/Seagate drive/NEW/cycle-update-20run")

neff <- read.table(file="Neff.csv", header=TRUE, sep=",")
neff$h2 <- rep( c("0.9", "0.6", "0.3"), each = 4200/3 )
neff$h2 <- as.numeric(neff$h2)

### FORMULA FOR GS ACCURACY ESTIMATE

# rggG = sqrt( (Np*h2) / (Np*h2 + Me) )

# rggG = accuracy of genomic selection
# Np = number of individuals in the training population 
# h2 = heritability 
# Me = 2NeL/log(4NeL)

# L = genome length in Morgans
# Ne = effective population size 

# NOTE: THE COMMON BEAN LINKAGE MAP IS 2,041 cM, WHICH IS 20.41 M; THEREFORE, L=20.41

### THE FINAL FORMULA IS: rggG = sqrt( (Np*h2) / (Np*h2 + 40.82*Ne/log(81.64*Ne) ) )


neff$rggG <- sqrt( (30*neff$h2) / (30*neff$h2 + 40.82*neff$Neff/log(81.64*neff$Neff) ) )

ne.sum <- as.data.frame(neff %>% group_by(Trait, Strategy, Cycle) %>% 
                    dplyr::summarize(mean= mean(rggG), sd= sd(rggG), n=n(), SE= sd(rggG)/sqrt(n())) %>%
                    filter(Strategy %in% c(2,4,6,8,10)))   

ne.sum$Trait <- as.factor(ne.sum$Trait)
ne.sum$Strategy <- as.factor(ne.sum$Strategy)

Strategytitle <- c("Mass selection", "Bulk breeding", "Single seed descent", "Pedigree method", "Modified pedigree method")
colours = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

                       
myplot <- ggplot(data=ne.sum,aes(x=Cycle, y=mean, group=Strategy)) + 
     facet_grid(~ factor(Trait, levels=c("DF","WM","SY")),
                scales = "free") +
     geom_line(aes(color = Strategy), size=0.4) + 
     geom_point(aes(color = Strategy), size=0.8) + 
     ylab("Accuracy") +
     scale_x_continuous(breaks=c(1:7), limits = c(0.7,7.3)) +     
     scale_y_continuous(limits = c(0,0.7)) +   
     scale_colour_manual(name = "Strategy",
                         values = colours,
                         labels = Strategytitle) +
     theme(legend.position="top")                                             
                         
                         
 
#setwd("/Volumes/Seagate drive/NEW")                         
#ggsave(file="gs-estimate-model-update.png", plot=myplot, width=8, height=4)  

