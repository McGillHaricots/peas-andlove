nReps = 5

for(rep in 1:nReps){
  # pull in desired script # 
  source("rrBLUP_random.R")
  write.csv(F1, "rrblup_random_sr_yieldF1.csv")
  write.csv(F2, "rrblup_random_sr_yieldF2.csv")
  write.csv(F3, "rrblup_random_sr_yieldF3.csv")
  write.csv(F4, "rrblup_random_sr_yieldF4.csv")
  write.csv(F5, "rrblup_random_sr_yieldF5.csv")
  write.csv(F6, "rrblup_random_sr_yieldF6.csv")
  write.csv(PYT, "rrblup_random_sr_yieldPYT.csv")
  write.csv(AYT, "rrblup_random_sr_yieldAYT.csv")
  write.csv(Variety, "rrblup_random_sr_yieldVariety.csv")
  write.csv(gv, "rrblup_random_sr_yieldgv.csv")
  write.csv(cor, "rrblup_random_sr_yieldcor.csv")
  }
