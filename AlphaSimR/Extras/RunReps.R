nReps = 5

for(rep in 1:nReps){
  # pull in desired script # 
  source("rrBLUP_random.R")
  output = list(F1, F2, F3, F4, F5, PYT, AYT, Variety, gv, cor)
  write.csv(output, "rrBLUP_random_SR_yield.csv")
  }
