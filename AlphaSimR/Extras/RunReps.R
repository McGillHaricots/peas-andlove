nReps = 25

for(rep in 1:nReps){
  # pull in desired script # 
  source(rrBLUP_random.R")
  output = (list(F1, F2, F3, F4, F5, PYT, AYT, Variety, gv, cor)
  write.csv(output, "model_trn_poptype_trait.csv")
  }
