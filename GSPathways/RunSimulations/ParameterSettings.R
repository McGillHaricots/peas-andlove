# Defines every available parameter of the simulation.

# Create directory name based on date and time
# Example: sim_2023Jun20_104607
getDirName <- function(){
  date <- format(Sys.time(), "%Y%b%d_%X")
  date <- gsub(':','', date )
  dirName <- paste("sim", date, sep="_")

  # Adds trailing '_' if directory already exists (highly unlikely)
  while(file.exists(dirName))
    dirName <- paste(dirName, "_", sep="")
  dirName
}

sim_params <- list(
    model = list("-m", "--model",
        choices=c("NoModel","rrblup_random","rrblup_stratifiedclusters", "rf_random","rf_stratifiedclusters", "svm_random","svm_stratifiedclusters", "ann_random", "ann_stratifiedclusters"),
        type="character", 
        default="rrblup_random",
        help="Model to be trained"),

    nCycles = list("-nc", "--nCycles", 
        type="integer", 
        default=3,
        help="Number of breeding cycles"),

    nReps = list("-nr", "--nReps", 
        type="integer", 
        default=10,
        help="Number of replicas"),

    trainGen = list("-tg", "--trainGen",
        choices=c("F2", "F3", "F4", "F5"),
        type="character", 
        default="F2",
        help="Generation to train the model each cycle"),

    trainingData = list("-td", "--trainingData",
        choices = c("F2","F5","F2_and_F5","ALL"),
        type = "character",
        default = "F2",
        help="Which generational data will be used to train the model"),
  
  parentSelections = list("-ps", "--parentSelections",
        choices=c("F2","F3","F4","F5","PYT"),
        type = "character",
        default = "F2",
        help = "Generation select parents for the next cycle")
)

run_params <- list(
    nCores = list("-nC", "--nCores",
        type="integer", 
        default=1,
        help="Number of cores to run the simulation"),

    outputDir = list("-od", "--outputDir",
        default=getDirName(),
        type="character",
        help="Directory to write simulation outputs"))

ignored_params <- list(
    noInteraction = list("-ni", "--noInteraction",
        default=FALSE,
        action="store_true",
        help="Deactivate initial command line interaction"
    )
)

parameters <- c(sim_params, run_params, ignored_params)
