
# QU-GENE
## Creating a .qug file
1. Open the excel file 00_qugTemplate2.xlsx
1. Edit the sheet using information gathered from the literature 
    * For map, locus effects, and marker effects:
        * The map contains all markers and QTLs
        * The locus effects only contains QTLs
        * The marker effects only contains markers 
1. Open the copy_paste file
1. Open the R console and change the working directory 
1. Run the code from the copy_past file in R. This should generate a .qug file 

## Creating a .qmp file
1. Download the template .qmp file 
1. Fill out information about the breeding strategy (e.g. initial population size, number of cycles, number of runs, selection methods, etc.)

## Running QU-GENE
1. Run the QU-GENE engine 
1. Select the .qug file to run 
1. If successful, the QU-GENE engine will generate a number of files 

## Running QuLinePlus
1. Create a .mio file, which should contain the following:
      * .ges file
      * .pop file
      * .qmp file 
      * your new file name
1. Run QuLinePlus. This should automatically generate a number of files 
   
