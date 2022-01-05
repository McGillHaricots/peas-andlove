To perform genomic selection, there are a couple of steps to follow
1) Create a population with LD using simuPOP. You can go to the folder titled "SimuPOP" for instructions and code
2) For the Excel template to generate the qug file, put QTL in the LocusEffects sheet and the markers in the MarkerEffects sheet
3) Run QuLinePlus to create a training population 
4) For the qmp file, put the following:

!*************General information for the simulation experiment****************
!See below for notes
!NumStr NumRun NumCyc NumCro CBUpdate OutGES OutPOP OutHIS OutROG OutCOE OutVar Cross   RMtimes PopSize CSV QTL NumPopulation
1       1      1      30    0        1      1      0      0      0      1       random  0       0       1   0   1

!***********Information for selection strategies to be simulated***************
!StrategyNumber StrategyName  NumGenerations
1               TRAIN         1

!NR SS  GT        PT          GA        RP PS    NL  ET...         Row 1
!                                                AT (ID SM SP)...  Row 2
!                                                WT (ID SM SP)...  Row 3
1 0    CB clone bulk 1 1 1 1
                0
                0
1 0    F1 singlecross bulk 1 1 1 1
                0
                0


5) After running QuLinePlus, you will need the following files:
    - "ztrain_001_001_001_000.pop"
    - "ztrain.pou"
    - Excel template file for the qug

6) Run the code provided in this folder in numeric order
