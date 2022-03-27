from simuOpt import setOptions
setOptions(alleleType='binary')
import simuPOP as sim

## set population parameters, size indicates 3 subpops, two infoFields, one for migration and one for selection ##

pop = sim.Population(size=[333]*3, ploidy=2, loci=1052, infoFields=('migrate_to', 'fitness'))

import random
sim.initGenotype(pop, freq=lambda: random.random())

##evolve the population ##

pop.evolve(
    initOps= sim.InitSex(),
    preOps=[
        sim.MapSelector(loci=0, fitness={(0,0):1, (0,1):0.97, (1,1):0.97}), ##selection##
        sim.Migrator(rate=[[0,0.1,0.1],[0,0,0.1],[0,0.1,0]]) ##migration##
        ],
    matingScheme=sim.SelfMating(ops=[sim.Recombinator(rates=0.1),sim.SelfingGenoTransmitter()]), ##selfing with recombination##
    postOps=[
        sim.Stat(alleleFreq=0, step=10), ##selection##
        sim.PyEval("'Gen:%3d' % gen", reps=0, step=10), ##selection##
        sim.PyEval(r"'%.3f\t' % alleleFreq[0][1]", step=10), ##selection###
        sim.PyOutput('\n', reps=-1, step=10), ##selection##
        sim.Stat(popSize=True), ##migration##
        sim.PyEval('subPopSize'),##migration##
        sim.PyOutput('\n') ##migration##
    ],
    gen=100
)
### Save the file so it can be processed later (see the file called "Processing-simuPOP-population.r")

from simuPOP.utils import saveCSV
saveCSV(pop, filename='stratSRNM.csv', affectionFormatter={True: 1, False: 2},
    genoFormatter=lambda geno: (geno[0] + 1, geno[1] + 1), sep=',')
