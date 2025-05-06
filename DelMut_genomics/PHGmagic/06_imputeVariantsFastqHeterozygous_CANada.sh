#!/bin/bash
#SBATCH --account=def-haricots
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=250G
#SBATCH --time=12:00
#SBATCH --mail-user=henry.cordoba@mail.mcgill.ca
#SBATCH --mail-type=ALL



# load modules 

module load StdEnv/2020 apptainer/1.1.3 java perl 
export _JAVA_OPTIONS=-Xmx50g # Setting this was very important

# define some directories

WORKING_DIR=/home/henry96/projects/def-haricots/cbsublfs1_Cornell/run_phg
DOCKER_CONFIG_FILE=/home/henry96/projects/def-haricots/cbsublfs1_Cornell/run_phg/phg/config.txt


# Run PHG docker container once downloaded from apptainer

## SAM to map=Here the mapping files are created, but the DB is not updated, so we can do it in parallel

apptainer exec --no-home --bind /home/henry96/projects/def-haricots/cbsublfs1_Cornell/run_phg/phg:/mnt/phg,/scratch/henry96/MAGIC_Rils_BAM:/mnt/MAGIC_Rils_BAM phg_1.4.sif \
    ./tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters /mnt/phg/config.txt -HaplotypeGraphBuilderPlugin -configFile /mnt/phg/config.txt -methods reads_aligned -endPlugin \
	-SAMToMappingPlugin -updateDB false -endPlugin 1> MAGIC_Lines_PHG.log 2> MAGIC_Lines_PHG.err



## Here we update the DB with the ImportReadMapping plugin. After, we change a few files: CHECK must now be the file that has all the data, make sure the pathFile and HapId file are consistent
 
apptainer exec --no-home --bind /home/henry96/projects/def-haricots/cbsublfs1_Cornell/run_phg/phg:/mnt/phg,/scratch/henry96/MAGIC_Rils_BAM:/mnt/MAGIC_Rils_BAM phg_1.4.sif \
    ./tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters /mnt/phg/config.txt -HaplotypeGraphBuilderPlugin -configFile /mnt/phg/config.txt -methods reads_aligned -endPlugin \
	-ImportReadMappingToDBPlugin -configFileForFinalDB /mnt/phg/config.txt -loadFromDB false -inputMappingDir /mnt/phg/debugDir -readMappingMethod MAGIC_Lines \
	-outputKeyFile /mnt/phg/CHECK -endPlugin


## Once the DB is updated, we find the paths using "diplodPath" as target. This are written in the DB

apptainer exec --no-home --bind /home/henry96/projects/def-haricots/cbsublfs1_Cornell/run_phg/phg:/mnt/phg,/scratch/henry96/MAGIC_Rils_BAM:/mnt/MAGIC_Rils_BAM phg_1.4.sif \
    ./tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters /mnt/phg/config.txt -ImputePipelinePlugin -imputeTarget diploidPath -endPlugin 1>diploidPath.log 2>diploidPath.err

## Since these are RILs, they are supposed to be "homozygous" so the proper path finding is "haplotype"

apptainer exec --no-home --bind /home/henry96/projects/def-haricots/cbsublfs1_Cornell/run_phg/phg:/mnt/phg,/scratch/henry96/MAGIC_Rils_BAM:/mnt/MAGIC_Rils_BAM phg_1.4.sif \
    ./tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters /mnt/phg/config.txt -ImputePipelinePlugin -imputeTarget path -endPlugin 1>path.log 2>path.err


## The final step is getting the VCF files from the paths

apptainer exec --no-home --bind /home/henry96/projects/def-haricots/cbsublfs1_Cornell/run_phg/phg:/mnt/phg,/scratch/henry96/MAGIC_Rils_BAM:/mnt/MAGIC_Rils_BAM phg_1.4.sif \
	./tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters /mnt/phg/config.txt -ImputePipelinePlugin -imputeTarget pathToVCF -endPlugin

