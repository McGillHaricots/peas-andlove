#!/bin/bash
#SBATCH --account=def-haricots
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=200G
#SBATCH --time=15:00:00
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
    ./tassel-5-standalone/run_pipeline.pl -Xmx500G -debug -configParameters /mnt/phg/config_aa.txt -HaplotypeGraphBuilderPlugin -configFile /mnt/phg/config_aa.txt -methods reads_aligned -endPlugin \
        -SAMToMappingPlugin -updateDB false -endPlugin
