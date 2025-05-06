#!/bin/bash
#SBATCH --account=def-haricots
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=40
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=18:00:00
#SBATCH --mail-user=henry.cordoba@mail.mcgill.ca
#SBATCH --mail-type=ALL

# Only load appropriate modules

module load StdEnv/2023 paml/4.10.7

### bash file
## Configure inputs

PathtoRepo="/home/henry96/projects/def-haricots/home_Cornell/legumesMSA/MSA/PAML"
InputFasta=$1
treeFile=$2
geneModel=$3

mkdir -p pamlRUN_null/$geneModel"_dir"
cp $InputFasta pamlRUN_null/$geneModel"_dir"/$geneModel
cp $treeFile pamlRUN_null/$geneModel"_dir"/RAxML_bestTree.$geneModel
cd pamlRUN_null/$geneModel"_dir"

#Run Naive model with whole tree
echo "Running baseml on $geneModel"
sed 's/GENE/'$geneModel'/g' $PathtoRepo/codeml_master_null.ctl > codeml.ctl
echo -ne '\n\n\n' | codeml
mv $geneModel.txt /scratch/henry96/dN_dS_null_andean

cd ..
rm -r $geneModel"_dir"
