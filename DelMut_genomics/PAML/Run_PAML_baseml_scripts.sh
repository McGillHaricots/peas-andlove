## Configure inputs

export PATH=/programs/paml-4.10.6/bin:$PATH

PathtoRepo="/workdir/hac89/PAML"
InputFasta=$1
treeFile=$2
geneModel=$3

mkdir -p pamlRUN/$geneModel"_dir"
cp $InputFasta pamlRUN/$geneModel"_dir"
cp $treeFile pamlRUN/$geneModel"_dir"
cd pamlRUN/$geneModel"_dir"

#Run Naive model with whole tree
echo "Running baseml on $geneModel"
sed 's/GENE/'$geneModel'/g' $PathtoRepo/baseml_master.ctl > baseml.ctl
echo -ne '\n\n\n' | baseml
cp rates /workdir/hac89/PAML/results/$geneModel"_rates"
