export PATH=/programs/RAxML-8.2.12:$PATH

raxmlHPC -m GTRGAMMA -p 96060 -s Phvul.001G000600.1.v2.1_msa.fa -# 20 -n Phvul.001G000600.1.v2.1_RAxML_Output

# In parallel for all the transcrips MSA

#!/bin/bash
export PATH=/programs/RAxML-8.2.12:$PATH

cd RAxML_cds

/programs/parallel/bin/parallel -j 75 "raxmlHPC -m GTRGAMMA -p 96060 -s {} -# 20 -n {/.}_RAxML_Output" \
:::: /workdir/hac89/MSAs_cds_forRAxML.list
