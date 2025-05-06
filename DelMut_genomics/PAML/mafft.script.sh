#!/bin/bash
export PATH=/programs/mafft/bin:$PATH
/programs/parallel/bin/parallel -j 15 "/programs/mafft/bin/mafft --ep 0 --genafpair --maxiterate 1000 --adjustdirection {} > /home/hac89/legumesMSA/MSA/MAFFT/MSAs_per_only_transcript/{/.}.fa" :::: /home/hac89/legumesMSA/MSA/MAFFT/only_transcripts_msa_v2.list

/programs/parallel/bin/parallel -j 15 "/programs/mafft/bin/mafft --ep 0 --genafpair --maxiterate 1000 --adjustdirection {} > /home/hac89/legumesMSA/MSA/MAFFT/MSAs_per_ext_transcript/{/.}.fa" :::: /home/hac89/legumesMSA/MSA/MAFFT/ext_transcripts_msa_v2.list
