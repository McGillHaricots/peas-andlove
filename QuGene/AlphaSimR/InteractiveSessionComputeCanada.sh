##log on to node##
ssh -Y ich@graham.computecanada.ca

##change directory to directory with necessaryfiles##
cd projects
cd def-haricots
cd ich

##start interactive session, 3 hours, 300G memory, def-haricots account##
salloc --time=3:0:0 --mem=300G --account=def-haricots

##load gcc and R##
module load gcc/9.3.0 r/4.1.2

##type R to launch R session##
R

##begin working##
