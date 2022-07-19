
##log in to node##
ssh -Y ich@graham.computecanada.ca

##install any R packages on the login node
module load gcc/9.3.0 r/4.1.2
R
install.packages("package")
quit()
##may take up to an hour to load package##

##after quitting R, change to the correct directory##
cd correctdirectory

##request time and memory for interactive job##
salloc --time=3:0:0 --mem=300G --account=def-haricots

##once nodes are granted, load R##
module load gcc/9.3.0 r/4.1.2
R

####you are now in an interactive R session, start running code###
