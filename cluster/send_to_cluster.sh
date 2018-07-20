#!/bin/bash

#create new folder for project
scp -r cluster uqtballa@tinaroo.rcc.uq.edu.au:~/
ssh uqtballa@tinaroo.rcc.uq.edu.au mv cluster recruitment

#copy sample files (assumes only files in the folder are starting values, no final samples)
scp data/derived/* uqtballa@tinaroo.rcc.uq.edu.au:~/recruitment/

#copy dmc
scp -r dmc uqtballa@tinaroo.rcc.uq.edu.au:~/recruitment/

#run job submission scripts
ssh uqtballa@tinaroo.rcc.uq.edu.au qsub -o ~/recruitment/ -e ~/recruitment/ recruitment/motion.pbs
ssh uqtballa@tinaroo.rcc.uq.edu.au qsub -o ~/recruitment/ -e ~/recruitment/ recruitment/brightness.pbs
