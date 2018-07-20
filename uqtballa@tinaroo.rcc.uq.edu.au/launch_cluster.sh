#!/bin/bash

#create new folder for project
scp cluster uqtballa@tinaroo.rcc.uq.edu.au:~/recruitment

#copy sample files (assumes only files in the folder are starting values, no final samples)
scp data/derived/* uqtballa@tinaroo.rcc.uq.edu.au:~/recruitment/

#copy sampling files


scp clu/4a_sampling.R uqtballa@tinaroo.rcc.uq.edu.au:~/recruitment/
scp processing/4b_sampling.R uqtballa@tinaroo.rcc.uq.edu.au:~/recruitment/

#copy job submission scripts


#upload model files to cmdstan folder on cluster
scp  uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/

#upload data to new model folder
if [ $1 == "ap" ]; then 
	echo "launching model of approach condition!"
	#upload data file to cluster
    scp data/clean/obs_ap_rdump.R uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/model/
	fi

if [ $1 == "av" ]; then 
	echo "launching model of avoidance condition!"
	#upload data file to cluster
    scp data/clean/obs_av_rdump.R uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/model/
	fi	

#run job script on cluster
ssh uqtballa@tinaroo.rcc.uq.edu.au bash cmdstan/model/2_make_goal.sh $1


