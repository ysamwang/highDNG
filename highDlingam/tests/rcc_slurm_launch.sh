#!/bin/bash

#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16


R CMD BATCH --no-save --no-restore highD_cluster.R
