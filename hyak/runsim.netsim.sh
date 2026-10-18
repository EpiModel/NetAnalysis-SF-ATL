#!/bin/bash

## Job Name
#SBATCH --job-name=NetSim

## Nodes
#SBATCH --nodes=1

## Tasks per node
#SBATCH --ntasks-per-node=28

## Walltime
#SBATCH --time=6:00:00

## Memory per node
#SBATCH --mem=100G

source ~/loadR.sh
Rscript sim.R

Rscript 05.netsim.R
