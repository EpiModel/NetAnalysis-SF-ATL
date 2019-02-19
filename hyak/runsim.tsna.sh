#!/bin/bash

## Nodes
#SBATCH --nodes=1

## Tasks per node
#SBATCH --ntasks-per-node=28

## Walltime
#SBATCH --time=12:00:00

## Memory per node
#SBATCH --mem=100G

source ~/loadR.sh
Rscript 06.tsna.R
