#!/bin/bash

## Nodes
#SBATCH --nodes=1

## Tasks per node
#SBATCH --ntasks-per-node=16

## Walltime
#SBATCH --time=24:00:00

## Memory per node
#SBATCH --mem=55G

source ~/loadR.sh
Rscript 06.tsna.R
