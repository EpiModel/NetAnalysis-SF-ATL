#!/bin/bash

## Job Name
#SBATCH --job-name=TSNA

## Nodes
#SBATCH --nodes=1

## Tasks per node
#SBATCH --ntasks-per-node=16

## Walltime
#SBATCH --time=4:00:00

## E-mail notification
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=sjennes@emory.edu

## Memory per node
#SBATCH --mem=58G

## Specify the working directory
#SBATCH --workdir=/suppscr/csde/sjenness/netparam

. /suppscr/csde/sjenness/spack/share/spack/setup-env.sh
module load gcc-8.1.0-gcc-4.4.7-eaajvcy
module load r-3.5.1-gcc-8.1.0-unb32sy

Rscript 06.tsna.R
