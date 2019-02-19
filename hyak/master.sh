#!/bin/bash

## network simulation
sbatch -p csde -A csde --export=ALL,CITY=S runsim.netsim.sh
sbatch -p csde -A csde --export=ALL,CITY=A runsim.netsim.sh
