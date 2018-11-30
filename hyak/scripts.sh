#!/bin/bash

## network simulation
sbatch -p csde -A csde --export=CITY=A runsim.netsim.sh
sbatch -p csde -A csde --export=CITY=S runsim.netsim.sh
