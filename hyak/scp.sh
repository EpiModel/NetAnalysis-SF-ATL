#!/bin/bash

# network simulations
scp hyak/*netsim.* hyak:/suppscr/csde/sjenness/netparam
scp data/*.NetEst.* hyak:/suppscr/csde/sjenness/netparam/input


scp hyak:/suppscr/csde/sjenness/netparam/input/*.NetSim.* data/

# TSNA
scp hyak/*.tsna.* hyak:/suppscr/csde/sjenness/netparam

scp hyak/*.tsna.* mox:/gscratch/csde/sjenness/netparam
scp data/*.NetSim.* mox:/gscratch/csde/sjenness/netparam/input

# done
sbatch -p csde -A csde --array=1-100 --export=ALL,CITY=atl,NET=main,INT=4 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-100 --export=ALL,CITY=atl,NET=casl,INT=4 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-100 --export=ALL,CITY=atl,NET=inst,INT=4 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-100 --export=ALL,CITY=atl,NET=all,INT=4 runsim.tsna.sh


# done
sbatch -p ckpt -A csde-ckpt --array=1-100 --export=ALL,CITY=sfo,NET=main,INT=4 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-100 --export=ALL,CITY=sfo,NET=casl,INT=4 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-100 --export=ALL,CITY=sfo,NET=inst,INT=4 runsim.tsna.sh


# Running on Mox
sbatch -p csde -A csde --array=1 --export=ALL,CITY=sfo,NET=all,INT=4 runsim.tsna.mox.sh
sbatch -p csde -A csde --array=2-49 --export=ALL,CITY=sfo,NET=all,INT=4 runsim.tsna.mox.sh
sbatch -p ckpt -A csde-ckpt --array=20-49 --export=ALL,CITY=sfo,NET=all,INT=4 runsim.tsna.mox.sh
sbatch -p ckpt -A csde-ckpt --array=50-100 --export=ALL,CITY=sfo,NET=all,INT=4 runsim.tsna.mox.sh



scp hyak:/suppscr/csde/sjenness/netparam/output/atl.*.rda data/

