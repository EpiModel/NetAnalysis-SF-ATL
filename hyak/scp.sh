#!/bin/bash

# NetSim Send
scp hyak/*.* mox:/gscratch/csde/sjenness/netparam
scp data/*.NetEst.* mox:/gscratch/csde/sjenness/netparam/data

scp hyak/*.* hyak:/gscratch/csde/sjenness/netparam
scp data/*.NetSim.* hyak:/gscratch/csde/sjenness/netparam/data


# NetSim Receive

scp mox:/gscratch/csde/sjenness/netparam/data/*.NetSim.* data/





# TSNA
scp hyak/*.tsna.* hyak:/suppscr/csde/sjenness/netparam

scp hyak/*.tsna.* mox:/gscratch/csde/sjenness/netparam
# scp data/*.NetSim.* mox:/gscratch/csde/sjenness/netparam/input

# done
sbatch -p csde -A csde --array=1-400 --export=ALL,CITY=atl,NET=main,INT=1 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-400 --export=ALL,CITY=atl,NET=casl,INT=1 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-400 --export=ALL,CITY=atl,NET=inst,INT=1 runsim.tsna.sh
sbatch -p ckpt -A csde-ckpt --array=1-400 --export=ALL,CITY=atl,NET=all,INT=1 runsim.tsna.sh

# done
sbatch -p csde -A csde --array=1-400 --export=ALL,CITY=sfo,NET=main,INT=1 runsim.tsna.ikt.sh
sbatch -p ckpt -A csde-ckpt --array=1-400 --export=ALL,CITY=sfo,NET=casl,INT=1 runsim.tsna.ikt.sh
sbatch -p ckpt -A csde-ckpt --array=1-400 --export=ALL,CITY=sfo,NET=inst,INT=1 runsim.tsna.ikt.sh
sbatch -p ckpt -A csde-ckpt --array=1-400 --export=ALL,CITY=sfo,NET=all,INT=1 runsim.tsna.ikt.sh

scp mox:/gscratch/csde/sjenness/netparam/output/done1.zip data/

