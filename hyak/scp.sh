#!/bin/bash

scp *.[Rs]* hyak:/suppscr/csde/sjenness/netparam
scp data/*.* hyak:/suppscr/csde/sjenness/netparam/data
scp hyak:/suppscr/csde/sjenness/netparam/data/*.NetEst.*.rda data/
scp hyak:/suppscr/csde/sjenness/netparam/data/*.NetSim.*.rda data/

shell
gsr
cd netparam
lspack
module load gcc-8.1.0-gcc-4.4.7-eaajvcy
module load r-3.5.0-gcc-8.1.0-bcqjjkd
R

scp data/*.sim1.* hyak:/suppscr/csde/sjenness/netparam/data

scp *.TsnaHyak.R hyak:/suppscr/csde/sjenness/netparam
scp runsim.sh hyak:/suppscr/csde/sjenness/netparam

sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=main,INT=8 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=casl,INT=8 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=inst,INT=8 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=all,INT=8 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=main,INT=8 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=casl,INT=8 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=inst,INT=8 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=all,INT=8 runsim.sh

sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=main,INT=4 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=casl,INT=4 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=inst,INT=4 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=all,INT=4 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=main,INT=4 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=casl,INT=4 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=inst,INT=4 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=all,INT=4 runsim.sh

sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=main,INT=1 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=casl,INT=1 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=inst,INT=1 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=atl,NET=all,INT=1 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=main,INT=1 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=casl,INT=1 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=inst,INT=1 runsim.sh
sbatch -p csde -A csde --array=1-100 --export=CITY=sfo,NET=all,INT=1 runsim.sh

scp hyak:/suppscr/csde/sjenness/netparam/data/atl.*.rda tsna/
scp hyak:/suppscr/csde/sjenness/netparam/data/sfo.*.rda tsna/
