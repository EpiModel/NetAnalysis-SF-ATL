
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

sbatch -p csde -A csde --export=CITY=A runsim.sh
sbatch -p csde -A csde --export=CITY=S runsim.sh
