## Join output and error in a single file
#PBS -j oe
## Export the environment vaiables from your shell
#PBS -V

cd $PBS_O_WORKDIR
python3 timecontrol.py 2
python3 timecontrol.py 1
