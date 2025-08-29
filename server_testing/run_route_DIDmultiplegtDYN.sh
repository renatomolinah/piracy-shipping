#!/bin/bash
#BSUB -J run_route_DIDmultiplegtDYN
#BSUB -P gctpms2
#BSUB -o %J.out
#BSUB -e %J.err
#BSUB -W 48:00
#BSUB -q gpu_h100
#BSUB -n 96
#BSUB -R "rusage[mem=600000]"
#BSUB -B
#BSUB -u jxv893@miami.edu
#BSUB -N
#
source ~/.bashrc
module load mambaforge
mamba activate R_Home

Rscript r/event_port_port.R
