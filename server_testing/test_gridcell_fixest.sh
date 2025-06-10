#!/bin/bash
#BSUB -J test_gridcell_DIDmultiplegtDYN
#BSUB -P gctpms2
#BSUB -o %J.out
#BSUB -e %J.err
#BSUB -W 1:00
#BSUB -q gpu_h100
#BSUB -n 24
#BSUB -R "rusage[mem=16000]"
#BSUB -B
#BSUB -u jxv893@miami.edu
#BSUB -N
#
source ~/.bashrc
module load mambaforge
mamba activate R_Home

Rscript r/test.R
