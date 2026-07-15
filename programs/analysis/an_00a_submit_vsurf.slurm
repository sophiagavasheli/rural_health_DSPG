#!/bin/bash

# script to run RF models with VSURF
# this job can be submitted in the rural_health_DSPG repo, no data copying required

#SBATCH --job-name=vsurf_grf
#SBATCH --account=dspg_viz
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=128G
#SBATCH --time=24:00:00
#SBATCH --mail-user=sophiag23@vt.edu # enter desired email address for updates
#SBATCH --mail-type=BEGIN # get emailed when job begins
#SBATCH --mail-type=END   # get emailed when job ends
#SBATCH --mail-type=FAIL  # get emailed if job fails

set -euo pipefail

module load R-bundle-CRAN/2025.11-foss-2025b

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

cd "/home/sophiag23/rural_health_DSPG"

Rscript programs/analysis/an_00b_RF_models_VSURF.R
