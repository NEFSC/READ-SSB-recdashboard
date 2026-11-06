#!/usr/bin/env bash
set -euo pipefail

# Script: run_pull_NAA.sh
# Purpose: Execute pull_NAA.R bypassing renv initialization (.Rprofile)
# Usage: open a terminal in the root of the repository.
#   type: ./stored_scripts/run_pull_NAA.sh 
# if you would like to skip groundfish or fluke, you can 
# set the appropriate value to 9999
# You might have issues with google drive tokens

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"


Rscript --vanilla "${SCRIPT_DIR}/pull_NAA.R" \
  management_cycle_groundfish=2027 \
  management_cycle_fluke=2026
  
# Get the 2026 groundfish data and skip fluke:   
#  Rscript --vanilla "${SCRIPT_DIR}/pull_NAA.R" \
#  management_cycle_groundfish=2026 \
#  management_cycle_fluke=9999