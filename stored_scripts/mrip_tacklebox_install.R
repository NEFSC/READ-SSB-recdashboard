# Purpose: Install mriptacklebox to a particular folder to facilitate having multiple versions if necessary 

################# Begin install script#################
# If you already have mriptacklebox installed, you can either uninstall it first or just comment 
# out the part that installs the main branch of mriptacklebox.
# But I don't know what happens if 
# you have development and main install and then just do library(mriptacklebox.) with no options.
# Because MRIPtacklebox is private, you will need a token/key registered. 
# If MRIPtacklebox becomes public, add auth_token=NULL

install.packages("usethis")
install.packages("gitcreds")

usethis::create_github_token()
library("gitcreds")
gitcreds_list_helpers()
gitcreds::gitcreds_set()
# if Rstudio is making you enter a username and PAT and then not letting you push, 
# enter this in the terminal and then enter user and PAT: 
#git config --global credential.helper store

# Setup libraries
tacklebox_main_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_main_install")
tacklebox_dev_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_dev_install")


dir.create(file.path(tacklebox_main_lib ), showWarnings = FALSE)
dir.create(file.path(tacklebox_dev_lib ), showWarnings = FALSE)

#install main  branch of mriptacklebox
remotes::install_github("NEFSC/READ-PDB-mriptacklebox")

remotes::install_github("NEFSC/READ-PDB-mriptacklebox", lib=tacklebox_main_lib, dependencies = TRUE, force = TRUE)
#error in line above, asked thru comment on min-yang's commit in github about it
remove.packages("mriptacklebox")

# Not run
# sample code to in stall a install development branch of mriptacklebox
# remotes::install_github("NEFSC/READ-PDB-mriptacklebox", ref = "dev", lib=tacklebox_dev_lib upgrade="never",dependencies = TRUE, force = TRUE).
#################END install script#################




# Begin code fragment 
# put this in your actual code.

#########################################################
#load the main version of MRIP Tacklebox
#tacklebox_main_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_main_install")
#library(mriptacklebox,lib.loc = tacklebox_main_lib)
#########################################################

#########################################################
#load the development version of MRIP Tacklebox
#tacklebox_dev_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_dev_install")
#library(mriptacklebox,lib.loc = tacklebox_dev_lib )
#########################################################

# 
# copilot explain:
#   This R script installs and manages the MRIPtacklebox package in separate library 
#locations, allowing users to maintain multiple versions (main and development) simultaneously.
# 
# Key Components:
#   Purpose: Facilitate parallel installations of different versions of the 
#MRIPtacklebox package without conflicts.
# 
# Installation Section (lines 11-24):
#   
#   Creates two separate library directories:
#   MRIPtacklebox_main_install — for the stable main branch
# MRIPtacklebox_dev_install — for the development branch (optional)
# Uses remotes::install_github() to install from the private NEFSC GitHub repository
# Requires authentication (token/key) since the repository is private
# The dev branch installation is commented out by default
# Usage Section (lines 30-43):
#   
#   Provides code snippets to load either version into your R environment using 
#library() with the lib.loc parameter
# Both sections are commented out—you uncomment the one you want to use
# Why This Approach?
#   Rather than having just one global installation, this method lets you:
#   
#   Keep the stable main version for production work
# Test development versions without affecting your main installation
# Easily switch between versions by commenting/uncommenting the relevant library() call
# Note: The authentication requirement suggests this was written before the repository became public; 
# the comment on line 9 indicates this might change if MRIPtacklebox becomes open-source.
# 
