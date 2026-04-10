# Purpose: Install mriptacklebox to a particular folder to facilitate having multiple versions if necessary 

################# Begin install script#################
# If you already have mriptacklebox installed, you can either uninstall it first or just comment 
# out the part that installs the main branch of mriptacklebox.
# But I don't know what happens if 
# you have development and main install and then just do library(mriptacklebox.) with no options.
# Because MRIPtacklebox is private, you will need a token/key registered. 
# If MRIPtacklebox becomes public, add auth_token=NULL

# Setup libraries
tacklebox_main_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_main_install")
tacklebox_dev_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_dev_install")


dir.create(file.path(tacklebox_main_lib ), showWarnings = FALSE)
dir.create(file.path(tacklebox_dev_lib ), showWarnings = FALSE)

#install main  branch of mriptacklebox
remotes::install_github("NEFSC/READ-PDB-mriptacklebox",lib=tacklebox_main_lib , dependencies = TRUE, force = TRUE)
#error in line above, asked thru comment on min-yang's commit in github about it


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

