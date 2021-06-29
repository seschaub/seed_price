################################################################################
# R code related to                                                            #
# "The costs of diversity: higher prices for more diverse grassland seed       #
# mixtures" by Sergei Schaub, Robert Finger, Vera Steiner, Nina Buchmann,      #
# Valentin H. Klaus                                                            #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub and Vera Steiner                                  #
# -----------------------------------------------------------------------------#
# master R script                                                              #
################################################################################


# table of contents:
# 0. settings
# 1. create folders and specify directories 
# 2. download data and call all r scripts


# !!! important remarks !!!:
# before running the R code you need to specify manually your main working directory, 
# i.e. 'cd_main' (you find it in section 0) , and create in the main directory the 
# folder 'r_code', in which you save all provided r scripts


################################################################################
# 0. settings
################################################################################

# clear global workspace (note that this only cleans the workspace and not other settings (https://www.tidyverse.org/blog/2017/12/workflow-vs-script/?s=09#whats-wrong-with-rmlist-ls)):
rm(list = ls()) 

# install.packages("renv")
# require(renv)
# renv::init() # initialize reproducible project

# install.packages("tidyverse","UpSetR","viridis","ggpubr","lmboot","scales","matrixStats","multiwayvcov","lmtest","scico","mgcv","fastDummies","margins")
require(tidyverse)
require(UpSetR)
require(viridis)
require(ggpubr)
require(lmboot)
require(scales)
require(matrixStats)
require(multiwayvcov)
require(lmtest)
require(scico)
require(fastDummies)
require(margins)
require(mgcv)

# renv::snapshot() # take snapshot of package version used
# renv::restore()  # restore package version originally used


################################################################################
# 1. create folders and specify directories 
################################################################################

# define main working directory 
cd_main <- "H:/Seed prices/R/Git/seed_price"        #### !!! needs to be specified manually !!!
setwd(cd_main) # set directory


# create additional folders and working directories 
## r code
cd_code <- paste0(cd_main,"/r_code")

## results
setwd(cd_main) # set directory
dir.create("results")
cd_results <- paste0(cd_main,"/results")


################################################################################
# 2. download data and call all r scripts
################################################################################

# download data directly from eth reseach collection
data <- read.csv("https://www.research-collection.ethz.ch/bitstream/handle/20.500.11850/485215/seed_price_data.csv?sequence=1&isAllowed=n")

# run r scripts
setwd(cd_results) # set directory
source("___.R")                 # xxx


