##########################################################
#                                                        #
# 2022/04                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Setup                                                  #
#                                                        #
##########################################################

## This code contains initial settings and libraries

## read in libraries
library(readxl)
library(ggpubr)
library(stargazer)
library(ggthemes)
library(haven)

library(multiwayvcov)
library(lmtest) 
library(staggered) 
library(did2s)
library(bacondecomp)
library(lfe)
library(plm)
library(rsdmx)
library(survival)
library(sandwich)


# tidyverse
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

library(here) # read in last to prevent conflicts with plyr

# create renv environment to ease reproducability (read renv documentation if unclear)
#renv::init() # switched off after initial init()

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

## set working directory
i_am("scripts/setup.R")

## set default theme for ggplot2 
theme_set(theme_bw())

## load own functions
source(here::here("scripts/functions.R")) 

