##########################################################
#                                                        #
# 2022/05                                                #
#                                                        #
# Data Analysis - Robustness Checks                      #
#                                                        #
##########################################################

# The estimations performed here rely on the functions defined 
# in the functions.R script. 

here::i_am("scripts/analysis_robustness.R")

## read in data frames & manipulate
#source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 

i_am("GitHub/lse-msc-thesis/scripts/analysis_regressions2.R")
load(here("SOEP/msc-thesis/dfmain.Rda"))
load(here("SOEP/msc-thesis/df1.Rda"))  
here::i_am("scripts/analysis_robustness.R") 



### Spillover / Validity of first cohort checks

states2011 <- c("BY", "NI")
states2012 <- c("BW", "BE", "BB", "HB")
states2013 <- c("NW")
dft1_2011 <- dft1[dft1$state %in% states2011,]
dft1_2012 <- dft1[dft1$state %in% states2012,]
dft1_2013 <- dft1[dft1$state %in% states2013,]

# parallel_trends has inputs centre, lags, leads, from which one can define which grad_cohorts to include
parallel_trends(data = dft1_2011, variable = "linc", ylab = "Log monthly income", scale = 9, 
                centre = 2011, lags = 0, leads = 2)
ggsave(here::here("plots/figure_parallel_lead_2011.pdf"), width = 14.2, height = 13.4, units = "cm")

parallel_trends(data = dft1_2012, variable = "linc", ylab = "Log monthly income", scale = 9, 
                centre = 2012, lags = 0, leads = 2)
ggsave(here::here("plots/figure_parallel_lead_2012.pdf"), width = 14.2, height = 13.4, units = "cm")

parallel_trends(data = dft1_2013, variable = "linc", ylab = "Log monthly income", scale = 9, 
                centre = 2013, lags = 0, leads = 2)
ggsave(here::here("plots/figure_parallel_lead_2013.pdf"), width = 14.2, height = 13.4, units = "cm")






