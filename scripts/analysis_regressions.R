##########################################################
#                                                        #
# 2022/05                                                #
#                                                        #
# Data Analysis - Regressions                            #
#                                                        #
##########################################################

# The estimations performed here rely on the functions defined 
# in the functions.R script. 

here::i_am("scripts/analysis_regressions.R")

## read in data frames & manipulate
#source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 

i_am("GitHub/lse-msc-thesis/scripts/analysis_regressions.R")
load(here("SOEP/msc-thesis/dfmain.Rda"))
load(here("SOEP/msc-thesis/df1.Rda"))  
here::i_am("scripts/analysis_regressions.R") 


## sub data frames
dfmain_2011 <- dfmain[dfmain$reg == 2011,]
dfmain_2012 <- dfmain[dfmain$reg == 2012,]
dfmain_2013 <- dfmain[dfmain$reg == 2013,]


## cross sectional model

# continuous outcomes
cross_section_plot(variable = "linc", ylab = "Log monthly real income", scale = 4, class = "cont")
ggsave(here::here("plots/figure_cross_linc.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot(variable = "lifesat", ylab = "Life satisfaction, 0 to 10", scale = 5, class = "cont")
ggsave(here::here("plots/figure_cross_lifesat.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot(variable = "health", ylab = "Health self-assessment, 1 to 5", scale = 2, class = "cont")
ggsave(here::here("plots/figure_cross_health.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot(variable = "hours3", ylab = "Annual Hours Worked", scale = 2000, class = "cont")
ggsave(here::here("plots/figure_cross_hours.pdf"), width = 14.2, height = 13.4, units = "cm")


# binary outcomes
cross_section_plot(variable = "edu_cur", ylab = "Logit-Coefficient of Treatment Dummy", scale = 4, class = "binary")
ggsave(here::here("plots/figure_cross_edu.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot(variable = "empl", ylab = "Logit-Coefficient of Treatment Dummy", scale = 4, class = "binary")
ggsave(here::here("plots/figure_cross_empl.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot2(variable = "edu_cur", ylab = "Raw Difference in Share", scale = 50)
ggsave(here::here("plots/figure_cross_edu2.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot2(variable = "empl", ylab = "Raw Difference in Share", scale = 50)
ggsave(here::here("plots/figure_cross_empl2.pdf"), width = 14.2, height = 13.4, units = "cm")


## panel models 
list_df <- list(dfmain, dfmain_2011, dfmain_2012, dfmain_2013)

# continous
df_linc <- panel_table(list_df, variable = "linc", class = "cont", out = "df")
df_health <- panel_table(list_df, variable = "health", class = "cont", out = "df")
df_lifesat <- panel_table(list_df, variable = "lifesat", class = "cont", out = "df")
df_hours <- panel_table(list_df, variable = "hours3", class = "cont", out = "df")

df_cont <- cbind(cbind(df_linc, df_hours[, c(2,3)]), cbind(df_health[, c(2,3)], df_lifesat[, c(2,3)]))
stargazer(df_cont, type = "latex", summary = FALSE)

# binary
df_edu <- panel_table(list_df, variable = "edu_cur", class = "binary", out = "df")
df_empl <- panel_table(list_df, variable = "empl", class = "binary", out = "df")

df_binary <- cbind(df_edu, df_empl[, c(2,3)])
stargazer(df_binary, type = "latex", summary = FALSE)






