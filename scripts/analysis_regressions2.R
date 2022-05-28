##########################################################
#                                                        #
# 2022/04                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Analysis - Regressions                            #
#                                                        #
##########################################################

here::i_am("scripts/analysis_regressions2.R")

## read in data frames & manipulate
#source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 

# comp 1
i_am("GitHub/lse-msc-thesis/scripts/analysis_regressions2.R")
load(here("SOEP/msc-thesis/dfmain.Rda"))
load(here("SOEP/msc-thesis/df1.Rda"))  

# comp 2
i_am("GitHub/lse-msc-thesis/scripts/analysis_regressions2.R")
load(here("SOEP/dfmain.Rda"))

here::i_am("scripts/analysis_regressions2.R") 


## sub data frames
dfmain_2011 <- dfmain[dfmain$reg == 2011,]
dfmain_2012 <- dfmain[dfmain$reg == 2012,]
dfmain_2013 <- dfmain[dfmain$reg == 2013,]


## cross sectional model

# continuous outcomes
cross_section_plot(var = "linc", ylab = "Log monthly real income", scale = 4, class = "cont")
ggsave(here::here("plots/figure_cross_linc.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot(var = "lifesat", ylab = "Life satisfaction, 1 to 10", scale = 5, class = "cont")
ggsave(here::here("plots/figure_cross_lifesat.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot(var = "health", ylab = "Health self-assessment, 1 to 5", scale = 2, class = "cont")
ggsave(here::here("plots/figure_cross_health.pdf"), width = 14.2, height = 13.4, units = "cm")

# binary outcomes
cross_section_plot_binary2(var = "edu_cur", ylab = "Logit-Coefficient of Treatment Dummy", scale = 4, class = "binary")
ggsave(here::here("plots/figure_cross_edu.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot_binary2(var = "empl", ylab = "Logit-Coefficient of Treatment Dummy", scale = 4, class = "binary")
ggsave(here::here("plots/figure_cross_empl.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot2(var = "edu_cur", ylab = "Raw Difference in Share", scale = 50)
ggsave(here::here("plots/figure_cross_edu2.pdf"), width = 14.2, height = 13.4, units = "cm")

cross_section_plot2(var = "empl", ylab = "Raw Difference in Share", scale = 50)
ggsave(here::here("plots/figure_cross_empl2.pdf"), width = 14.2, height = 13.4, units = "cm")


## panel models 
# y = t x T + FE: PID, State, calendar year, year

# example variables: 
# model > coeff per year

list_df <- list(dfmain, dfmain_2011, dfmain_2012, dfmain_2013)

panel_table(list_df, "linc")


## testing serial correlation in panel
model <- plm(linc ~ t_1xyears + factor(syear) + factor(birthregion), effect = "twoways",
             data=dfmain, model="within", index = c("pid", "years_grad"))
summary(model)
pwartest(model)


table(index(dfmain), useNA = "ifany")



list_df <- list(dfmain, dfmain_2011, dfmain_2012, dfmain_2013)

df_all <- data.frame(Data = character(), Variable = character(), c1 = numeric(), c2 = numeric())

for(df in list_df){
  df <- df[df$years_grad < 8, ]
  fe_1 <- round(summary(felm(df[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, 
                        df))$coefficients[1,c(1,2)], 2)
  fe_2 <- round(summary(felm(df[, "linc"] ~ t_1xyears | years_grad + pid + syear + sampreg | 0 | pid, 
                        df))$coefficients[1,c(1,2)], 2)
  
  df1 <- data.frame(Data = c("Cohort = 2011", paste0("N = ", sum(df$one))), 
                    Variable = c("Short HS * Years", " "), 
                    c1 = c(as.numeric(fe_1[1]), paste0("(", paste0(as.numeric(fe_1[2]),")"))),
                    c1 = c(as.numeric(fe_2[1]), paste0("(", paste0(as.numeric(fe_2[2]),")"))))
  
  df_all <- rbind(df_all, df1)
}
names(df_all) <- c("Data", "Variable", "(1)", "(2)")

stargazer(df_all, type = "latex", summary = FALSE)





fe_11 <- summary(felm(dfmain[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, dfmain))$coefficients[1,c(1,2)]
fe_12 <- summary(felm(dfmain[, "linc"] ~ t_1xyears | years_grad + pid + syear + sampreg | 0 | pid, dfmain))$coefficients[1,c(1,2)]

fe_21 <- felm(dfmain_2011[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, dfmain_2011)
fe_22 <- felm(dfmain_2011[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, dfmain_2011)

fe_31 <- felm(dfmain_2012[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, dfmain_2012)
fe_32 <- felm(dfmain_2012[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, dfmain_2012)

fe_41 <- felm(dfmain_2013[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, dfmain_2013)
fe_42 <- felm(dfmain_2013[, "linc"] ~ t_1xyears | years_grad + pid | 0 | pid, dfmain_2013)





stargazer(fe_11, fe_12, fe_21, 
          type = "latex", omit.stat=c("all"),
          star.char = NULL, align=TRUE,digits=2)


stargazer(fe_11, fe_12, fe_21, fe_22, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),
          star.char = NULL, align=TRUE,digits=2)








