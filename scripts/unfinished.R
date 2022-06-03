##########################################################
#                                                        #
# 2022/05                                                #
#                                                        #
# Unfinished Projects                                    #
#                                                        #
##########################################################

here::i_am("scripts/unfinished.R")

## read in data frames & manipulate
#source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 

i_am("GitHub/lse-msc-thesis/scripts/unfinished.R")
load(here("SOEP/msc-thesis/dfmain.Rda"))
load(here("SOEP/msc-thesis/df1.Rda"))  
here::i_am("scripts/unfinished.R") 


## state / reg region robust standard errors for conditional logit

# conditional logit model imported from survival package
# package nests clogit in coxph model
# cluster argument with more than one argument does not work
# solution: 
# 1. write panel robust cluster function myself and implement it
# problem: not easily understandable to stargazer function
# 2. figure out how robust and cluster interact in coxph

summary(clogit(edu_cur ~ t_1xyears + factor(years_grad) + 
                 strata(pid) + factor(syear), method = "approximate", cluster = c(pid, reg), dfmain))

summary(clogit(edu_cur ~ t_1xyears + factor(years_grad) + 
                 strata(pid) + factor(syear), method = "approximate", cluster = c(pid, reg), dfmain))

summary(clogit(edu_cur ~ t_1xyears + factor(years_grad) + 
                 strata(pid) + factor(syear), method = "approximate", cluster = c(pid, reg), dfmain))


## DD no spillover robustness check

# Check no spillover assumption with traditional diff-in-diff model that incorporates
# also second and third cohort post-reform






table(dft1_2011$grad_cohort)

centre_new <- ifelse(1, max(data$grad_cohort), centre)
df <- data[which(!is.na(data[, variable]) 
                 & data$grad_cohort >= (centre_new - lags) 
                 & data$grad_cohort <= (centre_new + leads)),]
df <- df %>% group_by(grad_cohort, years_grad) %>% summarise_at(vars(variable), list(mean_var = mean))
df <- as.data.frame(df)
df$grad_cohort <- as.factor(df$grad_cohort)
# plot
ggplot(data=df, aes(x=years_grad, y=mean_var)) +
  geom_point(aes(x=years_grad, y=mean_var, col = grad_cohort, shape = grad_cohort, fill = grad_cohort)) +
  geom_line(aes(x=years_grad, y=mean_var, col = grad_cohort), size = 0.75) + 
  labs(y=ylab, x="Years after Graduation") +
  scale_x_continuous(limits = c(0,13.5), expand = c(0,0), breaks = c(1,3,5,7,9,11,13)) +
  scale_y_continuous(limits = c(0,scale*1.1), expand = c(0,0), breaks = seq(1, scale, 2)) +
  scale_fill_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color interior
  scale_color_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color exterior
  theme_tufte() + 
  theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
















dfdd_2011 <- dfdd[dfdd$grad_cohort == 2011,]
dfdd_2012 <- dfdd[dfdd$grad_cohort == 2012,]

list_df <- list(dfdd, dfdd_2011, dfdd_2012)

dfdd_linc <- panel_table(list_df, variable = "linc", class = "cont", out = "df")
dfdd_edu <- panel_table(list_df, variable = "edu_cur", class = "binary", out = "df")



cross_section_plot(data = dfdd, variable = "linc", ylab = "Log monthly real income", scale = 4, class = "cont")
ggsave(here::here("plots/figure_cross_linc.pdf"), width = 14.2, height = 13.4, units = "cm")
cross_section_plot(data = dfdd_2011, variable = "linc", ylab = "Log monthly real income", scale = 4, class = "cont")
ggsave(here::here("plots/figure_cross_linc.pdf"), width = 14.2, height = 13.4, units = "cm")
cross_section_plot(data = dfdd_2012, variable = "linc", ylab = "Log monthly real income", scale = 4, class = "cont")
ggsave(here::here("plots/figure_cross_linc.pdf"), width = 14.2, height = 13.4, units = "cm")



cross_section_plot(variable = "edu_cur", ylab = "Logit-Coefficient of Treatment Dummy", scale = 4, class = "binary")
ggsave(here::here("plots/figure_cross_edu.pdf"), width = 14.2, height = 13.4, units = "cm")










