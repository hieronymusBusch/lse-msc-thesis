##########################################################
#                                                        #
# 2022/04                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Analysis - Regressions                            #
#                                                        #
##########################################################

here::i_am("scripts/analysis_regressions.R")

## read in data frames & manipulate
#source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 
i_am("GitHub/lse-msc-thesis/scripts/analysis_regressions.R")
load(here("SOEP/msc-thesis/dfmain.Rda"))
load(here("SOEP/msc-thesis/df1.Rda"))  
here::i_am("scripts/analysis_regressions.R") 


dfmain_2011 <- dfmain[dfmain$reg == 2011,]
dfmain_2012 <- dfmain[dfmain$reg == 2012,]
dfmain_2013 <- dfmain[dfmain$reg == 2013,]




data <- dfmain[!is.na(dfmain$linc),]
data$txyears <- data$years_grad * data$t_1 


form <- linc ~ t_1 * factor(years_grad) + factor(state)
wi <- plm(form, data = data, model = "within", index = c("pid", "syear"), effect = c("twoways"))
re <- plm(form, data = data, model = "random", index = c("pid", "syear"), effect = c("twoways"))
phtest(wi, re)
summary(wi)

        
        
        
# 2011 linear interaction
fe_11 <- felm(linc ~ t_1 + years_grad | state + pid + syear | 0 | pid, dfmain_2011[!is.na(dfmain_2011$linc),])
fe_12 <- felm(linc ~ t_1*years_grad | state  | 0 | pid, dfmain_2011[!is.na(dfmain_2011$linc),])
fe_13 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain_2011[!is.na(dfmain_2011$linc),])
stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)






# 2011 linear interaction
fe_11 <- felm(linc ~ t_1 + years_grad | state | 0 | pid, dfmain_2011[!is.na(dfmain_2011$linc),])
fe_12 <- felm(linc ~ t_1*years_grad | state  | 0 | pid, dfmain_2011[!is.na(dfmain_2011$linc),])
fe_13 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain_2011[!is.na(dfmain_2011$linc),])
stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 2012 linear interaction
fe_21 <- felm(linc ~ t_1 + years_grad | state  | 0 | pid, dfmain_2012[!is.na(dfmain_2012$linc),])
fe_22 <- felm(linc ~ t_1*years_grad | state  | 0 | pid, dfmain_2012[!is.na(dfmain_2012$linc),])
fe_23 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain_2012[!is.na(dfmain_2012$linc),])
stargazer(fe_21, fe_22, fe_23, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 2013 linear interaction
fe_31 <- felm(linc ~ t_1 + years_grad | 0 | 0 | pid, dfmain_2013[!is.na(dfmain_2013$linc),])
fe_32 <- felm(linc ~ t_1*years_grad | 0  | 0 | pid, dfmain_2013[!is.na(dfmain_2013$linc),])
fe_33 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg | 0 | pid, 
              dfmain_2013[!is.na(dfmain_2013$linc),])
stargazer(fe_31, fe_32, fe_33, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 4: 2011-2013 linear interaction
fe_41 <- felm(linc ~ t_1 + years_grad | state | 0 | pid, dfmain[!is.na(dfmain$linc),])
fe_42 <- felm(linc ~ t_1*years_grad | state | 0 | pid, dfmain[!is.na(dfmain$linc),])
fe_43 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state | 0 | pid, 
              dfmain[!is.na(dfmain$linc),])
stargazer(fe_41, fe_42, fe_43, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 5: 2011-2013 factor interaction
fe_51 <- felm(linc ~ t_1 + factor(years_grad) | state | 0 | pid, dfmain[!is.na(dfmain$linc),])
fe_52 <- felm(linc ~ t_1*factor(years_grad) | state | 0 | pid, dfmain[!is.na(dfmain$linc),])
fe_53 <- felm(linc ~ t_1*factor(years_grad) | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain[!is.na(dfmain$linc),])
stargazer(fe_51, fe_52, fe_53, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# graphical output
plot_event(fe_51, 9)
ggsave(here::here("plots/figure_linc50.pdf"), width = 14.2, height = 13.4, units = "cm")


## employment
fe_empl1 <- glm(empl ~ t_1 + years_grad + factor(state), family = binomial, dfmain[!is.na(dfmain$empl),])
round(exp(coef(fe_empl1)), 4)
fe_empl2 <- glm(empl ~ t_1*years_grad + factor(state), family = binomial, dfmain[!is.na(dfmain$empl),])
round(exp(coef(fe_empl2)), 4)
fe_empl3 <- glm(empl ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg) + + factor(state), 
             family = binomial, dfmain[!is.na(dfmain$empl),])
round(exp(coef(fe_empl3)), 4)
stargazer(fe_empl1, fe_empl2, fe_empl3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

fe_empl1 <- glm(empl ~ t_1 + years_grad + factor(state), family = binomial, dfmain_2011[!is.na(dfmain_2011$empl),])
round(exp(coef(fe_empl1)), 4)
fe_empl2 <- glm(empl ~ t_1*years_grad + factor(state), family = binomial, dfmain_2011[!is.na(dfmain_2011$empl),])
round(exp(coef(fe_empl2)), 4)
fe_empl3 <- glm(empl ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg) + + factor(state), 
                family = binomial, dfmain_2011[!is.na(dfmain_2011$empl),])
round(exp(coef(fe_empl3)), 4)
stargazer(fe_empl1, fe_empl2, fe_empl3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

fe_empl1 <- glm(empl ~ t_1 + years_grad + factor(state), family = binomial, dfmain_2012[!is.na(dfmain_2012$empl),])
round(exp(coef(fe_empl1)), 4)
fe_empl2 <- glm(empl ~ t_1*years_grad + factor(state), family = binomial, dfmain_2012[!is.na(dfmain_2012$empl),])
round(exp(coef(fe_empl2)), 4)
fe_empl3 <- glm(empl ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg) + factor(state), 
                family = binomial, dfmain_2012[!is.na(dfmain_2012$empl),])
round(exp(coef(fe_empl3)), 4)
stargazer(fe_empl1, fe_empl2, fe_empl3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

fe_empl1 <- glm(empl ~ t_1 + years_grad, family = binomial, dfmain_2013[!is.na(dfmain_2013$empl),])
round(exp(coef(fe_empl1)), 4)
fe_empl2 <- glm(empl ~ t_1*years_grad, family = binomial, dfmain_2013[!is.na(dfmain_2013$empl),])
round(exp(coef(fe_empl2)), 4)
fe_empl3 <- glm(empl ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg), 
                family = binomial, dfmain_2013[!is.na(dfmain_2013$empl),])
round(exp(coef(fe_empl3)), 4)
stargazer(fe_empl1, fe_empl2, fe_empl3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)


## currently in education
fe_edu_cur1 <- glm(edu_cur ~ t_1 + years_grad + factor(state), family = binomial, dfmain[!is.na(dfmain$edu_cur),])
round(exp(coef(fe_edu_cur1)), 2)
fe_edu_cur2 <- glm(edu_cur ~ t_1*years_grad + factor(state), family = binomial, dfmain[!is.na(dfmain$edu_cur),])
round(exp(coef(fe_edu_cur2)), 2)
fe_edu_cur3 <- glm(edu_cur ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg) + + factor(state), 
                family = binomial, dfmain[!is.na(dfmain$edu_cur),])
round(exp(coef(fe_edu_cur3)), 2)
stargazer(fe_edu_cur1, fe_edu_cur2, fe_edu_cur3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

fe_edu_cur1 <- glm(edu_cur ~ t_1 + years_grad + factor(state), family = binomial, dfmain_2011[!is.na(dfmain_2011$edu_cur),])
round(exp(coef(fe_edu_cur1)), 2)
fe_edu_cur2 <- glm(edu_cur ~ t_1*years_grad + factor(state), family = binomial, dfmain_2011[!is.na(dfmain_2011$edu_cur),])
round(exp(coef(fe_edu_cur2)), 2)
fe_edu_cur3 <- glm(edu_cur ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg) + + factor(state), 
                family = binomial, dfmain_2011[!is.na(dfmain_2011$edu_cur),])
round(exp(coef(fe_edu_cur3)), 2)
stargazer(fe_edu_cur1, fe_edu_cur2, fe_edu_cur3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

fe_edu_cur1 <- glm(edu_cur ~ t_1 + years_grad + factor(state), family = binomial, dfmain_2012[!is.na(dfmain_2012$edu_cur),])
round(exp(coef(fe_edu_cur1)), 2)
fe_edu_cur2 <- glm(edu_cur ~ t_1*years_grad + factor(state), family = binomial, dfmain_2012[!is.na(dfmain_2012$edu_cur),])
round(exp(coef(fe_edu_cur2)), 2)
fe_edu_cur3 <- glm(edu_cur ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg) + factor(state), 
                family = binomial, dfmain_2012[!is.na(dfmain_2012$edu_cur),])
round(exp(coef(fe_edu_cur3)), 2)
stargazer(fe_edu_cur1, fe_edu_cur2, fe_edu_cur3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

fe_edu_cur1 <- glm(edu_cur ~ t_1 + years_grad, family = binomial, dfmain_2013[!is.na(dfmain_2013$edu_cur),])
round(exp(coef(fe_edu_cur1)), 2)
fe_edu_cur2 <- glm(edu_cur ~ t_1*years_grad, family = binomial, dfmain_2013[!is.na(dfmain_2013$edu_cur),])
round(exp(coef(fe_edu_cur2)), 2)
fe_edu_cur3 <- glm(edu_cur ~ t_1*years_grad + sex + migback + factor(gebjahr) + factor(sampreg), 
                family = binomial, dfmain_2013[!is.na(dfmain_2013$edu_cur),])
round(exp(coef(fe_edu_cur3)), 2)
stargazer(fe_edu_cur1, fe_edu_cur2, fe_edu_cur3, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)


## health
fe_11 <- felm(health ~ t_1 + years_grad | state | 0 | pid, dfmain_2011[!is.na(dfmain_2011$health),])
fe_12 <- felm(health ~ t_1*years_grad | state  | 0 | pid, dfmain_2011[!is.na(dfmain_2011$health),])
fe_13 <- felm(health ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain_2011[!is.na(dfmain_2011$health),])
stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 2012 linear interaction
fe_21 <- felm(health ~ t_1 + years_grad | state  | 0 | pid, dfmain_2012[!is.na(dfmain_2012$health),])
fe_22 <- felm(health ~ t_1*years_grad | state  | 0 | pid, dfmain_2012[!is.na(dfmain_2012$health),])
fe_23 <- felm(health ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain_2012[!is.na(dfmain_2012$health),])
stargazer(fe_21, fe_22, fe_23, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 2013 linear interaction
fe_31 <- felm(health ~ t_1 + years_grad | 0 | 0 | pid, dfmain_2013[!is.na(dfmain_2013$health),])
fe_32 <- felm(health ~ t_1*years_grad | 0  | 0 | pid, dfmain_2013[!is.na(dfmain_2013$health),])
fe_33 <- felm(health ~ t_1*years_grad | sex + migback + gebjahr + sampreg | 0 | pid, 
              dfmain_2013[!is.na(dfmain_2013$health),])
stargazer(fe_31, fe_32, fe_33, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 4: 2011-2013 linear interaction
fe_41 <- felm(health ~ t_1 + years_grad | state | 0 | pid, dfmain[!is.na(dfmain$health),])
fe_42 <- felm(health ~ t_1*years_grad | state | 0 | pid, dfmain[!is.na(dfmain$health),])
fe_43 <- felm(health ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state | 0 | pid, 
              dfmain[!is.na(dfmain$health),])
stargazer(fe_41, fe_42, fe_43, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)


## life satisfaction
fe_11 <- felm(lifesat ~ t_1 + years_grad | state | 0 | pid, dfmain_2011[!is.na(dfmain_2011$lifesat),])
fe_12 <- felm(lifesat ~ t_1*years_grad | state  | 0 | pid, dfmain_2011[!is.na(dfmain_2011$lifesat),])
fe_13 <- felm(lifesat ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain_2011[!is.na(dfmain_2011$lifesat),])
stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 2012 linear interaction
fe_21 <- felm(lifesat ~ t_1 + years_grad | state  | 0 | pid, dfmain_2012[!is.na(dfmain_2012$lifesat),])
fe_22 <- felm(lifesat ~ t_1*years_grad | state  | 0 | pid, dfmain_2012[!is.na(dfmain_2012$lifesat),])
fe_23 <- felm(lifesat ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state  | 0 | pid, 
              dfmain_2012[!is.na(dfmain_2012$lifesat),])
stargazer(fe_21, fe_22, fe_23, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 2013 linear interaction
fe_31 <- felm(lifesat ~ t_1 + years_grad | 0 | 0 | pid, dfmain_2013[!is.na(dfmain_2013$lifesat),])
fe_32 <- felm(lifesat ~ t_1*years_grad | 0  | 0 | pid, dfmain_2013[!is.na(dfmain_2013$lifesat),])
fe_33 <- felm(lifesat ~ t_1*years_grad | sex + migback + gebjahr + sampreg | 0 | pid, 
              dfmain_2013[!is.na(dfmain_2013$lifesat),])
stargazer(fe_31, fe_32, fe_33, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# 4: 2011-2013 linear interaction
fe_41 <- felm(lifesat ~ t_1 + years_grad | state | 0 | pid, dfmain[!is.na(dfmain$lifesat),])
fe_42 <- felm(lifesat ~ t_1*years_grad | state | 0 | pid, dfmain[!is.na(dfmain$lifesat),])
fe_43 <- felm(lifesat ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state | 0 | pid, 
              dfmain[!is.na(dfmain$lifesat),])
stargazer(fe_41, fe_42, fe_43, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)










### by year of experience



### Graphical evidence: Yearly difference treatment effect


## income

df_avg <- ddply(dfmain[which(!is.na(dfmain$linc)),], 
             .(years_grad,t_1), summarise, linc_mean=mean(linc), N=sum(one))
df_avg <- cbind(df_avg[df_avg$t_1 == 1, c(1,3)], df_avg[df_avg$t_1 == 0, 3])
names(df_avg) <- c("years_grad", "linc_t", "linc_c")
df_avg$treatment <- df_avg$linc_t - df_avg$linc_c
df_avg <- df_avg[c(1:7),]

# plot
ggplot(data=df_avg, aes(x=years_grad, y=treatment)) +
  geom_point(aes(x=years_grad, y=treatment)) +
  geom_line(aes(x=years_grad, y=treatment), size = 0.75) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) + 
  labs(y="Log monthly income", x="Years after Graduation") +
  scale_x_continuous(limits = c(0,8), expand = c(0,0), breaks = c(1:7)) +
  scale_y_continuous(limits = c(-5,5), expand = c(0,0), breaks = c(-4,-2,0,2,4)) +
  theme_tufte() + 
  theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_treatment_inc.pdf"), width = 14.2, height = 13.4, units = "cm")


## currently in education

df_avg <- ddply(dfmain[which(!is.na(dfmain$edu_cur)),], 
                .(years_grad,t_1), summarise, edu_cur_mean=mean(edu_cur), N=sum(one))
df_avg <- cbind(df_avg[df_avg$t_1 == 1, c(1,3)], df_avg[df_avg$t_1 == 0, 3])
names(df_avg) <- c("years_grad", "edu_cur_t", "edu_cur_c")
df_avg$treatment <- (df_avg$edu_cur_t - df_avg$edu_cur_c) * 100
df_avg <- df_avg[c(1:7),]


# plot
ggplot(data=df_avg, aes(x=years_grad, y=treatment)) +
  geom_point(aes(x=years_grad, y=treatment)) +
  geom_line(aes(x=years_grad, y=treatment), size = 0.75) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) + 
  labs(y="Share in Higher Education", x="Years after Graduation") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), # y-axis in %
                     limits = c(-100,100), expand = c(0,0), breaks = c(-100,-50,0,50,100)) +
  scale_x_continuous(limits = c(0,8), expand = c(0,0), breaks = c(1:7)) +
  #scale_y_continuous(limits = c(-1.1,1.1), expand = c(0,0), breaks = c(-1,-0.5,0,0.5,1)) +
  theme_tufte() + 
  theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_treatment_edu.pdf"), width = 14.2, height = 13.4, units = "cm")


## 

df_avg <- ddply(dfmain[which(!is.na(dfmain$empl)),], 
                .(years_grad,t_1), summarise, empl_mean=mean(empl), N=sum(one))
df_avg <- cbind(df_avg[df_avg$t_1 == 1, c(1,3)], df_avg[df_avg$t_1 == 0, 3])
names(df_avg) <- c("years_grad", "empl_t", "empl_c")
df_avg$treatment <- (df_avg$empl_t - df_avg$empl_c) * 100
df_avg <- df_avg[c(1:7),]


# plot
ggplot(data=df_avg, aes(x=years_grad, y=treatment)) +
  geom_point(aes(x=years_grad, y=treatment)) +
  geom_line(aes(x=years_grad, y=treatment), size = 0.75) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) + 
  labs(y="Share in Higher Education", x="Years after Graduation") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), # y-axis in %
                     limits = c(-100,100), expand = c(0,0), breaks = c(-100,-50,0,50,100)) +
  scale_x_continuous(limits = c(0,8), expand = c(0,0), breaks = c(1:7)) +
  theme_tufte() + 
  theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_treatment_edu.pdf"), width = 14.2, height = 13.4, units = "cm")




