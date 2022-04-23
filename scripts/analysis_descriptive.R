##########################################################
#                                                        #
# 2022/04                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Analysis - Descriptives                           #
#                                                        #
##########################################################

here::i_am("scripts/analysis_descriptive.R")

## read in data frames & manipulate
source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 
i_am("GitHub/lse-msc-thesis/scripts/analysis_descriptive.R")
load("SOEP/msc-thesis/dfmain.Rda")
load("SOEP/msc-thesis/df1.Rda")  
here::i_am("scripts/analysis_descriptive.R")



#### Observation Count 

# only Thuringa, Saxony, MV, ST
dfeast <- df1[!duplicated(df1$pid) & !is.na(df1$t_1) & df1$state %in% c("TH", "SA", "MV", "ST"),]
table(dfeast$birth_, dfeast$t_1) 
dfeast <- NULL
# > too small of a sample, 3XX persons and many after 2010 > would be like following just one class

# unique observations (per pid)
dfmain_i <- dfmain[!duplicated(dfmain$pid),]
table(dfmain_i$birth_cohort, dfmain_i$t_1)
table(dfmain$birth_cohort, dfmain$syear)
table(dfmain$state)
table(dfmain_i$state)


# how many obs per individual? 
dfmain <- dfmain %>% group_by(pid) %>% mutate(count = sum(one))
dfmain <- dfmain %>% group_by(pid) %>% mutate(count_ideal = 2020 - (gebjahr + 18 - 1)) # starting at 18, how many possible years up to 2020
dfmain <- dfmain %>% group_by(pid) %>% mutate(count_miss = count_ideal - count)

table(dfmain$count)
mean(dfmain$count)
table(dfmain$count_miss, dfmain$birth_cohort)
table(dfmain$birth_cohort)
# many with several missings, but rather high average of 6 obs for the max 8 year period! 

# table for paper: 
# years_grad | total obs | 2011 treated | 2011 control | 2012 treated | 2012 control

years <- c(1:8)

tab <- table(dfmain$years_grad, dfmain$grad_cohort)
tab <- cbind(years,tab)
tab <- as.data.frame(tab)
tab$obs <- tab[, 2] + tab[, 3] + tab[, 4]
sumrow <- c("total", sum(tab[,2]), sum(tab[,3]), sum(tab[,4]), sum(tab[,5]))
tab <- rbind(tab, sumrow)
tab$years <- NULL
tab 


dfmain2011 <- dfmain[dfmain$did2011 == 1,]
tab1 <- table(dfmain2011$years_grad, dfmain2011$txc2011)
tab1 <- cbind(years,tab1)
tab1 <- as.data.frame(tab1)
tab1$obs <- tab1[, 2] + tab1[, 3] + tab1[, 4] + tab1[, 5]
colnames(tab) <- c("years", "2010 C", "2010 T", "2011 C", "2011 T", "Total Observations")
sumrow <- c("total", sum(tab1[,2]), sum(tab1[,3]), sum(tab1[,4]), sum(tab1[,5]), sum(tab1[,6]))
tab1 <- rbind(tab1, sumrow)
tab1 

dfmain2012 <- dfmain[dfmain$did2012 == 1,]
tab2 <- table(dfmain2012$years_grad, dfmain2012$txc2012)
tab2 <- cbind(years,tab2)
tab2 <- as.data.frame(tab2)
tab2$obs <- tab2[, 2] + tab2[, 3] + tab2[, 4] + tab2[, 5]
colnames(tab) <- c("years", "2011 C", "2011 T", "2012 C", "2012 T", "Total Observations")
sumrow <- c("total", sum(tab2[,2]), sum(tab2[,3]), sum(tab2[,4]), sum(tab2[,5]), sum(tab2[,6]))
tab2 <- rbind(tab2, sumrow)
tab2 

stargazer(tab, summary = FALSE, type = "latex")
stargazer(tab1, summary = FALSE, type = "latex")
stargazer(tab2, summary = FALSE, type = "latex")




##### graphical evidence of wage differences
# just average wage versus
dfmainGraph <- dfmain[!is.na(dfmain$linc), c("syear", "linc", "t_1", "years_grad", "one")]
dfmainGraph <- ddply(dfmainGraph, .(t_1, years_grad), summarise, linc_mean=mean(linc), linc_sd = sd(linc), N=sum(one))

# subtract

dfmainGraph$years_grad <- ifelse(dfmainGraph$t_1 == 1, dfmainGraph$years_grad + 0.2, dfmainGraph$years_grad) # slightly shift time var to distinguish points in plot
dfmainGraph$t_1 <- factor(ifelse(dfmainGraph$t_1 == 1, "short", "long"))

ggplot(data=dfmainGraph, aes(x=years_grad, y=linc_mean)) +
  geom_errorbar(aes(ymax = linc_mean + linc_sd, ymin = linc_mean - linc_sd)) + # put error bars behind points by loading early
  geom_point(aes(x=years_grad, y=linc_mean, col = t_1, shape = t_1, fill = t_1)) +
  geom_line(aes(x=years_grad, y=linc_mean, col = t_1), size = 0.75) + 
  labs(y="Log monthly income", x="Years since graduation") +
  scale_x_continuous(limits = c(0,9), expand = c(0,0), breaks = c(1:8)) +
  scale_y_continuous(limits = c(-2,14), expand = c(0,0), breaks = c(0,3,6,9,11)) +
  scale_shape_manual(values = c(21, 23)) + # choose point-style
  scale_fill_manual(values = c("#404040", "#a6a6a6")) + # color interior
  scale_color_manual(values = c("#404040", "#a6a6a6")) + # color exterior
  theme_tufte() + 
  theme(axis.line=element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position="bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_linc.pdf"), scale = 1.5, units = "cm")





# 2011 cohorts
dfmain2011 <- dfmain[dfmain$did2011 == 1 & !is.na(dfmain$linc), c("syear", "linc", "t_1", "years_grad", "one", "txc2011")]
dfmain2011 <- ddply(dfmain2011, .(txc2011, years_grad), summarise, linc_mean=mean(linc), linc_sd = sd(linc), N=sum(one))

# subtract

dfmain2011$years_grad <- ifelse(dfmain2011$t_1 == 1, dfmain2011$years_grad + 0.2, dfmain2011$years_grad) # slightly shift time var to distinguish points in plot
dfmain2011$t_1 <- factor(ifelse(dfmain2011$t_1 == 1, "short", "long"))

ggplot(data=dfmain2011, aes(x=years_grad, y=linc_mean)) +
  geom_errorbar(aes(ymax = linc_mean + linc_sd, ymin = linc_mean - linc_sd)) + # put error bars behind points by loading early
  geom_point(aes(x=years_grad, y=linc_mean, col = t_1, shape = t_1, fill = t_1)) +
  geom_line(aes(x=years_grad, y=linc_mean, col = t_1), size = 0.75) + 
  labs(y="Log monthly income", x="Years since graduation") +
  scale_x_continuous(limits = c(0,9), expand = c(0,0), breaks = c(1:8)) +
  scale_y_continuous(limits = c(-2,14), expand = c(0,0), breaks = c(0,3,6,9,11)) +
  scale_shape_manual(values = c(21, 23)) + # choose point-style
  scale_fill_manual(values = c("#404040", "#a6a6a6")) + # color interior
  scale_color_manual(values = c("#404040", "#a6a6a6")) + # color exterior
  theme_tufte() + 
  theme(axis.line=element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position="bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_linc2011.pdf"), scale = 1.5, units = "cm")

# 2012 cohorts
dfmain2012 <- dfmain[dfmain$grad_cohort == 2012 & !is.na(dfmain$linc), c("syear", "linc", "t_1", "years_grad", "one")]
dfmain2012 <- ddply(dfmain2012, .(t_1, years_grad), summarise, linc_mean=mean(linc), linc_sd = sd(linc), N=sum(one))
dfmain2012$years_grad <- ifelse(dfmain2012$t_1 == 1, dfmain2012$years_grad + 0.2, dfmain2012$years_grad) # slightly shift time var to distinguish points in plot
dfmain2012$t_1 <- factor(ifelse(dfmain2012$t_1 == 1, "short", "long"))

ggplot(data=dfmain2012, aes(x=years_grad, y=linc_mean)) +
  geom_errorbar(aes(ymax = linc_mean + linc_sd, ymin = linc_mean - linc_sd)) + # put error bars behind points by loading early
  geom_point(aes(x=years_grad, y=linc_mean, col = t_1, shape = t_1, fill = t_1)) +
  geom_line(aes(x=years_grad, y=linc_mean, col = t_1), size = 0.75) + 
  labs(y="Log monthly income", x="Years since graduation") +
  scale_x_continuous(limits = c(0,9), expand = c(0,0), breaks = c(1:8)) +
  scale_y_continuous(limits = c(-2,14), expand = c(0,0), breaks = c(0,3,6,9,12)) +
  scale_shape_manual(values = c(21, 23)) + # choose point-style
  scale_fill_manual(values = c("#404040", "#a6a6a6")) + # color interior
  scale_color_manual(values = c("#404040", "#a6a6a6")) + # color exterior
  theme_tufte() + 
  theme(axis.line=element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position="bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_linc2012.pdf"), scale = 1.5, units = "cm")




##### summary statistics
# interesting stats: time invariant, then compare only actual persons
# sex, gebjahr, migback, ew, parent education, 

colnames(dfmain)






table(dfmain$t_1, dfmain$years_grad)
table(dfmain$t_1, dfmain$syear)






dfe <- df1[df1$birth_cohort %in% c(2011), c("t_1", "inc", "health", "lifesat", "years_grad", "years_grad")]

dfe <- df1[, c("t_1", "inc", "health", "lifesat", "years_grad", "years_grad")]

dfe <- cbind(dfe, c(rep(1, 9891)))
colnames(dfe) <- c("t_1", "inc", "health", "lifesat", "years_grad", "years_grad", "id")

dfe2 <- ddply(dfe, .(t_1, years_grad), summarise, inc=mean(inc), health=mean(health), 
              lifesat=mean(lifesat), N=sum(id))
rownames(dfe2) <- NULL
dfe3 <- cbind(dfe2[dfe2$t_1 == 1 & dfe2$years_grad %in% c(1:9), ], dfe2[dfe2$t_1 == 0 & dfe2$years_grad %in% c(1:9), c("inc","N")])
colnames("t_1", "years_grad", "inc_1", "N_1", "inc_0", "N_0")


ggplot(data=dfe2, aes(x=years_grad, y=inc, group=t_1)) +
  geom_point() +
  geom_line(size = 0.75) + 
  scale_color_manual(values=c("#CC6666", "#9999CC")) +
  labs(y="Monthly income in Euro", x="Years since Graduation") +
  scale_x_continuous(limits = c(0.5,9.5), expand = c(0,0), breaks = c(1:9)) +
  scale_y_continuous(limits = c(0,2000), expand = c(0,0)) +
  theme_classic() 























df <- data.frame(FUN = c(-0.25,0.2,0,5,4.5,2.4,1,0,-0.2,-0.5), time = c(-3,-2,-1,0:6), 
                 se = c(.5,.4,.5,.45,.35,.6,.5,.35,.5,.7))



ggplot(data=df, aes(x=time, y=FUN)) +
  geom_point() +
  geom_line(color='black', size = 0.6) + 
  geom_errorbar(aes(ymax = FUN + se, ymin = FUN - se)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  labs(y="Self-reported FUN", x="Months") +
  scale_x_continuous(limits = c(0.5,10.5), expand = c(0,0), breaks = c(-3,-2,-1,0:6)) +
  theme_classic() 




  
  
  
  
