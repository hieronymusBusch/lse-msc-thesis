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

dfmain2011 <- dfmain[dfmain$reg == 2011,]
tab1 <- table(dfmain2011$years_grad, dfmain2011$t_1)
tab1 <- cbind(c(1:9),tab1)
tab1 <- as.data.frame(tab1)

dfmain2012 <- dfmain[dfmain$reg == 2012,]
tab2 <- table(dfmain2012$years_grad, dfmain2012$t_1)
tab2 <- cbind(c(1:8),tab2)
tab2 <- as.data.frame(tab2)

dfmain2013 <- dfmain[dfmain$reg == 2013,]
tab3 <- table(dfmain2013$years_grad, dfmain2013$t_1)
tab3 <- cbind(c(1:7),tab3)
tab3 <- as.data.frame(tab3)

tab <- merge(tab1, merge(tab2, tab3, by = "V1", all = T), by = "V1", all = T)
tab$obs <- rowSums(tab, na.rm = T) - tab$V1
colnames(tab) <- c("years", "2011 C", "2011 T", "2012 C", "2012 T", "2013 C", "2013 T", "Total")
tab[10, ] <- colSums(tab, na.rm = T)
rownames(tab) <- NULL

# output as table for latex
stargazer(tab, summary = FALSE, type = "latex")



### Parallel Trend assumptions


## 2011
dfmain2011_avg <- ddply(dfmain2011[which(!is.na(dfmainp2011$linc)),], 
                         .(grad_cohort, years_grad), summarise, linc_mean=mean(linc), N=sum(one))
dfmainp2011_avg$grad_cohort <- as.factor(dfmainp2011_avg$grad_cohort)

# plot
ggplot(data=dfmainp2011_avg, aes(x=years_grad, y=linc_mean)) +
  geom_point(aes(x=years_grad, y=linc_mean, col = grad_cohort, shape = grad_cohort, fill = grad_cohort)) +
  geom_line(aes(x=years_grad, y=linc_mean, col = grad_cohort), size = 0.75) + 
  labs(y="Log monthly income", x="Experience") +
  scale_x_continuous(limits = c(0,13.5), expand = c(0,0), breaks = c(1,3,5,7,9,11,13)) +
  scale_y_continuous(limits = c(0,9.5), expand = c(0,0), breaks = c(1,3,5,7,9)) +
  scale_fill_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color interior
  scale_color_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color exterior
  theme_tufte() + 
  theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_parallel_2011.pdf"), width = 14.2, height = 13.4, units = "cm")


## 2012
dfmainp2012_avg <- ddply(dfmainp2012[which(!is.na(dfmainp2012$linc)),], 
                         .(grad_cohort, years_grad), summarise, linc_mean=mean(linc), N=sum(one))
dfmainp2012_avg$grad_cohort <- as.factor(dfmainp2012_avg$grad_cohort)

# plot
ggplot(data=dfmainp2012_avg, aes(x=years_grad, y=linc_mean)) +
  geom_point(aes(x=years_grad, y=linc_mean, col = grad_cohort, shape = grad_cohort, fill = grad_cohort)) +
  geom_line(aes(x=years_grad, y=linc_mean, col = grad_cohort), size = 0.75) + 
  labs(y="Log monthly income", x="Experience") +
  scale_x_continuous(limits = c(0,12.5), expand = c(0,0), breaks = c(1,3,5,7,9,11)) +
  scale_y_continuous(limits = c(0,9.5), expand = c(0,0), breaks = c(1,3,5,7,9)) +
  scale_fill_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color interior
  scale_color_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color exterior
  theme_tufte() + 
  theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_parallel_2012.pdf"), width = 14.2, height = 13.4, units = "cm")


## 2013
dfmainp2013_avg <- ddply(dfmainp2013[which(!is.na(dfmainp2013$linc)),], 
                         .(grad_cohort, years_grad), summarise, linc_mean=mean(linc), N=sum(one))
dfmainp2013_avg$grad_cohort <- as.factor(dfmainp2013_avg$grad_cohort)

# plot
ggplot(data=dfmainp2013_avg, aes(x=years_grad, y=linc_mean)) +
  geom_point(aes(x=years_grad, y=linc_mean, col = grad_cohort, shape = grad_cohort, fill = grad_cohort)) +
  geom_line(aes(x=years_grad, y=linc_mean, col = grad_cohort), size = 0.75) + 
  labs(y="Log monthly income", x="Experience") +
  scale_x_continuous(limits = c(0,11.5), expand = c(0,0), breaks = c(1,3,5,7,9,11)) +
  scale_y_continuous(limits = c(0,9.5), expand = c(0,0), breaks = c(1,3,5,7,9)) +
  scale_fill_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color interior
  scale_color_manual(values = c("#CCCCCC", "#999999", "#666666", "#000000")) + # color exterior
  theme_tufte() + 
  theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
ggsave(here::here("plots/figure_parallel_2013.pdf"), width = 14.2, height = 13.4, units = "cm")



### summary statistics
# interesting stats: time invariant, then compare only actual persons
# sex, gebjahr, migback

dfmain_sum <- dfmain[, c("sex", "migback", "unempl", "t_1", "inc", "health", "lifesat", "edu_cur", 
                         "fullt", "empl")]
colnames(dfmain_sum) <- c("gender", "migration", "unemployment", "t_1", "monthly income", "self-rated health", 
                          "life satisfaction", "curr. in education", "full time", "in labour force")
dfmain_sum1 <- dfmain_sum[dfmain_sum$t_1 == 1,]
dfmain_sum2 <- dfmain_sum[dfmain_sum$t_1 == 0,]


stargazer(dfmain_sum1, dfmain_sum2, summary = TRUE, 
          omit.summary.stat = c("max","min"), out = 'tab.txt')























  
  
  
  
