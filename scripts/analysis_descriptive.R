##########################################################
#                                                        #
# 2022/05                                                #
#                                                        #
# Data Analysis - Descriptives                           #
#                                                        #
##########################################################

here::i_am("scripts/analysis_descriptive.R")

## read in data frames & manipulate
#source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 
i_am("GitHub/lse-msc-thesis/scripts/analysis_descriptive.R")
load(here::here("SOEP/msc-thesis/dfmain.Rda"))
load(here::here("SOEP/msc-thesis/dfmainp2011.Rda"))
load(here::here("SOEP/msc-thesis/dfmainp2012.Rda"))
load(here::here("SOEP/msc-thesis/dfmainp2013.Rda"))
load(here::here("SOEP/msc-thesis/df1.Rda"))
here::i_am("scripts/analysis_descriptive.R")



#### Observation Count 


# how many obs per individual? 
dfmain <- dfmain %>% group_by(pid) %>% mutate(count = sum(one))
dfmain <- dfmain %>% group_by(pid) %>% mutate(count_ideal = 2020 - (gebjahr + 18 - 1)) # starting at 18, how many possible years up to 2020
dfmain <- dfmain %>% group_by(pid) %>% mutate(count_miss = count_ideal - count)

table(dfmain$count)
mean(dfmain$count)
table(dfmain$count_miss, dfmain$birth_cohort)
table(dfmain$birth_cohort)
# many with several missings, but rather high average of 6 obs for the max 8 year period! 


## table for paper: 
# years_grad | total obs | 2011 treated | 2011 control | 2012 treated | 2012 control

dfmain2011 <- dfmain[dfmain$reg == 2011,]
tab1 <- table(dfmain2011$years_grad, dfmain2011$t_1)
tab1 <- cbind(c(1:7),tab1)
tab1 <- as.data.frame(tab1)

dfmain2012 <- dfmain[dfmain$reg == 2012,]
tab2 <- table(dfmain2012$years_grad, dfmain2012$t_1)
tab2 <- cbind(c(1:7),tab2)
tab2 <- as.data.frame(tab2)

dfmain2013 <- dfmain[dfmain$reg == 2013,]
tab3 <- table(dfmain2013$years_grad, dfmain2013$t_1)
tab3 <- cbind(c(1:7),tab3)
tab3 <- as.data.frame(tab3)

tab <- merge(tab1, merge(tab2, tab3, by = "V1", all = T), by = "V1", all = T)
tab$obs <- rowSums(tab, na.rm = T) - tab$V1
colnames(tab) <- c("years", "2011 C", "2011 T", "2012 C", "2012 T", "2013 C", "2013 T", "Total")
tab[8, ] <- colSums(tab, na.rm = T)
rownames(tab) <- NULL

# output as table for latex
stargazer(tab, summary = FALSE, type = "latex")



### Parallel Trend assumptions


## log income

# 2011
parallel_trends(data = dfmainp2011, variable = "linc", ylab = "Log monthly income", scale = 10)
ggsave(here::here("plots/figure_parallel_linc_2011.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2012
parallel_trends(data = dfmainp2012, variable = "linc", ylab = "Log monthly income", scale = 10)
ggsave(here::here("plots/figure_parallel_linc_2012.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2013
parallel_trends(data = dfmainp2013, variable = "linc", ylab = "Log monthly income", scale = 10)
ggsave(here::here("plots/figure_parallel_linc_2013.pdf"), width = 14.2, height = 13.4, units = "cm")



## health

# 2011
parallel_trends(data = dfmainp2011, variable = "health", ylab = "Health self-assessment, 1 to 5", scale = 5)
ggsave(here::here("plots/figure_parallel_health_2011.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2012
parallel_trends(data = dfmainp2012, variable = "health", ylab = "Health self-assessment, 1 to 5", scale = 5)
ggsave(here::here("plots/figure_parallel_health_2012.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2013
parallel_trends(data = dfmainp2013, variable = "health", ylab = "Health self-assessment, 1 to 5", scale = 5)
ggsave(here::here("plots/figure_parallel_health_2013.pdf"), width = 14.2, height = 13.4, units = "cm")


## life satisfaction

# 2011
parallel_trends(data = dfmainp2011, variable = "lifesat", ylab = "Life satisfaction, 0 to 10", scale = 10)
ggsave(here::here("plots/figure_parallel_lifesat_2011.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2012
parallel_trends(data = dfmainp2012, variable = "lifesat", ylab = "Life satisfaction, 0 to 10", scale = 10)
ggsave(here::here("plots/figure_parallel_lifesat_2012.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2013
parallel_trends(data = dfmainp2013, variable = "lifesat", ylab = "Life satisfaction, 0 to 10", scale = 10)
ggsave(here::here("plots/figure_parallel_lifesat_2013.pdf"), width = 14.2, height = 13.4, units = "cm")


## annual hours

# 2011
parallel_trends(data = dfmainp2011, variable = "hours3", ylab = "Annual Working Hours", scale = 3000)
ggsave(here::here("plots/figure_parallel_hours_2011.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2012
parallel_trends(data = dfmainp2012, variable = "hours3", ylab = "Annual Working Hours", scale = 3000)
ggsave(here::here("plots/figure_parallel_hours_2012.pdf"), width = 14.2, height = 13.4, units = "cm")

# 2013
parallel_trends(data = dfmainp2013, variable = "hours3", ylab = "Annual Working Hours", scale = 3000)
ggsave(here::here("plots/figure_parallel_hours_2013.pdf"), width = 14.2, height = 13.4, units = "cm")



### summary statistics
# sex, gebjahr, migback, ...

dfmain_sum <- dfmain[, c("sex", "mig", "t_1", "linc", "health", "lifesat", "edu_cur", 
                         "fullt", "empl", "hours3")]
colnames(dfmain_sum) <- c("gender", "migration", "t_1", "log monthly income", "self-rated health", 
                          "life satisfaction", "curr. in education", "full time", "employed", "ann. hours")
dfmain_sum1 <- dfmain_sum[which(dfmain_sum$t_1 == 1),]
dfmain_sum2 <- dfmain_sum[which(dfmain_sum$t_1 == 0),]

stargazer(dfmain_sum1, dfmain_sum2, summary = TRUE, omit.summary.stat = c("max","min"), type = "latex", digits = 2)


### Germany Map

dfsp <- st_read("data/shapefiles/250_NUTS1.shp")
dfsp <- dfsp[-c(17:22),] # delete water / oecean 

dfbl <- data.frame(first_cohort = c(2016,2010,2011,2012,2013,2013,9999,2012,2011,2009,
                                    2012,2012,2008,1949,2007,1949), 
                   study = c("other years", "other years", "2011", "2012", "2013", "other years", "other years", "2012", "2011", 
                             "other years", "2012", "2012", "other years", "other years", "other years", "other years"),
                   state = c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY",
                             "SL", "BE", "BB", "MV", "SA", "ST", "TH"), 
                   NUTS_NAME = c("Schleswig-Holstein", "Hamburg", "Niedersachsen", 
                                  "Bremen", "Nordrhein-Westfalen", "Hessen", 
                                  "Rheinland-Pfalz", "Baden-Württemberg", "Bayern", 
                                  "Saarland", "Berlin", "Brandenburg", "Mecklenburg-Vorpommern", 
                                  "Sachsen", "Sachsen-Anhalt", "Thüringen"))
dfsp <- merge(dfsp, dfbl, by = "NUTS_NAME")

# map for sample
ggplot(data = dfsp) +
  geom_sf(aes(alpha=study, fill=study)) +
  scale_fill_manual(values = c("#999999", "#666666", "#333333", "white")) +
  scale_alpha_manual(values=c(1,1,1,0)) +
  theme_void() + 
  theme(text = element_text(size = 15, color = "black"), 
        legend.position = "bottom", legend.title = element_blank())
ggsave(here::here("plots/map_sample.pdf"), width = 15, height = 25, units = "cm")










