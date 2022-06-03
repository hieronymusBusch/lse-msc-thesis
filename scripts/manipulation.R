##########################################################
#                                                        #
# 2022/05                                                #
#                                                        #
# Data Manipulation                                      #
#                                                        #
##########################################################

#### Dependent Variables
# inc : net laour income in EUR
# inc2 : individual labour earnings (pequiv data set)
# health : self rated health, 1 = v good, 5 = bad
# lifesat : self rated life satisfaction, 0 = bad, 10 = good
# empl : employment status, 0 = not employed, 1 = employed
# house : housing benefits, continuous
# hours : annual working hours
# cur_state: current state (numeric coded)


#### Education Variables
# edu_sec : secondary school degree, 
#           1 = Hauptschule, 2 = Realsch, 3 = FH Reife, 4 = Abitur, 5 = other, 6 = dropout, 7 = no degree yet, 8 = no school attended
# edu_ter_field : field of university degree
# edu_ter_type : type, e.g. 21 = Diplom
# edu_ys : education in years
# edu_isced97 : isced categories
# edu_isced11 : isced categories 
# edu_compl : completed education in the previous year
# edu_cur : currently in education


## read in data frames & manipulate
here::i_am("scripts/manipulation.R")
source(here::here("scripts/readin.R")) 
here::i_am("scripts/manipulation.R")

## data frame to change
df1 <- df


## recode NAs
df1$sampreg <- ifelse(df1$sampreg < 1, NA, df1$sampreg)
df1$birthregion <- ifelse(df1$birthregion < 1, NA, df1$birthregion)
df1$health <- ifelse(df1$health < 1, NA, df1$health)
df1$lifesat <- ifelse(df1$lifesat < 1, NA, df1$lifesat)
df1$unempl <- ifelse(df1$unempl < 1, NA, (df1$unempl - 1)*(-1) + 1) # 1 == unemployed
df1$empl1 <- ifelse(df1$empl1 < 1, NA, df1$empl1)


## recode unlogical NAs (e.g. if birth month missing in some years) that affect restrictions
# birthregion, gebjahr, gebmonat
df1 <- df1 %>% group_by(pid) %>% mutate(edu_sec2 = replace_na(edu_sec, d = NA)) # replace NA with previous value
sum(is.na(df1$edu_sec)) - sum(is.na(df1$edu_sec2)) # retrieved over 100k observations!
df1$edu_sec <- df1$edu_sec2
df1$edu_sec2 <- NULL

## uncomment to use, but no obs retrieved (so left out to enhance speed)

#df1 <- df1 %>% group_by(pid) %>% mutate(birthregion2 = replace_na(birthregion, d = NA)) # replace NA with previous value
#sum(is.na(df1$birthregion)) - sum(is.na(df1$birthregion2)) # retrieved 0 observations
#df1$birthregion2 <- NULL

#df1 <- df1 %>% group_by(pid) %>% mutate(gebjahr2 = max(gebjahr)) # replace NA with individual maximum
#sum(is.na(df1$gebjahr)) - sum(is.na(df1$gebjahr2)) # retrieved 0 observations
#df1$gebjahr2 <- NULL

#df1 <- df1 %>% group_by(pid) %>% mutate(gebmonat2 = max(gebmonat)) # replace NA with individual maximum
#sum(is.na(df1$gebmonat)) - sum(is.na(df1$gebmonat2)) # retrieved 0 observations
#df1$gebmonat2 <- NULL

#df1 <- df1 %>% group_by(pid) %>% mutate(sex2 = max(sex)) # replace NA with individual maximum
#sum(is.na(df1$sex)) - sum(is.na(df1$sex2)) # retrieved 0 observations
#df1$gebmonat2 <- NULL


## change wages to real wages (2010 base year) with OECD CPI
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/PRICES_CPI/DEU.CPALTT01.IXOBSA.A/all?startTime=2005&endTime=2022"
dfCPI <- as.data.frame(readSDMX(url)) # read in German inflation data (date last updated: May 2022)
dfCPI <- dfCPI[,c(9,10)]
names(dfCPI) <- c("year", "CPI")
dfCPI$CPI <- dfCPI$CPI / dfCPI[dfCPI$year == 2010, 2] # change ref year from 2015 to 2010
df1 <- merge(df1, dfCPI, by.x = "syear", by.y = "year", all.x = TRUE)
df1$inc <- df1$inc / df1$CPI 
df1$inc2 <- df1$inc2 / df1$CPI 
df1$CPI <- NULL
dfCPI <- NULL
url <- NULL


## apply sample restrictions
df1 <- df1[which(df1$gebjahr > 1984 & df1$syear >= 2005),] # cohorts affected
df1 <- df1[which(!is.na(df1$edu_sec) & df1$edu_sec == 4),] # only Abitur
df1 <- df1[which(df1$birthregion != 6),] # drop Hesse


## create vars
df1$age <- df1$syear - df1$gebjahr # current age
df1$sex <- ifelse(df1$sex > 0, (df1$sex - 2)*(-1), NA) # 1 = male, 0 = female
df1$empl <- ifelse(df1$empl1 %in% c(1,2), 1, ifelse(is.na(df1$empl1) == T, NA, 0))
df1$fullt <- ifelse(df1$empl1 == 1, 1, ifelse(is.na(df1$empl1) == T, NA, 0))
df1$partt <- ifelse(df1$empl1 == 2, 1, ifelse(is.na(df1$empl1) == T, NA, 0))
df1$edu_cur <- ifelse(df1$empl1 == 3, 1, ifelse(is.na(df1$empl1) == T, NA, 0))
df1$mig <- ifelse(df1$migback %in% c(2,3), 1, 0)
df1$one <- 1 # later needed for counting operations


# labour market entry (variable edu_cur is missing in 2020 data)
# finished education previous year & is not in education anymore
#df1$edu_cur <- ifelse(df1$edu_cur > 0, (df1$edu_cur - 2)*(-1), NA) # now: 1 = yes, 0 = no
#df1$edu_compl <- ifelse(df1$edu_compl > 0, (df1$edu_compl - 2)*(-1), NA) # now: 1 = yes, 0 = no
#df1$entry_lab <- ifelse(df1$edu_compl == 1 & df1$edu_cur == 0,1,0) # orig. 2 = no, 1 = yes; now 1 = entry this year/last year


## treatment variables

# first graduation cohorts by older birth-year (at graduation year with short HS you either turn 18 or 19) (omitting HE with 2012-2014)
first_cohort_values <- c(2016,2010,2011,2012,2013,9999,2012,2011,2009,2012,2012,2008,1949,2007,1949) - 19
dfbl <- data.frame(state_nr = c(1:5,7:16), first_cohort = first_cohort_values, 
                   state = c("SH", "HH", "NI", "HB", "NW", "RP", "BW", "BY",
                             "SL", "BE", "BB", "MV", "SA", "ST", "TH"))
df1 <- merge(df1, dfbl, by.x = "birthregion", by.y = "state_nr", all.x = T)
rm(dfbl)
df1$birth_cohort <- ifelse(df1$gebmonat <= 6, df1$gebjahr - 1, df1$gebjahr) # if born in early half, assign to cohort -1 (=normalise to early year)
# treated
df1$t_1 <- ifelse(df1$birth_cohort >= df1$first_cohort, 1, 0) # if yes = short high school SHS
df1$grad_cohort <- ifelse(df1$t_1 == 1, df1$birth_cohort + 19, df1$birth_cohort + 20) # short high school graduate at 19 (as defined in birth_cohort)

# how many years after graduation? 
df1$years_grad <- df1$syear - df1$grad_cohort


## regression variables
df1$inc <- ifelse(df1$inc < 0, NA, ifelse(df1$inc == 0, 1, df1$inc)) # change zeros in income to 1 ct (for logs)
df1$linc <- log(df1$inc) # set no income to 1 ct before logging
df1$hours3 <- ifelse(is.na(df1$linc), NA, df1$hours1) # hours only if income


## restricted samples for regressions
states <- c("SH", "HH", "NI", "HB", "NW", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SA", "ST", "TH")
statesT2011 <- c("BY", "MV", "NI", "SL", "SA", "ST", "TH")
statesT2012 <- c("BY", "MV", "NI", "SL", "SA", "ST", "TH", "BW", "BE", "BB", "HB")
statesC2011 <- setdiff(states, statesT2011)
statesC2012 <- setdiff(states, statesT2012)
statescore2011 <- c("BW", "BY", "BE", "BB", "HB", "HH", "NI", "NW", "RP", "SH")
statescore2012 <- c("BW", "BE", "BB", "HB", "HH", "NW", "RP", "SH")
states2011 <- c("BY", "NI")
states2012 <- c("BW", "BE", "BB", "HB")
states2013 <- c("NW")
statesReg2 <- union(states2011,union(states2012,states2013))


# dummies on whether observation is in 2011 or 2012 did (there is an overlap, so I define two dummies)
df1$did2011 <- ifelse(
    (df1$birth_cohort == 1990 & df1$grad_cohort == 2010) |  # 2011: 2011 C
    (df1$birth_cohort == 1991 & df1$grad_cohort == 2011 & df1$state %in% statesC2011) | # 2011: 2011 T
    (df1$birth_cohort == 1992 & df1$grad_cohort == 2011 & df1$state %in% statesT2011), # 2012: 2011 C
    1, 0) 

df1$did2012 <- ifelse(
  (df1$birth_cohort == 1991 & df1$grad_cohort == 2011 & df1$state %in% statesC2012) | # 2012: 2011 C
    (df1$birth_cohort == 1991 & df1$grad_cohort == 2011 & df1$state %in% statesT2012) | # 2012: 2011 T
    (df1$birth_cohort == 1992 & df1$grad_cohort == 2012 & df1$state %in% statesC2012) | # 2012: 2012 C
    (df1$birth_cohort == 1993 & df1$grad_cohort == 2012 & df1$state %in% statesT2012) # 2012: 2012 T
, 1, 0)

# treatment x cohort in 2010 (treatment not as in "treated in this year" but as group)
df1$txc2011 <- ifelse(df1$state %in% statesT2011, paste0(df1$grad_cohort,"T"), 
                      paste0(df1$grad_cohort,"C"))
df1$txc2012 <- ifelse(df1$state %in% statesT2012, paste0(df1$grad_cohort,"T"), 
                      paste0(df1$grad_cohort,"C"))

# treatment x years since graduation
df1$t_1xyears <- df1$t_1 * df1$years_grad

# dummy whether part of reg2 in cohort X
df1$reg <- ifelse(df1$grad_cohort == 2011 & df1$state %in% states2011, 2011, 
                   ifelse(df1$grad_cohort == 2012 & df1$state %in% states2012,2012,  
                          ifelse(df1$grad_cohort == 2013 & df1$state %in% states2013, 2013, NA)))

# dummy whether part of reg2 parallel trend comparison group
df1$regp2011 <- ifelse(df1$grad_cohort %in% c(2008:2011) & df1$state %in% states2011 & df1$t_1 == 0, 1, NA)
df1$regp2012 <- ifelse(df1$grad_cohort %in% c(2009:2012) & df1$state %in% states2012 & df1$t_1 == 0, 1, NA)
df1$regp2013 <- ifelse(df1$grad_cohort %in% c(2010:2013) & df1$state %in% states2013 & df1$t_1 == 0, 1, NA)

# dummy "core" regression (only just-treated)
#df1$core <- ifelse((df1$state %in% statescore2011 & df1$did2011 == 1) | 
#                   (df1$state %in% statescore2012 & df1$did2012 == 1), 1, 0)

# weight
df1$wght <- df1$phrf
df1$phrf <- NULL

# subset to main data frame
#dfmain <- df1[which(df1$years_grad %in% c(1:8) & (df1$did2011 == 1 | df1$did2012 == 1)),] 
dfmain <- df1[which(!is.na(df1$reg) & df1$years_grad %in% c(1:9) & 
                      !is.na(df1$unempl) & !is.na(df1$empl) & !is.na(df1$health) & 
                      !is.na(df1$lifesat) & !is.na(df1$fullt) & 
                      df1$years_grad < 8),] # EXCLUDE years 8 and 9 
dfmainp2011 <- df1[which(df1$regp2011 == 1 & df1$years_grad > 0),]
dfmainp2012 <- df1[which(df1$regp2012 == 1 & df1$years_grad > 0),]
dfmainp2013 <- df1[which(df1$regp2013 == 1 & df1$years_grad > 0),]


## Diff-in-Diff comparison as Robustness Check
dfdd <- df1[which(
  (df1$state %in% c("NW") & df1$grad_cohort %in% c(2011, 2012)) | # cohorts 2011 and 2012 from NRW
  (df1$state %in% c("NI", "BY") & df1$grad_cohort %in% c(2011, 2012) & df1$t_1 == 1)  # cohorts 2011 (treated) and 2012 from NI / BY
),]


## Further years as Robustness Check
dft1 <- df1[which(df1$state %in% statesReg2 & df1$t_1 == 1),]


## set environment to non-github to save files
i_am("GitHub/lse-msc-thesis/scripts/manipulation.R")


## safe output
save(dfmain, file = here("SOEP/msc-thesis/dfmain.Rda"))
save(dfmainp2011, file = here("SOEP/msc-thesis/dfmainp2011.Rda"))
save(dfmainp2012, file = here("SOEP/msc-thesis/dfmainp2012.Rda"))
save(dfmainp2013, file = here("SOEP/msc-thesis/dfmainp2013.Rda"))
save(df1, file = here("SOEP/msc-thesis/df1.Rda"))
save(dfdd, file = here("SOEP/msc-thesis/dfdd.Rda"))
save(dft1, file = here("SOEP/msc-thesis/dft1.Rda"))




