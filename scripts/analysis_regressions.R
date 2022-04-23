##########################################################
#                                                        #
# 2022/04                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Data Analysis - Regressions                            #
#                                                        #
##########################################################

here::i_am("scripts/analysis_regression.R")

## read in data frames & manipulate
#source(here::here("scripts/manipulation.R")) 

## read in data directly
source(here::here("scripts/setup.R")) 
i_am("GitHub/lse-msc-thesis/scripts/analysis_regression.R")
load("SOEP/msc-thesis/dfmain.Rda")
load("SOEP/msc-thesis/df1.Rda")  
here::i_am("scripts/analysis_regression.R") 

here::i_am("scripts/analysis_regression.R")


varlistX <- c("sex", "migback")



## base line estimates: LOG WAGE
fe_11 <- felm(linc ~ t_1 + years_grad  | state + syear | 0 | state + pid, dfmain[!is.na(dfmain$linc),])
summary(fe_11)

fe_12 <- felm(linc ~ t_1*years_grad | state + syear | 0 | state + pid, dfmain[!is.na(dfmain$linc),])
summary(fe_12)

fe_13 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state + syear | 0 | state + pid, dfmain[!is.na(dfmain$linc),])
summary(fe_13)

stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)


### estimate model with only "just-treated"
dfmaincore <- dfmain[dfmain$core == 1,]

fe_11 <- felm(linc ~ t_1 + years_grad | state + syear | 0 | state + pid, dfmaincore[!is.na(dfmaincore$linc),])
summary(fe_11)

fe_12 <- felm(linc ~ t_1*years_grad | state + syear | 0 | state + pid, dfmaincore[!is.na(dfmaincore$linc),])
summary(fe_12)

fe_13 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state + syear | 0 | state + pid, dfmaincore[!is.na(dfmaincore$linc),])
summary(fe_13)

stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# > effect vanishes with first controls


### estimate model on sub-dids
dfmain2011 <- dfmain[dfmain$did2011 == 1,]
dfmain2012 <- dfmain[dfmain$did2012 == 1,]

## base line estimates: LOG WAGE 2011 cohort
fe_11 <- felm(linc ~ t_1 + years_grad | state | 0 | state + pid, dfmain2011[!is.na(dfmain2011$linc),])
summary(fe_11)

fe_12 <- felm(linc ~ t_1*years_grad | state | 0 | state + pid, dfmain2011[!is.na(dfmain2011$linc),])
summary(fe_12)

fe_13 <- felm(linc ~ t_1*years_grad | sex + migback + gebjahr + sampreg + state | 0 | state + pid, dfmain2011[!is.na(dfmain2011$linc),])
summary(fe_13)

stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

## base line estimates: LOG WAGE 2012 cohort
fe_11 <- felm(linc ~ t_1 + years_grad  + years_grad_e2| state | 0 | state + pid, dfmain2012[!is.na(dfmain2012$linc),])
summary(fe_11)

fe_12 <- felm(linc ~ t_1*years_grad + years_grad_e2 | state | 0 | state + pid, dfmain2012[!is.na(dfmain2012$linc),])
summary(fe_12)

fe_13 <- felm(linc ~ t_1*years_grad + years_grad_e2 | sex + migback + gebjahr + sampreg + state | 0 | state + pid, dfmain2012[!is.na(dfmain2012$linc),])
summary(fe_13)

stargazer(fe_11, fe_12, fe_13, type = "latex", omit.stat=c("f", "ser", "rsq", "adj.rsq"),star.char = NULL, align=TRUE,digits=2)

# > effect is driven by 2012 comparison









## base line estimates: health
fe_11 <- felm(health ~ t_1 + years_grad  + years_grad_e2| state + syear | 0 | state + pid, dfmain[!is.na(dfmain$health),])
summary(fe_11)

fe_12 <- felm(health ~ t_1*years_grad + years_grad_e2 | state + syear | 0 | state + pid, dfmain[!is.na(dfmain$health),])
summary(fe_12)

fe_13 <- felm(health ~ t_1*years_grad + years_grad_e2 | sex + migback + gebjahr + sampreg + state + syear | 0 | state + pid, dfmain[!is.na(dfmain$health),])
summary(fe_13)

## base line estimates: life satisfaction
fe_11 <- felm(lifesat ~ t_1 + years_grad  + years_grad_e2| state + syear | 0 | state + pid, dfmain[!is.na(dfmain$lifesat),])
summary(fe_11)

fe_12 <- felm(lifesat ~ t_1*years_grad + years_grad_e2 | state + syear | 0 | state + pid, dfmain[!is.na(dfmain$lifesat),])
summary(fe_12)

fe_13 <- felm(lifesat ~ t_1*years_grad + years_grad_e2 | sex + migback + gebjahr + sampreg + state + syear | 0 | state + pid, dfmain[!is.na(dfmain$lifesat),])
summary(fe_13)

## base line estimates: unemployment
fe_11 <- felm(unempl ~ t_1 + years_grad  + years_grad_e2| state + syear | 0 | state + pid, dfmain[!is.na(dfmain$unempl),])
summary(fe_11)

fe_12 <- felm(unempl ~ t_1*years_grad + years_grad_e2 | state + syear | 0 | state + pid, dfmain[!is.na(dfmain$unempl),])
summary(fe_12)

fe_13 <- felm(unempl ~ t_1*years_grad + years_grad_e2 | sex + migback + gebjahr + sampreg + state + syear | 0 | state + pid, dfmain[!is.na(dfmain$unempl),])
summary(fe_13)

## base line estimates: employment
fe_11 <- felm(empl ~ t_1 + years_grad  + years_grad_e2| state + syear | 0 | state + pid, dfmain[!is.na(dfmain$empl),])
summary(fe_11)

fe_12 <- felm(empl ~ t_1*years_grad + years_grad_e2 | state + syear | 0 | state + pid, dfmain[!is.na(dfmain$empl),])
summary(fe_12)

fe_13 <- felm(empl ~ t_1*years_grad + years_grad_e2 | sex + migback + gebjahr + sampreg + state + syear | 0 | state + pid, dfmain[!is.na(dfmain$empl),])
summary(fe_13)

## base line estimates: university enrollment 
fe_11 <- felm(edu_cur ~ t_1 + years_grad  + years_grad_e2| state + syear | 0 | state + pid, dfmain[!is.na(dfmain$edu_cur),])
summary(fe_11)

fe_12 <- felm(edu_cur ~ t_1*years_grad + years_grad_e2 | state + syear | 0 | state + pid, dfmain[!is.na(dfmain$edu_cur),])
summary(fe_12)

fe_13 <- felm(edu_cur ~ t_1*years_grad + years_grad_e2 | sex + migback + gebjahr + sampreg + state + syear | 0 | state + pid, dfmain[!is.na(dfmain$edu_cur),])
summary(fe_13)

