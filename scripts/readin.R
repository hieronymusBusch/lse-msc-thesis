##########################################################
#                                                        #
# 2022/05                                                #
#                                                        #
# Data Read-In                                           #
#                                                        #
##########################################################


## read in data frames & manipulate
here::i_am("scripts/manipulation.R")
source(here::here("scripts/setup.R")) 


## as SOEP data should not be uploaded to GitHub, temporarily change directory
i_am("GitHub/lse-msc-thesis/scripts/manipulation.R")


## read in soep tracking file personal 
df <- read_dta(here::here("SOEP/2020/ppathl.dta"),
               col_select =  c("cid","pid","syear","sex", 
                               "gebjahr","sampreg","gebmonat","corigin","migback", 
                               "birthregion","birthregion_ew","phrf"))
df <- df[df$syear > 2000,]


## generated data (mainly for income / education) and directly stick them to df
df1 <- read_dta(here::here("SOEP/2020/pgen.dta"), 
                col_select =  c("pid","syear","pglabnet","pgemplst","pgnace", 
                                "pgallbet","pgpsbil","pgfield","pgdegree","pgisced97", "pgisced11"))
colnames(df1) <- c("pid","syear","inc","empl1","sector","csize","edu_isced97", "edu_isced11",
                   "edu_sec","edu_ter_field","edu_ter_type")
df1 <- df1[df1$syear > 2000,]
df <- merge(df, df1, by = c("pid", "syear"), all = TRUE)


## another data set of generated variables 
df1 <- read_dta(here::here("SOEP/2020/pequiv.dta"), 
                col_select =  c("pid", "syear", "p11101", "d11109", "m11126", "i11110", 
                                "e11102", "house", "e11101", "l11101"))
colnames(df1) <- c("pid","syear","edu_ys","hours1","empl2","housing","inc2","cur_state","health","lifesat")
df1 <- df1[df1$syear > 2000,]
df <- merge(df, df1, by = c("pid", "syear"), all = TRUE)
df1 <- NULL


## read in soep data personal and stick to df > plg0012 /edu_cur does not seem to exist in 2020 data set? 
#df1 <- read_dta(here::here("SOEP/2019/pl.dta"), 
#                col_select =  c("pid","syear","plg0072","plb0186_h", "plb0186_v2","plg0012","plb0021"))
#colnames(df1) <- c("pid","syear","unempl","hours2","hours_ovhours","edu_cur","edu_compl")
#df1 <- df1[df1$syear > 2000,]
#df <- merge(df, df1, by = c("pid", "syear"), all = TRUE)
#df1 <- NULL


df1 <- read_dta(here::here("SOEP/2020/pl.dta"), 
                col_select =  c("pid","syear","plg0072","plb0186_h", "plb0186_v2","plb0021"))
colnames(df1) <- c("pid","syear","unempl","hours2","hours_ovhours","edu_compl")
df1 <- df1[df1$syear > 2000,]
df <- merge(df, df1, by = c("pid", "syear"), all = TRUE)
df1 <- NULL




