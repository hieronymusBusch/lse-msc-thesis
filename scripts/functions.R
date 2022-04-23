##########################################################
#                                                        #
# 2022/04                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Functions                                              #
#                                                        #
##########################################################

# compute population standard error
se <- function(x){
  sd(x) / sqrt(length(x))
}

# function that replaces na's with most recent value
# curtesy of https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
replace_na <- function(x,p=is.na,d=0){
  c(d,x)[cummax(seq_along(x)*(!p(x)))+1]
}

# function that returns how many obs are available for all years & interval around treatment
calc_treated <- function(years, dfinput){
  df_fun <- subset(dfinput, pid %in% names(which(table(pid[syear %in% years]) == length(years))))
  df_fun <- df_fun[!duplicated(df_fun["pid"]), ] # throw out repeated observations (several waves)
  table(df_fun$t_1)
}

# wrapper for calc_treated for several year-ranges
sum_treated <- function(list_yr, dfinput){
  list_range <- lapply(list_yr, calc_treated, dfinput)
  df_fun <- data.frame(list_range[[1]])
  for(i in 2:length(list_yr)){
    df_fun <- cbind(df_fun, as.data.frame(list_range[[i]])[,1])
  }
  df_fun <- t(df_fun)
  df_fun <- df_fun[2:(length(list_yr)+1),]
  df_fun <- cbind(as.data.frame(as.character(list_yr)), df_fun)
  names(df_fun) <- c("years", "treated", "not-treated")
  row.names(df_fun) <- NULL
  df_fun
  #df_fun <- NULL
}


# balanced obs for 2010 to X, 1 year range, mothers
#list_yr <- list(c(2010:2014), c(2010:2015), c(2010:2016), c(2010:2017), c(2010:2018), c(2010:2019))
#obs_edu_sec <- sum_treated(list_yr, c(0:999999), df5mum)
#b_mom_1
