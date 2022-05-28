##########################################################
#                                                        #
# 2022/04                                                #
#                                                        #
# Alexander Busch (a.busch@lse.ac.uk)                    #
#                                                        #
# Functions                                              #
#                                                        #
##########################################################

# calculates for a list of data frames and a given variable two panel models (varying in controls)
# and gives out a stargazer table 
panel_table <- function(list_df, var, class, out = "latex"){
  # create empty data frame
  df_all <- data.frame(Data = character(), Variable = character(), c1 = numeric(), c2 = numeric())
  # run loop that runs for each data frame two panel models, and combine output in data frame defined above
  a <- length(list_df)
  list_cohorts <- c("Cohort = all", "Cohort = 2011", "Cohort = 2012", "Cohort = 2013")
  for(i in 1:a){
    df <- list_df[[i]]
    df <- df[df$years_grad < 8, ]
    # if continous, only use estimate and standard error
    if(class == "cont"){
      fe_1 <- round(summary(felm(df[, var] ~ t_1xyears | years_grad + pid | 0 | pid, 
                                 df))$coefficients[1,c(1,2)], 2)
      fe_2 <- round(summary(felm(df[, var] ~ t_1xyears | years_grad + pid + syear + cur_state | 0 | pid, 
                                 df))$coefficients[1,c(1,2)], 2)
      df1 <- data.frame(Data = c(list_cohorts[i], paste0("N = ", sum(df$one))), 
                        c1 = c(as.numeric(fe_1[1]), paste0("(", paste0(as.numeric(fe_1[2]),")"))),
                        c2 = c(as.numeric(fe_2[1]), paste0("(", paste0(as.numeric(fe_2[3]),")"))) 
      )
    }else{ # logit case (use conditional logit model from survival package with same cases / controls)
      fe_1 <- round(summary(clogit(df[, var] ~ t_1xyears + factor(years_grad) + strata(pid), 
                                   df))$coefficients[1,c(1,2,3)], 2)
      fe_2 <- round(summary(clogit(df[, var] ~ t_1xyears + factor(years_grad) + strata(pid)
                                   + factor(syear) + factor(cur_state), 
                                   df))$coefficients[1,c(1,2,3)], 2)
      df1 <- data.frame(Data = c(list_cohorts[i], paste0("N = ", sum(df$one)), ""), 
                        c1 = c(as.numeric(fe_1[2]), as.numeric(fe_1[1]), 
                               paste0("(", paste0(as.numeric(fe_1[3]),")"))),
                        c2 = c(as.numeric(fe_2[2]), as.numeric(fe_2[1]), 
                               paste0("(", paste0(as.numeric(fe_2[3]),")"))) 
      )
    }
    df_all <- rbind(df_all, df1)
  }
  names(df_all) <- c("Data", "(1)", "(2)")
  # stargazer direct or data frame
  if(out == "latex"){
    stargazer(df_all, type = "latex", summary = FALSE)
  }else{
    df_all
  }
}

# plot treatment coefficient +- standard error
cross_section_plot <- function(data = dfmain, var, ylab, scale, class){
  df <- data.frame("Std. Error" = numeric(), "Estimate" = numeric(), "Year" = numeric())
  # create data frame of estimates (and standard errors)
  for(i in 1:7){
    dfyear <- data[data$years_grad == i, ]
    model <- paste0(var, " ~ t_1 + factor(state) + sex + mig + gebjahr + factor(cur_state)")
    if(class == "cont"){
      effect <- as.data.frame(t(summary(lm(model, dfyear))$coefficients[2,c(2,1)]))
    }else{
      effect <- as.data.frame(t(summary(glm(model, family = binomial, dfyear))$coefficients[2,c(2,1)]))
    }
    effect$Year <- i
    df <- rbind(df, effect)
  }
  names(df) <- c("error", "Estimate", "Year")
  # plot coefficients
  ggplot(data=df, aes(x=Year, y=Estimate)) +
    geom_line(aes(x=Year, y=Estimate), size = 0.75) + 
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) + 
    geom_ribbon(aes(ymin=Estimate + error, ymax=Estimate - error), alpha=0.5, fill = "grey", 
                linetype = "blank") +
    labs(y=ylab, x="Years after Graduation") +
    scale_x_continuous(limits = c(0.5,7.5), expand = c(0,0), breaks = c(1:7)) +
    scale_y_continuous(limits = c(-scale,scale), expand = c(0,0), breaks = c(-scale,-0.5*scale,0,0.5*scale,scale)) +
    theme_tufte() + 
    theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
          legend.position = "bottom", legend.title = element_blank())
}

# plot raw difference in shares between treatment and control
cross_section_plot2 <- function(data = dfmain, var, ylab, scale){
  # create data frame of estimates (and standard errors)
  df <- data[data$years_grad < 8 & !is.na(data[, var]),]
  df <- df %>% group_by(years_grad, t_1) %>% summarise_at(vars(var), list(name = mean))
  df <- as.data.frame(df)
  df <- cbind(df[df$t_1 == 1, c(1,3)], df[df$t_1 == 0, 3])
  names(df) <- c("Year", "v_mean_t", "v_mean_c")
  df$Effect <- (df$v_mean_t - df$v_mean_c) * 100
  # plot data
  ggplot(data=df, aes(x=Year, y=Effect)) +
    geom_line(aes(x=Year, y=Effect), size = 0.75) + 
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) + 
    labs(y=ylab, x="Years after Graduation") +
    scale_x_continuous(limits = c(0.5,7.5), expand = c(0,0), breaks = c(1:7)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), # y-axis in %
                       limits = c(-(scale*1.1),(scale*1.1)), expand = c(0,0), 
                       breaks = c(-scale,-scale/2,0,scale/2,scale)) +
    theme_tufte() + 
    theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
          legend.position = "bottom", legend.title = element_blank())
}

# plot for a binary variable function plot and plot2 in one graph with two scales
cross_section_plot3 <- function(data = dfmain, var, ylab, scale_est, scale_per, ylab_est, ylab_per){
  # create data frame of estimates (and standard errors)
  df <- data.frame("Std. Error" = numeric(), "Estimate" = numeric(), "Year" = numeric())
  for(i in 1:7){
    dfyear <- data[data$years_grad == i, ]
    model <- paste0(var, " ~ t_1 + factor(state) + sex + mig + gebjahr + factor(cur_state)")
    effect <- as.data.frame(t(summary(glm(model, family = binomial, dfyear))$coefficients[2,c(2,1)]))
    effect$Year <- i
    df <- rbind(df, effect)
  }
  names(df) <- c("error", "Estimate", "Year")
  # create data frame of percentage differences
  df2 <- data[data$years_grad < 8 & !is.na(data[, var]),]
  df_avg <- df2 %>% group_by(years_grad, t_1) %>% summarise_at(vars(var), list(name = mean))
  df_avg <- as.data.frame(df_avg)
  df_avg <- cbind(df_avg[df_avg$t_1 == 1, c(1,3)], df_avg[df_avg$t_1 == 0, 3])
  names(df_avg) <- c("Year", "v_mean_t", "v_mean_c")
  df_avg$Effect <- (df_avg$v_mean_t - df_avg$v_mean_c) * 100
  # combine data
  df <- merge(df, df_avg, by = "Year")
  scale_factor <- scale_per/scale_est
  # plot data
  ggplot(data=df, aes(x=Year)) + 
    geom_line(aes(y=Effect)) + 
    geom_line(aes(y=Estimate * scale_factor)) + 
    geom_ribbon(aes(ymin=Estimate + error, ymax=Estimate - error), alpha=0.5, fill = "grey", 
                linetype = "blank") +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) + 
    labs(x="Years after Graduation") +
    scale_x_continuous(limits = c(0.5,7.5), expand = c(0,0), breaks = c(1:7)) +
    scale_y_continuous(
      #labels = scales::percent_format(scale = 1), # y-axis in %
      limits = c(-(scale_per),(scale_per)), expand = c(0,0), 
      breaks = c(-scale_per,-scale_per/2,0,scale_per/2,scale_per), 
      name = ylab_per, 
      sec.axis = sec_axis(~./ scale_factor, name = ylab_est)
    ) + 
    theme_tufte() + 
    theme(axis.line = element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
          legend.position = "bottom", legend.title = element_blank())
}


# plot event study-style graph
plot_event <- function(reg_result, n){
  # first step: prepare data frame
  a <- length(reg_result$coefficients)
  df <- data.frame(coeff = as.data.frame(reg_result$coefficients)[(a-(n-2)):a,], 
                   se = as.data.frame(reg_result$se)[(a-(n-2)):a,], 
                   years = c(2:n))
  df <- rbind(df, data.frame(coeff = 0, se = NA, years = 1))
  # second step: define ggplot
  ggplot(data=df, aes(x=years, y=coeff)) +
    geom_errorbar(aes(ymax = coeff + se, ymin = coeff - se), width = .2) + 
    geom_point(aes(x=years, y=coeff)) +
    geom_line(aes(x=years, y=coeff), size = 0.75) + 
    geom_hline(yintercept = 0) + 
    labs(y="Impact on log income", x="Experience") +
    scale_x_continuous(limits = c(0,n+1), expand = c(0,0), breaks = c(1:n)) +
    scale_y_continuous(limits = c(-3,3), expand = c(0,0), breaks = c(-2:2)) +
    theme_tufte() + 
    theme(axis.line=element_line(size = 0.75), text = element_text(size = 15, color = "black"), 
          legend.position="bottom", legend.title = element_blank())
}

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
