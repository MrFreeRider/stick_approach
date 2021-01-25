#############################################
# Paper: El Niño is coming 
# Author: Jose David Lopez Rivas
# Propensity score matching
#############################################

rm(list = ls())

library(ggplot2)
library(dplyr)
library(plm) #Panel data linear models
library(stargazer)
library(tidyverse)
library(ggpubr)
library(wesanderson)
library(cobalt) #Covariance balance tables and plots
library(stargazer) # Table-Output
library(survival) #Survival analysis
library(compareGroups) # Descriptoive analysis by groups
library("broom") # Convert statistical objects in tidy tibbles
library("haven") #Import/export Stata/SPSS/SAS
library("rlang")
library("clubSandwich") #Cluster-robust variance estimators

detach(data)
setwd("~/Documents/GitHub/stick_approach")
data <- read_csv("data_elnino_pre.csv") #Load data


names(data)
attach(data)

attr(data, "model.varnames") <- c("wss","Distance to border (km)",
                                 "Distance to border (km)","Distance to border (km)",
                                 "Temperature (ºC)","Rainfall (mm/month)",
                                 "Gini Index WSS","Herfindahl Index WSS",
                                 "Users subsidized (%)","Distance to Magdalena river (km)",
                                 "Affected","Water consumption per user(m3) (Original)",
                                 "Users (#)","Total water consumption (m3)",
                                 "Water consumption per user(m3) (fixed)",
                                 "Water consumption per user(m3) (cut 1%)",
                                 "Water consumption per user(m3) (cut 5%)",
                                 "Water consumption per user(m3) (cut 10%)",
                                 "Water consumption per user(m3) (adjusted)",
                                 "Years in operation WSS","Type of WSS",
                                 "Location","State-owned utility (SO)",
                                 "Local Government (LG)","Cummunity-based",
                                 "Private-owned utility","Private use",
                                 "WSS is in a rural area","Water rate (USD/m3)",
                                 "Urban population","Total population",
                                 "Rurality Index","Municipality Area (km2)",
                                 "Altitude (m)","Distance to nearest market (km)",
                                 "Unsatisfied Basic Needs Index",
                                 "Multidimensional Poverty Index",
                                 "Infant mortality rate","Aqueduct coverage (%)",
                                 "Sanitation coverage (%)","Sewage coverage (%)",
                                 "Municipality income (USD)")

treated <- count(data, treated_1)
knitr::kable(treated,format="latex")

varlist <- c("cons_po","cons_ph","cons_pf","cons_fix","cons_phcut1",
             "cons_phcut5","cons_phcut10","users_fix","n_sub", "start_org",
             "typeorg1","typeorg2","typeorg3","typeorg4","rural_org",
             "bill_pf","ig","ih","dist_border_1", "temperature","rainfall",
             "dist_mag_km","pobl_tot","indrural","areaoficialkm2",
             "altura","dismdo","nbi","IPM","TMI","tacued","taseo","talcan",
             "y_corr_usd")

stats_treated <- data %>%
  filter(treated_1 == 1) %>%
  select(one_of(varlist)) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            `Std. Dev.` = sd(value, na.rm = TRUE),
            Count = n())

stats_control <- data %>%
  filter(treated_1 == 0) %>%
  select(one_of(varlist)) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            `Std. Dev.` = sd(value, na.rm = TRUE),
            Count = n())

stargazer::stargazer(cbind(stats_control,stats_treated), type = "latex",
                     col.names = c('Variable', 'Mean', 'Std. Dev.','N',
                                   'Variable','Mean', 'Std. Dev.','N'),
                     covariate.labels = attr(data, "model.varnames"))


knitr::kable(cbind(stats_control,stats_treated), digits = 2,format ="latex",
             col.names = c('Variable', 'Mean', 'Std. Dev.','N',
                           'Variable','Mean', 'Std. Dev.','N'),
             align = "lccclccc", escape = F, label = "tab:summarystats",
             row.names = FALSE,
             caption = "Summary Statistics.",
             varlabels =T,
             booktabs = TRUE)

#Functions
cluster_se <- function(mod, cluster, type = "CR2") {
  vcov <- vcovCR(mod, cluster = cluster, type = type)
  coef_test(mod, vcov = vcov) %>%
    rownames_to_column(var = "term") %>%
    as_tibble() %>%
    select(term, estimate = beta, std.error = SE)
}
calc_diffs <- function(x) {
  # programmatically create the formula for lm
  f <- quo(!!sym(x) ~ treated_1)
  mod <- lm(f, data = data)  # nolint
  out <- cluster_se(mod, cluster = data[["wss"]])
  out[["response"]] <- x
  out
}

## Differences in means
group_diffs <- map_dfr(varlist, calc_diffs) %>%
  select(response, term, estimate, std.error) %>%
  mutate(term = str_replace(term, "^treated_1", ""))


fmt_num <- function(x) {
  prettyNum(x, digits = 3, format = "f", big.mark = ",", drop0trailing = FALSE)
}



group_diffs %>%
  mutate(estimate = str_c(fmt_num(estimate), " (", fmt_num(std.error), ")")) %>%
  select(-std.error) %>%
  spread(term, estimate) %>%
  knitr::kable(digits = 3,format ="latex", label="tab:balancecheck",
               col.names = c('Variable', 'Control Mean', 'Difference'),
               align = "lcc",
               caption = "Balance check",
               booktabs = TRUE) %>%
  kable_styling(full_width = FALSE)


ggplot(filter(group_diffs, term != "(Intercept)"),
       aes(x = term, y = estimate,
           ymin = estimate - 2 * std.error,
           ymax = estimate + 2 * std.error)) +
  geom_hline(yintercept = 0, colour = "white", size = 1) +
  geom_pointrange() +
  facet_grid(response ~ ., scales = "free_y")




data %>% # Standard Deviations
  pull(cons_ph) %>% #Pull out the column Citation
  mean() %>%
  signif(4) #Numbers


ggplot(data, aes(x=cons_pf))+
  geom_histogram(color = "black", bins = 10, alpha=0.8)+
  geom_vline(aes(xintercept =  mean(data$cons_pf,na.rm = TRUE)), linetype = 1, colour="red")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))+
  ggtitle("Consumption per user (original)")+
  xlab("Cubic Meters")+ ylab("Count") 




hist_cons_po <- gghistogram(data, x = "cons_po", 
                           fill = "#7D407F",
                           binwidth = 1,
                           color = "black",
                           add_density = F,
                           alpha = 0.2) +
  labs(title="Consumption per user (original)") +
  labs(x="Cubic Meters", y="Count")

hist_cons_pf <- gghistogram(match_data, x = "cons_pf", 
                            fill = "#00AFBB",
                            binwidth = 1,
                            color = "black",
                            add_density = F,
                            alpha = 0.2) +
  labs(title="Consumption per user (fixed)") +
  labs(x="Cubic Meters", y="Count")

hist_cons_pcut1 <- gghistogram(match_data, x = "cons_phcut1", 
                            fill = "#00AFBB",
                            binwidth = 1,
                            color = "black",
                            add_density = TRUE,
                            alpha = 0.2) +
  labs(title="Consumption per user (Cut 1%)") +
  labs(x="Cubic Meters", y="Count")

hist_cons_pcut5 <- gghistogram(match_data, x = "cons_phcut5", 
                               fill = "#FD3A4A",
                               binwidth = 1,
                               color = "black",
                               add_density = TRUE,
                               alpha = 0.5) +
  labs(title="Consumption per user (Cut 5%)") +
  labs(x="Cubic Meters", y="Count")



summary <- data %>%
  select(distance_1,distance_2,distance_3)

stargazer(as.data.frame(summary),
          summary.stat = c("n", "mean", "sd", "min", "max"),
          type = "latex", digits=2,flip = FALSE,
          covariate.labels = c("distance_1", "distance_2","distance_3" )) %>%
  cat(., file = "summary_stats.tex")


## APA theme to figures
apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

dist.beh.ps <- gghistogram(data, x = "distance_1", 
                           fill = "#00AFBB",
                           binwidth = 2,
                           color = "black",
                           add_density = TRUE,
                           alpha = 0.2) +
  labs(title="Distance") +
  labs(x="kilometers", y="Count")

# Covariates that explain the treatment status at baseline.

data_pre <- data %>%
  filter(post==0) %>%
  select(id_wss,post,distance, users_fix, temperature, rainfall,
         y_corr_usd,indrural,altura,areaoficialkm2,IPM,
         cons_po,cons_pf,cons_ph,cons_phcut1,cons_phcut5,cons_phcut10,
         cons_phcut25,bill_po,bill_pf) 

compareGroups(treated ~ ., data = data)


sum_data <-  data %>%
  select(treated, id_mpio ,distance, users_fix, temperature, rainfall,
                     y_corr_usd,indrural,altura,areaoficialkm2,IPM) 

sum_data <- sum_data %>%
  group_by(post) %>%
  sumarize()



  




match_var <- match_data %>%
  select(id_mpio ,distance, users_fix, temperature, rainfall,
                   y_corr_usd,indrural,altura,areaoficialkm2,IPM)



require(MatchIt) 

m.out.nn <- matchit(treated~match_var, method = "nearest",
                    data = match_data, ratio = 1, replace=TRUE)



detach(data)