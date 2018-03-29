###############################################################################
##
## nonlinear_regression.R
## 
## Purpose: Run a set of non-linear regression models on several example data
##          sets and save the resulting tables and visualizations.
##
## Depends on:  01_correlation.csv
##              02_exponential_decay.csv
##              03_sine_regression.csv
##              04_sine_regression_casualties.csv
##              05_bin_logit_conflict.csv
##              06_poisson_sigacts.csv
##              07_bin_logit_alliance.csv
##
## Function:  Loads each data set, runs the appropriate statistical model,
##            and saves output tables and visualizations to disk.
##
## Output:  PNG files for each model visualization, and HTML files for
##          Microsoft Word for each model output.
##
###############################################################################

###############################################################################
##
## Setting up workspace
##
###############################################################################

## Clear previous workspace, if any
rm(list=ls())

## Set working directory
os_detect <- Sys.info()['sysname']
if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/Research/StatsChapter')
} 

## Load packages for analysis
pacman::p_load(
  data.table, tidyverse, ggplot2, stargazer, easynls, pscl
)



###############################################################################
##
## Section 1.1: introduction to linear regression
##
###############################################################################

## Read in spring data
spring_data <- read_csv('./Data/01_correlation.csv')

#######
#
# Spring data correlation
#
#######

## Print data as a tibble
print(spring_data)

## Calculate and print correlation matrix
print(cor(spring_data))

## Generate a plot visualizing the data
spring_cor_plot <- ggplot(
  aes(x = x, y = y)
  , data = spring_data) + 
  geom_point() + 
  annotate(
    'text'
    , x = 100
    , y = 0.75
    , label = 'Correlation coefficient:\n 0.999272'
    , hjust = 0) + 
  ggtitle('Spring data scatterplot') + 
  theme_bw()

## Print the plot to console
plot(spring_cor_plot)

## Save to file
dev.off()
png(file = './Plots/spring_data_correlation.png', width = 600, height = 400)
plot(spring_cor_plot)
dev.off()


#######
#
# Spring data regression: model fitting
#
#######

## Generate model
spring_model <- lm(y ~ x, data = spring_data)

## Summarize model results
print(summary(spring_model))

## Generate a new plot containing the fitted line
spring_plot2 <- ggplot(
  aes(x = x, y = y)
  , data = spring_data) + 
  geom_point() + 
  annotate(
    'text'
    , x = 100
    , y = 0.75
    , label = 'R^2 == 0.998'
    , parse = T
    , hjust = 0) + 
  ggtitle('Spring data scatterplot with fitted line') + 
  stat_smooth(method = 'lm', col = 'black', se = F) +
  theme_bw()

## Print the plot to console
plot(spring_plot2)

## Save to file
dev.off()
png(file = './Plots/spring_data_regression.png', width = 600, height = 400)
plot(spring_plot2)
dev.off()


#######
#
# SIGACTS OLS regression: model fitting
#
#######

sigacts_data <- read_csv('./Data/06_poisson_sigacts.csv')

## Generate model
sigacts_model_ols <- lm(sigacts_2008 ~ literacy, data = sigacts_data)

## Summarize model results
print(summary(sigacts_model_ols))

## Generate HTML tables with diagnostic statistics
stargazer(
  sigacts_model_ols
  , title = 'Explaining SIGACTS in Philippines'
  , covariate.labels = c('Literacy rate')
  , type = 'html'
  , out = './Tables/sigacts_regression_ols.html'
  )

## Generate a new plot containing the fitted line
sigacts_plot <- ggplot(
  aes(x = literacy, y = sigacts_2008)
  , data = sigacts_data) + 
  geom_point() + 
  annotate(
    'text'
    , x = 77
    , y = 55
    , label = 'R^2 == 0.077'
    , parse = T
    , hjust = 0) + 
  xlab('Literacy %') + 
  ylab('Number of violent events') + 
  ggtitle('Literacy and violent events in the Philippines') + 
  stat_smooth(method = 'lm', col = 'black', se = F) +
  theme_bw()

## Print the plot to console
print(sigacts_plot)

## Save to file
dev.off()
png(file = './Plots/sigacts_regression_ols.png', width = 600, height = 400)
plot(sigacts_plot)
dev.off()


###############################################################################
##
## Section 1.2: modeling nonlinear data
##
###############################################################################

recovery_data <- read_csv('./Data/02_exponential_decay.csv')


##############
#
# Recovery data: correlation
#
##############

## Print data as a tibble
print(recovery_data)

## Calculate and print correlation matrix
print(cor(recovery_data))

## Generate a new plot containing the fitted line
recovery_plot <- ggplot(
  aes(x = T, y = Y)
  , data = recovery_data) + 
  geom_point() + 
  annotate(
    'text'
    , x = 30
    , y = 30
    , label = 'Correlation coefficient:\n -0.941'
    , parse = F
    , hjust = 0) + 
  xlab('Days in the hospital') + 
  ylab('Recovery index') + 
  ggtitle('Linear model: predicted recovery index') + 
  theme_bw()

## Print the plot to console
print(recovery_plot)

## Save to file
dev.off()
png(file = './Plots/recovery_visualization.png', width = 600, height = 400)
plot(recovery_plot)
dev.off()


##############
#
# Recovery data linear regression: model fitting
#
##############

## Generate model
recovery_model <- lm(Y ~ T, data = recovery_data)

## Summarize model results
print(summary(recovery_model))

## Generate HTML tables with diagnostic statistics
stargazer(
  recovery_model
  , title = 'Estimating recovery index'
  , covariate.labels = 'Days in hospital'
  , type = 'html'
  , out = './Tables/recovery_regression.html'
  )

stargazer(
  anova(recovery_model)
  , title = 'ANOVA model diagnostic statistics'
  , type = 'html'
  , out = './Tables/recovery_anova.html'
  )


##############
#
# Recovery data linear regression: residual analysis
#
##############

## Get residuals and percent relative error
recovery_residuals <- mutate(
  recovery_data
  , index = 1:nrow(recovery_data)
  , predicted = predict(recovery_model)
  , residuals = Y - predicted
  , pct_relative_error = residuals / Y * 100
)

## Print table to console
print(recovery_residuals)

## Generate a new plot showing residuals
recovery_res_plot <- ggplot(
  aes(x = T, y = residuals)
  , data = recovery_residuals) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  ylim(-10, 10) + 
  xlab('Time in hospital') +
  ylab('Residuals') + 
  ggtitle('Linear model residuals by time in hospital') + 
  theme_bw()

## Print the plot to console
plot(recovery_res_plot)

## Save to file
dev.off()
png(file = './Plots/recovery_residuals.png', width = 600, height = 400)
plot(recovery_res_plot)
dev.off()


###############################################################################
##
## Section 1.2: quadratic regression modeling
##
###############################################################################

##############
#
# Recovery data quadratic regression: model fitting
#
##############

## Generate model
recovery_model2 <- lm(Y ~ T + I(T^2), data = recovery_data)

## Summarize model results
print(summary(recovery_model2))

## Generate HTML tables with diagnostic statistics
stargazer(
  recovery_model2
  , title = 'Estimating recovery index'
  , covariate.labels = c('Days in hospital', 'Days in hospital (squared)')
  , type = 'html'
  , out = './Tables/recovery_regression2.html'
)

stargazer(
  anova(recovery_model2)
  , title = 'ANOVA model diagnostic statistics'
  , type = 'html'
  , out = './Tables/recovery_anova2.html'
)


##############
#
# Recovery data quadratic regression: residual analysis
#
##############


## Get residuals and percent relative error
recovery_residuals2 <- mutate(
  recovery_data
    , index = 1:nrow(recovery_data)
    , predicted = predict(recovery_model2)
    , residuals = Y - predicted
    , pct_relative_error = residuals / Y * 100
  )

## Print table to console
print(recovery_residuals2)


## Generate a new plot showing residuals
recovery_res_plot2 <- ggplot(
  aes(x = T, y = residuals)
  , data = recovery_residuals2) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  ylim(-10, 10) + 
  xlab('Days in hospital') +
  ylab('Residuals') + 
  ggtitle('Quadratic model residuals by time in hospital') + 
  theme_bw()

## Print the plot to console
plot(recovery_res_plot2)

## Save to file
dev.off()
png(file = './Plots/recovery_residuals2.png', width = 600, height = 400)
plot(recovery_res_plot2)
dev.off()


##############
#
# Recovery data quadratic regression: visual diagnostics
#
##############

## Generate predicted recovery index for T = 1:120
recovery_pred_data <- tibble(
  T = 1:120
  , predicted = predict(recovery_model2, newdata = tibble(T = 1:120))
)

## Generate a new plot containing the fitted line
recovery_pred_plot <- ggplot(
  aes(x = T, y = predicted)
  , data = recovery_pred_data) + 
  geom_point() + 
  xlab('Days in hospital') +
  ylab('Predicted recovery index') + 
  ggtitle('Quadratic model: predicted recovery index') + 
  theme_bw()

## Print the plot to console
plot(recovery_pred_plot)

## Save to file
dev.off()
png(file = './Plots/recovery_predicted_quadratic.png', width = 600, height = 400)
plot(recovery_pred_plot)
dev.off()


###############################################################################
##
## Section 1.4: exponential decay
##
###############################################################################

##############
#
# Recovery data exponential regression: model fitting
#
##############

## Generate model
recovery_model3 <- nls(
  Y ~ a * (exp(b * T))
  , data = recovery_data
  , start = c(
    a=1
    , b=0.05
  )
  , trace = T
)


## Summarize model results
print(recovery_model3)

## Generate prediction points to visualize fitted line
exponential_predline <- tibble(
  line_x = seq(1, 75, by = 0.1)
  , line_y = predict(recovery_model3, newdata = tibble(T = seq(1, 75, by = 0.1)))
)

## Generate a new plot containing the fitted line
recovery_plot3 <- ggplot(
  aes(x = T, y = Y)
  , data = recovery_data) + 
  geom_point() + 
  geom_point(aes(x = line_x, y = line_y), data = exponential_predline, size = 0.5) + 
  # geom_smooth(method = 'lm', formula = (Y ~ exp(T))) +
  xlab('Days in hospital') + 
  ylab('Recovery index') + 
  ggtitle('Exponential decay model: predicted recovery index') + 
  theme_bw()

## Print the plot to console
print(recovery_plot3)

## Save to file
dev.off()
png(file = './Plots/recovery_predicted_exponential.png', width = 600, height = 400)
plot(recovery_plot3)
dev.off()


##############
#
# Recovery data exponential regression: residual analysis
#
##############

## Get residuals and percent relative error
recovery_residuals3 <- mutate(
  recovery_data
  , predicted = 58.6065*exp(-0.0396*T)
  , residuals = Y - predicted
  , pct_relative_error = residuals / Y * 100
)

## Print table to console
print(recovery_residuals3)


## Generate a new plot showing residuals by time in hospital
recovery_res_plot3 <- ggplot(
  aes(x = T, y = residuals)
  , data = recovery_residuals3) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  ylim(-5, 5) + 
  xlab('Time in hospital') +
  ylab('Residuals') + 
  ggtitle('Exponential decay model residuals by days in hospital') + 
  theme_bw()

## Print the plot to console
plot(recovery_res_plot3)

## Save to file
dev.off()
png(file = './Plots/recovery_residuals3.png', width = 600, height = 400)
plot(recovery_res_plot3)
dev.off()


## Generate a new plot showing residuals by recovery index
recovery_res_plot4 <- ggplot(
  aes(x = Y, y = residuals)
  , data = recovery_residuals3) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  ylim(-5, 5) + 
  xlab('Recovery index') +
  ylab('Residuals') + 
  ggtitle('Exponential decay model residuals by recovery index') + 
  theme_bw()

## Print the plot to console
plot(recovery_res_plot4)

## Save to file
dev.off()
png(file = './Plots/recovery_residuals4.png', width = 600, height = 400)
plot(recovery_res_plot4)
dev.off()


##############
#
# Recovery data exponential regression: visual diagnostics
#
##############

## Generate predicted recovery index for T = 1:120
recovery_pred_data2 <- tibble(
  T = 1:120
  , predicted = 58.6065*exp(-0.0396*1:120)
)

## Generate a new plot containing the fitted line
recovery_pred_plot <- ggplot(
  aes(x = T, y = predicted)
  , data = recovery_pred_data2) + 
  geom_point() + 
  xlab('Days in hospital') +
  ylab('Predicted recovery index') + 
  ggtitle('Quadratic model: predicted recovery index') + 
  theme_bw()

## Print the plot to console
plot(recovery_pred_plot)

###############################################################################
##
## Section 1.5: sine regression of shipping data
##
###############################################################################

shipping_data <- read_csv('./Data/03_sine_regression_shipping.csv')

##############
#
# Shipping data correlation
#
##############

## Print data as a tibble
print(shipping_data)

## Calculate and print correlation matrix
print(cor(shipping_data))

## Generate a data scatterplot
shipping_cor_plot <- ggplot(
  aes(x = Month, y = UsageTons)
  , data = shipping_data) + 
  geom_point() + 
  annotate(
    'text'
    , x = 2
    , y = 35
    , label = 'Correlation coefficient:\n 0.673'
    , parse = F
    , hjust = 0) + 
  ylim(0,40) + 
  xlab('Month') + 
  ylab('Usage (tons)') + 
  ggtitle('Shipping data scatterplot') + 
  theme_bw()

## Print the plot to console
print(shipping_cor_plot)

## Save to file
dev.off()
png(file = './Plots/shipping_data_correlation.png', width = 600, height = 400)
plot(shipping_cor_plot)
dev.off()


##############
#
# Shipping data: visualisation with splines
#
##############

## Generate data for a line based on spline fitting
shipping_splines <- as.data.frame(
  do.call(
    'cbind'
    , spline(shipping_data$Month, shipping_data$UsageTons)
    )
  )

## Generate a new plot containing a fitted line
shipping_spline_plot <- ggplot(
  aes(x = Month, y = UsageTons)
  , data = shipping_data) + 
  geom_point() + 
  annotate(
    'text'
    , x = 2
    , y = 35
    , label = 'Correlation coefficient:\n 0.673'
    , parse = F
    , hjust = 0) +
  geom_line(data = shipping_splines, aes(x = x, y = y)) + 
  ylim(0,40) + 
  xlab('Month') + 
  ylab('Usage (tons)') + 
  ggtitle('Shipping data spline plot') + 
  theme_bw()

## Print the plot to console
print(shipping_spline_plot)

## Save to file
dev.off()
png(file = './Plots/shipping_data_splines.png', width = 600, height = 400)
plot(shipping_spline_plot)
dev.off()


##############
#
# Shipping data: linear regression
#
##############

## Generate model
shipping_model1 <- lm(UsageTons ~ Month, data = shipping_data)

## Summarize model results
summary(shipping_model1)

## Generate HTML tables with diagnostic statistics
stargazer(
  shipping_model1
  , title = 'Estimating usage (tons)'
  , covariate.labels = c('Month')
  , type = 'html'
  , out = './Tables/shipping_regression1.html'
)

stargazer(
  anova(shipping_model1)
  , title = 'ANOVA model diagnostic statistics'
  , type = 'html'
  , out = './Tables/shipping_anova1.html'
)



##############
#
# Shipping data: sine regression
#
##############

## Generate model
shipping_model2 <- nls(
  UsageTons ~ a * Month + b*sin(c*Month) + d*cos(c*Month) + e
  , data = shipping_data
  , start = c(
    a=5
    , b=10
    , c=1
    , d=1
    , e=10
    )
  , trace = T
)


## Generate data for a line based on sinusoidal regression
shipping_predict <- tibble(
  y = predict(shipping_model2, newdata = tibble(Month = seq(1, 20, by = 0.01)))
  , x = seq(1, 20, by = 0.01)
)
  

## Generate a new plot containing a fitted line
shipping_sine_plot <- ggplot(
  aes(x = Month, y = UsageTons)
  , data = shipping_data) + 
  geom_point() + 
  geom_line(data = shipping_predict, aes(x = x, y = y)) + 
  ylim(0,40) + 
  xlab('Month') + 
  ylab('Usage (tons)') + 
  ggtitle('Sinusoidal model: predicting usage in metric tons') + 
  theme_bw()

## Print the plot to console
print(shipping_sine_plot)

## Save to file
dev.off()
png(file = './Plots/shipping_data_sine.png', width = 600, height = 400)
plot(shipping_sine_plot)
dev.off()


##############
#
# Shipping data: residuals analysis
#
##############

## Get residuals and percent relative error
shipping_residuals <- mutate(
  shipping_data
  , predicted = predict(shipping_model2)
  , residuals = UsageTons - predicted
  , pct_relative_error = residuals / UsageTons * 100
)


###############################################################################
##
## Section 1.5.1: sine regression of Afghanistan casualties data
##
###############################################################################

afghan_data <- read_csv('./Data/04_sine_regression_casualties.csv')

## Format and subset casualties data
afghan_data <- mutate(
  afghan_data
  , Date = as.Date(paste0(Year, '-', Month, '-', '01'), format = '%Y-%m-%d')
  ) %>% filter(
    Date >= as.Date('2006-01-01')
    , Date <= as.Date('2008-12-01')
  ) %>% mutate(
    DateIndex = 1:36
  )


##############
#
# Casualties data: visualization
#
##############

## Print data as a tibble
print(afghan_data)


## Generate a data scatterplot
afghan_plot <- ggplot(
  aes(x = Date, y = Casualties)
  , data = afghan_data) + 
  geom_point() + 
  xlab('Date') + 
  ylab('US casualties') + 
  ggtitle('US casualties in Afghanistan, 2001-204') + 
  theme_bw()

## Print the plot to console
print(afghan_plot)

## Save to file
dev.off()
png(file = './Plots/afghan_casualties_visualization.png', width = 600, height = 400)
plot(afghan_plot)
dev.off()


##############
#
# Casualties data: sine regression
#
##############

## Generate model
afghan_model <- nls(
  Casualties ~ a * DateIndex + b*sin(c*DateIndex) + d*cos(c*DateIndex) + e
  , data = afghan_data
  , start = c(
    a=1
    , b=10
    , c=1
    , d=10
    , e=1
  )
  , trace = T
)


## Generate data for a line based on sinusoidal regression
afghan_predict <- tibble(
  y = predict(afghan_model, newdata = tibble(DateIndex = seq(1, 36, by = 0.01)))
  , x = seq(1, 36, by = 0.01)
)


## Generate a new plot containing a fitted line
afghan_sine_plot <- ggplot(
  aes(x = DateIndex, y = Casualties)
  , data = afghan_data) + 
  geom_point() + 
  geom_line(data = afghan_predict, aes(x = x, y = y)) + 
  xlab('Month') + 
  ylab('US Casualties') + 
  ggtitle('Sinusoidal model: predicting usage in metric tons') + 
  theme_bw()

## Print the plot to console
print(afghan_sine_plot)

## Save to file
dev.off()
png(file = './Plots/afghan_data_sine.png', width = 600, height = 400)
plot(afghan_sine_plot)
dev.off()


##############
#
# Casualties data sine regression: residuals analysis
#
##############

## Get residuals and percent relative error
afghan_residuals <- mutate(
  afghan_data
  , predicted = predict(afghan_model)
  , residuals = Casualties - predicted
  , pct_relative_error = residuals / Casualties * 100
)

## Print table to console
print(afghan_residuals)


## Generate a new plot showing residuals by casualty count
afghan_res_plot <- ggplot(
  aes(x = Casualties, y = residuals)
  , data = afghan_residuals) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  xlab('US Casualties') +
  ylab('Residuals') +
  ggtitle('Sinusoidal model residuals by casualty count') +
  theme_bw()

## Print the plot to console
plot(afghan_res_plot)

## Save to file
dev.off()
png(file = './Plots/afghan_residuals.png', width = 600, height = 400)
plot(afghan_res_plot)
dev.off()


###############################################################################
##
## Section 1.5.2: logistic regression of war outcomes
##
###############################################################################

war_data <- read_csv('./Data/05_bin_logit_conflict.csv')


##############
#
# War data: logistic regression
#
##############

## Generate model
war_model <- glm(
  side_a ~ cd_pct
  , data = war_data
  , family = binomial(link = 'logit')
  )

## Summarize model results
print(summary(war_model))

## Generate HTML tables with diagnostic statistics
stargazer(
  war_model
  , title = 'Explaining international alliances'
  , covariate.labels = 'Shared IGO membership'
  , dep.var.caption = 'Presence of alliance'
  , type = 'html'
  , out = './Tables/war_regression.html')


###############################################################################
##
## Section 1.5.3: Poisson regression of SIGACTS counts
##
###############################################################################

sigacts_data <- read_csv('./Data/06_poisson_sigacts.csv')


##############
#
# SIGACTS data: histogram visualization
#
##############

## Generate a histogram plot
sigacts_hist <- ggplot(
  aes(x = sigacts_2008)
  , data = sigacts_data) + 
  geom_histogram() + 
  xlab('Count of SIGACTS in 2008') + 
  ggtitle('Histogram of SIGACTS counts') + 
  theme_bw()

## Print the plot to console
plot(sigacts_hist)

## Save to file
dev.off()
png(file = './Plots/sigacts_histogram.png', width = 600, height = 400)
plot(sigacts_hist)
dev.off()


##############
#
# SIGACTS data: Poisson regression
#
##############

## Generate model
sigacts_model <- glm(
  sigacts_2008 ~ ggi_2008 + literacy + poverty
  , data = sigacts_data
  , family = poisson
)


## Summarize model results
print(summary(sigacts_model))

## Generate HTML tables with diagnostic statistics
stargazer(sigacts_model, type = 'html', out = './Tables/sigacts_regression.html')



###############################################################################
##
## Section 1.6: logistic regression of alliance formation
##
###############################################################################

alliance_data <- read_csv('./Data/07_bin_logit_alliance.csv')

##############
#
# Alliance data: logistic regression
#
##############

## Generate model
alliance_model <- glm(
  alliance_present ~ igo_overlap
  , data = alliance_data
  , family = binomial(link = 'logit')
  )

sigacts_output <- mutate(
  tidy(sigacts_model)
  , coef_oddsratio = exp(estimate)
)



## Summarize model results
print(summary(alliance_model))

data_2005 %>%
  ggplot(aes(igo_overlap, alliance_present)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Number of shared IGO memberships") +
  ylab("Probability of sharing an alliance tie") + 
  theme_bw()

## Generate HTML tables with diagnostic statistics
stargazer(alliance_model, type = 'html', out = './Tables/alliance_regression.html')
stargazer(sigacts_output, type = 'html', out = './Tables/alliance_oddsratios.html')

