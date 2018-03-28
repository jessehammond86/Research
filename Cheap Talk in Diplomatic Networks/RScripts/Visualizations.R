
## Visualizations
pacman::p_load(data.table, ggplot2, stargazer, xtable, R2HTML)

################################################################################
## Table: Example ICEWS events
################################################################################

sample_icews <- data.table(
  datecol = c(as.character(rep(20150101, )))
  , sourcename = c('United Kingdom', 'United States', 'Palestinian Territories', 'Israel')
  , targetname = c('Pakistan', 'Syria', 'Israel', 'Nigeria')
  , actiondesc = c('Consult', 'Employ Aerial Weapons', 'Bring Lawsuit', 'Provide aid')
  , actioncode = c(40, 195, 115, 70)
  , rootcode = c(4, 19, 1, 7)
  , goldstein = c(1, -10, -2, 7)
)

HTML(
  sample_icews
  , file = './PlotsTables/sample_icews.html'
)


################################################################################
## Table: ICEWS event types and Goldstein scores
################################################################################

cameo_table <- fread('./Data/AnalysisData/CameoScores.csv')
HTML(
  cameo_table
  , file = './PlotsTables/cameo_table.html'
)
rootcode_table <- cameo_table[, round(mean(GoldScore), 2), by = RootCode]
rootcode_table[
  , rootcode_type := c(
    'Make Public Statement', 'Appeal', 'Express Intent to Cooperate'
    , 'Consult', 'Engage in Diplomatic Cooperation', 'Engage in Material Cooperation'
    , 'Provide Aid', 'Yield', 'Investigate', 'Demand', 'Disapprove', 'Reject'
    , 'Threaten', 'Protest', 'Exhibit Military Posture', 'Reduce Relations'
    , 'Coerce', 'Assault', 'Fight', 'Engage in Unconventional Mass Violence'
  )
  ]
setcolorder(rootcode_table, c(1, 3,2))
setnames(rootcode_table, c('CAMEO Root Code', 'Event Category', 'Mean Goldstein Score'))
HTML(
  rootcode_table
  , file = './PlotsTables/rootcode_table.html'
)


################################################################################
## Figure: temporal overlap of data
################################################################################
time_data <- data.table(
  dataset = as.factor(c(
    'Dyadic MID', 'Alliance Formation', 'External Support', 'ICEWS Events'
    , 'Trade Flow', 'IGO Overlap', 'Religious Distance', 'Polity Difference'
    , 'Relative Capacity'
  ))
  , time_start = c(
    1993, 1990, 1990, 1995
    , 1990, 1990, 1990, 1990, 1990
  )
  , time_end = c(
    2001, 2012, 2009, 2015
    , 2009, 2005, 2010, 2015, 2010
  )
  # , open_start = c(
  #   0, 1, 1, 0
  #   , 1, 1, 1, 1, 1
  # )
  # , line_color = c(
  #   'red', 'black', 'black', 'red'
  #   , 'black', 'black', 'black', 'black', 'black'
  # )
)

time_data2 <- data.table(
  dataset = as.factor(c(
    'Dyadic MID', 'Alliance Formation', 'External Support', 'ICEWS Events'
    , 'Trade Flow', 'IGO Overlap', 'Religious Distance', 'Polity Difference'
    , 'Relative Capacity'
  ))
  , time_start = rep(1995, 9)
  , time_end = rep (2001, 9)
)


sub_data <- data.table(
  dataset = c(2, 4)
  , time_start = c(1995, 1995)
  , time_end = c(2001, 2001)
  , line_color = 'red'
)



data_timeplot <- ggplot(time_data) +
  geom_segment(
    aes(
      x = time_start, xend = time_end
      , y = dataset, yend = dataset
      )
    # , colour = time_data$line_color
    , size = 1.25
    , color = 'grey60'
    ) +
  geom_segment(
    aes(
      x = time_start, xend = time_end
      , y = dataset, yend = dataset
    )
    , colour = 'blue'
    , size = 2
    , data = time_data2
  ) +
  geom_vline(
    xintercept = c(1995, 2001)
    , colour = 'grey80'
  ) +
  labs(x = 'Year', y = 'Data Source') +
  theme_bw()

data_timeplot

# dev.off()
# png(filename = './PlotsTables/DataTimespan.png')
# data_timeplot
# dev.off()

################################################################################
## Very crude tables: mean and SD of variables
################################################################################

HTML(
    yearly_data_relevant[
      , list(
        mean(mean_weight_pos, na.rm = T)
        , mean(mean_weight_neg, na.rm = T)
        , sum(alliance_formed, na.rm = T)
        , mean(trade_flow, na.rm = T)
        , mean(igo_overlap, na.rm = T)
        , mean(relig_distance, na.rm = T)
        , mean(polity_diff, na.rm = T)
        , sum(joindem, na.rm = T)
        , mean(relcap, na.rm = T)
        , sum(support_state == 1, na.rm = T)
        , sum(support_state == -1, na.rm = T)
        , sum(mid_start)
        )
      ]
    , digits = 2
    , file = './PlotsTables/SummaryMeans.html'
)

HTML(
  yearly_data_relevant[
    , list(
      sd(mean_weight_pos, na.rm = T)
      , sd(mean_weight_neg, na.rm = T)
      , sum(alliance_formed, na.rm = T)
      , sd(trade_flow, na.rm = T)
      , sd(igo_overlap, na.rm = T)
      , sd(relig_distance, na.rm = T)
      , sd(polity_diff, na.rm = T)
      , sum(joindem, na.rm = T)
      , sd(relcap, na.rm = T)
      , sum(support_state == 1, na.rm = T)
      , sum(support_state == -1, na.rm = T)
      , sum(mid_start)
    )
    ]
  , digits = 2
  , file = './PlotsTables/SummarySD.html'
)


################################################################################
## Tables: MID onset models
################################################################################

summary(mid_logit_1)
summary(mid_logit_2)
summary(mid_logit_30)
summary(mid_logit_31)
summary(mid_logit_32)
summary(mid_logit_33)

stargazer(
  mid_logit_1, mid_logit_2, mid_logit_30, mid_logit_31, mid_logit_32, mid_logit_33
  , digits = 2
  , type = 'html'
  , out = './PlotsTables/DyadicMIDTables.html'
  )


################################################################################
## Additional columns: training/testing fit statistics
################################################################################

########## Calculate model fit on TRAINING data

## Model 1 (baseline)
pred_mid_m1 <- data.frame(
  lag_midstart = mid_train_relevant$lag_midstart
  , pred = predict(mid_logit_1, newdata = mid_train_relevant, type = 'response')
)

## Model 2 (no weight)
pred_mid_m2 <- data.frame(
  lag_midstart = mid_train_relevant$lag_midstart
  , pred = predict(mid_logit_2, newdata = mid_train_relevant, type = 'response')
)

## Model 3.0 (positive weight)
pred_mid_m30 <- data.frame(
  lag_midstart = mid_train_relevant$lag_midstart
  , pred = predict(mid_logit_30, newdata = mid_train_relevant, type = 'response')
)

## Model 3.1 (negative weight)
pred_mid_m31 <- data.frame(
  lag_midstart = mid_train_relevant$lag_midstart
  , pred = predict(mid_logit_31, newdata = mid_train_relevant, type = 'response')
)

## Model 3.2 (positive + negative weight)
pred_mid_m32 <- data.frame(
  lag_midstart = mid_train_relevant$lag_midstart
  , pred = predict(mid_logit_32, newdata = mid_train_relevant, type = 'response')
)

## Model 3.3 (positive * negative weight)
pred_mid_m33 <- data.frame(
  lag_midstart = mid_train_relevant$lag_midstart
  , pred = predict(mid_logit_33, newdata = mid_train_relevant, type = 'response')
)

## Calculate AUC
auc(pred_mid_m1$lag_midstart, pred_mid_m1$pred)
auc(pred_mid_m2$lag_midstart, pred_mid_m2$pred)
auc(pred_mid_m30$lag_midstart, pred_mid_m30$pred)
auc(pred_mid_m31$lag_midstart, pred_mid_m31$pred)
auc(pred_mid_m32$lag_midstart, pred_mid_m32$pred)
auc(pred_mid_m33$lag_midstart, pred_mid_m33$pred)




########## Calculate model fit on TESTING data

## Model 1 (baseline)
pred_mid_m1 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_1, newdata = mid_test_relevant, type = 'response')
)

## Model 2 (no weight)
pred_mid_m2 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_2, newdata = mid_test_relevant, type = 'response')
)

## Model 3.0 (positive weight)
pred_mid_m30 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_30, newdata = mid_test_relevant, type = 'response')
)

## Model 3.1 (negative weight)
pred_mid_m31 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_31, newdata = mid_test_relevant, type = 'response')
)

## Model 3.2 (positive + negative weight)
pred_mid_m32 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_32, newdata = mid_test_relevant, type = 'response')
)

## Model 3.3 (positive * negative weight)
pred_mid_m33 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_33, newdata = mid_test_relevant, type = 'response')
)

## Calculate AUC
auc(pred_mid_m1$lag_midstart, pred_mid_m1$pred)
auc(pred_mid_m2$lag_midstart, pred_mid_m2$pred)
auc(pred_mid_m30$lag_midstart, pred_mid_m30$pred)
auc(pred_mid_m31$lag_midstart, pred_mid_m31$pred)
auc(pred_mid_m32$lag_midstart, pred_mid_m32$pred)
auc(pred_mid_m33$lag_midstart, pred_mid_m33$pred)


################################################################################
## Figure: MID interaction term
################################################################################

par(mfrow = c(1,1))
interplot(
  mid_logit_33
  , var1 = 'mean_weight_neg'
  , var2 = 'mean_weight_pos'
) +
  xlab('Level of weighted cooperation') +
  ylab('Coefficient of weighted conflict')

coop_interact <- interplot(
  mid_logit_33
  , var1 = 'mean_weight_pos'
  , var2 = 'mean_weight_neg'
  , hist = T
) +
  xlab('Level of weighted conflict') +
  ylab('Coefficient of weighted cooperation') +
  theme_bw()
coop_interact

dev.off()
png(filename = './PlotsTables/CoopInteract.png')
coop_interact
dev.off()


################################################################################
## Table: Alliance Heckman models
################################################################################

stargazer(
  ally_1, ally_2, ally_30, ally_31, ally_32, ally_33
  , digits = 2
  , type = 'html'
  , selection.equation = T
  , out = './PlotsTables/AllianceSelection.html'
  )

stargazer(
  ally_1, ally_2, ally_30, ally_31, ally_32, ally_33
  , digits = 2
  , type = 'html'
  , selection.equation = F
  , out = './PlotsTables/AllianceOutcome.html'
)


AICsummary(ally_1)
summary(ally_2)
summary(ally_30)
summary(ally_31)
summary(ally_32)
summary(ally_33)


################################################################################
## Calculate model fit on TRAINING data
################################################################################

## Model 1: no weight
pred_ally_m1 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_1, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 2: no weight
pred_ally_m2 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_2, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.0: positive weight
pred_ally_m30 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_30, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.1: negative weight
pred_ally_m31 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_31, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.2: positive + negative weight
pred_ally_m32 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_32, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.3: positive * negative weight
pred_ally_m33 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_33, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)


## Calculate AUC
auc(pred_ally_m1$lag_allianceformation, pred_ally_m1$pred)
auc(pred_ally_m2$lag_allianceformation, pred_ally_m2$pred)
auc(pred_ally_m30$lag_allianceformation, pred_ally_m30$pred)
auc(pred_ally_m31$lag_allianceformation, pred_ally_m31$pred)
auc(pred_ally_m32$lag_allianceformation, pred_ally_m32$pred)
auc(pred_ally_m33$lag_allianceformation, pred_ally_m33$pred)


################################################################################
## Calculate model fit on TEST data
################################################################################

## Model 1: no weight
pred_ally_m1 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_1, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 2: no weight
pred_ally_m2 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_2, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.0: positive weight
pred_ally_m30 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_30, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.1: negative weight
pred_ally_m31 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_31, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.2: positive + negative weight
pred_ally_m32 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_32, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.3: positive * negative weight
pred_ally_m33 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_33, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)


## Calculate AUC
auc(pred_ally_m1$lag_allianceformation, pred_ally_m1$pred)
auc(pred_ally_m2$lag_allianceformation, pred_ally_m2$pred)
auc(pred_ally_m30$lag_allianceformation, pred_ally_m30$pred)
auc(pred_ally_m31$lag_allianceformation, pred_ally_m31$pred)
auc(pred_ally_m32$lag_allianceformation, pred_ally_m32$pred)
auc(pred_ally_m33$lag_allianceformation, pred_ally_m33$pred)

## t-test analyzing cooperation within long-term alliances vs outside
ally_ttest_data <- rbind(ally_train_relevant, ally_test_relevant)
ally_ttest <- t.test(
  ally_ttest_data[constant_alliance == 1, mean_weight_pos]
  , ally_ttest_data[constant_alliance == 0, mean_weight_pos]
)


################################################################################
################################################################################
## t-test analyzing cooperation within long-term alliances vs outside
ally_ttest_data <- rbind(ally_train_relevant, ally_test_relevant)
t.test(
  ally_ttest_data[constant_alliance == 1, mean_weight_pos]
  , ally_ttest_data[constant_alliance == 0, mean_weight_pos]
)


################################################################################
## Table: Multinomial logit output
################################################################################

stargazer(
  inter_1, inter_2, inter_30, inter_31, inter_32, inter_33
  , digits = 2
  , type = 'html'
  , selection.equation = T
  , out = './PlotsTables/InterventionTables.html'
)


dev.off()
png(filename = './PlotsTables/InterInteract.png')
plot(
  effect(
    "mean_weight_pos*mean_weight_neg"
    , inter_33
    , xlevels = list(
      mean_weight_pos = seq(0, 9, 5)
      , mean_weight_neg = seq(0, 9, 5)
    )
  )
  , xlab = 'Low-intensity cooperation'
  , ylab = 'Expected outcome (probability)'
  , main = NULL
)
dev.off()



plot(
  effect(
    "mean_weight_neg*mean_weight_pos"
    , inter_33
    , xlevels = list(
      mean_weight_pos = seq(0, 9, 5)
      , mean_weight_neg = seq(0, 9, 5)
    )
  )
)



## Model 2: no weight
pred_inter_m2 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_2, newdata = inter_test_relevant)
)

## Model 3.0: positive weight
pred_inter_m30 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_30, newdata = inter_test_relevant)
)

## Model 3.1: negative weight
pred_inter_m31 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_31, newdata = inter_test_relevant)
)

## Model 3.2: positive + negative weight
pred_inter_m32 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_32, newdata = inter_test_relevant)
)

## Model 3.3: positive * negative weight
pred_inter_m33 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_33, newdata = inter_test_relevant)
)

##### Generate confusion matrices
nlcv::confusionMatrix(pred_inter_m2$lag_supportstate, pred_inter_m2$pred, dnn = c('Observed', 'Predicted'))
confusionMatrix(table(pred_inter_m33$lag_supportstate, pred_inter_m33$pred, dnn = c('Observed', 'Predicted')))



################################################################################
################################################################################
## Visualizing interaction term for multinomial logit
plot(
  effect(
    "mean_weight_pos*mean_weight_neg"
    , inter_33
    , xlevels = list(
      mean_weight_pos = seq(0, 9, 5)
      , mean_weight_neg = seq(0, 9, 5)
    )
  )
)
Effect(c('mean_weight_pos', 'mean_weight_neg'), inter_33)
