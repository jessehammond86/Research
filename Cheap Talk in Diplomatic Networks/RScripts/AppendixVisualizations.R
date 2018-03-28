
## Visualizations
pacman::p_load(data.table, ggplot2, stargazer, xtable, R2HTML)


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
  , out = './PlotsTables/DyadicMIDAppendix.html'
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
  , out = './PlotsTables/AllianceSelectionAppendix.html'
  )

stargazer(
  ally_1, ally_2, ally_30, ally_31, ally_32, ally_33
  , digits = 2
  , type = 'html'
  , selection.equation = F
  , out = './PlotsTables/AllianceOutcomeAppendix.html'
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
confusionMatrix(table(pred_inter_m2$lag_supportstate, pred_inter_m2$pred, dnn = c('Observed', 'Predicted')))
confusionMatrix(table(pred_inter_m33$lag_supportstate, pred_inter_m33$pred, dnn = c('Observed', 'Predicted')))




################################################################################
## Visualizing interaction term for multinomial logit
################################################################################
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



################################################################################
## Table: Cox proportional hazards models
################################################################################

cox.zph(mid_cox_4)

stargazer(
  mid_cox_1, mid_cox_2, mid_cox_3, mid_cox_4
  , digits = 2
  , type = 'html'
  , selection.equation = T
  , out = './PlotsTables/SurvivalMIDTables.html'
)

