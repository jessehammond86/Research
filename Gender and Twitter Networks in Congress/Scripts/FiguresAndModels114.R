rm(list = ls())
pacman::p_load(data.table, igraph, dplyr, stringr, boot, stargazer, intergraph, statnet
               , interplot, ergm, ggplot2, gridExtra
               )
set.seed(1)
# setwd("/Users/localadmin/Dropbox/Women in Politics")
setwd("C:/Users/Jesse/Dropbox/Women in Politics")

################################################################################
##
## Load in data
##
################################################################################

leg_data <- fread('./member_data_114_730.csv', stringsAsFactor = T)
# load('/Users/localadmin/Dropbox/Women in Politics/cong_net114_full_730.Rdata')
load('C:/Users/Jesse/Dropbox/Research/LegislativeNetworks/Data/LegislatorNetworks/cong_net114_full.Rdata')
# load('C:/Users/Jesse/Dropbox/Research/LegislativeNetworks/Data/LegislatorNetworks/cosp_114.Rdata')


##### Overall network: change over time

net_stats <- data.table(
  connected_nodes_full = rep(NA_real_)
  , density_full = NA_real_
  , mean_degree_full = NA_real_
)

## Size
net_stats[, connected_nodes_full := sum(igraph::degree(this_net_full) > 0)]
net_stats[, density_full := edge_density(this_net_full)]
net_stats[, mean_degree_full := mean(igraph::degree(this_net_full) > 0)]


################################################################################
##
## ERGMs
##
################################################################################

V(this_net_full)$icpsr <- V(this_net_full)$name
V(this_net_full)$name <- 1:446

vertex_data <- leg_data[, list(icpsr, female, leadership, dem, StateName, seniority, twitter)]
setkeyv(vertex_data, 'icpsr')
V(this_net_full)$female <- vertex_data$female
V(this_net_full)$leadership <- vertex_data$leadership
V(this_net_full)$dem <- vertex_data$dem
V(this_net_full)$StateName <- vertex_data$StateName
V(this_net_full)$seniority <- vertex_data$seniority
V(this_net_full)$twitter <- vertex_data$twitter

femtoleader_edges <- data.table(as_edgelist(this_net_full))
setkeyv(femtoleader_edges, c('V1', 'V2'))
female_nodes <- which(leg_data$female == 0)
leader_nodes <- which(leg_data$leadership == 1)
femtoleader_edges[, femtoleader := 0L]
femtoleader_edges[V1 %in% female_nodes & V2 %in% leader_nodes, femtoleader := 1]
this_net_full <- set_edge_attr(this_net_full, 'femtoleader', value = femtoleader_edges$femtoleader)

femtoleader_edges <- as.matrix(femtoleader_edges[femtoleader > 0, list(V1, V2)])
femtoleader_matrix <- matrix(0, nrow = 446, ncol = 446)
femtoleader_matrix[femtoleader_edges] <- 1
femtoleader_network <- as.network.matrix(femtoleader_matrix)

# notwitter <- which(is.na(leg_data$twitter))
# femtoleader_matrix <- femtoleader_matrix[-notwitter, ]
# femtoleader_matrix <- femtoleader_matrix[, -notwitter ]

congnet <- asNetwork(this_net_full)
# congnet$gal$multiple <- F


congnet <- congnet %s% which(congnet %v% 'twitter' != 1)

structural_model <- ergm(congnet ~
               # nodeofactor('female')
             + nodeofactor('leadership')
             # + nodeocov('dem')
             + nodeocov('seniority')
             + nodematch('dem')
             + nodematch('StateName')
             # + nodematch('female', diff=F)
             # + edgecov(femtoleader_network)
             # + nodemix(c('female', 'leadership')
                       # , base = c(-7,-15)
                       # )
             + edges
             + isolates
             + mutual
             # + ttriple
             # + gwesp(fixed = T, alpha = 1.25)
             # + gwidegree(fixed = T, decay = 1)
             # + gwodegree(fixed = T, decay = 1)
             # , estimate = 'MPLE'
             , control=control.ergm(
              parallel=8, parallel.type="PSOCK"
             , MCMC.samplesize=8000,MCMC.interval=16000
             )
)
summary(structural_model)

gender_model <- ergm(congnet ~
               nodeofactor('female')
             + nodeofactor('leadership')
             # + nodeocov('dem')
             + nodeocov('seniority')
             + nodematch('dem')
             + nodematch('StateName')
             + nodematch('female', diff=T)
             # + edgecov(femtoleader_network)
             + nodemix(c('female', 'leadership')
                       , base = c(-7,-15)
             )
             + edges
             + isolates
             + mutual
             # + ttriple
             # + gwesp(fixed = T, alpha = 1.25)
             # + gwidegree(fixed = T, decay = 1)
             # + gwodegree(fixed = T, decay = 1)
             # , estimate = 'MPLE'
             , control=control.ergm(
              parallel=8, parallel.type="PSOCK"
             , MCMC.samplesize=8000,MCMC.interval=16000
             )
)
summary(gender_model)



stargazer(structural_model, gender_model, digits = 2)
plot(gof(test1))


dyad_ids <- data.table(expand.grid(1:446, 1:446))
dyad_ids[, dyadid := (Var1 * 10000) + Var2]
dyadid_mat <- matrix(dyad_ids$dyadid, nrow = 446)

test1_mple <- ergmMPLE(congnet ~
                nodeofactor('female')
              + nodeofactor('leadership')
              # + nodeocov('dem')
              + nodeocov('seniority')
              + nodematch('dem')
              + nodematch('StateName')
              + nodematch('female', diff=T)
              # + edgecov(femtoleader_network)
              + nodemix(c('female', 'leadership')
                        , base = c(-7, -15
                                   # , -13, -5)
                        ))
              + edges
              + isolates
              + mutual
              + edgecov(dyadid_mat)
              # + ttriple
              # + gwesp(fixed = T, alpha = 1.25)
              # + gwidegree(fixed = T, decay = 1)
              # + gwodegree(fixed = T, decay = 1)
              # , estimate = 'MPLE'
              # , control=control.ergm(
              # parallel=4, parallel.type="PSOCK"
              # , MCMC.samplesize=8000,MCMC.interval=16000
              # )
)

ties_data <- data.table(as_edgelist(this_net_full))
ties_data[, dyadid := (V1 * 10000) + V2]
ties_data[, tie := 1]
ties_data <- ties_data[, list(dyadid, tie)]

test1_data <- data.table(test1_mple$predictor)
setnames(test1_data, 'edgecov.dyadid_mat', 'dyadid')

logit_data <- dyad_ids
logit_data[, Var1 := NULL]
logit_data[, Var2 := NULL]
logit_data <- merge(logit_data, test1_data, by = 'dyadid')
logit_data <- merge(ties_data, test1_data, by = 'dyadid', all.x = T, all.y = T)
logit_data[is.na(tie), tie := 0]
logit_data[, dyadid := NULL]
logit_data[, edges := NULL]
logit_data[, isolates := NULL]
logit_data[, mutual := NULL]

structural_logit <- glm(
  tie ~
    nodeofactor.leadership.1
  + nodeocov.seniority
  + nodematch.dem
  + nodematch.StateName
  , data = logit_data
  , family = binomial(link = 'logit')
)
summary(structural_logit)


gender_logit <- glm(
  tie ~ 
    .
  , data = logit_data
  , family = binomial(link = 'logit')
)
summary(gender_logit)


stargazer(structural_logit, gender_logit, structural_model, gender_model
          , digits = 2, style = 'apsr')



################################################################################
##
## Plots
##
################################################################################

dev.off()
png(file = '/Users/jesse/Dropbox/Women in Politics/cong_114_better.png', height = 800, width = 800)
plot(
  this_net_full
  , vertex.size = 4
  , vertex.label = NA
  , vertex.color = ifelse(V(this_net_full)$dem == 1, 'blue', 'red')
  , vertex.shape = ifelse(V(this_net_full)$female == 1, 'square', 'circle')
  , edge.arrow.size = 0.5
  , edge.color = 'grey60'
  # , rescale = F, ylim=c(-5,15),xlim=c(-5,15), asp = 0
  # , layout = layout.kamada.kawai
  )
legend('topleft',legend=c('Dem/Female', 'Dem/Male', 'Rep/Female', 'Rep/Male'),col=c('blue', 'blue', 'red', 'red'),pch=c(15,16,15,16))
dev.off()

################################################################################
##
## Simple analytics
##
################################################################################


###########
## Gender differences
##
## NOTE: instead of t-tests, I use nonparametric bootstrap evaluation as per this
## suggestion: https://stats.stackexchange.com/questions/136661/using-bootstrap-under-h0-to-perform-a-test-for-the-difference-of-two-means-repl
## 
###########
# test_data <- leg_data[twitter != '0']
test_data <- leg_data
setkeyv(test_data, 'twitter')
# test_data <- test_data[!duplicated(test_data[, twitter])]


## Sample mean function
samplemean <- function(x, d){
  return(mean(x[d]))
}



#####
## Are men more connected than women?
#####

## Plain t-test: out-degree
female_out <- test_data[female == 1, twitter_out_full]
male_out <- test_data[female == 0 , twitter_out_full]
t.test(female_out, male_out)

## Plain t-test: in-degree
female_in <- test_data[female == 1, twitter_in_full]
male_in <- test_data[female == 0 , twitter_in_full]
t.test(female_in, male_in)



#####
## Do men and women tend to connect more with one another?
## Key statistic: what % of your incoming ties are from members with the same gender?
##
## Statistical test: is the % of same-gender ties the same as the overall %
##  of men/women in the network?
##
## Idea: If men connect with men (or women with women) more readily than with opposite-gender
##  members, then the % of ties with same-gender members should resemble the population mean
##
#####

##### Population share of men in the network
pct_female <- sum(test_data$female == 1) / nrow(test_data)
female_samegender_out <- test_data[female == 1 & twitter_out_full > 0, twitter_out_gendershare_full]
male_samegender_out <- test_data[female == 0 & twitter_out_full > 0, twitter_out_gendershare_full]

female_samegender_in <- test_data[female == 1 & twitter_out_full > 0, twitter_in_gendershare_full]
male_samegender_in <- test_data[female == 0 & twitter_out_full > 0, twitter_in_gendershare_full]

print(c(quantile(female_samegender, probs = c(0.025, 0.975)), pct_female))

## Plain t-test: out-degree
female_out <- test_data[female == 1, twitter_out_full]
male_out <- test_data[female == 0 , twitter_out_full]
t.test(female_out, male_out)

## Plain t-test: in-degree
female_in <- test_data[female == 1, twitter_in_full]
male_in <- test_data[female == 0 , twitter_in_full]
t.test(female_in, male_in)


#####
## Do men have more mentorship ties than women?
#####

## Plain t-test: out-degree, share
female_out <- test_data[female == 1, twitter_out_leadershare_full]
male_out <- test_data[female == 0, twitter_out_leadershare_full]
t.test(female_out, male_out)

## Plain t-test: in-degree, share
female_in <- test_data[female == 1, twitter_in_leadershare_full]
male_in <- test_data[female == 0, twitter_in_leadershare_full]
t.test(female_in, male_in)


#####
## Do men and women tend to connect more with one another?
## Key statistic: what % of your incoming ties are from members with the same gender?
##
## Statistical test: is the % of same-gender ties the same as the overall %
##  of men/women in the network?
##
## Idea: If men connect with men (or women with women) more readily than with opposite-gender
##  members, then the % of ties with same-gender members should resemble the population mean
##
#####

##### Population share of men in the network
pct_male <- sum(test_data$female == 0) / nrow(test_data)


##### Do men connect with men more/less than we would expect given population share?

## Out-degree combined, all
x <- test_data[female == 0 & twitter_out_full > 0, twitter_out_gendershare_full]
b1 <- boot(x, samplemean, R = 1000)
boot_vals1 <- data.table(b1$t)*100
ci <- boot.ci(b1, type = "basic")
print(ci)
print(mean(x))
print(pct_male)


mm_plot <- ggplot(
  data = boot_vals1
  , aes(V1, ..density..)
  , geom = 'histogram'
  , main = 'Test'
  
) + geom_histogram(
  fill = 'grey90'
  , col = 'grey80'

) + xlab(
  'Share of male-male Twitter engagements'

) + geom_vline(
  xintercept = 81.3
  , lty = 2
  , colour = 'gray20'
  
) + ylab(
  'Density, 1000 bootstrap draws'

) + annotate(
  "text"
  , label = "Share of male \n Representatives"
  , x = 81.5
  , y = 0.125
  , hjust = 0
  , cex = 3.5
  
) + ylim(0, 0.15
         
) + xlim(65, 90
         
) + theme_bw(
    
)


##### Do women connect with women more/less than we would expect given population share?

## Out-degree combined, all
x <- test_data[female == 1 & twitter_out_full > 0, twitter_out_gendershare_full]
b2 <- boot(x, samplemean, R = 1000)
boot_vals2 <- data.table(b2$t)*100
ci <- boot.ci(b2, type = "basic")
print(ci)
print(mean(x))
print(1-pct_male)


ff_plot <- ggplot(
  data = boot_vals2
  , aes(V1, ..density..)
  , geom = 'histogram'
  , main = 'Test'
  
) + geom_histogram(
  fill = 'grey90'
  , col = 'grey80'
  
) + xlab(
  'Share of same-gender engagements, female legislators'
  
) + geom_vline(
  xintercept = 18.6
  , lty = 2
  , colour = 'gray20'
  
) + ylab(
  'Density, 1000 bootstrap draws'
  
) + annotate(
  "text"
  , label = "Share of female-female Twitter engagementws"
  , x = 20
  , y = 0.12
  , hjust = 0
  , cex = 3.5
  
) + ylim(0, 0.15
         
) + xlim(15, 60
         
) + theme_bw(
  
)



###### Write plots to file

dev.off()
png(file = '/Users/localadmin/Dropbox/Women in Politics/plots and graphs/FF_MM_engagements.png', width = 900, height = 500)
grid.arrange(mm_plot, ff_plot, ncol = 2)
dev.off()



################################################################################
##
## Statistical models
##
################################################################################

##### Model set 1: cosponsorship centrality
leg_data[, twitter_out_full_s := as.vector(scale(twitter_out_full))]
leg_data[, isolate := ifelse(twitter_in_full == 0 & twitter_out_full == 0, 1, 0)]
## Support: combined in-degree * female status
m1 <- lm(cosp_in_strong_s
         ~ twitter_in_full_s
         + isolate
         + cosp_out_strong_s
         + dem
         + female
         + leadership 
         + seniority_s
         , data = leg_data
)

## Mentorship: any connection from senior leadership, in-degree
m2 <- lm(cosp_in_strong_s
         ~ twitter_in_leaders_full_s
         + isolate
         + cosp_out_strong_s
         + dem
         + female
         + leadership 
         + seniority_s
         , data = leg_data
)

## Mentorship * female, in-degree
m3 <- lm(cosp_in_strong_s
         ~ twitter_in_leaders_full_s * female
         + isolate
         + cosp_out_strong_s
         + dem
         + female
         + leadership 
         + seniority_s
         , data = leg_data
)


summary(m1)
summary(m2)
summary(m3)

stargazer(m1, m2, m3, digits = 2)

interplot(m3, var1 = 'twitter_in_leaders_full_s', var2 = 'female')
