rm(list = ls())

install.packages('GERGM')
library(GERGM)
install.packages('pacman')
pacman::p_load(data.table, igraph)
set.seed(12345)
data("lending_2005")
data("covariate_data_2005")
data("net_exports_2005")
plot_network(lending_2005) 


set.seed(1)
# setwd("/Users/localadmin/Dropbox/Women in Politics")
setwd("C:/Users/Jesse/Dropbox/Women in Politics")

################################################################################
##
## Load in data
##
################################################################################

leg_data <- fread('./member_data_114.csv', stringsAsFactor = T)
# load('/Users/localadmin/Dropbox/Research/LegislativeNetworks/Data/LegislatorNetworks/cong_net114_full.Rdata')
load('C:/Users/Jesse/Dropbox/Women in Politics/cong_net114_weighted.Rdata')

cong_mat <- as_adjacency_matrix(this_net_full, sparse = F)[1:50, 1:50]
rownames(cong_mat) <- 1:50
colnames(cong_mat) <- 1:50
cong_data <- leg_data[1:50]

foo2 <- graph_from_adjacency_matrix(cong_mat)
plot(
  foo2
  , vertex.label = NA
  , vertex.size = 3
  , edge.arrow.size = 0.1
)

formula <- cong_mat ~ edges + mutual(alpha = 0.8) + in2stars(alpha = 0.5) + sender('female') + nodematch('dem') + nodematch('state') + sender('seniority')

test1 <- gergm(formula
               , covariate_data = cong_data,
               number_of_networks_to_simulate = 10000,
               thin = 1/100,
               proposal_variance = 0.05,
               MCMC_burnin = 1000,
               seed = 456,
               convergence_tolerance = 0.5
)


formula <- lending_2005 ~ edges + mutual(alpha = 0.8) + sender("log_GDP") + 
  receiver("log_GDP") + nodemix("G8", base = "No") + netcov(net_exports_2005) 
test <- gergm(formula,
              covariate_data = covariate_data_2005,
              number_of_networks_to_simulate = 40000,
              thin = 1/100,
              proposal_variance = 0.05,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.5)
