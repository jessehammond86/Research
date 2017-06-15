rm(list = ls())
pacman::p_load(data.table, tm, jsonlite, igraph
               , network, intergraph, yaml, dplyr, stringr, lubridate)

station = 'localadmin'
# setwd(paste0('/Users/', station, '/Dropbox/Research/LegislativeNetworks'))
setwd(paste0('C:\\Users\\Jesse\\Dropbox\\Research\\LegislativeNetworks'))

################################################################################
##
## Session-level characteristics
##
################################################################################

year_data <- data.table(
  start = c(2009, 2011, 2013, 2015)
  , end = c(2011, 2013, 2015, 2017)
)

session_data <- data.table(
  year = c(2009:2016)
  , session = c(111,111,112,112,113,113,114,114)
)

sessions <- c(111,112,113, 114)

################################################################################
##
## Load in data
##
################################################################################

##### Load/create legislator Twitter handles
# setwd("/Users/localadmin/Dropbox/Research/LegislativeNetworks/Data/Cosponsorship")
setwd("C:/Users/Jesse/Dropbox/Research/LegislativeNetworks/Data/Cosponsorship")
dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)
source("C:/Users/Jesse/Dropbox/Research/LegislativeNetworks/Scripts/01-data.r")  # data collection
# source("/Users/localadmin/Dropbox/Research/LegislativeNetworks/Scripts/01-data.r")  # data collection
# setwd(paste0('/Users/', station, '/Dropbox/Research/LegislativeNetworks'))
setwd(paste0('C:\\Users\\Jesse\\Dropbox\\Research\\LegislativeNetworks'))

leg_twitter <- data.table(s)
leg_twitter <- leg_twitter[!duplicated(leg_twitter[, list(thomas)])]
leg_twitter <- leg_twitter[, list(icpsr, thomas, twitter, sex, mandate, party)]
# leg_twitter <- leg_twitter[!is.na(twitter) & !is.na(icpsr)]
leg_twitter <- unique(leg_twitter)


##### Load legislator Twitter ties
leg_tweets <- fread('./Data/positive_legmentions.csv')[, list(created_at, handle, V1, compound)]
leg_tweets <- leg_tweets[handle != V1]
setnames(leg_tweets, c('created_at', 'twitter', 'to', 'weight'))
leg_tweets[, weight := as.numeric(weight)]
leg_tweets <- leg_tweets[!is.na(weight)]
leg_tweets[, created_at := as.numeric(format(as.Date(as.POSIXlt(leg_tweets$created_at, format = '%m/%d/%Y')), '%Y'))]
# leg_tweets <- leg_tweets[created_at <= 2016]

leg_tweets <- merge(leg_tweets, leg_twitter[, list(twitter, icpsr)], by = 'twitter')
setnames(leg_tweets, 'icpsr', 'icpsr_from')
leg_tweets <- merge(leg_tweets, leg_twitter[, list(twitter, icpsr)], by.x = 'to', by.y = 'twitter')
setnames(leg_tweets, 'icpsr', 'icpsr_to')

leg_tweets <- leg_tweets[, list(created_at, icpsr_from, icpsr_to, weight)]

leg_tweets <- merge(leg_tweets, session_data, by.x = 'created_at', by.y = 'year')

leg_tweets <- leg_tweets[session == 114]

## Merge Twitter data and legislator data
# leg_data <- merge(leg_tweets, leg_twitter, by  = 'twitter')
# leg_data <- leg_data[to %in% leg_twitter$twitter]

## Static legislator data (remove yearly/session data)
# leg_chardata <- leg_data[!duplicated(icpsr)]

##### Load data: house leadership positions
leg_leaders <- fread('./Data/LegislatorCommittees/house_assignments_111-115.csv')
leg_leaders <- leg_leaders[Congress == 114]
leg_status <- leg_leaders[SeniorMember > 0]
leg_status <- leg_status[!duplicated(leg_status[, list(ID, SeniorMember)])]
leg_status[, leadership := 1]
leg_status <- leg_status[!duplicated(leg_status[, ID]), list(ID, leadership)]

leg_leaders <- leg_leaders[!duplicated(leg_leaders[, ID])]
leg_leaders <- merge(leg_leaders, leg_status, by = 'ID', all.x = T)
leg_leaders[is.na(leadership), leadership := 0]

leg_data <- merge(leg_leaders, leg_twitter, by.x = 'ID', by.y = 'icpsr', all.x = T)
setnames(leg_data, 'ID', 'icpsr')

##### Load data: legislative efficacy
leg_eff <- fread('./Data/LegislatorEffectiveness/legeff_111_114.csv')
setnames(leg_eff, 'congress', 'session')

leg_eff <- leg_eff[session == 113, list(icpsr, seniority)]
leg_eff[, seniority := seniority + 1]

leg_data <- merge(leg_data, leg_eff, by = 'icpsr', all.x = T)
leg_data[is.na(seniority), seniority := 1]

leg_data[, female := ifelse(sex == 'F', 1, 0)]
leg_data[, dem := ifelse(party == 'DEM', 1, 0)]


##### Load data: cosponsorship networks
load('./Data/Cosponsorship/net_us.rda')

## Merge in cosponsorship data
cosp <- asIgraph(net_us_hr114)
cosp_mean <- mean(E(cosp)$gsw)
cosp_sd <- sd(E(cosp)$gsw)
weakties <- which(abs(E(cosp)$gsw) < (cosp_mean + cosp_sd))
cosp <- igraph::delete.edges(cosp, weakties)
cosp_data <- data.table(
  icpsr = V(cosp)$icpsr
  , cosp_in_strong = degree(cosp, mode = 'in', normalized = F)
  , cosp_out_strong = degree(cosp, mode = 'out', normalized = F)
)

leg_data <- merge(leg_data, cosp_data, by = 'icpsr', all.x = T, sort = F)

## Manually add in gender because what the fuck
leg_data[is.na(female), female := c(rep(0,14), 1, 1, 1, 0, 0, 1)]



################################################################################
##
## Internal functions
##
################################################################################

#### Function: take the first N-k letters of a string
# substrLeft <- function(x, n){
#   substr(x, 1, nchar(x)-n+1)
# }

f_dowle2 = function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), (i):=0]
}



################################################################################
##
## Descriptive statistics: Twitter usage
##
################################################################################

leg_handles <- data.table(
  handle = leg_data[!is.na(twitter), twitter]
  , num_tweets = NA_integer_
  , earliest_tweet = as.Date('2000-01-01')
)
setkey(leg_handles, 'handle')


for(handle in leg_handles[, handle]){
  this_file <- paste0('/Users/localadmin/Dropbox/Research/LegislativeNetworks/data/LegislatorTweets/', handle, '_tweets.csv')
  try({
    this_tweets <- fread(this_file)
    this_tweets[, year := as.numeric(format(as.Date(as.POSIXlt(this_tweets$created_at, format = '%Y-%m-%d')), '%Y'))]
    this_tweets[, created_at := as.Date(as.POSIXlt(this_tweets$created_at, format = '%Y-%m-%d'))]
    leg_handles[handle, num_tweets := nrow(this_tweets)]
    leg_handles[handle, earliest_tweet := min(this_tweets[, created_at])]
  }, silent = T)
}

hist(leg_handles[, num_tweets])



################################################################################
##
## Twitter networks
##
################################################################################

leg_data[, icpsr := factor(icpsr)]
setkeyv(leg_data, 'icpsr')


## Create edgelists  
leg_tweets[, icpsr_from := factor(as.integer(as.character(icpsr_from)), levels = levels(leg_data$icpsr))]
leg_tweets[, icpsr_to := factor(as.integer(as.character(icpsr_to)), levels = levels(leg_data$icpsr))]
leg_tweets <- leg_tweets[!is.na(icpsr_from) & !is.na(icpsr_to)]
leg_tweets <- data.table(aggregate(. ~ icpsr_from + icpsr_to, data = leg_tweets, FUN = length))[, list(icpsr_from, icpsr_to, weight)]

#leg_tweets <- leg_tweets[, .N, by = c('created_at', 'icpsr_from', 'icpsr_to')]
#setnames(leg_tweets, 'N', 'weight')

# this_threshold <- mean(leg_tweets$weight) + sd(leg_tweets$weight)
# this_threshold <- quantile(leg_tweets, probs = 0.75)
# this_threshold <- 2
# leg_tweets <- leg_tweets[weight >= this_threshold]


## Combined network
this_net_full <- graph_from_edgelist(
  as.matrix(
    leg_tweets[, list(
      as.integer(icpsr_from))
      , as.integer(icpsr_to)
      ]
  )
)
E(this_net_full)$weight <- leg_tweets[, weight]
this_net_full <- add_vertices(this_net_full, length(unique(leg_data$icpsr)) - vcount(this_net_full))
V(this_net_full)$name <- as.integer(as.character(leg_data$icpsr))

## Add nodal variables
V(this_net_full)$female <- leg_data$female
V(this_net_full)$leadership <- leg_data$leadership
V(this_net_full)$dem <- leg_data$dem
V(this_net_full)$seniority <- leg_data$seniority
V(this_net_full)$hastwitter <- ifelse(leg_data$twitter != '0', 1, 0)
V(this_net_full)$state <- leg_data$StateName

## Save network
save(this_net_full, file = 'C:/Users/Jesse/Dropbox/Women in Politics/cong_net114_weighted.Rdata')
