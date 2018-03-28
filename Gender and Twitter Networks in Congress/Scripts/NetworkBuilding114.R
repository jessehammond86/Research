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

leg_tweets <- leg_tweets[, .N, by = c('created_at', 'icpsr_from', 'icpsr_to')]
setnames(leg_tweets, 'N', 'weight')
this_threshold <- mean(leg_tweets$weight) + sd(leg_tweets$weight)
# this_threshold <- quantile(leg_tweets, probs = 0.75)
# this_threshold <- 2
leg_tweets <- leg_tweets[weight >= this_threshold]


## Combined network
this_net_full <- graph_from_edgelist(
  as.matrix(
    leg_tweets[, list(
      as.integer(icpsr_from))
      , as.integer(icpsr_to)
      ]
  )
)
# E(this_net_full)$weight <- leg_tweets[, weight]
this_net_full <- add_vertices(this_net_full, length(unique(leg_data$icpsr)) - vcount(this_net_full))
V(this_net_full)$name <- as.integer(as.character(leg_data$icpsr))


leg_data[, twitter_in_full := degree(this_net_full, mode = 'in', normalized = F)]
leg_data[, twitter_out_full := degree(this_net_full, mode = 'out', normalized = F)]

leg_data[, twitter_in_senior_full := NA_real_]
leg_data[, twitter_in_junior_full := NA_real_]
leg_data[, twitter_in_samegender_full := NA_real_]
leg_data[, twitter_in_gendershare_full := NA_real_]
leg_data[, twitter_out_samegender_full := NA_real_]
leg_data[, twitter_out_gendershare_full := NA_real_]
leg_data[, twitter_returnedties := NA_real_]
leg_data[, twitter_returnedties_share := NA_real_]


leg_data[, twitter_in_sameparty_full := NA_real_]
leg_data[, twitter_out_sameparty_full := NA_real_]
leg_data[, twitter_in_partyshare_full := NA_real_]

leg_data[, twitter_in_leaders_full := NA_real_]
leg_data[, twitter_in_leadershare_full := NA_real_]

connected_members <- which(leg_data[, twitter_in_full] > 0)

for(j in connected_members){
  this_seniority <- leg_data[j, seniority]
  this_gender <- leg_data[j, female]
  this_party <- leg_data[j, dem]
  
  this_ties_idx <- as.integer(neighbors(this_net_full, j, mode = 'in'))
  this_ties_senior <- leg_data[this_ties_idx, seniority]
  this_ties_gender <- leg_data[this_ties_idx, female]
  this_ties_party <- leg_data[this_ties_idx, dem]
  this_ties_leadership <- leg_data[this_ties_idx, leadership]
  
  ## Keep senior partners
  this_partners_dropties <- this_ties_idx[which(this_ties_senior <= this_seniority)]
  if(length(this_partners_dropties) > 0){
    this_idx_pairs <- rep(j, length(this_partners_dropties))
    this_idx_reps <- list(this_partners_dropties, this_idx_pairs)
    edge_drop_pairs <- unlist(this_idx_reps)[order(sequence(vapply(this_idx_reps, length, 0L)))]
    edge_drop_idx <- get.edge.ids(this_net_full, edge_drop_pairs)
    this_net_drop <- delete_edges(this_net_full, edge_drop_idx)
    leg_data[j, twitter_in_senior_full := degree(this_net_drop, mode = 'in', normalized = T)[j]]
  } else {
    leg_data[j, twitter_in_senior_full := leg_data[j, twitter_in_full]]
  }
  
  ## Keep junior partners
  this_partners_dropties <- this_ties_idx[which(this_ties_senior > this_seniority)]
  if(length(this_partners_dropties) > 0){
    this_idx_pairs <- rep(j, length(this_partners_dropties))
    this_idx_reps <- list(this_partners_dropties, this_idx_pairs)
    edge_drop_pairs <- unlist(this_idx_reps)[order(sequence(vapply(this_idx_reps, length, 0L)))]
    edge_drop_idx <- get.edge.ids(this_net_full, edge_drop_pairs)
    this_net_drop <- delete_edges(this_net_full, edge_drop_idx)
    leg_data[j, twitter_in_junior_full := degree(this_net_drop, mode = 'in', normalized = T)[j]]
  } else {
    leg_data[j, twitter_in_junior_full := leg_data[j, twitter_in_full]]
  }
  
  ## Keep same-gender partners (INCOMING)
  this_partners_keepties <- this_ties_idx[which(this_ties_gender == this_gender)]
  if(length(this_partners_keepties) > 0){
    leg_data[j, twitter_in_samegender_full := length(this_partners_keepties)]
    leg_data[j, twitter_in_gendershare_full := length(this_partners_keepties) / length(this_ties_gender)]
  } else {
    leg_data[j, twitter_in_samegender_full := 0]
    leg_data[j, twitter_in_gendershare_full := 0]
  }
  
  
  ## Keep same-party partners
  this_partners_keepties <- this_ties_idx[which(this_ties_party == this_party)]
  if(length(this_partners_keepties) > 0){
    leg_data[j, twitter_in_sameparty_full := length(this_partners_keepties)]
    leg_data[j, twitter_in_partyshare_full := length(this_partners_keepties) / length(this_ties_party)]
  } else {
    leg_data[j, twitter_in_sameparty_full := 0]
    leg_data[j, twitter_in_partyshare_full := 0]
  }
  
  ## Differentiate same/different gender/party partners
  ss_in1 <- this_ties_idx[which(this_ties_party == this_party & this_ties_gender == this_gender)]
  sd_in1 <- this_ties_idx[which(this_ties_party == this_party & this_ties_gender != this_gender)]
  ds_in1 <- this_ties_idx[which(this_ties_party != this_party & this_ties_gender == this_gender)]
  dd_in1 <- this_ties_idx[which(this_ties_party != this_party & this_ties_gender != this_gender)]
  leg_data[j, ss_in := length(ss_in1) / length(this_ties_idx)]
  leg_data[j, sd_in := length(sd_in1) / length(this_ties_idx)]
  leg_data[j, ds_in := length(ds_in1) / length(this_ties_idx)]
  leg_data[j, dd_in := length(dd_in1) / length(this_ties_idx)]
  
  ## Keep partners in leadership
  # this_partners_dropties <- this_ties_idx[which(this_ties_leadership == 0)]
  # leg_data[j, twitter_in_leaders_full := sum(this_ties_leadership %in% 1)]
  
  this_partners_keepties <- this_ties_idx[which(this_ties_leadership == 1)]
  if(length(this_partners_keepties) > 0){
    leg_data[j, twitter_in_leaders_full := length(this_partners_keepties)]
    leg_data[j, twitter_in_leadershare_full := length(this_partners_keepties)  / length(this_ties_leadership)]
  } else {
    leg_data[j, twitter_in_leaders_full := 0]
    leg_data[j, twitter_in_leadershare_full := 0]
  }
  
  
}

connected_members_out <- which(leg_data[, twitter_out_full] > 0)
for(j in connected_members_out){
  this_seniority <- leg_data[j, seniority]
  this_gender <- leg_data[j, female]
  this_party <- leg_data[j, dem]
  
  this_ties_idx_out <- as.integer(neighbors(this_net_full, j, mode = 'out'))
  this_ties_idx_in <- as.integer(neighbors(this_net_full, j, mode = 'in'))
  this_ties_gender_out <- leg_data[this_ties_idx_out, female]
  this_ties_party_out <- leg_data[this_ties_idx_out, dem]
  this_ties_leadership_out <- leg_data[this_ties_idx_out, leadership]
  this_ties_leadership_in <- leg_data[this_ties_idx_in, leadership]
  
  
  ss <- length(this_ties_idx_out[which(this_ties_party_out == this_party & this_ties_gender_out == this_gender)]) / length(this_ties_idx_out)
  sd <- length(this_ties_idx_out[which(this_ties_party_out == this_party & this_ties_gender_out != this_gender)]) / length(this_ties_idx_out)
  ds <- length(this_ties_idx_out[which(this_ties_party_out != this_party & this_ties_gender_out == this_gender)]) / length(this_ties_idx_out)
  dd <- length(this_ties_idx_out[which(this_ties_party_out != this_party & this_ties_gender_out != this_gender)]) / length(this_ties_idx_out)
  
  leg_data[j, ss_out := ss]
  leg_data[j, sd_out := sd]
  leg_data[j, ds_out := ds]
  leg_data[j, dd_out := dd]
 
  
  
  ## Calculate reciprocity: % of outgoing ties returned
  if(sum(this_ties_idx_out %in% this_ties_idx_in) > 0){
    leg_data[j, twitter_returnedties := sum(this_ties_idx_out %in% this_ties_idx_in)]
    leg_data[j, twitter_returnedties_share := sum(this_ties_idx_out %in% this_ties_idx_in) / length(this_ties_idx_out)]
  } else {
    leg_data[j, twitter_returnedties := 0]
    leg_data[j, twitter_returnedties_share := 0]
  }
  
  ## Calculate reciprocity: % of outgoing ties to leadership returned
  this_partners_keepties_out <- this_ties_idx_out[which(this_ties_leadership_out == 1)]
  this_partners_keepties_in <- this_ties_idx_in[which(this_ties_leadership_in == 1)]
  if(length(this_partners_keepties_out) > 0){
    leg_data[j, twitter_returnedties_leaders := sum(this_partners_keepties_out %in% this_partners_keepties_in)]
    leg_data[j, twitter_returnedtiesshare_leaders := sum(this_partners_keepties_out %in% this_partners_keepties_in) / length(this_ties_leadership_out)]
  } else {
    leg_data[j, twitter_returnedties_leaders := 0]
    leg_data[j, twitter_returnedtiesshare_leaders := 0]
  }
  
  ## Keep partners in leadership
  # this_partners_dropties <- this_ties_idx[which(this_ties_leadership == 0)]
  # leg_data[j, twitter_in_leaders_full := sum(this_ties_leadership %in% 1)]
  
  this_partners_keepties_out <- this_ties_idx_out[which(this_ties_leadership_out == 1)]
  if(length(this_partners_keepties_out) > 0){
    leg_data[j, twitter_out_leaders_full := length(this_partners_keepties_out)]
    leg_data[j, twitter_out_leadershare_full := length(this_partners_keepties_out) / length(this_ties_leadership_out)]
  } else {
    leg_data[j, twitter_out_leaders_full := 0]
    leg_data[j, twitter_out_leadershare_full := 0]
  }
  
  ## Keep same-gender partners (OUTGOING)
  this_partners_keepties <- this_ties_idx_out[which(this_ties_gender_out == this_gender)]
  if(length(this_partners_keepties) > 0){
    leg_data[j, twitter_out_samegender_full := length(this_partners_keepties)]
    leg_data[j, twitter_out_gendershare_full := length(this_partners_keepties) / length(this_ties_gender_out)]
  } else {
    leg_data[j, twitter_out_samegender_full := 0]
    leg_data[j, twitter_out_gendershare_full := 0]
  }
  
  
  ## Keep same-party partners
  this_partners_keepties <- this_ties_idx[which(this_ties_party == this_party)]
  if(length(this_partners_keepties) > 0){
    leg_data[j, twitter_out_sameparty_full := length(this_partners_keepties)]
  } else {
    leg_data[j, twitter_out_sameparty_full := 0]
  }
  
}


f_dowle2(leg_data)

setkeyv(leg_data, c('icpsr'))

leg_data[, twitter_in_full_s := as.vector(scale(twitter_in_full))]
leg_data[, twitter_in_gendershare_full_s := as.vector(scale(twitter_in_gendershare_full))]
leg_data[, twitter_out_gendershare_full_s := as.vector(scale(twitter_out_gendershare_full))]
leg_data[, twitter_in_senior_full_s := as.vector(scale(twitter_in_senior_full))]
leg_data[, twitter_in_junior_full_s := as.vector(scale(twitter_in_junior_full))]
leg_data[, twitter_in_leaders_full_s := as.vector(scale(twitter_in_leaders_full))]
leg_data[, twitter_in_leadershare_full_s := as.vector(scale(twitter_in_leadershare_full))]
leg_data[, twitter_out_leaders_full_s := as.vector(scale(twitter_out_leaders_full))]
leg_data[, twitter_out_leadershare_full_s := as.vector(scale(twitter_out_leadershare_full))]
leg_data[, twitter_in_samegender_full_s := as.vector(scale(twitter_in_samegender_full))]

leg_data[, cosp_out_strong_s := as.vector(scale(cosp_out_strong))]
leg_data[, cosp_in_strong_s:= as.vector(scale(cosp_in_strong))]
# leg_data[, votepct_s := as.vector(scale(votepct))]
leg_data[, seniority_s := as.vector(scale(seniority))]



write.csv(leg_data, file = './Data/member_data_114_730.csv', row.names = F)
save(this_net_full, file = './Data/LegislatorNetworks/cong_net114_full_730.Rdata')
save(cosp, file = './Data/LegislatorNetworks/cosp_114_730.Rdata')

# write.csv(leg_data, file = '/Users/localadmin/Dropbox/Women in Politics/member_data_114.csv', row.names = F)
# save(this_net_full, file = '/Users/localadmin/Dropbox/Women in Politics/cong_net114_full.Rdata')
# save(cosp, file = '/Users/localadmin/Dropbox/Women in Politics/cosp_114.Rdata')

write.csv(leg_data, file = 'C:/Users/Jesse/Dropbox/Women in Politics/member_data_114_730.csv', row.names = F)
save(this_net_full, file = 'C:/Users/Jesse/Dropbox/Women in Politics/cong_net114_full_730.Rdata')
save(cosp, file = 'C:/Users/Jesse/Dropbox/Women in Politics/cosp_114_730.Rdata')
