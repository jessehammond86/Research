###############################################################################
##
## ProcessReligionData.R
## 
## Purpose: Intakes raw World yearly_religion data and outputs dyadic distance data.
##  Similarity is a multi-dimensional distance metric based on the proportion
##  of N major religious identities within a state:
##  1. Christianity (Protestant)
##  2. Christianity (Catholic)
##  3. Islam (Sunni)
##  4. Islam (Shiite)
##  5. Buddhism (all)
##  6. Hinduism (all)
##  7. Judaism (all)
##
## Data timespan: 1995 - 2010 directed-dyad-year records (A -> B)
##  NOTE: data is observed at 5-year intervals. I use linear imputation
##  to create yearly values.
##
## Output: An .Rdata object containing a data.table with directed-dyad-year
##  records indicating the oveerall level of religious membership overlap 
##  between states A and B.
##
##
## Output files: 
##  - yearly_religion_distance.Rdata
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')

pacman::p_load(data.table, countrycode, imputeTS)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  # setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}


###############################################################################
##
## Load in data: World Religion Survey (national level)
##
###############################################################################

yearly_religion <- fread('./Data/religion/WRP_national.csv')


###############################################################################
##
## Process data: subsetting and adjusting variables
##
###############################################################################

yearly_religion <- yearly_religion[
  year >= 1995
  , list(year, state
         , chrstprotpct, chrstcatpct, islmsunpct, islmshipct
         , budgenpct, hindgenpct, judgenpct)
  ]

yearly_religion[, state := countrycode(
  state
  , origin = 'cown'
  , destination = 'iso3c'
  )
]

yearly_religion <- yearly_religion[!is.na(state)]

###############################################################################
##
## Impute missing data by year, as WR data is measured every 5 years.
##
###############################################################################

years <- data.table(
  expand.grid(
    c(1995:2010)
    , unique(yearly_religion$state)
  )
)
setnames(years, c('year', 'state'))

yearly_religion <- merge(years, yearly_religion, by = c('year', 'state'), all.x = T)
setkeyv(yearly_religion, c('year', 'state'))

impute_values <- function(x){
  na.interpolation(ts(x))
}

yearly_religion[state != 'MNE', chrstprotpct := impute_values(chrstprotpct), by = state]
yearly_religion[state != 'MNE', chrstcatpct := impute_values(chrstcatpct), by = state]
yearly_religion[state != 'MNE', islmsunpct := impute_values(islmsunpct), by = state]
yearly_religion[state != 'MNE', islmshipct := impute_values(islmshipct), by = state]
yearly_religion[state != 'MNE', budgenpct := impute_values(budgenpct), by = state]
yearly_religion[state != 'MNE', hindgenpct := impute_values(hindgenpct), by = state]
yearly_religion[state != 'MNE', judgenpct := impute_values(judgenpct), by = state]


###############################################################################
##
## Calculating distance, treating each yearly_religion as one dimension and situating
##  states according to their values along each dimension.
##
## NOTE: Higher values indicate greater distance or lower similarity within a
##  given state dyad.
##
###############################################################################

yearly_relig_distance <- data.table(
  year = integer()
  , statea = character()
  , stateb = character()
  , relig_distance = numeric()
)

religion_years <- unique(yearly_religion$year)

for (this_year in 1:length(religion_years)){
  
  ## Subset by year
  this_relig <- yearly_religion[year == religion_years[this_year]]
  relig_states <- unique(this_relig$state)
  
  ## Calculate multi-dimensional distance
  religmat <- as.matrix(this_relig[, -c(1:2), with = F])
  religdist <- as.matrix(dist(religmat))
  rownames(religdist) <- yearly_religion[year == religion_years[this_year], state]
  colnames(religdist) <- yearly_religion[year == religion_years[this_year], state]
  
  relig_out <- data.table(
    religion_years[this_year]
    , expand.grid(relig_states, relig_states)
  )
  setnames(relig_out, c('year', 'statea', 'stateb'))
  relig_out[, relig_distance := as.vector(religdist)]
  
  ## Append to output file
  yearly_relig_distance <- rbind(yearly_relig_distance, relig_out)
}


###############################################################################
##
## Save output file
##
###############################################################################

save(yearly_relig_distance, file = './Data/AnalysisData/yearly_relig_distance.Rdata')
