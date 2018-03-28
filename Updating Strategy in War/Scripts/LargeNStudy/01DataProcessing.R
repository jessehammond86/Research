rm(list = ls())
library(data.table)
library(rgdal)
library(raster)
library(maptools)
library(lubridate)
setwd('C:/Users/Jesse/Dropbox/Dissertation/Data/LargeNStudy')
# setwd('/Users/jesse/Dropbox/Dissertation/Data/LargeNStudy')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/LargeNStudy')

## Read in data files
# ACLED conflict events 1997-2015
acled <- readOGR(getwd(), 'ACLED_Version6_Shpfile')
# ACLED actor-conflict merge data
conflict_actors <- fread('actor_conflict.csv')
# Manually correct because fucking ugh
conflict_actors$ACLED[79] <- 'Seleka Rebel Coalition'
conflict_actors$ACLED[31] <- 'Boko Haram - Jamaatu Ahli is-Sunnah lid-Dawatai wal-Jihad'


# GED conflict events 1989-2015 (not used)
# ged <- readOGR(getwd(), 'ged40')
# UCDP conflict state-years
ucdp <- fread('UCDP_ACD_4_2015.csv')
# GRUMP urban-rural divide data
grump <- raster(paste0(getwd(), '/afurextents.asc'))
# UCDP conflict termination
term <- fread('ucdpterm2015.csv')
# Manually fix an Ivory Coast entry
term[grep('Ivory', SideA), GWNoA := 437]

# Subset ACLED data
acleddata <- as.data.table(acled@data)
acleddata <- acleddata[INTERACTIO %in% c(12, 13, 15, 16, 17, 23, 25, 26, 27, 36, 37)]
acleddata <- acleddata[as.integer(EVENT_TYPE) %in% c(1, 2, 3, 6, 9)]
acleddata <- acleddata[GWNO %in% ucdp$GWNoLoc]
acleddata[, eventcount := .N, by = GWNO]
acleddata[, actorcount1 := .N, by = ACTOR1]
# acleddata <- acleddata[actorcount1 > 10]
# acleddata <- acleddata[eventcount >= 50]
acleddata[, NOTES := NULL]
# Manually correct because fucking ugh
acleddata$ACTOR1 <- gsub('Séléka', 'Seleka', acleddata$ACTOR1)
acleddata$ACTOR2 <- gsub('Séléka', 'Seleka', acleddata$ACTOR2)
acleddata$ACTOR1 <- gsub('Jama??atu', 'Jamaatu', acleddata$ACTOR1)
acleddata$ACTOR2 <- gsub('Jama??atu', 'Jamaatu', acleddata$ACTOR2)

# Subset UCDP data
ucdp <- ucdp[Year >= 1997 & as.integer(GWNoLoc) %in% acleddata$GWNO]

# Subset conflict termination data
term <- term[ConflictId %in% ucdp$ConflictId & Year >= 1997 & as.integer(GWNoLoc) %in% acleddata$GWNO]
term <- term[, list(ConflictId, Year, SideA, GWNoA, SideB, SideBID, GWNoLoc, StartDate, EpEndDate, Outcome)]
term <- term[!is.na(Outcome)]
term$StartDate <- as.Date(term$StartDate, format = '%Y-%m-%d')
term$EpEndDate2 <- as.Date(term$EpEndDate, format = '%m/%d/%Y')
term$EpEndDate2[is.na(term$EpEndDate2)] <- as.Date(term$EpEndDate[is.na(term$EpEndDate2)] , format = '%Y-%m-%d')
term$EpEndDate <- term$EpEndDate2
term[, EpEndDate2 := NULL]
term[, episode := seq(1:.N), by = ConflictId]
term[, StartDate := floor_date(StartDate, 'month')]
term[, EpEndDate := ceiling_date(EpEndDate, 'month')]


# Finish subsetting UCDP data
ucdp <- ucdp[ConflictId %in% term$ConflictId]
ucdp <- ucdp[EpEnd == 1]

# Finish subsetting ACLED data
acleddata <- acleddata[GWNO %in% unique(as.integer(term$GWNoLoc))]
# format date data
acleddata$EVENT_DATE <- as.Date(as.character(acleddata$EVENT_DATE), format = '%Y/%m/%d')
acleddata[, EVENT_DATE := floor_date(EVENT_DATE, 'month')]

##
##
## Write out subsetted actor lists for conflict termination / conflict duration and ACLED actor codes
# write.csv(unique(ucdp$SideB), file = 'ucdp_actorset.csv', row.names = F)
# acled_actors <- rbind(unique(acleddata[, list(GWNO, ACTOR1)])
#                       , unique(acleddata[, list(GWNO, ACTOR2)]), fill = T)
# write.csv(acled_actors, file = 'acled_actorset2.csv', row.names = F)
# out1 <- acleddata[!duplicated(paste(GWNO, ACTOR1)), list(GWNO, ACTOR1)]
# setnames(out1, c('GWNO', 'ACTOR'))
# out2 <- acleddata[!duplicated(paste(GWNO, ACTOR2)), list(GWNO, ACTOR2)]
# setnames(out2, c('GWNO', 'ACTOR'))
# out3 <- unique(rbind(out1, out2))
# out4 <- term[, list(ConflictId, GWNoLoc, SideB, Outcome)]
# write.csv(out3, file = 'acled_actors.csv')
# write.csv(out4, file = 'ucdp_actors.csv')

##
##


# Create list of rebel and non-rebel actors
rebels <- c(unique(conflict_actors$ACLED)
            ,unique(acleddata$ACTOR1)[grep("Unidentified Armed Group|Militia",unique(acleddata$ACTOR1))])
state <- c(unique(acleddata$ACTOR1)[intersect(grep("Military Forces|Police|Prison|Anti-Terror",unique(acleddata$ACTOR1))
                                                   ,grep("Former|Mutiny|Faction",unique(acleddata$ACTOR1),invert=TRUE))]
                 , unique(acleddata$ACTOR2)[intersect(grep("Military Forces|Police|Prison|Anti-Terror",unique(acleddata$ACTOR2))
                                                      ,grep("Former|Mutiny|Faction",unique(acleddata$ACTOR2),invert=TRUE))])
civilians <- c('CIVILIANS', unique(acleddata$ACTOR1)[grep("Civilians|Protesters",unique(acleddata$ACTOR1))]
               , unique(acleddata$ACTOR2)[grep("Civilians|Protesters",unique(acleddata$ACTOR2))])

# Merge with actor table to get conflict ID
acleddata$key1 <- paste(acleddata$GWNO, acleddata$ACTOR1)
acleddata$key2 <- paste(acleddata$GWNO, acleddata$ACTOR2)
conflict_actors$key <- paste(conflict_actors$GWNO, conflict_actors$ACLED)
acleddata <- merge(acleddata, conflict_actors[, list(key, ConfIdGuess)], by.x = 'key1', by.y = 'key', all.x = T)
acleddata <- merge(acleddata, conflict_actors[, list(key, ConfIdGuess)], by.x = 'key2', by.y = 'key', all.x = T)

acleddata[, ConflictId := NA_character_]
acleddata[is.na(ConfIdGuess.x) & !is.na(ConfIdGuess.y), ConflictId := ConfIdGuess.y]
acleddata[!is.na(ConfIdGuess.x) & is.na(ConfIdGuess.y), ConflictId := ConfIdGuess.x]
acleddata[, ConfIdGuess.x := NULL]
acleddata[, ConfIdGuess.y := NULL]

# Don't let civilians start events
acleddata[ACTOR1 %in% civilians, SWAP := 1]
acleddata[ACTOR1 %in% civilians, ACTOR1 := acleddata[ACTOR1 %in% civilians, ACTOR2]]
acleddata[SWAP == 1, ACTOR2 := 'CIVILIANS']
acleddata[, SWAP := NULL]

# Only keep events involving actors who are fighting a given conflict
acleddata[, keep := 0L]
acleddata[, state_init := 0]
acleddata[, rebel_init := 0]
acleddata[ACTOR1 %in% rebels, rebel_init := 1]
acleddata[ACTOR1 %in% state, state_init := 1]

acleddata[(ACTOR1 %in% state & ACTOR2 %in% rebels)
          | (ACTOR1 %in% rebels & ACTOR2 %in% state)
          | (ACTOR1 %in% state & ACTOR2 %in% civilians)
          | (ACTOR1 %in% rebels & ACTOR2 %in% civilians), keep := 1]
acleddata <- acleddata[keep == 1]
acleddata[, keep := NULL]

# Identify urban-rural divide in event location
acled_coords <- project(cbind(acleddata$LONGITUDE, acleddata$LATITUDE), proj4string(grump))
test <- SpatialPointsDataFrame(cbind(acleddata$LONGITUDE, acleddata$LATITUDE), proj4string = CRS(proj4string(grump)), data = acleddata)
test <- spTransform(test, proj4string(grump))
acleddata$URBAN <- extract(grump, test)
acleddata <- data.table(acleddata)

# Final sad task: drop all conflicts where we have NO insurgent activity
acleddata <- acleddata[!ConflictId %in% c('1-131', '1-192')]
term <- term[!ConflictId %in% c('1-131', '1-192')]
# And one conflict where we have no activity AT ALL
# acleddata <- acleddata[!(ConflictId %in% '1-255' & YEAR < 2007)]
# acleddata <- acleddata[!(ConflictId %in% '1-225' & YEAR < 2011)]
# term <- term[!(ConflictId %in% '1-255' & episode == 1)]
# term <- term[!(ConflictId %in% '1-225' & episode == 1)]
term <- merge(ucdp[, list(ConflictId, Year, StartDate2)], term, by = c('ConflictId', 'Year'))
term[, StartDate2 := floor_date(as.Date(term$StartDate2), 'month')]

### RESULT: Conflict events that occur DURING conflicts that have ENDED in some way

### KLUDGY FIX: Get all STATE actions by pulling the union of actions that:
## 1. are associated with a specific conflict AND perpetrated by states
## 2. are in a given state AND perpetrated by states
### This lets me get all actions perpetrated by states against both rebels
###  and civilians during a conflict
# test2 <- acleddata[(ConflictId == '1-86' & state_init == 1) | (GWNO == 490 & state_init == 1)]

##### Generate monthly activity by dyad
merged_data <- data.table(
  conflict_id = character()
  , episode = integer()
  , gwno = integer()
  , month = as.Date(character())
  , state_events = numeric()
  , rebel_events = numeric()
  , state_indis = numeric()
  , rebel_indis = numeric()
  , state_civ = numeric()
  , rebel_civ = numeric()
  , state_urban = numeric()
  , rebel_urban = numeric()
  , outcome = integer()
  )

for(i in 1:nrow(term)){
  this_conf <- term$ConflictId[i]
  conf_ep <- term$episode[i]
  episode_data <- term[ConflictId %in% this_conf & episode %in% conf_ep]
  dates <- data.table(
    month = seq.Date(
      episode_data$StartDate2
      # max(episode_data$StartDate2, as.Date('1997-01-01'))
      , max(episode_data$EpEndDate, (episode_data$StartDate2 + 365)), by = 'month'))
  test <- acleddata[(((ConflictId %in% this_conf & state_init == 1)
                      | (GWNO %in% episode_data$GWNoA & state_init == 1))
                     | (ConflictId %in% this_conf & rebel_init == 1))
                    & (EVENT_DATE >= dates[1, month] & EVENT_DATE <= dates[nrow(dates), month])]

  test_out <- test[, list(
    conflict_id = this_conf
    , episode = unique(conf_ep)
    , gwno = unique(test$GWNO)
    , state_events = sum(state_init)
    , rebel_events = sum(rebel_init)
    , state_indis = sum(EVENT_TYPE == 'Remote violence' & state_init == 1) / sum(state_init)
    , rebel_indis = sum(EVENT_TYPE == 'Remote violence' & rebel_init == 1) / sum(rebel_init)
    , state_civ = sum(ACTOR2 %in% civilians & state_init == 1) / sum(state_init)
    , rebel_civ = sum(ACTOR2 %in% civilians & rebel_init == 1) / sum(rebel_init)
    , state_urban = sum(state_init == 1 & URBAN == 2) / sum(state_init)
    , rebel_urban = sum(rebel_init == 1 & URBAN == 2) / sum(rebel_init)
  ), by = list(EVENT_DATE)]

  setkeyv(test_out, 'EVENT_DATE')

  test_out <- merge(dates, test_out, by.x = 'month', by.y = 'EVENT_DATE', all.x = T)
  test_out[is.na(conflict_id), conflict_id := this_conf]
  test_out[is.na(episode), episode := conf_ep]
  test_out[is.na(gwno), gwno := unique(test_out$gwno[!is.na(test_out$gwno)])[1]]
  test_out[, outcome := term$Outcome[i]]
  merged_data <- rbind(merged_data, test_out)
}



#################
##
## Fill in missing data points with previous values
##
#################

## Nice function from Stackoverflow to deal with NA's
repNAmed <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    x
  }  else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}

replaceNaWithLatest <- function(dfIn, nameColNa = names(dfIn)[1]
){
  dtTest <- data.table(dfIn)
  setnames(dtTest, nameColNa, "colNa")
  dtTest[is.nan(colNa), colNa := NA]
  dtTest[, segment := cumsum(!is.na(colNa))]
  dtTest[, colNa := colNa[1], by = "segment"]
  dtTest[, segment := NULL]
  setnames(dtTest, "colNa", nameColNa)
  return(dtTest)
}


merged_data[, state_combined1 := 0.0]
merged_data[, rebel_combined1 := 0.0]
merged_data[, state_combined2 := 0.0]
merged_data[, rebel_combined2 := 0.0]

for(i in 1:nrow(term)){
  this_conf <- term$ConflictId[i]
  this_ep <- term$episode[i]
  this_data <- merged_data[conflict_id %in% this_conf & episode %in% this_ep]

  for(name in names(this_data)[-c(13:16)]){
    this_data <- replaceNaWithLatest(this_data, name)
  }

  for(name in names(this_data)[-c(1:4, 13:16)]){
    this_data[which(!is.finite(name) | is.na(name)), name] <- 0
    this_data <- replaceNaWithLatest(this_data, name)
  }
  merged_data[conflict_id %in% this_conf & episode %in% this_ep] <- this_data

  # Create combined metric(s) of violence
  merged_data[conflict_id %in% this_conf & episode %in% this_ep
            , state_combined1 := as.numeric(rowSums(cbind(state_indis, state_civ, state_urban), na.rm =T))]
  merged_data[conflict_id %in% this_conf & episode %in% this_ep
            , rebel_combined1 := as.numeric(rowSums(cbind(rebel_indis, rebel_civ, rebel_urban), na.rm =T))]

  merged_data[conflict_id %in% this_conf & episode %in% this_ep
            , state_combined2 := as.numeric(rowSums(cbind(scale(state_indis), scale(state_civ), scale(state_urban)), na.rm =T))]
  merged_data[conflict_id %in% this_conf & episode %in% this_ep
            , rebel_combined2 := as.numeric(rowSums(cbind(scale(rebel_indis), scale(rebel_civ), scale(rebel_urban)), na.rm = T))]

}

# Format conflict IDs
merged_data[, conflict_id := as.integer(sapply(strsplit(merged_data$conflict_id, '-'), '[', 2))]



# Save to file
write.csv(merged_data, file = 'monthly_violence.csv', row.names = F)









