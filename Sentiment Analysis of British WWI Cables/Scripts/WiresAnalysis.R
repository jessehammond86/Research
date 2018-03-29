###############
##
## Set up workspace
##
###############

## Load packages

rm(list = ls())

if (!'pacman' %in% installed.packages()) {
  install.packages('pacman')
}

pacman::p_load(data.table, )


## Set working directory

os_system <- Sys.info()['sysname']

if (os_system == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/Research/CablesSentiment')
  
} else if (os_system == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/Speeches')
  
} else if (os_system == 'Linux'){
  setwd('/media/Files/jesse/Dropbox/Speeches')
}


###############
##
## Load in data
##
###############

## Read processed data

ukdata <- fread('./Data/ProcessedData/cables_processed_6words.csv')
ukpca <- prcomp(
  ukdata[, list(posneg_scores_sum, strongweak_scores_sum, actpas_scores_sum)]
  , scale. = T
  )

plot(ukpca$x[, 3])
################################################################################

kerry <- fread('./Data/ProcessedData/kerryblogs_processed.csv')
kerry$date <- as.Date(kerry$date)

plot(
  kerry[
    date > as.Date('2006-09-01') & date < as.Date('2007-04-01')
    , negpos_scores
    ] ~ kerry[
      date > as.Date('2006-09-01') & date < as.Date('2007-04-01')
      , date
      ]
  , type = 'l'
  , ylim = c(-2, 0)
  )

lines(
  kerry[
    date > as.Date('2006-09-01') & date < as.Date('2007-04-01')
    , vader_scores
    ] ~ kerry[
      date > as.Date('2006-09-01') & date < as.Date('2007-04-01')
      , date
      ]
  , col = 'red'
)

lines(
  kerry[
    date > as.Date('2006-07-01') & date < as.Date('2007-04-01')
    , strongweak_scores
    ] ~ kerry[
      date > as.Date('2006-07-01') & date < as.Date('2007-04-01')
      , date
      ]
  , col = 'blue'
)

lines(
  kerry[
    date > as.Date('2006-07-01') & date < as.Date('2007-04-01')
    , actpas_scores
    ] ~ kerry[
      date > as.Date('2006-07-01') & date < as.Date('2007-04-01')
      , date
      ]
  , col = 'purple'
)

spd <- fread('./Data/StanfordDictionary/SPD.csv')
spd[is.na(spd)] <- 0
spd <- unique(spd)
spd[, PosNeg := Positive - Negative]
spd[, StrWk := Strong - Weak]
spd[, ActPas := Active - Passive]


## Documents location

docs_loc <- './Data/IndividualWires'

## Documents ID table
docs_table <- fread('./Data/OriginsOfWar/1914_record-3_jrh.csv')


###############
##
## Pre-formatting data
##
###############

# Process document dates to DATE format

docs_table$DateSent <- paste(docs_table$DateSent, ', 1914', sep = '')
docs_table$DateSent <- as.Date(docs_table$DateSent, format = '%B %d, %Y')
docs_table$DateReceived <- paste(docs_table$DateReceived, ', 1914', sep = '')
docs_table$DateReceived <- as.Date(docs_table$DateReceived, format = '%B %d, %Y')
setkeyv(docs_table, 'Filename')


## Split docs on TO vs FROM Grey & Nicolson

to_docs <- grep('Grey|Nicol', docs_table$Recipient)
from_docs <- grep('Grey|Nicol', docs_table$Author)
both_docs <- intersect(to_docs, from_docs)
to_docs <- setdiff(to_docs, both_docs)


###############
##
## Generating document corpus
##
###############

## Read in documents to corpus

docs <- Corpus(DirSource(docs_loc))



corpus_processing <- function(
  main_corp
  , search_string
  , n_words
) {


  ###############
  ##
  ## Corpus analysis 1: calculate raw word-count
  ##
  ###############
  
  ## FUNCTION: process corpus to raw words
  
  clean_corpus <- function(corp){
    
    ## Cycle through individual documents
    for(i in 1:length(corp)){
      doc1 <- corp[[i]][1]$content
      ## Remove carriage returns
      doc1 <- gsub("\r?\n|\r|\n", " ", doc1)
      ## Get rid of all punctuation
      # doc1 <- gsub("[[:punct:]]", "", doc1)
      ## Remove empty strings
      doc1 <- doc1[-which(doc1 == '')]
      ## Append all sentence/wordstrings to one large string
      doc1 <- paste(doc1, collapse = ' ')
      ## Get rid of extra spaces
      # doc1 <- gsub("\\s+", " ", doc1)
      ## Split into individual words
      doc1s <- unlist(strsplit(doc1, split = ' '))
      ## Remove non-ASCII characters
      invalid_idx <- grep(
        'doc1s'
        , iconv(doc1s, 'latin1', 'ASCII', sub = 'doc1s')
        , value = F)
      if (length(invalid_idx) > 0){
        doc1s <- doc1s[-invalid_idx]
      }
      
      ## Replace original entry with cleaned entry
      corp[[i]][1]$content <- doc1s[!doc1s %in% '']
    }
    
    ## Clean and convert to lowercase
    corp <- tm_map(corp, PlainTextDocument, mc.cores = 1)
    corp <- tm_map(corp, tolower, mc.cores = 1)
    corp <- tm_map(corp, removeNumbers, mc.cores = 1)
    corp <- tm_map(corp, removePunctuation, mc.cores = 1)
    corp <- tm_map(corp, stripWhitespace, mc.cores = 1)
    corp <- tm_map(corp, PlainTextDocument, mc.cores = 1)
    
    return(corp)
  }
  
  
  ###############
  ##
  ## Corpus analysis 2: stem and prepare for text analysis
  ##
  ###############
  
  ## FUNCTION: clean and stem pre-processed corpus
  
  stem_corpus <- function(corp){
    
    corp <- tm_map(corp, PlainTextDocument, mc.cores = 1)
    corp <- tm_map(corp, tolower, mc.cores = 1)
    corp <- tm_map(corp, removeWords, stopwords(kind="en")[-c(167)], mc.cores = 1)
    corp <- tm_map(corp, stemDocument, mc.cores = 1)
    corp <- tm_map(corp, PlainTextDocument, mc.cores = 1)
    return(corp)
  }
  
  
  ###############
  ##
  ## Corpus analysis 3: retrieve words surrounding key search terms
  ##
  ###############
  
  ## FUNCTION: search out key words and retrieve N words on either side
  
  search_keywords <- function(
    corp
    , searchstring
    , nwords
    ){
    
    for(i in 1:length(corp)){
      doc1 <- corp[[i]][1]$content
      search_idx <- grep(searchstring, doc1)
      search_before_idx <- search_idx - nwords
      search_after_idx <- search_idx + nwords
      search_extract <- data.frame(
        'before_idx' = search_before_idx
        , 'after_idx' = search_after_idx
        , 'keyword_idx' = search_idx)
      
      if(nrow(search_extract) > 0){
        search_extract[which(search_extract[,1] <= 0), 1] <- 1
        search_extract[which(search_extract[,2] <= 0), 2] <- 1
        search_extract[nrow(search_extract), 2] <- min(search_extract[nrow(search_extract), 2], length(doc1))
        
        word_indices <- c()
        for(j in 1:nrow(search_extract)){
          word_indices <- c(word_indices, seq(search_extract[j, 1], search_extract[j,2]))
        }
        
        word_indices <- word_indices[!word_indices %in% search_idx]
        word_indices <- unique(word_indices)
        
        doc1s <- doc1[word_indices]
        doc1s <- doc1s[doc1s %in% spd$Word]
        doc1s <- paste(doc1s, collapse = ' ')
        corp[[i]][1]$content <- doc1s
      } else {
        corp[[i]][1]$content <- ''
      }
    }
    
    corp <- tm_map(corp, PlainTextDocument, mc.cores = 1)
    
    return(corp)
  }
  
  
  ###############
  ##
  ## Merge with SPD and extract scores by cable and day sent
  ##
  ###############
  
  ## FUNCTION: extract daily mean SPD scores of key words near search terms
  
  extract_scores <- function(
    corp
    , dates_list
    , state = 'received'
    , dir = 'in'){
    
    ## Create output object to hold daily scores
    daily_data <- data.frame(
      date = dates_list
      , 'daily_posneg' = rep(0, length(dates_list))
      , 'daily_strwk' = rep(0, length(dates_list))
      , 'daily_actpas' = rep(0, length(dates_list))
    )
    
    ## Set date of sending vs date of receipt
    if(state == 'received'){
      datedir <- 'DateReceived'
    } else{
      datedir <- 'DateSent'
    }
    
    ## Loop through dates
    for(i in 1:length(dates_list)){
      this_date <- dates_list[i]
      
      if(dir == 'in'){
        this_doctable <- docs_table[to_docs]
        these_docs <- which(this_doctable[[datedir]] %in% this_date)
      } else if(dir == 'out') {
        this_doctable <- docs_table[from_docs]
        these_docs <- which(this_doctable[[datedir]] %in% this_date)
      }
      
      ## Subset corpus to only include this date's set of documents
      this_corpus <- corp[these_docs]
      
      ## Calculate the number of times each key word appears in the corpus
      if(length(this_corpus) > 0){
        doc_dtm <- DocumentTermMatrix(
          this_corpus
          , control = list(
            wordLengths = c(2, Inf)
            )
          )
        
        doc_dtm <- as.matrix(doc_dtm)
        doc_spd <- colSums(doc_dtm)
        
        words <- data.frame(
          'word' = colnames(doc_dtm)
          , 'weight' = colSums(
            as.matrix(doc_dtm)
            )
          )
        
        ## Pull out words that appear in the SPD
        this_spd <- spd[Word %in% words$word]
        this_spd$thisdoc_words <- doc_spd
        
        ## Calculate MEAN WEIGHTS of today's SPD words
        # daily_data$daily_posneg[i] <- mean(rep(this_spd$PosNeg, this_spd$thisdoc_words))
        # daily_data$daily_strwk[i] <- mean(rep(this_spd$StrWk, this_spd$thisdoc_words))
        # daily_data$daily_actpas[i] <- mean(rep(this_spd$ActPas, this_spd$thisdoc_words))
  #       
  #       daily_data$daily_posneg[i] <- mean(rep(this_spd$PosNeg[this_spd$PosNeg != 0], this_spd$thisdoc_words[this_spd$PosNeg != 0]))
  #       daily_data$daily_strwk[i] <- mean(rep(this_spd$StrWk[this_spd$StrWk != 0], this_spd$thisdoc_words[this_spd$StrWk != 0]))
  #       daily_data$daily_actpas[i] <- mean(rep(this_spd$ActPas[this_spd$ActPas != 0], this_spd$thisdoc_words[this_spd$ActPas != 0]))
        
        daily_data$daily_posneg[i] <- sum(this_spd$PosNeg * this_spd$thisdoc_words)
        daily_data$daily_strwk[i] <- sum(this_spd$StrWk * this_spd$thisdoc_words)
        daily_data$daily_actpas[i] <- sum(this_spd$ActPas * this_spd$thisdoc_words)
      } else {
        daily_data$daily_posneg[i] <- NA
        daily_data$daily_strwk[i] <- NA
        daily_data$daily_actpas[i] <- NA
      }
    }
    
    return(daily_data)
  }
  
  
  
  
  ###############
  ##
  ## Cleaning, stemming, and analyzing text corpus data
  ##
  ###############
  
  
  ## Clean corpus and count unique words
  
  cleaned_corpus <- clean_corpus(docs)
  
  unique_wordcount <- ncol(as.matrix(DocumentTermMatrix(cleaned_corpus)))
  
  
  ## Stem corpus
  
  stemmed_corpus <- stem_corpus(cleaned_corpus)
  
  
  ## Identify words surrounding keywords
  
  keywords_corpus <- search_keywords(
    corp = stemmed_corpus
    , searchstring = search_string
    , nwords = n_words)
  
  
  ## Format and merge date-of-sending/-receiving data for cables
  
  # Drop communiques after UK enters war on 8/4/14
  sent_dates_list <- sort(
    unique(
      docs_table$DateSent
    )
  )[-c(41:46)] 
  
  # Drop communiques after UK enters war on 8/4/14
  rec_dates_list <- sort(
    unique(
      docs_table$DateReceived
    )
  )[-c(41:46)]
  
  # Calculate INCOMING daily SPD scores
  incoming_dailyscores <- extract_scores(
    corp = keywords_corpus
    , dates_list = rec_dates_list
    , state = 'received'
    , dir = 'in'
  )
  
  # Calculate OUTGOING daily SPD scores
  outgoing_dailyscores <- extract_scores(
    corp = keywords_corpus
    , dates_list = rec_dates_list
    , state = 'received'
    , dir = 'out'
  )
  
  # Merge with date range to fill in missing dates
  full_datesrange <- seq.Date(
    as.Date('06-19-1914', format = '%m-%d-%Y')
    , as.Date('08-04-1914', format = '%m-%d-%Y')
    , by = 'day')
  full_datesrange <- data.frame('date' = full_datesrange)
  
  incoming_dailyscores <- merge(incoming_dailyscores, full_datesrange, by = 'date', all.x = T, all.y = T)
  outgoing_dailyscores <- merge(outgoing_dailyscores, full_datesrange, by = 'date', all.x = T, all.y = T)
  
  
  ## Combine output data to one table
  
  out_data <- data.table(
    incoming_dailyscores
    , outgoing_dailyscores[
      , c('daily_posneg', 'daily_strwk', 'daily_actpas')]
    )
  
  setnames(
    out_data
    , c('date'
        , paste(
          c('over_pn', 'over_sw', 'over_ap'
            , 'lond_pn', 'lond_sw', 'lond_ap'
            )
          , n_words
          , sep = '_'
          )
        )
  )
  
  ## Return combined data
  
  return(out_data)
}



###############
##
## Generate daily SPD-weight data
##
###############

search_string <- 'german|austr|hunga|dual|alliance'
n_words = 6

out_data <- wordcount_all(main_corp = docs, search_string = 'french', n_words = 6)
write.csv(
  out_data[, list(date, lond_pn_6, lond_sw_6, lond_ap_6)]
  , file = './londoncables_france_6words.csv'
  , row.names = F)


###############
##
## Save output to file
##
###############


###### Save data to file

out_data <- data.table(in_outgroup_dailyscores
                       , out_outgroup_dailyscores[, c('daily_posneg', 'daily_strwk', 'daily_actpas')])
setnames(out_data, c('date', c('over_pn_9', 'over_sw_9', 'over_ap_9'
                               , 'lond_pn_9', 'lond_sw_9', 'lond_ap_9')))

write.csv(out_data, file = 'daily_9words.csv', row.names = F, na = '.')

###### Make table of cable frequency by day
all_dates <- data.table(
  'DateSent' = seq.Date(
    from = as.Date('1914-06-19')
    , to = as.Date('1914-08-04')
    , by = 'day')
  )
out_freq <- docs_table[from_docs, .N, by = DateSent]
sent_table <- merge(all_dates, out_freq, by = 'DateSent', all.x = T)
sent_table[is.na(N), N := 0]
write.csv(sent_table, file = 'london_sent_freq.csv', row.names = F)


######################## OUTGROUP PLOTS
## Plots of INCOMING documents
dev.off()
pdf(file = 'Incoming_Outgroup_ActivePassive.pdf', width = 10, height = 6)
plot(in_outgroup_dailyscores$daily_actpas ~ in_outgroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Active/Passive Language'
     , xlab = 'Date'
     , main = 'Active vs Passive Language TO G & N')

var_loess <- loess(daily_actpas ~ seq(1:nrow(in_outgroup_dailyscores))
                      , data = in_outgroup_dailyscores)
var_predict <- predict(var_loess, in_outgroup_dailyscores, se = T)
lines(var_predict$fit ~ in_outgroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ in_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ in_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')

dev.off()

pdf(file = 'Incoming_Outgroup_PositiveNegative.pdf', width = 10, height = 6)
plot(in_outgroup_dailyscores$daily_posneg ~ in_outgroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Positive/Negative Language'
     , xlab = 'Date'
     , main = 'Positive vs Negative Language TO G & N')

var_loess <- loess(daily_posneg ~ seq(1:nrow(in_outgroup_dailyscores))
                   , data = in_outgroup_dailyscores)
var_predict <- predict(var_loess, in_outgroup_dailyscores, se = T)
lines(var_predict$fit ~ in_outgroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ in_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ in_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()

pdf(file = 'Incoming_Outgroup_StrongWeak.pdf', width = 10, height = 6)
plot(in_outgroup_dailyscores$daily_strwk ~ in_outgroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Strong/Weak Language'
     , xlab = 'Date'
     , main = 'Strong vs Weak Language TO G & N')

var_loess <- loess(daily_strwk ~ seq(1:nrow(in_outgroup_dailyscores))
                   , data = in_outgroup_dailyscores)
var_predict <- predict(var_loess, in_outgroup_dailyscores, se = T)
lines(var_predict$fit ~ in_outgroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ in_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ in_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()


## Plots of OUTGOING documents
dev.off()
pdf(file = 'Outgoing_Outgroup_ActivePassive.pdf', width = 10, height = 6)
plot(out_outgroup_dailyscores$daily_actpas ~ out_outgroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Active/Passive Language'
     , xlab = 'Date'
     , main = 'Active vs Passive Language FROM G & N')

var_loess <- loess(daily_actpas ~ seq(1:nrow(out_outgroup_dailyscores))
                   , data = out_outgroup_dailyscores)
var_predict <- predict(var_loess, out_outgroup_dailyscores, se = T)
lines(var_predict$fit ~ out_outgroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ out_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ out_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')

dev.off()

pdf(file = 'Outgoing_Outgroup_PositiveNegative.pdf', width = 10, height = 6)
plot(out_outgroup_dailyscores$daily_posneg ~ out_outgroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Positive/Negative Language'
     , xlab = 'Date'
     , main = 'Positive vs Negative Language FROM G & N')

var_loess <- loess(daily_posneg ~ seq(1:nrow(out_outgroup_dailyscores))
                   , data = out_outgroup_dailyscores)
var_predict <- predict(var_loess, out_outgroup_dailyscores, se = T)
lines(var_predict$fit ~ out_outgroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ out_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ out_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()

pdf(file = 'Outgoing_Outgroup_StrongWeak.pdf', width = 10, height = 6)
plot(out_outgroup_dailyscores$daily_strwk ~ out_outgroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Strong/Weak Language'
     , xlab = 'Date'
     , main = 'Strong vs Weak Language FROM G & N')

var_loess <- loess(daily_strwk ~ seq(1:nrow(out_outgroup_dailyscores))
                   , data = out_outgroup_dailyscores)
var_predict <- predict(var_loess, out_outgroup_dailyscores, se = T)
lines(var_predict$fit ~ out_outgroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ out_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ out_outgroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()









######################## INGROUP PLOTS
## Plots of INCOMING documents
dev.off()
pdf(file = 'Incoming_Ingroup_ActivePassive.pdf', width = 10, height = 6)
plot(in_ingroup_dailyscores$daily_actpas ~ in_ingroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Active/Passive Language'
     , xlab = 'Date'
     , main = 'Active vs Passive Language TO G & N')

var_loess <- loess(daily_actpas ~ seq(1:nrow(in_ingroup_dailyscores))
                   , data = in_ingroup_dailyscores)
var_predict <- predict(var_loess, in_ingroup_dailyscores, se = T)
lines(var_predict$fit ~ in_ingroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ in_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ in_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')

dev.off()

pdf(file = 'Incoming_Ingroup_PositiveNegative.pdf', width = 10, height = 6)
plot(in_ingroup_dailyscores$daily_posneg ~ in_ingroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Positive/Negative Language'
     , xlab = 'Date'
     , main = 'Positive vs Negative Language TO G & N')

var_loess <- loess(daily_posneg ~ seq(1:nrow(in_ingroup_dailyscores))
                   , data = in_ingroup_dailyscores)
var_predict <- predict(var_loess, in_ingroup_dailyscores, se = T)
lines(var_predict$fit ~ in_ingroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ in_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ in_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()

pdf(file = 'Incoming_Ingroup_StrongWeak.pdf', width = 10, height = 6)
plot(in_ingroup_dailyscores$daily_strwk ~ in_ingroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Strong/Weak Language'
     , xlab = 'Date'
     , main = 'Strong vs Weak Language TO G & N')

var_loess <- loess(daily_strwk ~ seq(1:nrow(in_ingroup_dailyscores))
                   , data = in_ingroup_dailyscores)
var_predict <- predict(var_loess, in_ingroup_dailyscores, se = T)
lines(var_predict$fit ~ in_ingroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ in_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ in_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()


## Plots of OUTGOING documents
dev.off()
pdf(file = 'Outgoing_Ingroup_ActivePassive.pdf', width = 10, height = 6)
plot(out_ingroup_dailyscores$daily_actpas ~ out_ingroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Active/Passive Language'
     , xlab = 'Date'
     , main = 'Active vs Passive Language FROM G & N')

var_loess <- loess(daily_actpas ~ seq(1:nrow(out_ingroup_dailyscores))
                   , data = out_ingroup_dailyscores)
var_predict <- predict(var_loess, out_ingroup_dailyscores, se = T)
lines(var_predict$fit ~ out_ingroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ out_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ out_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')

dev.off()

pdf(file = 'Outgoing_Ingroup_PositiveNegative.pdf', width = 10, height = 6)
plot(out_ingroup_dailyscores$daily_posneg ~ out_ingroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Positive/Negative Language'
     , xlab = 'Date'
     , main = 'Positive vs Negative Language FROM G & N')

var_loess <- loess(daily_posneg ~ seq(1:nrow(out_ingroup_dailyscores))
                   , data = out_ingroup_dailyscores)
var_predict <- predict(var_loess, out_ingroup_dailyscores, se = T)
lines(var_predict$fit ~ out_ingroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ out_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ out_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()

pdf(file = 'Outgoing_Ingroup_StrongWeak.pdf', width = 10, height = 6)
plot(out_ingroup_dailyscores$daily_strwk ~ out_ingroup_dailyscores$date
     , type = 'l'
     , ylab = 'Net Strong/Weak Language'
     , xlab = 'Date'
     , main = 'Strong vs Weak Language FROM G & N')

var_loess <- loess(daily_strwk ~ seq(1:nrow(out_ingroup_dailyscores))
                   , data = out_ingroup_dailyscores)
var_predict <- predict(var_loess, out_ingroup_dailyscores, se = T)
lines(var_predict$fit ~ out_ingroup_dailyscores$date, col = 'blue')
lines(var_predict$fit - 1.964*(var_predict$se.fit)  ~ out_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
lines(var_predict$fit + 1.964*(var_predict$se.fit)  ~ out_ingroup_dailyscores$date
      , lty = 2, col = 'grey60')
abline(h = 0, col = 'red')
dev.off()
