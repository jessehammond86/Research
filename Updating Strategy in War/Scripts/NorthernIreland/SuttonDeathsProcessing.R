library(data.table)
library(gsubfn)
library(lubridate)


#setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland')
setwd('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland')
sutton <- read.csv('SuttonDeaths.csv')


sutton_dates_idx <- grep('[0-9]+-[A-Z][a-z]+', sutton$Data)

sutton_dates <- as.character(sutton$Data[sutton_dates_idx])
sutton_dates <- strptime(sutton_dates, format = '%d-%b-%y')
sutton_out <- data.frame(sutton_dates)
names(sutton_out) <- 'Date'
sutton_out$Year <- as.integer(strftime(sutton_dates, format = '%Y'))

sutton_names_idx <- sutton_dates_idx + 1
sutton_out$Name <- strapplyc(as.character(sutton$Data[sutton_names_idx]), '[[:alpha:]]+, [[:alpha:]]+', simplify = T)
sutton_out$Age <- as.integer(strapplyc(as.character(sutton$Data[sutton_names_idx]), '[[:digit:]]+', simplify = T))
sutton_out$Religion <- strapplyc(as.character(sutton$Data[sutton_names_idx]), '[[:alpha:]]+$', simplify = T)

sutton_status_idx <- sutton_dates_idx + 2
sutton_out$Status <- do.call('c', strapply(as.character(sutton$Data[sutton_status_idx]), '(?<=: )(.+)(?=,)', perl = T))
sutton_out$KilledBy <- do.call('c', strapply(as.character(sutton$Data[sutton_status_idx]), '(?<=Killed by: )(.+)$', perl = T))

sutton_details_idx <- sutton_dates_idx + 3
sutton_out$Details <- sutton$Data[sutton_details_idx]
write.csv(sutton_out, file = 'SuttonDeathsTable.csv', row.names = F)
