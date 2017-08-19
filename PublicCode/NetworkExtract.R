###############################################################################
##
## Function: NetworkExtract
##
## Purpose: split each tsna object into a list of monthly arrays,
##  where every array slice maps onto one root code and every element (i, j, x)
##  contains the *normalized* level of interaction between states i and j on
##  dimension x during a given month.
##
## Arguments:
##  - in_data: list of tsna objects produced by phoenix_net() function.
##  - tie_weights ('raw', 'goldstein'): should ties be weighted by count (raw)
##    or by count * Goldstein score (goldstein)? Goldstein scores include
##    more nuance about event severity, but are also unreliable/uninformative.
##
## Outputs:
##  - list of arrays where each array 'slice' is one network adjacency matrix.
##
###############################################################################

NetworkExtract <- function(
  in_data
  , tie_weights = 'raw'
  , ties_normalize = 'state'
  , exclude_codes = NULL
  , time_window = 'month'
){
  ## Set up dates
  global_start_end <- get.network.attribute(in_data[[1]],'net.obs.period')$observations[[1]]
  global_start_date_int <- global_start_end[1]
  global_end_date_int <- global_start_end[2]
  global_start_date <- as.Date(as.character(global_start_date_int), format = '%Y%m%d')
  global_end_date <- as.Date(as.character(global_end_date_int), format = '%Y%m%d')
  dates <- as.integer(format(seq.Date(global_start_date, global_end_date, time_window), '%Y%m%d'))
  start_date <- dates[1]
  end_date <- last(dates)

  ## Get size of global network
  n_states <- network.size(in_data[[1]])

  ## Get names of states
  states <- network::get.vertex.attribute(in_data[[1]], 'vertex.names')

  ## Subset n_codes to exclude certain classes of event
  n_codes <- length(in_data)
  if(!is.null(exclude_codes)){
    n_codes <- setdiff(n_codes, exclude_codes)
  }

  ## Pull out individual rootcode-networks, normalize, and store each adjacency
  ##  matrix as one layer in a monthly event-network-array.
  interactions <- list()
  events_array <- array(0, dim = c(n_states, n_states, n_codes))
  dimnames(events_array)[[1]] <- states
  dimnames(events_array)[[2]] <- states
  dimnames(events_array)[[3]] <- names(in_data)

  for(this_date in 1:length(dates)){

    ## Create array to hold event network matrices
    thismonth_events <- events_array

    for(this_code in n_codes){

      ## Extract single event network
      events_tsna <- in_data[[this_code]]

      ## Create matrix for event interaction data at time T
      events_net <- network.collapse(events_tsna, at = dates[this_date])
      events_mat <- try(
        as.matrix.network(events_net, attrname = list.edge.attributes(events_net)[1])
        , silent = T)


      ## Normalize the events network: what share of state i's interactions does
      ##  dyad (i,j) make up?
      ## NOTE: The goal of normalization here is to allow more direct comparison
      ##  between different categories of events, as some events are much more
      ##  common (~10x) than others. Normalizing each event type means that we
      ##  can directly compare "conflictual" and "cooperative" relations despite
      ##  cooperative acts generally being more common.If 90% of state i's
      ##  conflictual relations are with state j, then that is likely a more
      ##  fraught relationship, even (especially?) if state i does not engage
      ##  in a high leve of overall conflict.
      if (ties_normalize == 'state'){
        if(sum(events_mat) > 0){
          # events_mat <- events_mat / sum(events_mat)
          events_rowsums <- rowSums(events_mat)
          events_rowsums[events_rowsums == 0] <- 1
          events_mat <- events_mat / events_rowsums
        }
      } else if (ties_normalize == 'global'){
        if(sum(events_mat) > 0){
          events_mat <- events_mat / sum(events_mat)
        }
      }

      ## Store normalized event-month network in this array
      thismonth_events[, , this_code] <- events_mat
    }

    ## Store fully populated array in array list by date
    interactions[[this_date]] <- thismonth_events
    names(interactions) <- dates

  }
  return(interactions)
}
