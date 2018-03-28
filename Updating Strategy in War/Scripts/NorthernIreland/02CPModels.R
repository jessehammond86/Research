rm(list = ls())
setwd('/Users/localadmin/Dropbox/Research/Dissertation/Data/NorthernIreland')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/NorthernIreland')
pacman::p_load(rgdal, foreign, data.table, reshape2, TSA
               , vars, zoo, lubridate
               , xts, forecast, ecp, ggplot2)

## Read in data
all_deaths <- fread('NI_quarter_deaths.csv')
belfast_deaths <- fread('belfast_quarter_deaths.csv')
rural_deaths <- fread('rural_quarter_deaths.csv')

test_data <- data.table(
  date = seq.Date(as.Date('2011-01-01'), as.Date('2012-01-01'), by = 'week')
  , testvar = c(
    rep(1, 26) + rnorm(26, 0, 2)
    , rep(1, 27) + rnorm(27, 5, 2)
    )
)

x <- test_data


########################################
##
##  Name: cp_function
##  Purpose: detect and plot change points via Bayesian analysis (ecp)
##  Arguments:
##    x: input data frame
##    y: name of values column on which to detect change points
##    datecol: name of date column
##    sig.lvl: P-value cutoff for detecting change points
##
##



## Changepoint detection and plotting
# 1. Decompose time series of actor behavior and take 'trend' component
# 2. Use e.divisive change point analysis to find the major point(s) at which
#    actor behavior changes.
# 3. Use ggplot to show where changepoints occur, and regime-level means.

cp_function <- function(x, y, sig.lvl = 0.05
                        , xlabel = 'X'
                        , ylabel = 'Y'
                        , maintitle = 'Test'){
  
  ## Double-check to make sure date is formatted as a Date object
  if(!is.Date(x[[y]])){
    x[[y]] <- as.Date(x[[y]])
    if(is.na(x[[y]][1])){
      break("Please make sure that the date variable is properly specified.")
    }
  }
  
  ## Decompose quarterly time series to remove periodic component
  x[[y]] <- ts(x[[y]])
  
  ##### CP detection
  input_cp <- e.divisive(
    as.matrix(x[[y]])
    , R = 999
    , min.size = 20
    , sig.lvl = sig.lvl
    )
  
  input_change_idx <- input_cp$estimates[c(2,3)]
  if(length(unique(input_cp$cluster)) == 1){
    break('No changepoints detected.')
  }
  input_means <- data.frame('mean' = numeric(1)
                            , 'from' = structure(numeric(1), class = 'Date')
                            , 'to' = structure(numeric(1), class = 'Date')
                            , 'to2' = structure(numeric(1), class = 'Date'))
  idx <- c(input_change_idx, nrow(inputs))
  for(i in 1:length(idx)){
    i1 <- idx[i-1]
    if(length(i1) == 0){
      i1 <- 1
    }
    i2 <- idx[i]
    if(i2 > nrow(inputs)){
      i2 <- nrow(inputs)
    }
    input_means <- rbind(input_means, list(mean(inputs[i1:i2, y], na.rm = T)
                                           , inputs[i1, quarter]
                                           , inputs[, data.table::shift(quarter)][i2]
                                           , inputs[i2, quarter]))
  }
  input_means <- input_means[-1, ]
  input_means <- input_means[complete.cases(input_means), ]
  
  ## Print means and change points
  print(input_means[, c('mean', 'from', 'to2')])
  
  ## Drop final date entry - we don't need a Vline on the last recorded date
  input_means[nrow(input_means), 'to2'] <- NA
  
  ## Plot CP
  cp_plot <- ggplot() +
    geom_line(data = inputs, aes(quarter, y)) +
    geom_rect(data = input_means, aes(xmin = to, xmax = to2, ymin = -Inf, ymax = Inf)
              , alpha = 0.5, fill = c('blue')) +
    geom_segment(data = input_means, aes(x = from, xend = to
                                         , y = mean, yend = mean), color = 'red') +
    labs(x = xlabel, y = ylabel) +
    ggtitle(maintitle) + 
    theme(panel.background = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , axis.text=element_text(size=12)
          , axis.title=element_text(size=14)
          , plot.margin=unit(c(1,1,1,1),"cm"))
  
  return(cp_plot)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## Hypothesis 1:
## State violence in Belfast/Derry -> Republican violence in rural areas
p1 <- cp_function(all_deaths, 'STATEURBAN'
                  , xlabel = 'Time'
                  , ylabel = '% of British fatalities in urban areas'
                  , maintitle = '')       # Change: 1983-07-01, 1989-01-01
p2 <- cp_function(all_deaths, 'REPUBURBAN'
                  , xlabel = 'Time'
                  , ylabel = '% of Republican fatalities in urban areas'
                  , maintitle = '')       # Change: 1982-10-01, 1991-07-01
cp_function(all_deaths, 'UNIONURBAN')       # Change: No change

dev.off()
pdf(file = 'StateUrbanRepubUrbanCP.pdf', width = 10, height = 10)
multiplot(p1, p2)
dev.off()

dev.off()
pdf(file = 'StateUrbanRepubUrbanCP_2.pdf', width = 10, height = 5)
plot(p2)
dev.off()

## Hypothesis 2:
## Republican attacks on civilians -> Unionist indiscriminate -> Republican attacks on civilians
cp_function(all_deaths, 'repub_civ_pct')       # Change: No change
cp_function(all_deaths, 'union_indis_pct')       # Change: 1977-07-01
cp_function(all_deaths, 'repub_indis_pct')       # Change: 1992-07-01


## Hypothesis 3:
## State killing Republicans -> Republican kills -> State killing Republicans
cp_function(all_deaths, 'STATEMILKILLS')       # Change: 1975-07-01, 1992-04-01
cp_function(belfast_deaths, 'STATEMILKILLS')   # Change: 1978-07-01
cp_function(rural_deaths, 'STATEMILKILLS')     # Change: 1992-04-01

cp_function(all_deaths, 'REPUBKILLS')      # Change: 1978-04-01, 1992-04-01
cp_function(belfast_deaths, 'REPUBKILLS')  # Change: 1977-10-01, 1982-10-01
cp_function(rural_deaths, 'REPUBKILLS')  # Change: 1977-04-01, 1991-01-01

cp_function(all_deaths, 'REPUBKILLSBASES')      # Change: No change 
cp_function(belfast_deaths, 'REPUBKILLSBASES')  # Change: No change
cp_function(rural_deaths, 'REPUBKILLSBASES')  # Change: No change


p1 <- cp_function(all_deaths, 'STATEMILKILLS'
                  , xlabel = 'Time'
                  , ylabel = '# of British fatal attacks'
                  , maintitle = '')       # Change: 1983-07-01, 1989-01-01
p2 <- cp_function(all_deaths, 'REPUBKILLS'
                  , xlabel = 'Time'
                  , ylabel = '# of Republican fatal attacks'
                  , maintitle = '')       # Change: 1982-10-01, 1991-07-01

dev.off()
pdf(file = 'StateKillsRepubKillsCP.pdf', width = 10, height = 10)
multiplot(p1, p2)
dev.off()

## Hypothesis 4:
## Unionist violence in Catholic areas -> Republican violence in Protestant areas
cp_function(all_deaths, 'REPUBKILLSCATH')      # Change: 1987-04-01
cp_function(belfast_deaths, 'REPUBKILLSCATH')  # Change: 1987-07-01
cp_function(rural_deaths, 'REPUBKILLSCATH')    # Change: No change

cp_function(all_deaths, 'UNIONKILLSCATH')      # Change: 1978-10-01
cp_function(belfast_deaths, 'UNIONKILLSCATH')  # Change: No change
cp_function(rural_deaths, 'UNIONKILLSCATH')    # Change: 1979-10-01, 1985-04-01



## Major change points: late 1970s (1977-1979), early 1990s (1991-2)




