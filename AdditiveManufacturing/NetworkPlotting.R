pacman::p_load(data.table, igraph)
test1 <- fread('/Users/jesse/Dropbox/Research/AdditiveManufacturing/laser_sintering.csv')

test1 <- test1[, list(CellID, CitationIDs)]
setkeyv(test1, c('CellID', 'CitationIDs'))
test1 <- unique(test1)


nodelist <- data.table(
  nodeid = factor(
    unique(
      c(test1[, CellID], test1[, CitationIDs])
    )
  )
)

nodelist[which(as.numeric(as.character(nodelist$nodeid)) %in% test1$CellID), nodesource := 1]
nodelist[is.na(nodesource), nodesource := 0]
table(nodelist$nodesource)


testdata <- test1
testdata[, CellID := as.integer(factor(CellID, levels = as.character(sort(nodelist$nodeid))))]
testdata[, CitationIDs := as.integer(factor(CitationIDs, levels = as.character(sort(nodelist$nodeid))))]

setkeyv(testdata, c('CellID', 'CitationIDs'))


testplot <- graph_from_edgelist(
  as.matrix(testdata[, list(CellID, CitationIDs)])
  , directed = T)



V(testplot)$source <- NA
V(testplot)$source[nodelist[nodesource == 1, nodeid]] <- 'blue'
V(testplot)$source[nodelist[nodesource == 2, nodeid]] <- 'blue'
V(testplot)$source[nodelist[nodesource == 0, nodeid]] <- 'green'


dev.off()
png(filename = '/Users/jesse/Dropbox/Research/AdditiveManufacturing/laserSintering.png'
    , height = 900
    , width = 900)

par(mar=c(0,0,0,0))
plot(
  testplot
  , vertex.label = NA
  , vertex.size = 2
  , vertex.color = 'blue'
  , vertex.frame.color = NA
  , edge.arrow.size = 0.025
  , edge.color = adjustcolor("black", alpha.f = .25)
  , layout = layout_nicely
)

dev.off()







## Data set 1
test1 <- test1[, list(CellID, CitationIDs)]

setkeyv(test1, c('CellID', 'CitationIDs'))
test1 <- unique(test1)


## Data set 2
test2 <- test2[, list(CellID, CitationIDs)]

setkeyv(test2, c('CellID', 'CitationIDs'))
test2 <- unique(test2)





nodelist <- data.table(
  nodeid = factor(
    unique(
      c(test1[, CellID], test1[, CitationIDs])
    )
  )
)

nodelist[which(as.numeric(as.character(nodelist$nodeid)) %in% test1$CellID), nodesource := 1]
nodelist[which(as.numeric(as.character(nodelist$nodeid)) %in% test2$CellID), nodesource := 2]
nodelist[is.na(nodesource), nodesource := 0]
table(nodelist$nodesource)


testdata <- rbind(test1)
testdata[, CellID := as.integer(factor(CellID, levels = as.character(sort(nodelist$nodeid))))]
testdata[, CitationIDs := as.integer(factor(CitationIDs, levels = as.character(sort(nodelist$nodeid))))]

setkeyv(testdata, c('CellID', 'CitationIDs'))


testplot <- graph_from_edgelist(
  as.matrix(testdata[, list(CellID, CitationIDs)])
  , directed = T)



V(testplot)$source <- NA
V(testplot)$source[nodelist[nodesource == 1, nodeid]] <- 'red'
V(testplot)$source[nodelist[nodesource == 2, nodeid]] <- 'blue'
V(testplot)$source[nodelist[nodesource == 0, nodeid]] <- 'green'



plot(
  testplot
  , vertex.label = NA
  , vertex.size = 0.5
  , vertex.color = V(testplot)$source
  , vertex.frame.color = NA
  , edge.arrow.size = 0.05
)

