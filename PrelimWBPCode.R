##### Sample WBP code for preliminary data #####

# supplemental information could be on aspect, site location
# preliminary data could include cluster size, average stem dbh (per cluster)

# this data frame would be grouping organisms by cluster and not by individual 

# clusterSize = total number of trees within one cluster, individual stems within one clustered grouping
# avgDBH = average diameter at breast height for each cluster. sum of all dbh's of individual stems in cluster divided by the total number of individual stems
# healthScore = a numeric ranking system to show the apparent health of the tree based on physical symptoms. ranked from 1-5, 1 being in seemingly perfect health, 5 being seemingly close to death

# need to figure out how DBH and average DBH would really be recoreded data sheets

WBP <- data.frame()

colnames(WBP) <- c('siteName','aspect','surveyDate','clusterSize','avgDBH', 'healthScore')

hist(clusterSize)

# goal here could be to first graph the frequency of cluster sizes






