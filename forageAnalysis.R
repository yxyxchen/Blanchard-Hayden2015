# load libraries
library("tidyverse")
source("subFxs/loadFxs.R")
source("subFxs/plotThemes.R")
source("subFxs/helpFxs.R")
dir.create("figures")
dir.create("figures/forageAnalysis")

# load data
allData = loadAll()
hdrData = allData$hdrData
trialData = allData$trialData
hdrData$nTrial =  sapply(trialData,nrow) # add number of trials 
hdrData$foragingTime = rep(NA, nrow(hdrData)) # add foraging time if applicable
hdrData$foragingTime[hdrData$task == "forage"] =  sapply(trialData[hdrData$task == "forage"], function(x) unique(x$foragingTime))

# subjects 
subs = unique(hdrData$sub)
nSub = length(subs)

###################### prepare data ##############################
select = hdrData$task == "forage"
tempt = do.call(rbind, trialData[select])
tempt$sub = rep(hdrData$sub[select],
                   sapply(trialData[select],nrow))
tempt$sess = rep(as.numeric(hdrData$sess[select]),
                 sapply(trialData[select],nrow))
tempt$id = rep(hdrData$id[select],
               sapply(trialData[select],nrow))
tempt$date = rep(hdrData$date[select],
                 sapply(trialData[select],nrow))
tempt$lastTrial = c(ifelse(tail(tempt$trial, -1) == 1, T, F), T) 
# exclude constant columns 
constants = tempt[1,c("timeToFixate", "minFixTimeToStart", "holdFixTime",
                      "timeToChoose", "ITI", "shrinkRate", "handlingTime",
                      paste0("allJuiceAmounts_", 1:17))]
tempt = tempt %>% select(-c("timeToFixate", "minFixTimeToStart", "holdFixTime",
                            "timeToChoose", "ITI", "shrinkRate", "handlingTime",
                            paste0("allJuiceAmounts_", 1:17)))
# truncate the last block at the end of each session to avoid right censoring
startIdxs = tempt %>% group_by(id) %>% summarise(max(which(choiceMade == "leave"))) +
  cumsum(c(0, head(sapply(trialData[select], nrow), -1)))
endIdxs = cumsum(c(sapply(trialData[select], nrow)))
junk = list()
for(i in 1 : length(endIdxs)){
  a = startIdxs[i,2]
  b = endIdxs[i]
  junk[[i]] = (a+1) : b
}
delete = unlist(junk)
tempt = tempt[(!(1 : nrow(tempt) %in% delete)), ]

######################### stay lengths ###########################
# calculate stay lengths
block = cumsum(tempt$choiceMade == "leave")
tempt$block = block
nStay = table(block)
nStay[2 : length(nStay)] = nStay[2 : length(nStay)] - 1 # since leave is counted as a stay as well after the first block
# nStay = pmin(nStay, 17) 
forageData = tempt[which(tempt$choiceMade == "leave")-1,]
forageData$nStay = head(nStay, -1)

# replicate Figure 3
forageData  %>% group_by(sub, foragingTime) %>%
  dplyr::summarise(mu = mean(nStay)) %>%
  ggplot(aes(factor(foragingTime), mu)) +
  geom_bar(stat = "identity") + facet_grid(~sub) + myTheme + 
  xlab("Traveling time") + ylab("Number of stays in a patch")

# plot hist of stay lengths for Monkey  H
forageData %>% filter(sub == "H") %>% group_by(date, foragingTime) %>%
  ggplot(aes(nStay)) +
  geom_histogram() + facet_grid(foragingTime~date) + myTheme + 
  xlab("Number of stays in a patch") + ylab("Count")

# plot hist of stay lengths for Monkey  J
forageData %>% filter(sub == "J") %>% group_by(date, foragingTime) %>%
  ggplot(aes(nStay)) +
  geom_histogram() + facet_grid(foragingTime~date) + myTheme + 
  xlab("Number of stays in a patch") + ylab("Count")

