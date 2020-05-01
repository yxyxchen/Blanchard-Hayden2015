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
hdrData$nTrial =  sapply(trialData,nrow)

# prepare data 
subs = unique(hdrData$sub)
nSub = length(subs)
tempt = do.call(rbind, trialData[hdrData$task == "forage"])
tempt$sub = rep(hdrData$sub[hdrData$task == "forage"],
                   sapply(trialData[hdrData$task == "forage"],nrow))
tempt$sess = rep(as.numeric(hdrData$sess[hdrData$task == "forage"]),
                 sapply(trialData[hdrData$task == "forage"],nrow))
tempt$id = rep(hdrData$id[hdrData$task == "forage"],
               sapply(trialData[hdrData$task == "forage"],nrow))
tempt$lastTrial = c(ifelse(tail(tempt$trial, -1) == 1, T, F), T)
## exclude constant columns 
constants = tempt[1,c("timeToFixate", "minFixTimeToStart", "holdFixTime",
                      "timeToChoose", "ITI", "shrinkRate", "handlingTime",
                      paste0("allJuiceAmounts_", 1:17))]
tempt = tempt %>% select(-c("timeToFixate", "minFixTimeToStart", "holdFixTime",
                            "timeToChoose", "ITI", "shrinkRate", "handlingTime",
                            paste0("allJuiceAmounts_", 1:17)))

# truncate the last block at the end of each session
truncateIdxs = tempt %>% group_by(id) %>% summarise(max(which(choiceMade == "leave"))) +
  cumsum(c(0, head(sapply(trialData[hdrData$task == "forage"], nrow), -1)))
endIdxs = cumsum(c(sapply(trialData[hdrData$task == "forage"], nrow)))
junk = list()
for(i in 1 : length(endIdxs)){
  a = truncateIdxs[i,2]
  b = endIdxs[i]
  junk[[i]] = (a+1) : b
}
delete = unlist(junk)
tempt = tempt[!(1 : nrow(tempt) %in% delete), ]


# calculate stay lengths
group = c(0, head(cumsum(tempt$choiceMade == "leave"), -1))
tempt$group = group
nStay = table(group)
uniGroup = unique(group)
nStay = nStay - 1 # since leave is counted as a stay as well
# nStay = pmin(nStay, 17) #vdoes it make sesnses
forageData = tempt[which(tempt$choiceMade == "leave")-1,]
forageData$nStay = nStay


# plot stay lengths
forageData %>% group_by(sub, foragingTime) %>%
  ggplot(aes(nStay)) +
  geom_histogram() + facet_grid(foragingTime~sub) + myTheme

forageData %>% group_by(sub, foragingTime) %>%
   dplyr::summarise(mu = mean(nStay)) %>%
  ggplot(aes(factor(foragingTime), mu)) +
  geom_bar(stat = "identity") + facet_grid(~sub) + myTheme 

# calculate the optimal strategy 
## we assume the reward starts at 333 mL and decreases 22 mL for each 1.7s (1-s ITI + 0.6-s handling time + 0.1-s fixation)
Gts = cumsum(as.numeric(constants[paste0("allJuiceAmounts_", 1:17)])) / 0.2 * 333
ts = 1.7 * 1 : 17
tIdxs = 1 : 17
rwdRate4.8s = Gts / (ts + 4.8)
rwdRate8s = Gts / (ts + 8)
rwdRate12s = Gts / (ts + 12)
optimDf = data.frame(
  travelTime = factor(c(4.8, 8, 12)), 
  tStar =  ts[c(which.max(rwdRate4.8s), which.max(rwdRate8s), which.max(rwdRate12s))],
  tIdxStar = c(which.max(rwdRate4.8s), which.max(rwdRate8s), which.max(rwdRate12s)),
  rwdRateStar = c(max(rwdRate4.8s), max(rwdRate8s), max(rwdRate12s))
)
## plot the optimal strategy
data.frame(
  Gt = Gts,
  t = ts,
  tIdx = tIdxs,
  rwdRate4.8 = rwdRate4.8s,
  rwdRate8 = rwdRate8s,
  rwdRate12 = rwdRate12s
) %>% gather(key = "travelTime", value = "rwdRate", -c("Gt", "t", "tIdx")) %>%
  mutate(travelTime = rep(factor(c(4.8, 8, 12)), each = 17)) %>%
  ggplot(aes(t, rwdRate, color = travelTime))  + 
  geom_point() +
  geom_segment(data = optimDf, aes(x = tStar, xend =  tStar, y = -2, yend = rwdRateStar, color = travelTime), linetype = "dashed", inherit.aes = F) +
  myTheme + xlab("Time in patch (s)") + ylab("Reward rate (ml / s)") 

# calcualte the value index
forageData$vObtained = ifelse(forageData$nStay<= 17, Gts[forageData$nStay], max(Gts)) / ((forageData$nStay * 1.7) + forageData$foragingTime * 2.4)
forageData$vOptimal = ifelse(forageData$foragingTime == 2, optimDf$rwdRateStar[1], ifelse(forageData$foragingTime == 5, optimDf$rwdRateStar[2], optimDf$rwdRateStar[3]))
forageData$vIndex = (forageData$vObtained - forageData$vOptimal) / (forageData$vObtained + forageData$vOptimal)
forageData %>% ggplot(aes(vIndex)) + geom_histogram() + facet_grid(~sub)
