# load libraries 
library("tidyverse")
source("subFxs/loadFxs.R")
source("subFxs/plotThemes.R")
source("subFxs/helpFxs.R")
dir.create("figures")
dir.create("figures/timeAnalysis")

# load data
allData = loadAll()
hdrData = allData$hdrData
trialData = allData$trialData
rwds = c(75, 97, 135, 175, 212)
  
# prepare data 
subs = unique(hdrData$sub)
nSub = length(subs)
timeData = do.call(rbind, trialData[hdrData$task == "time"])
timeData$leftRwd = sapply(timeData$leftcolor, FUN = function(x) rwds[x-1])
timeData$rightRwd = sapply(timeData$rightcolor, FUN = function(x) rwds[x-1])
timeData$rwdDiff = abs(timeData$leftRwd - timeData$rightRwd)
timeData$waitDiff = abs(timeData$leftwait - timeData$rightwait)
timeData$smallRwd = pmin(timeData$rightRwd, timeData$leftRwd)
timeData$sub = rep(hdrData$sub[hdrData$task == "time"],
                   sapply(trialData[hdrData$task == "time"],nrow))
timeData$SS = (timeData$choice == 1 & timeData$leftcolor < timeData$rightcolor) |
  (timeData$choice == 2 & timeData$leftcolor > timeData$rightcolor)# whether choose the sooner/smaller option
  
# plot pSS
timeData %>% 
  mutate(rwdDiff = cut(rwdDiff, c(20, 60, 100, 140)),
         waitDiff = cut(waitDiff, seq(0, 6, length.out = 7), labels = (0:5 + 1:6) / 2)) %>%
  group_by(rwdDiff, waitDiff, sub) %>%
  dplyr::summarise(pSS = mean(SS)) %>% 
  ggplot(aes(waitDiff, pSS, color = rwdDiff)) + 
  geom_point() + facet_grid(~sub) + 
  ylab("SS choice (%)") + xlab("Delay difference (s)") +
  myTheme
  
######################### discounting rate #########################
for(sIdx in 1 : nSub){
  thisSub = subs[sIdx]
  data = timeData[timeData$sub == thisSub,]
  # estimate exponential discount factor
  ## c is the discounting factor as in Kable and Glimcher, 2007
  ## tau is the softmax parameter 
  cInits = seq(0.2, 1, length.out = 5)
  tauInits = seq(0.001, 0.1, length.out = 5)
  expLoss = 1000000
  for(cInit in cInits){
    for(tauInit in tauInits){
      optimResults = optim(c(cInit, tauInit), expLossFun,lower = c(0, 0), data = data)
      if(optimResults$value < expLoss){
        expLoss = optimResults$value
        expParas = optimResults$par
      }
    }
  }
  c = expParas[1]
  # estimate hyperbolic discount factor
  ## k is the discounting factor as in Kable and Glimcher, 2007
  ## tau is the softmax parameter 
  kInits = seq(0.2, 5, length.out = 5)
  tauInits = seq(0.001, 0.2, length.out = 5)
  hyperLoss = 1000000
  for(kInit in kInits){
    for(tauInit in tauInit){
      optimResults = optim(c(kInit, tauInit), hyperLossFun,lower = c(0, 0), data = data)
      if(optimResults$value == 1000000){
        print("failed")
      }
      if(optimResults$value < hyperLoss){
        hyperLoss= optimResults$value
        hyperParas = optimResults$par
      }
    }
  }
  k = hyperParas[1]
  
  # make predictions for the hyperbolic model
  data$pSSHatExp = expPredict(expParas, data)
  data$pSSHatHyper = hyperPredict(hyperParas, data)
  data = data %>% mutate(
    smallRwd = pmin(leftRwd, rightRwd),
    largeRwd = pmax(leftRwd, rightRwd),
    smallwait = pmin(leftwait, rightwait),
    largewait = pmax(leftwait, rightwait),
    diffExpSV = exp(-c * smallwait) * smallRwd - exp(-c * largewait) * largeRwd,
    diffHyperSV = smallRwd / (1 + k * smallwait) - largeRwd / (1 + k * largewait)
  )
  nCut = 7
  limits = seq(min(data$diffHyperSV), max(data$diffHyperSV), length.out = nCut + 1)
  limits[1] = min(data$diffHyperSV) - 0.1
  limits[length(limits)] = max(data$diffHyperSV) + 0.1
  mids = (head(limits, -1) + tail(limits, -1)) / 2
  
  # plot for the hyperbolic model
  data %>% mutate(
    diffHyperSV= cut(diffHyperSV, breaks = limits)
  ) %>% mutate(
    diffHyperSV = as.numeric(diffHyperSV),
    preference = mids[diffHyperSV]
  ) %>%
    group_by(preference) %>%
    dplyr::summarise(mu = mean(SS), 
                     se = sd(SS) / sqrt(length(SS)),
                     min = mu - se,
                     max = mu + se,
                     n = length(SS),
                     muHat = mean(pSSHatHyper)) %>% 
    ggplot(aes(preference, mu)) + geom_point() +
    geom_errorbar(aes(ymin = min, ymax = max)) +
    geom_line(aes(preference, muHat), linetype = "dashed") +
    myTheme + xlab("Preference for SS") +
    ylab("SS choice (%)") + ylim(c(0, 1)) +
    ggtitle(sprintf("%s, k = %.2f, tau = %.2f", thisSub, hyperParas[1], hyperParas[2]))
    ggsave(sprintf("figures/timeAnalysis/predict_%s.png", thisSub), width = 4.5, height = 4.5)
  
}



