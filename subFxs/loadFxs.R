loadAll = function(){
  files = dir("data")
  nFile = length(files)
  
  # initialize 
  sub = vector(length = nFile)
  task = vector(length = nFile)
  date = vector(length = nFile)
  sess = vector(length = nFile)
  trialData = list()
  
  # loop 
  for(i in 1 : nFile){
    file = files[i]
    tempt = strsplit(file, "\\.")
    sub[i]= substr(tempt[[1]][1], 1, 1)
    task[i] = ifelse(tempt[[1]][3] == "C", "forage", "time")
    date[i] = substr(tempt[[1]][1], 2, nchar(tempt[[1]][1]))
    sess[i]= tempt[[1]][2]
    thisTrialData = read.csv(file.path("data", file))
    if(task[i] == "time"){
      thisTrialData$leftwait = thisTrialData$leftwait * 6
      thisTrialData$rightwait = thisTrialData$rightwait * 6
    }
    trialData[[i]] = thisTrialData
  }
  hdrData = data.frame(id = 1 : nFile, sub = sub,
                       task = task, date = date, sess = sess)
  
  # outputs
  outputs = list(
    hdrData = hdrData,
    trialData = trialData
  )
  return(outputs)
}
