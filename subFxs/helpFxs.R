expLossFun = function(paras, data){
  c = paras[1]
  tau = paras[2]
  smallRwd = pmin(data$leftRwd, data$rightRwd)
  largeRwd = pmax(data$leftRwd, data$rightRwd)
  smallwait = pmin(data$leftwait, data$rightwait) 
  largewait = pmax(data$leftwait, data$rightwait) 
  smallSV = exp(-c * smallwait) * smallRwd
  largeSV = exp(-c * largewait) * largeRwd
  DV = smallSV - largeSV
  pSSHat = 1 / (1 + exp(-tau * DV)) 
  likelihood = ifelse(data$SS, pSSHat, 1 - pSSHat)
  loss = -sum(log(likelihood))
  if(is.infinite(loss)){
    loss = 1000000
  }
  return(loss)
}
expPredict = function(paras, data){
  c = paras[1]
  tau = paras[2]
  smallRwd = pmin(data$leftRwd, data$rightRwd)
  largeRwd = pmax(data$leftRwd, data$rightRwd)
  smallwait = pmin(data$leftwait, data$rightwait) 
  largewait = pmax(data$leftwait, data$rightwait) 
  smallSV = exp(-c * smallwait) * smallRwd
  largeSV = exp(-c * largewait) * largeRwd
  DV = smallSV - largeSV
  pSSHat = 1 / (1 + exp(-tau * DV)) 
  return(pSSHat)
}

hyperLossFun = function(paras, data){
  k = paras[1]
  tau = paras[2]
  smallRwd = pmin(data$leftRwd, data$rightRwd)
  largeRwd = pmax(data$leftRwd, data$rightRwd)
  smallwait = pmin(data$leftwait, data$rightwait)
  largewait = pmax(data$leftwait, data$rightwait)
  smallSV =  smallRwd / (1 + k * smallwait)
  largeSV = largeRwd / (1 + k * largewait)
  DV = smallSV - largeSV
  pSSHat = 1 / (1 + exp(-tau * DV)) 
  likelihood = ifelse(data$SS, pSSHat, 1 - pSSHat)
  loss = -sum(log(likelihood))
  if(is.infinite(loss)){
    loss = 1000000
  }
  return(loss)
}

hyperPredict = function(paras, data){
  k = paras[1]
  tau = paras[2]
  smallRwd = pmin(data$leftRwd, data$rightRwd)
  largeRwd = pmax(data$leftRwd, data$rightRwd)
  smallwait = pmin(data$leftwait, data$rightwait)
  largewait = pmax(data$leftwait, data$rightwait)
  smallSV =  smallRwd / (1 + k * smallwait)
  largeSV = largeRwd / (1 + k * largewait)
  DV = smallSV - largeSV
  pSSHat = 1 / (1 + exp(-tau * DV)) 
  return(pSSHat)
}
