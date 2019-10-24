type = "bar"
inputData = data.frame(x = rep(1:3,2), y = 2*1:6, label = rep(letters[1:2],3))

browseURL(quickchartR(type,inputData))
