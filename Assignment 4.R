
setwd("C:\\Users\\Ibrahim-main\\Dropbox\\UU Kunstmatige Intelligentie\\Experimentele methode & statistiek\\Project\\Assig3")

load("allData2018.Rdata")

dualtaskData <- allData[allData$partOfExperiment == "dualTask",]

wordsdualtaskData <- dualtaskData[dualtaskData$Eventmessage2 == "correctNewWord",] 

switchWindowData <- dualtaskData[dualtaskData$Eventmessage1 == "SwitchWindow" | dualtaskData$Eventmessage2 == "correctNewWord",]

