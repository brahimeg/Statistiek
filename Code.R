setwd("C:\\Users\\Marilva\\Documents\\Statistiek\\Eindopdracht")
load("allData2018(1).Rdata")
WoordenlijstTraining <- read.csv("trainingList.csv", header = TRUE)

DualTaskOnly <- allData[allData$partOfExperiment == "dualTask" , ]
PracticeScrabbleData <- allData[allData$partOfExperiment == "practiceScrabble" & allData$TrialNumber != "0", ]
CorrectWords <- PracticeScrabbleData[PracticeScrabbleData$Eventmessage2 == "correctNewWord" , ]

TestString <- list("e", "a", "n", "d", "o", "w", "f")
EersteWoord <- "dan"

Randomwoord <- function(lijst)
{
  Nieuwwoord <- lijst[sample(nrow(lijst), 1), ]
  as.character(Nieuwwoord[[1,2]])
}
Randomwoord(WoordenlijstTraining)

LengteHetzelfde <- function(woord, lijst)
{
  lengte <- nchar(woord)
  KeuzeWoorden <- lijst[lijst$nrLetters == lengte, ]
  Nieuwwoord <- KeuzeWoorden[sample(nrow(KeuzeWoorden), 1), ]    
  as.character(Nieuwwoord[[1,2]])
}

LengteHetzelfde("kaas", WoordenlijstTraining)



