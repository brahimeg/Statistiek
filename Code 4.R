setwd("C:\\Users\\Marilva\\Documents\\GitHub\\Statistiek")
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
  as.character(Nieuwwoord[1,2])
}

LengteHetzelfde("kaas", WoordenlijstTraining)

LetterVeranderen <- function(woord, lijst)
{
  for (r in 1:nrow(lijst))
  {
  verschil <- Reduce(setdiff, strsplit(c(woord, lijst$eandowf), split = ""))
  hoeveel <- nchar(verschil)
  if {
    
  }
  }
}




eersteLetterHetzelfde <- function(woord, lijst)
{
WoordenlijstTraining$beginLetter <- NA
header <- 
for (r in 1:nrow(lijst))
  {
  string <- lijst[r,]$eandowf
  lijst[r,]$beginLetter <- substr(string, 1, 1)
}

begin <- substr(woord, 1, 1)
Keuzes <- lijst[lijst$beginLetter == begin, ]
output <- Keuzes[sample(nrow(Keuzes), 1), ]
as.character(output[1,2])
}

eersteLetterHetzelfde('aardbei', WoordenlijstTraining)

