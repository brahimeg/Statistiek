
setwd("C:\\Users\\Ibrahim-main\\Dropbox\\UU Kunstmatige Intelligentie\\Experimentele methode & statistiek\\Project\\Final\\Statistiek")


load("allData2018.Rdata")

wordlist_easy1 <- read.csv("easy1.csv", header = TRUE)

wordlist_practice <- read.csv("trainingList.csv", header = TRUE)

dualtaskData <- allData[allData$partOfExperiment == "dualTask",]

wordsdualtaskData <- dualtaskData[dualtaskData$Eventmessage2 == "correctNewWord",] 

practiceData <- allData[allData$partOfExperiment == "practiceScrabble",]

pDataCorrectwords <- practiceData[practiceData$Eventmessage2 == "correctNewWord",]




wordSameLength <- function(woord)
{
  lengte <- nchar(woord)
  KeuzeWoorden <- wordlist_practice[wordlist_practice$nrLetters == lengte, ]
  Nieuwwoord <- KeuzeWoorden[sample(nrow(KeuzeWoorden), 1), ]    
  as.character(Nieuwwoord[[1,2]])
}

MakeRandomString <- function(n=1, lenght=12)
{
  randomString <- c(1:n)                  
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(alphabet,lenght),
                             collapse="")
  }
  return(randomString)
}



sameFirstLetter <- function(letter, lijst)
{
  headers <- colnames(lijst)
  
  alphabet_list <- strsplit(headers[2],"")
  
  alphabet <- unlist(alphabet_list)
  
  lijst$firstLetter <- substr(lijst$eandowf,1,1)
  
  Keuzewoorden <- lijst[lijst$firstLetter == letter, ]
  
  NieuwWoord <- Keuzewoorden[sample(nrow(Keuzewoorden), 1), ] 
  
  as.character(NieuwWoord[[1,2]])
}

loopFirstLetter <- function(lijst)
{
  headers <- colnames(lijst)
  
  alphabet_list <- strsplit(headers[2],"")
  
  alphabet <- unlist(alphabet_list)
  
  for(i in alphabet)
  {
    sameFirstLetter(i, lijst)
  }
  
}







