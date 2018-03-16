
setwd("C:\\Users\\Ibrahim-main\\Dropbox\\UU Kunstmatige Intelligentie\\Experimentele methode & statistiek\\Project\\Final\\Statistiek")


load("allData2018.Rdata")

wordlist_easy1 <- read.csv("easy1.csv", header = TRUE)

wordlist_practice <- read.csv("trainingList.csv", header = TRUE)

dualtaskData <- allData[allData$partOfExperiment == "dualTask",]

wordsdualtaskData <- dualtaskData[dualtaskData$Eventmessage2 == "correctNewWord",] 

practiceData <- allData[allData$partOfExperiment == "practiceScrabble",]

pDataCorrectwords <- practiceData[practiceData$Eventmessage2 == "correctNewWord",]

headers <- colnames(wordlist_practice)

alphabet_list <- strsplit(headers[2],"")

alphabet <- unlist(alphabet_list)


wordSameLength <- function(word)
{
  lengte <- nchar(word)
  
  
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













