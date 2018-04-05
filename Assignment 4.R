
setwd("C:\\Users\\Ibrahim\\Dropbox\\UU Kunstmatige Intelligentie\\Experimentele methode & statistiek\\Project\\Final\\Statistiek")


load("allData2018.Rdata")

wordlist_easy1 <- read.csv("easy1.csv", header = TRUE)

wordlist_practice <- read.csv("trainingList.csv", header = TRUE)

dualtaskData <- allData[allData$partOfExperiment == "dualTask",]

wordsdualtaskData <- dualtaskData[dualtaskData$Eventmessage2 == "correctNewWord",] 

practiceData <- allData[allData$partOfExperiment == "practiceScrabble",]

pDataCorrectwords <- practiceData[practiceData$Eventmessage2 == "correctNewWord",]



##srategie 1
inputSameFirstLetter <- function(letter, lijst)
{
  headers <- colnames(lijst)
  
  alphabet_list <- strsplit(headers[2],"")
  
  alphabet <- unlist(alphabet_list)
  
  lijst$firstLetter <- substr(lijst[,2],1,1)
  
  if(letter %in% alphabet)
  {
  Keuzewoorden <- lijst[lijst$firstLetter == letter, ]
  NieuwWoord <- Keuzewoorden[sample(nrow(Keuzewoorden), 1), ] 
  return(as.character(NieuwWoord[[1,2]]))
  }
  return(NULL)
    
  
}



sameFirstLetter <- function(woord, lijst)
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


wordSameLength <- function(woord, lijst)
{
  lengte <- nchar(woord)
  KeuzeWoorden <- lijst[lijst[,1] == lengte,]
  Nieuwwoord <- KeuzeWoorden[sample(nrow(KeuzeWoorden), 1), ]    
  as.character(Nieuwwoord[[1,2]])
}

Randomwoord <- function(lijst)
{
  Nieuwwoord <- lijst[sample(nrow(lijst), 1), ]
  as.character(Nieuwwoord[[1,2]])
  
}


Randomwoord_lengte <- function(lijst, lengte) 
{
  woord <- Randomwoord(lijst)
  while(nchar(woord) != lengte )
    {
      woord <- Randomwoord(lijst)
    }
  return(woord)
}



LoopStrategie1 <- function(lijst)
{
  list1 <- lijst[,2]
  list2 <- data.frame(woord=NA, tijdpunten=NA)
  
  
  headers <- colnames(lijst)
  
  alphabet_list <- strsplit(headers[2],"")
  
  alphabet_test <- unlist(alphabet_list)
  
  alphabet <- sample(alphabet_test)
  
  lijst$firstLetter <- substr(lijst[,2],1,1)
  
  while (length(list1) != 0)
  {
    repeat
    {
    for(i in alphabet)
    {
      aantal1 <- nrow(lijst[lijst$firstLetter == i,])
      #aantal2 <- aantal1 / 2
      aantal <- sample(0:aantal1, 1)
      
      while (aantal != 0)
      {
        
        woord <- inputSameFirstLetter(i, lijst)
        
        if(woord %in% list1)
        {
          list1 <- list1[!list1 %in% woord]
          list1 <- droplevels(list1)
          tijdpunten <- 10
          list2[nrow(list2)+1,] = c(woord,tijdpunten)
          aantal <- aantal - 1
          lijst <- lijst[!(lijst[,2] == woord),] 
        }
      }
      
    }
    
    if (nrow(lijst) == 0){break}
    }
  }
  return(list2[-1,])
}


LoopStrategie2 <- function(lijst)
{

  lijst <- lijst
  list1 <- data.frame(woord = NA, tijdpunten = NA)
  
  
  list2 <- lijst[,2]
  
  wordLengths <- sample(unique(lijst[,1]))
  
  
  while(length(list2) != 0)
  {
    for (i in wordLengths)
    {
      aantal <- nrow(lijst[lijst$nrLetters == i,])
      
      nieuwwoord <- Randomwoord_lengte(lijst, i)
      
      while(aantal != sample(1:aantal, 1))
      {
        
        nieuwwoord <- wordSameLength(nieuwwoord, lijst)
        
        if(nieuwwoord %in% list2)
        {
         
          list2 <- list2[!list2 %in% nieuwwoord]
          list2 <- droplevels(list2)
          tijdpunten <- 10
          list1[nrow(list1)+1,] = c(nieuwwoord,tijdpunten)
          aantal <- aantal - 1
          
          
        }
      }
      
    }
    
  }

  
 return(list1[-1,])
  
    
}

LoopStrategie3 <- function(lijst)
{
  lijst <- lijst
  list1 <- data.frame(woord = NA, tijdpunten = NA)
  
  
  list2 <- lijst[,2]
  
  
      
  while(length(list2) != 0)
  {
        
    nieuwwoord <- Randomwoord(lijst)
        
    if(nieuwwoord %in% list2)
    {
          
      list2 <- list2[!list2 %in% nieuwwoord]
      list2 <- droplevels(list2)
      tijdpunten <- 10
      list1[nrow(list1)+1,] = c(nieuwwoord,tijdpunten)
          
          
    }
  }
  
  return(list1[-1,])
}

multiStrategie <- function(lijst,ratioS1,ratioS2)
{
  #lijst <- wordlist_practice
  #ratioS1 <- 0.2
  #ratioS2 <- 0.3
  
  lijst <- lijst[sample(nrow(lijst)),]
  
  lengte <- nrow(lijst)
  lengteS1 <- floor(ratioS1*lengte)
  lengteS2 <- floor(ratioS2*lengte) + lengteS1
  
  
  lijst1 <- LoopStrategie1(lijst[1:lengteS1,])
  lijst2 <- LoopStrategie2(lijst[(lengteS1+1):(lengteS2),])
  lijst3 <- LoopStrategie3(lijst[(lengteS2+1):lengte,])
  
  Total <- rbind(lijst1,lijst2,lijst3)
  return(Total)
}


yolo <- multiStrategie(wordlist_practice,0.1,0.3)
