### LIBRARIES ###
# install.packages("jsonlite")
library("jsonlite")

### PARAMETERS ###
FirstYear=2016
LastYear=2022
DataDirectory="Data"
ResultDirectory="Results"
MTGFormat="Modern"
RawFile=paste(MTGFormat,"data.json",sep="_")

### DATA IMPORT ###
rawData=fromJSON(paste(DataDirectory,RawFile,sep="/"))[[1]]
rawData$Date = as.Date(rawData$Date)

### Filter MTGO Data only ###
MTGOData=rawData[grep(pattern = "https://www.mtgo.com/en/mtgo/decklist",x=rawData$AnchorUri),]
# Remove some events that are posted with an algorithm that make them poor indicators of diversity
MTGOData=MTGOData[!grepl("League", MTGOData$Tournament),]
MTGOData=MTGOData[!grepl("Daily Swiss", MTGOData$Tournament),]

unique(MTGOData$Tournament)

### Combine deck lists in a single column, in addition of being split between Mainboard and Sideboard ###
MTGOData$Allboards = rep(0,nrow(MTGOData))
pb = winProgressBar(title = "progress bar", min = 0,
                     max = nrow(MTGOData), width = 300)
for (i in 1:nrow(MTGOData)){
  mainboardi = MTGOData$Mainboard[[i]]
  sideboardi = MTGOData$Sideboard[[i]]
  MTGOData$Allboards[i] = list(aggregate(. ~ CardName, rbind(mainboardi, sideboardi), 
                                         sum, na.rm = TRUE, na.action = NULL))
  setWinProgressBar(pb, i, title=paste( round(i/nrow(MTGOData)*100, 0),"% done"))
}
close(pb)

### Split the data by year, putting all the resulting dataframes in a list
YearList = c(FirstYear:LastYear)

YearData = lapply(YearList,function(x) 
  subset(MTGOData, Date >= as.Date(paste(x,"-01-01",sep="")) 
         & Date < as.Date(paste(x,"-12-31",sep=""))))

names(YearData)=YearList

### Get the list of different cards and their deck count per year ###

# Function to iterate over all the deck lists and count how many include the card called "x"
getDeckCount = function(x, df){
  sum(unlist(lapply(df[["Allboards"]], function(y) x %in% y$CardName)))
}

# Function to generate the results for a specific dataframe
CardsPresence = function(df){
  CardsNames = unique(unlist(sapply(c(1:nrow(df)), function(i) list(df[["Allboards"]][[i]]$CardName))))
  DecksCounts = unlist(lapply(CardsNames, getDeckCount, df=df))
  CardsData=setNames(data.frame(CardsNames,DecksCounts), c("CardNames", "DeckCount"))
  return(list("Number of different cards" = length(CardsNames),
              "Number of decks" = nrow(df),
              "Different cards by deck" = signif(length(CardsNames)/nrow(df),3),
              "Average of card presence in decks" =  paste(100*signif(mean(DecksCounts)/nrow(df),3),"%"),
              "Standard deviation of card presence in decks" =  paste(100*signif(sd(DecksCounts)/nrow(df),3),"%"),
              "Maximum of card presence in decks" =  paste(100*signif(max(DecksCounts)/nrow(df),3),"%")))
  
}

# Get one row of results by year
YearResult = lapply(YearData, CardsPresence)

# Make the output cleaner
YearResultDf = do.call(rbind.data.frame, YearResult)
names(YearResultDf) = gsub(x = names(YearResultDf), pattern = "\\.", replacement = " ") 
View(YearResultDf)
write.csv(YearResultDf, paste(ResultDirectory,"\\",MTGFormat," card diversity by year.csv",sep=""), row.names=TRUE)
