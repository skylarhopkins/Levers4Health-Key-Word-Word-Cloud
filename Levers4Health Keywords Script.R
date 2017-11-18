#Skylar Hopkins - November 2017

#load libraries
library(bibliometrix) #for pulling text from web of science bibtex 
library(tm)  #for text mining
library(SnowballC) #for text stemming
library(wordcloud) #word-cloud generator 
library(RColorBrewer) #color palettes

#Outside of this script, I used Web of Knowledge to create a bibtex file
#containing 41 papers that we previously deemed relevant to levers4health

RawBibData <- readFiles("~/Documents/Levers4Health Keywords.bib")
Data <- convert2df(RawBibData, dbsource = "isi", format = "bibtex")

#You can look at some general bibliometric stats
BibResults<-biblioAnalysis(Data, sep = ";")
BibResultsSummary<-summary(object = BibResults, k = 10, pause = FALSE)

#Here are the most relevant author keywords (DE) and WoK "keywords plus" (ID)
#None are shared by more than 7/41 articles, though
BibResultsSummary$MostRelKeywords

#Pull the words in the title (TI), abstract (AB), and keywords (DE and ID) for text mining
#We turn this into a "Corpus" for text mining
Words <- VCorpus(DataframeSource(x=cbind(Data$TI, Data$AB, Data$DE, Data$ID)))

##We clean up the text to prepare for further analysis
# Convert the text to lower case
Words <- tm_map(Words, content_transformer(tolower))
# Remove numbers
Words <- tm_map(Words, removeNumbers)
# Remove english common stopwords
Words <- tm_map(Words, removeWords, stopwords("english"))
# specify your stopwords as a character vector
Words <- tm_map(Words, removeWords, c("can", "new", "one", "two", "also", "may", "three", "however", "will")) 
# Remove punctuations
Words <- tm_map(Words, removePunctuation)
# Eliminate extra white spaces
Words <- tm_map(Words, stripWhitespace)

# Text stemming - this removes suffixes so we're working w/ root words
#But stemCompletion isn't working - maybe package error? - so I'm skipping it
#Words.copy<-Words #make a copy to use for stemCompletion
#Words <- tm_map(Words, stemDocument)
#Words <- tm_map(Words, stemCompletion, dictionary=Words.copy)

#Build a word frequency table
dtm <- TermDocumentMatrix(Words)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
Table <- data.frame(word = names(v),freq=v)
head(Table, 100)

##I want parasite/diseases, dam/dams, etc to be one word each
#Here's a clunky solution
Table$freq[Table$word=="disease"]<-Table$freq[Table$word=="diseases"]+Table$freq[Table$word=="disease"]
Table$freq[Table$word=="dam"]<-Table$freq[Table$word=="dams"]+Table$freq[Table$word=="dam"]
Table$freq[Table$word=="service"]<-Table$freq[Table$word=="services"]+Table$freq[Table$word=="service"]
Table$freq[Table$word=="individual"]<-Table$freq[Table$word=="individuals"]+Table$freq[Table$word=="individual"]
Table$freq[Table$word=="population"]<-Table$freq[Table$word=="populations"]+Table$freq[Table$word=="population"]
Table$freq[Table$word=="community"]<-Table$freq[Table$word=="communities"]+Table$freq[Table$word=="community"]
Table$freq[Table$word=="human"]<-Table$freq[Table$word=="humans"]+Table$freq[Table$word=="human"]
Table$freq[Table$word=="intervention"]<-Table$freq[Table$word=="interventions"]+Table$freq[Table$word=="intervention"]
Table<-Table[Table$word!="diseases",]
Table<-Table[Table$word!="dams",]
Table<-Table[Table$word!="services",]
Table<-Table[Table$word!="individuals",]
Table<-Table[Table$word!="populations",]
Table<-Table[Table$word!="communities",]
Table<-Table[Table$word!="humans",]
Table<-Table[Table$word!="interventions",]

#Build our word cloud
numwords<-150
#aspect ratio can only be variable if percent of rotate words = 0
wordcloud(words = Table$word, freq = Table$freq, min.freq = 1, max.words=numwords, random.order=FALSE, rot.per=0, fixed.asp=F, colors=brewer.pal(8, "Dark2"))

#Save the wordcloud to a high res .png
#if you don't make the dimensions wide enough, the words don't all fit 
png("WordCloud150Words.png", width = 7, height= 3, units = "in", res = 1200)
par(mar = c(0, 0, 0, 0))
wordcloud(words = Table$word, freq = Table$freq, min.freq = 1, max.words=numwords, random.order=FALSE, rot.per=0, fixed.asp=F, colors=brewer.pal(8, "Dark2"))
dev.off()

which(Table$word=="ecosystems") #ecosystems doesn't make it on