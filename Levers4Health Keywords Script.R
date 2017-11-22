#Skylar Hopkins - November 2017

#load libraries
library(bibliometrix) #for pulling text from web of science bibtex 
library(tm)  #for text mining
library(SnowballC) #for text stemming
library(wordcloud) #word-cloud generator 
library(RColorBrewer) #color palettes

#######################################################################
#####41 papers that had mid-high relevance to levers4health############
#######################################################################

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

#Build a word frequency table
dtm <- TermDocumentMatrix(Words)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
Table <- data.frame(word = names(v),freq=v)
head(Table, 15)

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

#######################################################################
#####313 papers from a trial WoK search w/ relevant keywords############
#######################################################################

#Outside of this script, I used Web of Knowledge to create a bibtex file
#containing 313 papers that were found using these search terms
#(“human disease*” OR “Human infect*” OR “Global health” OR “One health” OR “Planetary health” OR zoonotic OR zoonoses OR DALY OR ecohealth) AND TOPIC: (“Environmental change” OR “Landscape change” OR “ecosystem function” OR “forest conservation” OR “Forest preservation” OR “Wetland conservation” OR “Wetland preservation” OR “Species conservation” OR “ecosystem service*” OR “Disease ecology” OR “biodiversity conservation”)

RawBibData <- readFiles("~/Documents/Levers4Health Keywords from 313 papers.bib")
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

#Build a word frequency table
dtm <- TermDocumentMatrix(Words)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
Table <- data.frame(word = names(v),freq=v)
head(Table, 15)

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
png("WordCloud313Papers.png", width = 7, height= 3, units = "in", res = 1200)
par(mar = c(0, 0, 0, 0))
wordcloud(words = Table$word, freq = Table$freq, min.freq = 1, max.words=numwords, random.order=FALSE, rot.per=0, fixed.asp=F, colors=brewer.pal(8, "Dark2"))
dev.off()

#################################################################################
#################Are the 41 papers in 313 papers?##############################
##################################################################################
##Database of 41 medium and high relevalence papers
RawBibData <- readFiles("~/Documents/Levers4Health Keywords.bib")
#RawBibData <- readFiles("~/Documents/Levers4Health Keywords/12 Relevant Papers.bib")
Data <- convert2df(RawBibData, dbsource = "isi", format = "bibtex")

#Database w/ 313 papers
RawBibData2 <- readFiles("~/Documents/Levers4Health Keywords/Levers4Health Keywords from 313 papers.bib")
Data2 <- convert2df(RawBibData2, dbsource = "isi", format = "bibtex")
Titles<-Data2$TI

Data$TI %in% Titles
sum(Data$TI %in% Titles) #4/41 papers

#That's fine, but we can't repeat it for many searches, especially because
#Web of Science only let's us download Bibtex files containing 500 papers
#So we need a way to do the whole process inside of R

#################################################################################
################Let's try pubmed searches in R##############################
##################################################################################
library(RISmed)

##I got 519 papers using this query online, and I got 519 out from R
searchfunction<-'("human health"[Title/Abstract]) AND "environmental health"[Title/Abstract])' 
res <- EUtilsSummary(searchfunction, type="esearch", db="pubmed", datetype='pdat', mindate=1900, maxdate=2017, retmax=10000)
QueryCount(res)

searchfunction<-'(((malaria*[Title/Abstract] OR schistosom*[Title/Abstract] OR diarrhea*[Title/Abstract] OR vector*[Title/Abstract] OR infectio*[Title/Abstract] OR transmission[Title/Abstract] OR zoono*[Title/Abstract] OR pathogen*[Title/Abstract] OR parasit*[Title/Abstract] OR DALY[Title/Abstract] OR bacteria*[Title/Abstract] OR “Planetary health”[Title/Abstract] OR Ecohealth[Title/Abstract] OR “One health”[Title/Abstract] OR arbovir*[Title/Abstract] OR viral[Title/Abstract] OR disease*[Title/Abstract])) AND (human*[Title/Abstract] OR people[Title/Abstract] OR childhood[Title/Abstract])) AND (“Environmental change”[Title/Abstract] OR “Landscape change”[Title/Abstract] OR “ecosystem function”[Title/Abstract] OR “forest conservation”[Title/Abstract] OR “Forest preservation”[Title/Abstract] OR “Wetland conservation”[Title/Abstract] OR “Wetland preservation”[Title/Abstract] OR “Species conservation”[Title/Abstract] OR “ecosystem service*”[Title/Abstract] OR “Disease ecology”[Title/Abstract] OR “Biodiversity conservation”[Title/Abstract] OR “Climate change”[Title/Abstract])'
res <- EUtilsSummary(searchfunction, type="esearch", db="pubmed", datetype='pdat', mindate=1900, maxdate=2017, retmax=10000)
QueryCount(res)
Titles<-ArticleTitle(EUtilsGet(res))

##Some of  the pulled titles are bracketed, but this will fix that 
Titles<-str_replace(Titles, "]", "")
Titles<-str_replace(Titles, "\\[", "") ##YAYYYYY

####################################################################################
#####Search for confirmed relevant papers within pubmed search results##############
#################USING DOIS IS A BIG 'OL FAIL######################################
####################################################################################
#First we need the list of relevant papers. It makes more sense to do this via DOIs 
#than title matching, because of case sensitivity and similar complications

#Load the DOI list for 40/41 of our papers
DOIs <- read_csv("~/Documents/Levers4Health Keywords/DOI list 41 papers.csv")
DOIlist<-as.vector(DOIs$DOI)
DOIlist[1] 
#Ok, but I think we need the first element to be in parentheses to run a pubmed search?
#DOIlist[1]<-paste(c("(", DOIlist[1], ")"), sep="", collapse="")

#Convert list of DOIs into one string for searching
DOIsearch<-paste(DOIlist, sep="", collapse=" OR ")

searchfunction<-DOIsearch
res <- EUtilsSummary(searchfunction, type="esearch", db="pubmed", datetype='pdat', mindate=1900, maxdate=2017, retmax=10000)
QueryCount(res) #bad - should be 40. And more than 3 papers are in pubmed
Titles<-ArticleTitle(EUtilsGet(res))

##Agh just kidding. That isn't working, because pubmed doesn't have all of these
##articles. Might try that in scopus later. For now, let's  go one at a time and pull
#the titles in a way that makes it easy to see what we're missing
DOIlist<-DOIlist[-c(5,9,13,14,15,19,20,24,33,35,39,40)]; length(DOIlist) #temporarily remove missing DOIs manually, automate later
DOIdb<-as.data.frame(DOIlist); DOIdb$Title<-rep(NA, length(DOIlist))
for (i in 1:length(DOIlist)) {
  DOIdb[i,2]<-ArticleTitle(EUtilsGet(EUtilsSummary(DOIlist[i], type="esearch", db="pubmed", datetype='pdat', mindate=1900, maxdate=2017, retmax=10000)))[1]
}
DOIdb[12,] #This isn't the correct title for that DOI. Not sure why.

#Ok, I'm giving up on the DOI approach for now. It  might make more sense to search
#by title?
