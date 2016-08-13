install.packages('tm')
install.packages("ggplot2")
install.packages("SnowballC")
library(ggplot2)
library(tm)

libs<-c('tm','plyr','class')
lapply(libs, require, character.only=TRUE)

text_data<-scan("/media/maverick/New Volume/DS/RFiles/doc_clas_trg_data", character(0), sep = "\n")

typeof(text_data)  ##list

typeof(head(text_data,1))  ##
head(text_data,4)
##see<-text_data[2:3]
text_data2<-text_data[2:5486]

#####SUBSTRING
a="abcd"
a[1]
nchar(a) #for length
is.character(a)
substr(a,1,1)
substr(a,1,1)<-"f"
substr(a,1,1)<-NULL
a
#####

sample<-head(text_data2,3)
sample[1]
x=0
substr(sample,1,1)<-" "

len=length(sample)

dc_df<- data.frame(x = numeric, y = character, stringsAsFactors = FALSE)

for (i in 1:len) {
  #print(sample[i])
  
  z<-substr(sample,1,1)
  y<-substr(sample,3,nchar(sample))
  dc_df<-data.frame(z,y)
}

is.data.frame(dc_df)

cleanCorpus<- function(corpus){
  corpus.tmp <- tm_map(corpus,removePunctuation)
  corpus.tmp <- tm_map(corpus,stripWhitespace)
  corpus.tmp <- tm_map(corpus,content_transformer(tolower))
  corpus.tmp <- tm_map(corpus,removeWords,stopwords("english"))
}

text_data3 <- as.character(dc_df$y)
text_data3 <- as.vector(dc_df$y)
typeof(text_data3)

cleanCorpus(text_data3[1])

docs <- Corpus(VectorSource(text_data3))


cleanCorpus<-function(path){
  
  docs<-tm_map(docs,content_transformer(tolower))
  docs<-tm_map(docs,content_transformer(removeNumbers))
  docs<-tm_map(docs,content_transformer(removePunctuation))
  docs<-tm_map(docs,removeWords,stopwords('en'))
  docs<-tm_map(docs,stripWhitespace)
  docs<-tm_map(docs,stemDocument)
  dtm<-DocumentTermMatrix(docs)
}
#################################################### final 1

require('SnowballC')
install.packages("slam")
install.packages("RTextTools")
install.packages("rattle",dependencies=c("Depends","Suggests"))

library("tm","/home/maverick/R/x86_64-pc-linux-gnu-library/3.2/tmp")
library('SnowballC')
library("RTextTools")
library("slam")
library(tm)

libs<-c('plyr','class')
lapply(libs, require, character.only=TRUE)

#text_data<-scan("/media/maverick/New Volume/DS/RFiles/doc_clas_trg_data", character(0), sep = "\n")
text_data<-scan("D:/DS/RFiles/doc_clas_trg_data", character(0), sep = "\n")
text_data2<-text_data[2:5486]

dc_df<- data.frame(x = numeric, y = character, stringsAsFactors = FALSE)
len<-length(text_data2)
for (i in 1:len) {
  z<-substr(text_data2[i],1,1)
  y<-substr(text_data2[i],3,nchar(text_data2[i]))
  dc_df<-rbind(dc_df,data.frame(z,y))
}

###trace("create_matrix",edit=TRUE)

text_data3<-as.character(dc_df$y)
text_data3<-as.vector(text_data3)

docs <- Corpus(VectorSource(text_data3))
docs<-tm_map(docs,content_transformer(tolower),lazy=TRUE)
docs<-tm_map(docs,content_transformer(removeNumbers))
docs<-tm_map(docs,content_transformer(removePunctuation))
docs<-tm_map(docs,removeWords,stopwords('en'))
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument)
dtm<-DocumentTermMatrix(docs)

container <- create_container(dtm, dc_df$z, trainSize=1:5485, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)

predictionData <- list("helig meyers co hmy year feb shr dlrs vs dlrs net mln dlrs vs mln revs mln vs mln note results reflect year month period because company changed fiscal year to end february from march reuter ",
"kelly oil and gas partners kly year dec shr cts vs cts net mln vs mln revs mln vs mln reuter ",
"japan seeks to strengthen paris currency accord japan will seek to strengthen the paris accord on currency stability at the meeting of the group of seven leading industrial nations tomorrow japanese officials said however the officials travelling with japanese finance minister kiichi miyazawa and who asked not to be identified would not provide any details of how they wanted the accord which was signed by the six leading industrial democracies in february to be strengthened currency target zones or reference ranges will not be discussed at the g meeting which is scheduled for tomorrow the japanese officials said the meeting which is being held in conjunction with this week s international monetary fund world bank sessions will reaffirm the currency pact and there is no need for changing the language used in the paris accord the officials said miyazawa met with u s treasury secretary james baker early in this afternoon and discussed the dollar yen exchange rates officials said but they declined to disclosed the details of that discussion the japanese officials also declined to detail what miyazawa and baker discussed on the subject of greater joint intervention in currency markets to stabilize the dollar or on independent american intervention the officials said such a money market action to stabilize the dollar is not only for the benefit of japan which is suffering from a sharp appreciation in its currency but also for the benefit of the united states as well as to u s urgings for japan to take steps to boost its domestic demand to reduce its trade surplus japan will explain economic measures to the g the officials said however miyazawa failed to outline the size of the japanese economic package in his meeting with baker today because the japanese budget has not been authorized by the diet or parliament despite the new fiscal year which started april one the officials said japan s ruling liberal democratic party revealed its own economic package today calling for more than billion yen in additional spending reuter ",
"tcw convertible securities cvt sets dividend tcw convertible securities fund inc said its board declared an initial quarterly dividend of three cents per share payable april to shareholders of record april tcw said it anticipates paying a regular quarterly dividend the company made its initial public stock offering march five reuter ",
"south korean won fixed at month high the bank of korea said it fixed the midrate of the won at to the dollar its highest level since february when it was the won was set at yesterday the won has risen pct against the dollar so far this year after rising pct in reuter ",
"australian unions launch new south wales strikes australian trade unions said they have launched week long strikes and other industrial action in new south wales nsw to protest against new laws that would reduce injury compensation payments union sources said talks with the state government broke down last night but the two sides are scheduled to meet later today in an attempt to find a compromise rail freight and shipping cargo movements in the country s most populous state were the first to be affected and union officials said almost every business sector will be hit unless there is a quick settlement the state government recently introduced a new workers compensation act which would cut the cash benefits to injured workers by up to a third the act is now awaiting parliamentary ratification nsw state premier barrie unsworth has said workers compensation has risen steeply in recent years and the proposed cuts would save hundreds of mlns of dollars a year union officials said industrial action could spread to other states as the federal government also plans to make sharp cuts in workers compensation reuter"
)
predictionData<- list()
predictionData <- append(predictionData,"This is a document");
predictionData<- append(predictionData,"this is another document");,"documents are seperated by newlines");
predictionData<- list("helig meyers co hmy year feb shr dlrs vs dlrs net mln dlrs vs mln revs mln vs mln note results reflect year month period because company changed fiscal year to end february from march reuter ",
                      "kelly oil and gas partners kly year dec shr cts vs cts net mln vs mln revs mln vs mln reuter ",
                      "japan seeks to strengthen paris currency accord japan will seek to strengthen the paris accord on currency stability at the meeting of the group of seven leading industrial nations tomorrow japanese officials said however the officials travelling with japanese finance minister kiichi miyazawa and who asked not to be identified would not provide any details of how they wanted the accord which was signed by the six leading industrial democracies in february to be strengthened currency target zones or reference ranges will not be discussed at the g meeting which is scheduled for tomorrow the japanese officials said the meeting which is being held in conjunction with this week s international monetary fund world bank sessions will reaffirm the currency pact and there is no need for changing the language used in the paris accord the officials said miyazawa met with u s treasury secretary james baker early in this afternoon and discussed the dollar yen exchange rates officials said but they declined to disclosed the details of that discussion the japanese officials also declined to detail what miyazawa and baker discussed on the subject of greater joint intervention in currency markets to stabilize the dollar or on independent american intervention the officials said such a money market action to stabilize the dollar is not only for the benefit of japan which is suffering from a sharp appreciation in its currency but also for the benefit of the united states as well as to u s urgings for japan to take steps to boost its domestic demand to reduce its trade surplus japan will explain economic measures to the g the officials said however miyazawa failed to outline the size of the japanese economic package in his meeting with baker today because the japanese budget has not been authorized by the diet or parliament despite the new fiscal year which started april one the officials said japan s ruling liberal democratic party revealed its own economic package today calling for more than billion yen in additional spending reuter ",
                      "tcw convertible securities cvt sets dividend tcw convertible securities fund inc said its board declared an initial quarterly dividend of three cents per share payable april to shareholders of record april tcw said it anticipates paying a regular quarterly dividend the company made its initial public stock offering march five reuter ",
                      "south korean won fixed at month high the bank of korea said it fixed the midrate of the won at to the dollar its highest level since february when it was the won was set at yesterday the won has risen pct against the dollar so far this year after rising pct in reuter ",
                      "australian unions launch new south wales strikes australian trade unions said they have launched week long strikes and other industrial action in new south wales nsw to protest against new laws that would reduce injury compensation payments union sources said talks with the state government broke down last night but the two sides are scheduled to meet later today in an attempt to find a compromise rail freight and shipping cargo movements in the country s most populous state were the first to be affected and union officials said almost every business sector will be hit unless there is a quick settlement the state government recently introduced a new workers compensation act which would cut the cash benefits to injured workers by up to a third the act is now awaiting parliamentary ratification nsw state premier barrie unsworth has said workers compensation has risen steeply in recent years and the proposed cuts would save hundreds of mlns of dollars a year union officials said industrial action could spread to other states as the federal government also plans to make sharp cuts in workers compensation reuter" );
n<-as.integer(readline())
predictionData<- list()
for(i in 1:n) {
  predictionData<-append(predictionData,readline());
}
  
predMatrix <- create_matrix(predictionData, originalMatrix=dtm)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,8), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
results

##### or
dtm1 <- DocumentTermMatrix(docs)
colTotals <-  col_sums(dtm1)
dtm2 <- dtm1[,which(colTotals > 20)]
colTotals <-  col_sums(dtm2)

container <- create_container(dtm2, dc_df$z, trainSize=1:5485, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)

predMatrix <- create_matrix(predictionData, originalMatrix=dtm2)

predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
results


######################################################   SEE later

dc_df<-data.frame(
  lapply(sample,function(x) {
    z<-substr(x,1,1)
    y<-substr(x,3,nchar(x))
    z 
  }),
  lapply(sample,function(x) {
    z<-substr(x,1,1)
    y<-substr(x,3,nchar(x))
    y 
  })
)

#dc_df[[3]]


##dc_df<-data.frame("1",sample[1])
dc_df<- data.frame(x = numeric, y = character, stringsAsFactors = FALSE)
#df1<-data.frame(x = numeric, y = character, stringsAsFactors = FALSE)

dc_df<-lapply(sample,function(x) {
  z<-substr(x,1,1)
  y<-substr(x,3,nchar(x))
  df<-data.frame(z,y);
  dc_df <- rbind(dc_df,df);  
})
as.data.frame(do.call(rbind, dc_df))

dc_df<- data.frame(x = numeric, y = character, stringsAsFactors = FALSE)
dc_df<-lapply(text_data2,function(x) {
  z<-substr(x,1,1)
  y<-substr(x,3,nchar(x))
  df<-data.frame(z,y);
  dc_df <- rbind(dc_df,df);
})

dc_df
is.data.frame(dc_df)
typeof(dc_df)
##it is a list

dc_df[1]