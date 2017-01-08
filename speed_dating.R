library("VIM")
rm(list = ls())

Speed.Dating.Data <- read.csv("C:/Users/Student/Desktop/DS 4559 2016/speed_dating/Speed Dating Data.csv", header=TRUE, na.strings = c(""," ", "NA"))
#View(Speed.Dating.Data)

SD <- Speed.Dating.Data
SD$iid <- as.factor(SD$iid)
SD$id <- as.factor(SD$id)
SD$pid <- as.factor(SD$pid)
SD$idg <- as.factor(SD$idg)
SD$gender <- as.factor(SD$gender)
SD$condtn <- as.factor(SD$condtn)
SD$wave <- as.factor(SD$wave)

SD$match <- as.factor(SD$match)
SD$samerace <- as.factor(SD$samerace)
SD$race_o <- as.factor(SD$race_o)
SD$dec_o <- as.factor(SD$dec_o)
SD$zipcode <- as.factor(SD$zipcode)
SD$race <- as.factor(SD$race)
SD$goal <- as.factor(SD$goal)
SD$date <- as.factor(SD$date)
SD$go_out <- as.factor(SD$go_out)
SD$field_cd <- as.factor(SD$field_cd)
SD$career_c <- as.factor(SD$career_c)
SD$length <- as.factor(SD$length)
SD$numdat_2 <- as.factor(SD$numdat_2)
SD$mn_sat <- as.numeric(gsub(",","",levels(SD$mn_sat)))[SD$mn_sat]
SD$date_3 <- as.factor(SD$date_3)

#--------------Cleaning----------------------
#--------------------------------------------
#1ST - determine which columns have high number of NA's ( > ~10%)
#i'm being relatively strict due to time constraints
colnames(SD)[colSums(is.na(SD)) > 800]
# mostly consist on last survey sent out (eg. attr7_3)
# Imputation on these columns will be tricky, so I'll leave them out for now (cuts out 35 attributes)
# mn_sat is probably the only attribute I might want to actually use later
SD_clean <- SD[,colnames(SD)[colSums(is.na(SD)) < 800]]

#-----------Numeric Imputation---------------
# using the mice package for imputation
# mice has built in tools for binary, categorical, and numerical imputation
#install.packages("mice")
library(mice)
library(VIM)

# only keeping numerical data and categorical with small number of categories
# imputation on something like or location is very inneffective
SD_num <- subset(SD_clean, select = -c(iid, id, pid, idg, condtn, wave,
                                      field, from, career, round,position, order, partner))

#getting a feel for missing data
mice_plot<-aggr(SD_num, col=c('navyblue','yellow'),
                numbers=TRUE, sortVars=TRUE,
                labels=names(SD_clean), cex.axis=.7,
                gap=3, ylab=c("Missing data","Pattern"))
#imputation method can only handle 50 attributes at max
#so I am goinf to impute twice, first on the first 50 and then on the remaining 
#SD_imp is a special object containing information about the imputation
SD_imp <- mice(SD_num, m=1, maxit = 2, seed = 100, MaxNWts = 3000)
SD_clean_num <- mice::complete(SD_imp) #all NA's are gone!

#new attribute based on how similarly they rated eachother
SD_clean_num[["similarity"]] <- sqrt((SD_clean_num$attr-SD_clean_num$attr_o)^2 + 
                                       (SD_clean_num$sinc-SD_clean_num$sinc_o)^2 + 
                                       (SD_clean_num$intel-SD_clean_num$intel_o)^2 + 
                                       (SD_clean_num$fun-SD_clean_num$fun_o)^2 + 
                                       (SD_clean_num$amb-SD_clean_num$amb_o)^2)
SD_clean_num[["confidence"]] <- sqrt((SD_clean_num$attr3_1 + SD_clean_num$sinc3_1 + SD_clean_num$intel3_1 + 
                                        SD_clean_num$fun3_1 +  SD_clean_num$amb3_1))

SD_clean_num[["age_diff"]] <- SD_clean_num$age - SD_clean_num$age_o


###normalizing attributes (NOT USED CURRENTLY)
# attribute ratings are done differently in wave 6-9
# in wave 6-9, some attributes are from 1-10
# in other waves, there are 100 points alloted for all attributes
# to normalize between waves, I will set each quality as the proportion of total points in that quality

endings_100 <- c("1_1", "4_1", "2_1", "1_s", "7_2", "1_2", "4_2", "2_2", "1_3", "7_3","4_3","2_3")
  # normalizing values that can be either 1-100 or 1-10

for(i in 1:nrow(SD_clean_num)){
  for(end in endings_100){
    attr <- paste("attr", end, sep = "")
    sinc <- paste("sinc", end, sep = "")
    fun  <- paste("fun" , end, sep = "")
    intel <- paste("intel", end, sep = "")
    amb   <- paste("amb", end, sep = "")
    shar <- paste("shar", end, sep = "") # column doesn't always exist, but this is ok
    total <- SD_clean_num[i,attr] + SD_clean_num[i,sinc] + SD_clean_num[i,fun] + SD_clean_num[i,intel] + SD_clean_num[i,amb] # SD_clean_num[i,shar] 
    #print(total)
    SD_clean_num[i,attr] <- SD_clean_num[i,attr]/total
    print(sd_row[,sinc])
    SD_clean_num[i,sinc] <- SD_clean_num[i,sinc]/total
    SD_clean_num[i,fun] <- SD_clean_num[i,fun]/total
    SD_clean_num[i,intel] <- SD_clean_num[i,intel]/total
    SD_clean_num[i,amb] <- SD_clean_num[i,amb]/total
    #SD_clean_num[i,shar] <- SD_clean_num[i,shar]/total
  }
 # print(sd_row[[sinc]])
  #SD_clean_num[i,] <- sd_row
}


# normalizing the rest
# only normalizing vars that are numeric
for(i in 1:ncol(SD_clean_num)) {
  if(is.numeric(SD_clean_num[,i])) {
    SD_clean_num[,i] <- SD_clean_num[,i]/max( SD_clean_num[,i],na.rm = TRUE) #should be no NA's at this point, but just to be safe
  }
}





# ------------- Making Decision Trees ------------------
# ------------------------------------------------------
# First, I am going to make a two difference decision trees: 1 for male and 1 for female
# I am going to compare the attributes that are important to each
# I am going to see which tree can lead to more accurate match prediction

SD_tree_vars <- subset(SD_clean_num, select = c(match, gender, samerace, attr_o, sinc_o, intel_o, fun_o, amb_o, 
                                                race, career_c,age_diff,
                                                int_corr, similarity, confidence, date, field_cd))


SD_tree_vars2 <- subset(SD_clean_num, select = c(gender, pf_o_att, pf_o_sin,  pf_o_fun,  pf_o_amb,  pf_o_sha, confidence, imprace, imprelig))


library(C50)
library(RWeka)
library(randomForest)
library(mlbench)
library(caret)
library(ggplot2)

#first tree
#testing to see if gender can be determined by person's preferences, career, and field
#leaving out interests because it would be too easy most likely
set.seed(13)
SD_tree_vars2 <- SD_tree_vars2[order(runif(8378)), ] 
SD_train <- SD_tree_vars2[1:6800,]
SD_test <- SD_tree_vars2[6801:nrow(SD_tree_vars2),]
SD_m <- randomForest(gender ~ ., SD_train)
varImpPlot(SD_m)
SD_m <- JRip(gender~.,data=SD_train)
SD_m <- C5.0(SD_train[,-1], SD_train$gender)
SD_p <- predict(SD_m, SD_test)
confusionMatrix(SD_p,SD_test$gender)
plot(SD_m, type = "simple")

# second tree
# testing to see difference in atrribute importance between genders
set.seed(13)             
SD_tree_vars <- SD_tree_vars[order(runif(8378)), ] 


#split into genders
SD_male   <- SD_tree_vars[SD_tree_vars$gender == 1, -2]
SD_male_train <- SD_male[1:3500,]
SD_male_test <-  SD_male[3501:nrow(SD_male),]
SD_model <- C5.0(SD_male_train[,-1], SD_male_train$match)
summary(SD_male_train)
male_m <- randomForest(match ~ ., SD_male_train)
importance <- varImp(male_m, scale = FALSE)
varImpPlot(male_m)
male_m <- JRip(match~.,data=SD_male_train)
male_p <- predict(male_m, SD_male_test)
confusionMatrix(male_p,SD_male_test$match)

SD_female <- SD_tree_vars[SD_tree_vars$gender == 0, -2]
SD_female_train <- SD_female[1:3500,]
SD_female_test <-  SD_female[3501:nrow(SD_female),]
SD_model <- C5.0(SD_female_train[,-1], SD_female_train$match)
summary(SD_female_train)
female_m <- randomForest(match ~ ., SD_female_train)

varImpPlot(female_m)
female_m <- JRip(match~.,data=SD_female_train)
female_p <- predict(female_m, SD_female_test)
confusionMatrix(female_p,SD_female_test$match)
#selecting the subset of features to include in data frames


############################################
#---------Sentiment Analysis---------------#
############################################

library(RTextTools)
library(twitteR)
library(base64enc)
library(ROAuth)
require(RCurl)
library(stringr)
library(ggmap)
library(plyr)
library(dplyr)
#library(plyr)
library(tm)
library(wordcloud)
  
## Now we authenticate

key='pwUcnjjRJLPzGL8TtQxgBM4vW'
secret='R0RgXRZhZ0eML9xV211AUTublpl7Nx9rqAmIvjVLSeniaLIGN1'
setwd("C:/Users/Student/Desktop/DS 4559 2016/speed_dating")
access_token='1371008792-y3CGIhNZjvd4EyNguNFpOFXOJ4mwuMN4wE008Z0'
access_token_secret='npSEbjZmIRMdZO44xxyNLLT2wGTjyzom1NSqm5MUFbxAO'

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/Student/Desktop/DS 4559 2016/speed_dating/cacert.pem",
              method="auto")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret, access_token, access_token_secret)
save(authenticate, file="twitter_authentication.Rdata")

# harvest some tweets
# Getting Tweets from new york or cville
some_tweets = searchTwitter("tinder", n=2000, geocode = '40.730610,-73.935242,10mi', retryOnRateLimit=1000)#2016-10-19, lang="en",since ='2016-10-17',
some_tweets = searchTwitter("tinder", n=2000, geocode='34.0522,-118.2437,300mi')# retryOnRateLimit=30000, since='2015-10-19')
#until ='2016-10-19',  retryOnRateLimit=120)

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL


col=brewer.pal(6,"Dark2")
wordcloud(some_txt, min.freq=20, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=50, random.order=F,colors=col)

## Here we perform some sentiment analysis:

## First we inidicate lists of positive and negative words. These are located in your 

positives= readLines("positive_words.txt")
negatives= readLines("negative_words.txt")

## Below is a function that scores sentiment on a scale of -5 to 5 (-5 being the most negative
## and 5 being the most positive).  A score is determined for each tweet based on its correlation
## with the positive words and the negative words.  This is original code written by a veteran R
##user that functions as part of an old package called "sentiment" that is no longer available.

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

## Now apply the function to our actual data.  

Score <- score.sentiment(some_txt,positives,negatives,.progress='none')

## Score has two fields: score and text.  We 
## are interested in score at this point, but we can look at a few of the tweets' text and
## the associated score first.

head(Score)

## Let’s plot a histogram of the sentiment score:

hist(Score$score,xlab="Sentiment Score ",main="Sentiment of sample tweets that have Tinder in them ",
     border="black",col="skyblue")

sum(Score$score)



## Let's take a look at the emotional content of the tweets:

classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("C:/Users/Student/Desktop/DS 4559 2016/emotions.csv.gz",header=FALSE)
  ##lexicon <- read.csv(system.file("data/emotions.csv.gz",package="sentiment"),header=FALSE)
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}

classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("C:/Users/Student/Desktop/DS 4559 2016/subjectivity.csv.gz",header=FALSE)
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
library(ggplot2)
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets")


## Make a plot of the overall polarity of the tweets:

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity))

## Finally, you can convert to a document term matrix and look for frequencies and associations:

text.corpus <- Corpus(VectorSource(some_txt))
dtm <- DocumentTermMatrix(text.corpus)


