#Install the appropriate packages
#install.packages("twitteR")
library("twitteR")

#REPLACE '####' with the appropriate values from your twitter app
consumerKey=
consumerSecret=
accessToken=
accessTokenSecret=

#connect to twitter
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#pull tweets
tweets <- userTimeline('coffee_dad', n=10, maxID=NULL, sinceID=NULL, includeRts=FALSE, 
                       excludeReplies=FALSE)

#tweets to data frame
tweetsDF <- twListToDF(tweets)

#generate list of first words
allwords = str_split_fixed(tweetsDF[["text"]], " ", Inf)
allwords[,1]

#install packages if needed

#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("stringr")
#install.packages("tidyr")


library("dplyr")
library("tidytext")
library("stringr")
library("tidyr")

#make a tribble thing
text_df <- tibble(text = tweetsDF$text)
remove_reg <- "&amp;|&lt;|&gt;"
text_df

#make list of bigrams by frequency
bot_bigrams <- text_df %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("w1", "w2"), sep = " ") %>%
  count(w1, w2, sort = FALSE)

bot_bigrams


#make list of words by frequency
bot_words <- text_df %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "ngrams", n=1) %>%
  count(word, sort = FALSE)

bot_words

#create matrix structure
wordList <- matrix(bot_words$word)
wordList

#Probability Matrix done by hand

P = matrix(c(0,0,1,0,0,0,0,0,0,0,0,0,
             0,0,1,0,0,0,0,0,0,0,0,0,
             0,0,1/8,1/8,1/8,1/4,0,1/8,0,0,0,1/4,
             0,0,0,0,0,0,0,0,0,1,0,0,
             0,0,0,0,0,0,0,0,1,0,0,0,
             0,0,1,0,0,0,0,0,0,0,0,0,
             0,1,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,1,0,
             0,0,0,0,0,0,1,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,
             0,0,1,0,0,0,0,0,0,0,0,0), nrow = 12, byrow=T)
rownames(P)=wordList
colnames(P)=wordList
P


matrixpower(P,50)
matrixpower(P,100)

init = (c(1,0,0,0,0,0,0,0,0,0,0,0)) #Start word set to 'a'

sim = markov(init,P,5,wordList) #Tweet length set to 6 words
sim
