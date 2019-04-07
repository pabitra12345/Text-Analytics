require(rJava)
require(NLP)
require(openNLP)
library(textstem)
library(stringr)
library(stringi)
library(SentimentAnalysis)
library(sentimentr)
library(tokenizers)
library(tm)


#text from servqual dataset
servqual=read.csv("E:/Text analytics/practice1/Online Review Data and Servqual Model.csv")
servqual$Review_Text=as.character(servqual$Review_Text)
s=servqual$Review_Text[1]
class(s)
s=as.String(s)
class(s)

#sentence and word token annotations
sent_token_annotator=Maxent_Sent_Token_Annotator()
word_token_annotator=Maxent_Word_Token_Annotator()
a2=annotate(s,list(sent_token_annotator,word_token_annotator))
class(a2)
head(a2)

pos_tag_annotator=Maxent_POS_Tag_Annotator()

a3=annotate(s,pos_tag_annotator,a2)
head(a3)
a3w=subset(a3,type=="word")
head(a3w)
a3w
tags=sapply(a3w$features,'[[',"POS")
tags
table(tags)

#extract token/POS pairs (all)
sprintf("%s%s",s[a3w],tags)

sprintf("%s",s[aspect_terms])




# #aspect terms
aspect_terms=c("business","charges","appointment","infrastructure")
# aspect_terms=lemmatize_words(aspect_terms)
# 
# 
# #extracting aspect terms and related words
# 
# str_count(s,aspect_terms)
# str_extract_all(s,aspect_terms)

#extract characters from string and form a table with aspect+_2 words
Maxent_Word_Token_Annotator(s)

#table of extracted terms of aspects
View(table(str_extract_all(s,aspect_terms)))

#table of previous words from extracted terms
# for (i in 1:4) {
#   x=aspect_terms[i]
#   senti_table=aspect_terms
#   senti_table[i]=str_extract_all(s," \\w+\\s+x")
#   senti_table=unlist(senti_table)
# }
senti_table=aspect_terms
x1=aspect_terms[1]
x1
x2=aspect_terms[2]
x2
x3=aspect_terms[3]
x3
x4=aspect_terms[4]
x4
senti_table[1]=str_extract_all(s," \\w+\\s+business")
senti_table[2]=str_extract_all(s," \\w+\\s+charges")
senti_table[3]=str_extract_all(s," \\w+\\s+appointment")
senti_table[4]=str_extract_all(s," \\w+\\s+infrastructure")

#table contains aspect terms and previous word
senti_table=unlist(senti_table)
senti_table

#highlighting the sentiment terms of 1st review
s_variable %>% sentiment_by(by=NULL) %>% highlight()

#table with sentiment scores of aspect terms
sentiment(s_variable)
sentiment(senti_table)

#creating a text variable and extracting sentiment terms from 1st review
s_variable=get_sentences(sample(s[[1]],1,replace = TRUE))
s_variable
s_variable%>% extract_sentiment_terms()










# #analyzing sentiment
# s_vector=VectorSource(s)
# s_corpus=VCorpus(s_vector)
# analyzeSentiment(s_corpus)
# 
# 
# #practice sentimentr and sentimentanalysis
# sen1=get_sentences(sample(servqual$Review_Text,100,replace = TRUE))
# sen1 %>% extract_sentiment_terms()
# sen1 %>% sentiment_by(by=NULL) %>% highlight()
# analyzeSentiment(servqual_corpus)
# ?extract_sentiment_terms





###################
#New approach to aspect mining-
#1. Break review to sentences 
#2. Find out POS tags of individual sentence
#3. Identify noun, verb, adjective and adverbs
#4. Identify aspects from above pos tags
#5. Calculate sentiment scores sentence wise using those aspect terms.
####################


#extracting data from 1st review
servqual=read.csv("E:/Text analytics/practice1/Online Review Data and Servqual Model.csv")
servqual$Review_Text=as.character(servqual$Review_Text)
s=servqual$Review_Text[1]
s=as.String(s)


###### 2 ways to split document sentence wise
#1. creating a text variable using get_sentence of 1st review
s_variable=get_sentences(sample(s[[1]],1,replace = TRUE))
s_variable
#2. splitting document sentence wise(creating a list) and finding out sentiment terms
s_split=str_split(s,"[:punct:]")
class(s_split)

#extracting individual sentences 
s_split[[1]][1]
s_split[[1]][2]
s_split[[1]][3]


#pos tagging after sentence and word annotation
sent_token_annotator=Maxent_Sent_Token_Annotator()
word_token_annotator=Maxent_Word_Token_Annotator()
a2=annotate(s_variable,list(sent_token_annotator,word_token_annotator))
class(a2)
head(a2)
a2

pos_tag_annotator=Maxent_POS_Tag_Annotator()
a3=annotate(s_variable,pos_tag_annotator,a2)
head(a3)

#dividing the document to sentence and words tags
a3w=subset(a3,type=="word")
a3s=subset(a3,type=="sentence")
head(a3w)
head(a3s)

#extracting pos tags word wise
tags_word=sapply(a3w$features,'[[',"POS")
tags_word
table(tags_word)


s_new=s_split
s_word_tag=sprintf("%s%s%s",s[a3w],"/",tags_word)
s_sen_tag=sprintf("%s%s%s",s[a3s],"/",tags_word)


#function for extracting pos tags sentence wise
servqual=read.csv("E:/Text analytics/practice1/Online Review Data and Servqual Model.csv")
s <- as.String(servqual$Review_Text[1])
s_split=str_split(s,"[:punct:]")


fun_pos_sen= function(s_split){
  
for (i in 1:9) {
  
  s1=as.String(s_split[[1]][i])
  sent_token_annotator=Maxent_Sent_Token_Annotator()
  word_token_annotator=Maxent_Word_Token_Annotator()
  a2=annotate(s1,list(sent_token_annotator,word_token_annotator))
  pos_tag_annotator=Maxent_POS_Tag_Annotator()
  a3=annotate(s1,pos_tag_annotator,a2)
  a3w=subset(a3,type=="word")
  tags_word=sapply(a3w$features,'[[',"POS")
  s_word_tag=as.String(sprintf("%s%s%s",s1[a3w],"/",tags_word))
  s_split[[1]][i]=str_replace_all(s_word_tag,"\n"," ")
  
}
}

#function to extract noun terms from sentence 
s_new=s_split
for (i in 1:9) {
  s_new[[1]][i]=str_extract_all(s_split[[1]][i]," \\w+\\/NN")
  
}

#function to extract verb and adverb terms
s_new=s_split
for (i in 1:9) {
  s_new[[1]][i]=str_extract_all(s_split[[1]][i]," \\w+\\/RB")
  
}

#function to extract verb terms
s_new=s_split
for (i in 1:9) {
  s_new[[1]][i]=str_extract_all(s_split[[1]][i]," \\w+\\/VB")
  
}






