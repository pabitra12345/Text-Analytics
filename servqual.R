#servequal reveals categories of customer satisfaction/categories revealing the customer outcome for a particular product.
#install libraries used for servequal project
library(quanteda)
library(dplyr)
library(ggplot2)
library(SnowballC)
library(qdap)
library(stringr)
library(stringi)

#Import dataset
servqual=read.csv("E:/Text analytics/practice1/Online Review Data and Servqual Model.csv")
View(servqual)

#inspecting variables of each column-converting classification to factor and text to character
str(servqual)
servqual$Review_Text=as.character(servqual$Review_Text)

#creating subset of servqual(pre-processing) containing text and number of words
# ser1=servqual[,1:2]
# View(ser1)
# ser1$len=nchar(ser1$Review_Text)
# View(ser1)
# summary(ser1$len)

#length of text vector/number of words
servqual$len=nchar(servqual$Review_Text)

#visualization using text length and rating 
ggplot(servqual,aes(x=len,filled.contour=Star_Rating))+theme_bw()+geom_histogram(binwidth=5)+labs(y="rating of comments",x="number of words",title = "distribution of words with rating")
ggplot(servqual,aes(x=Star_Rating,fill="green"))+geom_histogram(binwidth=0.2)
ggplot(servqual,aes(x=len,fill=Star_Rating))+geom_histogram(binwidth = 10)


#creating different types of tokens(word,fasterword,fastestword,character,sentence)
tokens_word=tokens(servqual$Review_Text,what="word",remove_numbers=TRUE,remove_punct=FALSE,remove_symbols=FALSE,remove_separators=TRUE,remove_hyphens=TRUE,remove_url=TRUE)


#lower case
tokens_word=tokens_tolower(tokens_word)
#tokens_fasterword=tokens_tolower(tokens_fasterword)
#tokens_fastestword=tokens_tolower(tokens_fastestword)
#tokens_character=tokens_tolower(tokens_character)
#tokens_sentence=tolower(tokens_sentence)


#remove stopwords
tokens_word=tokens_select(tokens_word,pattern=stopwords("en"),selection = "remove")
# tokens_fasterword=tokens_select(tokens_fasterword,pattern=stopwords("en"),selection = "remove")
# tokens_fastestword=tokens_select(tokens_fastestword,pattern=stopwords("en"),selection = "remove")
# tokens_character=tokens_select(tokens_character,pattern=stopwords("en"),selection = "remove")
# tokens_sentence=tokens_select(tokens_sentence,pattern=stopwords("en"),selection = "remove")


#stemming
tokens_word=tokens_wordstem(tokens_word)

#dfm matrix/bag of words model
servqual_dfm=dfm(tokens_word,tolower = FALSE)
servqual_matrix=as.matrix(servqual_dfm)
servqual_df=as.data.frame(servqual_dfm)

#data pre-processing

#manually
#servqual_dfm[which(colnames(servqual_dfm) %in% c('business.i','uncle.i','it.i','card.i','.','it.med','s','t','u','op','andoperations.but','7th','d','mu','etc.mi','700rs'))]=NULL
#servqual_dfm=servqual_dfm[,-which(colnames(servqual_dfm) %in% c('business.i','uncle.i','it.i','card.i','.','it.med','s','t','u','op','andoperations.but','7th','d','mu','etc.mi','700rs'))]

#totoal number of tokens(unique words) in dfm matrix=5020,rows=329

#using function
# #Approach-
# 1. Deleting columns wth numbers
# 2. Deleting columns with special characters
# 3. Deleting columns with single words
# 4. Identifying and deleting columns with business domain insignificant words(doctor/pharmacy/hospital)
servqual_dfm_colnames=colnames(servqual_dfm)
View(servqual_dfm_colnames)



#deleting columns with numbers
del_num=function(w,i,y,z){
  for (i in c(1:ncol(w))) {
      x=featnames(w[,i])
      for (y in c(1:nchar(x))) {
        for (z in c(0:9)) {
           if (substr(x,y,y)==z) {
             print(x)
             # w=w[,-i]
             # dim(w)
           }
           
          }
        }
      }
}


#deleting numbers with special characters
#~!@#$%^&*(){}_+:"<>?,./;'[]-=
special_char=c('~','!','@','#','$','%','^','&','*','(',')','{','}','_','+',':','"','<','>','?',',','.','/',';',"'",'[',']','-','=')

del_splchar=function(w,i,y,z){
  for (i in c(1:ncol(w))) {
    x=featnames(w[,i])
    special_char=c('~','!','@','#','$','%','^','&','*','(',')','{','}','_','+',':','"','<','>','?',',','.','/',';',"'",'[',']','-','=')
    for (y in c(1:nchar(x))) {
      for (z in c(1:length(special_char))) {
        if (substr(x,y,y)==special_char[z]) {
          print(x)
          # w=w[,-i]
          # dim(w)
        }
        
      }
    }
  }
}



#deleting columns with single words
del_singlword=function(w,i){
  for (i in c(1:ncol(w))) {
    x=featnames(w[,i])
    if (nchar(x)==1) {
      print(x)
      # w=w[,-i]
      # dim(w)
    }
          
  }
  }



#deleting insignificant words
#value passed(w) must be dataframe(not quanteda)- to be converted later

del_insigword=function(w,i,j){
  for (i in c(1:ncol(w))) {
    for (j in c(1:nrow(w))) {
      if (w[i,j]==5) {
        print("Level 1":colnames(w[,i:(i+1)])[1])
      }
      else
      {print("Level 2":colnames(w[,i:(i+1)])[1])
        
      }
      # w=w[,-i]
      # dim(w)
      }
    }
    }
        

























