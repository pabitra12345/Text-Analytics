#####################################################################################################################################################
####################################################################################################################################################
#################################CODES FOR EXTRACTING SERVICE DIMENSIONS FROM REVIEWS OF MOUTHSHUT AND GOOGLE######################################
####################################################################################################################################################
#####################################################################################################################################################

#####Explanation of logic with input and output files-
##1. Split review to sentences and extract pos tags for each word.
##2. identify number of nouns in each sentence.
##3. For sentence with single noun, extract forward wise. If no relevant tags found, extract backward.
##4. For several nouns, extract forward till next noun. If no results, extract backward.
##5. Repeat for all reviews and extract sentiment scores for each sentence and each noun(sentiment of relevant verb, adverb and adjective combined)

##The files are split into 3 parts-
## 1. servqual_output1_manipal.csv- consisting of all information(reviews,sentence split, pos parts and rating) excluding dimension
## 2. servqual_output2_manipal.csv- output1 + dimension
## 3. servqual_output3_manipal.csv- final file for analysis consisting of reviews with each dimension score and rating to be analyzed further


#java heap memory allocation should be always done before using library rJava
options(java.parameters = "-Xmx4096m")

if (!require("rJava")) install.packages("rJava")
if (!require("NLP")) install.packages("NLP")
if (!require("openNLP")) install.packages("openNLP")
if (!require("textstem")) install.packages("textstem")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("SentimentAnalysis")) install.packages("SentimentAnalysis")
if (!require("sentimentr")) install.packages("entimentr")
if (!require("tokenizers")) install.packages("tokenizers")
if (!require("tm")) install.packages("tm")
if (!require("tibble")) install.packages("tibble")
if (!require("quanteda")) install.packages("quanteda")
if (!require("qdap")) install.packages("qdap")
if (!require("wordnet")) install.packages("wordnet")


setDict("C:/Program Files (x86)/WordNet/2.1/dict")
Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1")                  ##set directory for word net dictionary
library(wordnet)


#########################################################
################LIST OF FUNCTIONS USED##################
########################################################

#function to return pos tags to all words of each sentence
function_postag=function(review){
  for (l in 1:length(review)) {
    tryCatch(
      {
        
        string1=as.String(review[l])
        annotate_word=annotate(string1,list(Maxent_Sent_Token_Annotator(),Maxent_Word_Token_Annotator()))
        annotate_pos=annotate(string1,Maxent_POS_Tag_Annotator(),annotate_word)                            ##annotators of openNLP package to extract pos tags 
        subset_word=subset(annotate_pos,type=="word")
        tags_word=sapply(subset_word$features,'[[',"POS")
        s_word_tag=as.String(sprintf("%s%s%s",string1[subset_word],"/",tags_word))
        review[l]=str_replace_all(s_word_tag,"\n"," ")
        
      },error=function(e){}
    )  
  }
  return(review)
}


##function for removing sentences without any noun
function_withoutnoun_senremoval=function(review_split){
  for(m in 1:length(review_split)){ 
    tryCatch(
      {
        if(!str_detect(review_split[m],"/NN")){                   ##removing unwanted sentences(without nouns) will enhance/optimize execution time
          review_split=review_split[-m]
        }
      },error=function(e){}
    )  
  }
  
  return(review_split)
}


###function for removing sentences with 0 sentiment scores
function_0sentiment_senremoval=function(review){
  
  tryCatch({
      suppressWarnings(for(j in 1:length(review_split)){                        ##removing sentences with 0 sentiment scores
        if(round(sentiment(review_split[j])$sentiment,2)==0.00){                ##warning suppression using suppresWarnings(something related to external regressors)
          ##error message-no non-missing arguments to max; returning -Inf
          review_split=review_split[-j]
        }
      })
      return(review)
    } ,error=function(e){}
  )  
}


##function for cleaning the review before pos tagging
function_clean_review=function(review){
  suppressWarnings(
    tryCatch(
      {
        
        review = str_replace_all(review," x-ray "," Xray ")
        review=tolower(review)                                               ##converting review to lower case
        review=str_replace_all(review,"[[:digit:]].[[:digit:]]","")
        review = gsub("[[:digit:]]", "", review)                             ##removing digits from review
        review = str_replace_all(review," \\w "," ")                         ## removing singular words of review
        review = str_replace_all(review," dr. "," doctor ")
        review = str_replace_all(review," dr "," doctor ")
        review = str_replace_all(review," cuz "," because ")
        review = str_replace_all(review," bcoz "," because ")
        review = str_replace_all(review," mr. ","")
        review = str_replace_all(review," ms. ","")
        review = str_replace_all(review," don "," dont ")
        
        #review = rm_stopwords(review,stopwords = qdapDictionaries::Top25Words,unlist = FALSE, separate = FALSE)     ##removing stopwords
        return(review)
      },error=function(e){}
    )
  )
}



###final clean including all functions
function_finalclean=function(review){
  tryCatch(
    {
      if(!is.na(review)){
        
        review=function_clean_review(review)           ##cleaning review before pos tagging
        
        review_split=sent_detect(review,endmarks = c("?",".","!","..","...","|"),incomplete.sub = TRUE)       ##splitting review string to sentences
        
        review_split=str_replace_all(review_split,"[[:punct:]]","")             ##replacing commas to null value
        
        #review_split=function_0sentiment_senremoval(review_split)       ##removing sentences with 0 sentiment scores
        
        review_split=function_postag(review_split)      ##applying pos tag function to get word tags in sentence
        
        #review_split=str_remove_all(review_split,"[[:punct:]]/[[:punct:]]")
        review_split=str_replace_all(review_split,"  "," ")
      }
      
      return(review_split)
    },error=function(e){}
  )  
  
}




##################################################################################################################
#####################################################PART 1#######################################################
##################################################################################################################
###################################################################################################################
##############################EXTRACTING NOUN ONE BY ONE(FORWRD AND BACKWARD EXTRACTION)############################
###################################################################################################################

#importing base file(reviews)
servqual=read.csv("C:/Users/NEHA/Dropbox/Pabitra/Project- mapping service dimension with hospital reviews/Sourcefile_manipal_google+mouthshut.csv")
servqual$Text=as.character(servqual$Text)             #converting factor to string

#dummy dataframe with respective colnames
step1_df=data.frame(matrix(ncol=26))
colnames(step1_df)=c("Review_no","Review","Source","Organization","Sentence_split","Noun","Noun_plural","Noun_proper","Noun_plural_proper","Verb","Verb_Past","Verb_gerund","Verb_past_participle","Verb_nonsingluar","Verb_singular","Adverb","Adverb_comparative","Adverb_superlative","Adjective","Adjective_comparative","Adjective_superlative","conjunction","Sentiment_pos","Dimension","Rating","Sentence_sentiment")
step1_df[is.na(step1_df)]=""                      # converting na values to whitespace
head(step1_df)
j=1                                               #row number incremented after every loop


suppressWarnings(for(l in 1:nrow(servqual)) {
  review=servqual$Text[l]
  
  #test of parser
  review_split=function_finalclean(review)                         ##clean the reviews after pos tagging
  
  review_split=function_0sentiment_senremoval(review_split)        ##remove sentences with 0 sentiment scores
  
  review_split=function_withoutnoun_senremoval(review_split)       ##remove sentences without noun in it
  
  #print(review_split)
  
  step1_df[j,1]=paste("Review",l)                    ##review number in dataframe
  step1_df[j,2]=as.character(review)                 ##complete review into the dataframe
  
  for(k in 1:length(review_split))
  {
    tryCatch({
    review_split_l=review_split[k]                                               #single sentence after pos tagging to be operated
    noun_count=lengths(str_locate_all(review_split_l,"\\b\\w+/NN\\b"))/2                #number of nouns
    review_len=sapply(strsplit(review_split_l, " "), length)                     #length of review
    
    if(noun_count==1){
      location_start_1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][1]
      location_end_1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][1,2]
      noun1=str_sub(review_split_l,location_start_1,location_end_1)                    #first noun extracted from sentence
      noun_index_1=match(noun1,unlist(str_split(review_split_l," ")))                  #index number of noun in sentence
      
      if(noun_index_1==review_len){                                                   ##if noun is the last word in sentence(sigular noun sentence), include whole sentence and extract
      forward=word(review_split_l,1,noun_index_1)
      
      step1_df[j,3]=as.character(servqual$Source[l])
      step1_df[j,4]=as.character(servqual$Organization[l])
      step1_df[j,5]=as.character(review_split_l)
      step1_df[j,6]=as.character(noun1)
      step1_df[j,7]=str_replace(str_extract(forward,"\\b\\w+/NNS\\b"),"/NNS","")
      step1_df[j,8]=str_replace(str_extract(forward,"\\b\\w+/NNP\\b"),"/NNP","")
      step1_df[j,9]=str_replace(str_extract(forward,"\\b\\w+/NNPS\\b"),"/NNPS","")
      step1_df[j,10]=str_replace(str_extract(forward,"\\b\\w+/VB\\b"),"/VB","")
      step1_df[j,11]=str_replace(str_extract(forward,"\\b\\w+/VBD\\b"),"/VBD","")
      step1_df[j,12]=str_replace(str_extract(forward,"\\b\\w+/VBG\\b"),"/VBG","")
      step1_df[j,13]=str_replace(str_extract(forward,"\\b\\w+/VBN\\b"),"/VBN","")
      step1_df[j,14]=str_replace(str_extract(forward,"\\b\\w+/VBP\\b"),"/VBP","")
      step1_df[j,15]=str_replace(str_extract(forward,"\\b\\w+/VBZ\\b"),"/VBZ","")
      step1_df[j,16]=str_replace(str_extract(forward,"\\b\\w+/RB\\b"),"/RB","")
      step1_df[j,17]=str_replace(str_extract(forward,"\\b\\w+/RBR\\b"),"/RBR","")
      step1_df[j,18]=str_replace(str_extract(forward,"\\b\\w+/RBS\\b"),"/RBS","")
      step1_df[j,19]=str_replace(str_extract(forward,"\\b\\w+/JJ\\b"),"/JJ","")
      step1_df[j,20]=str_replace(str_extract(forward,"\\b\\w+/JJR\\b"),"/JJR","")
      step1_df[j,21]=str_replace(str_extract(forward,"\\b\\w+/JJS\\b"),"/JJS","")
      step1_df[j,22]=str_replace(str_extract(forward,"\\b\\w+/CC\\b"),"/CC","")
      step1_df[j,23]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,6:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
      step1_df[j,25]=servqual$Rating[l]
      step1_df[j,26]=sentiment(review_split[k])$sentiment
      
      
      step1_df[is.na(step1_df)]=""
      
      }
      else {
        
        forward=word(review_split_l,noun_index_1+1,review_len)                           ##else go for last but one noun in sentence
        
        step1_df[j,3]=as.character(servqual$Source[l])
        step1_df[j,4]=as.character(servqual$Organization[l])
        step1_df[j,5]=as.character(review_split_l)
        step1_df[j,6]=as.character(noun1)
        step1_df[j,7]=str_replace(str_extract(forward,"\\b\\w+/NNS\\b"),"/NNS","")
        step1_df[j,8]=str_replace(str_extract(forward,"\\b\\w+/NNP\\b"),"/NNP","")
        step1_df[j,9]=str_replace(str_extract(forward,"\\b\\w+/NNPS\\b"),"/NNPS","")
        step1_df[j,10]=str_replace(str_extract(forward,"\\b\\w+/VB\\b"),"/VB","")
        step1_df[j,11]=str_replace(str_extract(forward,"\\b\\w+/VBD\\b"),"/VBD","")
        step1_df[j,12]=str_replace(str_extract(forward,"\\b\\w+/VBG\\b"),"/VBG","")
        step1_df[j,13]=str_replace(str_extract(forward,"\\b\\w+/VBN\\b"),"/VBN","")
        step1_df[j,14]=str_replace(str_extract(forward,"\\b\\w+/VBP\\b"),"/VBP","")
        step1_df[j,15]=str_replace(str_extract(forward,"\\b\\w+/VBZ\\b"),"/VBZ","")
        step1_df[j,16]=str_replace(str_extract(forward,"\\b\\w+/RB\\b"),"/RB","")
        step1_df[j,17]=str_replace(str_extract(forward,"\\b\\w+/RBR\\b"),"/RBR","")
        step1_df[j,18]=str_replace(str_extract(forward,"\\b\\w+/RBS\\b"),"/RBS","")
        step1_df[j,19]=str_replace(str_extract(forward,"\\b\\w+/JJ\\b"),"/JJ","")
        step1_df[j,20]=str_replace(str_extract(forward,"\\b\\w+/JJR\\b"),"/JJR","")
        step1_df[j,21]=str_replace(str_extract(forward,"\\b\\w+/JJS\\b"),"/JJS","")
        step1_df[j,22]=str_replace(str_extract(forward,"\\b\\w+/CC\\b"),"/CC","")
        step1_df[j,23]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,6:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
        step1_df[j,25]=servqual$Rating[l]
        step1_df[j,26]=sentiment(review_split[k])$sentiment
        
        
        step1_df[is.na(step1_df)]=""
        
        
      
      if(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,7:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " ")==""){
        backward=word(review_split_l,1,noun_count-1)                                     ##if there is no relevant words in forward extraction, for for backward extraction
        
        step1_df[j,3]=as.character(servqual$Source[l])
        step1_df[j,4]=as.character(servqual$Organization[l])
        step1_df[j,5]=as.character(review_split_l)
        step1_df[j,7]=str_replace(str_extract(backward,"\\b\\w+/NNS\\b"),"/NNS","")
        step1_df[j,8]=str_replace(str_extract(backward,"\\b\\w+/NNP\\b"),"/NNP","")
        step1_df[j,9]=str_replace(str_extract(backward,"\\b\\w+/NNPS\\b"),"/NNPS","")
        step1_df[j,10]=str_replace(str_extract(backward,"\\b\\w+/VB\\b"),"/VB","")
        step1_df[j,11]=str_replace(str_extract(backward,"\\b\\w+/VBD\\b"),"/VBD","")
        step1_df[j,12]=str_replace(str_extract(backward,"\\b\\w+/VBG\\b"),"/VBG","")
        step1_df[j,13]=str_replace(str_extract(backward,"\\b\\w+/VBN\\b"),"/VBN","")
        step1_df[j,14]=str_replace(str_extract(backward,"\\b\\w+/VBP\\b"),"/VBP","")
        step1_df[j,15]=str_replace(str_extract(backward,"\\b\\w+/VBZ\\b"),"/VBZ","")
        step1_df[j,16]=str_replace(str_extract(backward,"\\b\\w+/RB\\b"),"/RB","")
        step1_df[j,17]=str_replace(str_extract(backward,"\\b\\w+/RBR\\b"),"/RBR","")
        step1_df[j,18]=str_replace(str_extract(backward,"\\b\\w+/RBS\\b"),"/RBS","")
        step1_df[j,19]=str_replace(str_extract(backward,"\\b\\w+/JJ\\b"),"/JJ","")
        step1_df[j,20]=str_replace(str_extract(backward,"\\b\\w+/JJR\\b"),"/JJR","")
        step1_df[j,21]=str_replace(str_extract(backward,"\\b\\w+/JJS\\b"),"/JJS","")
        step1_df[j,22]=str_replace(str_extract(backward,"\\b\\w+/CC\\b"),"/CC","")
        step1_df[j,23]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,6:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
        step1_df[j,25]=servqual$Rating[l]
        step1_df[j,26]=sentiment(review_split[k])$sentiment
        
        step1_df[is.na(step1_df)]=""
        
        j=j+1
      }
      
      
      
    }
      
    }
    else {
      for(t in 1:noun_count){
        tryCatch({
        
        location_start_1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t]
        location_end_1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t,2]
        noun1=str_sub(review_split_l,location_start_1,location_end_1)
        noun_index_1=match(noun1,unlist(str_split(review_split_l," ")))
        
        if(noun_index_1!=review_len){
          location_start_2=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t+1]
          location_end_2=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t+1,2]
          noun2=str_sub(review_split_l,location_start_2,location_end_2)
          noun_index_2=match(noun2,unlist(str_split(review_split_l," ")))
          
          forward=word(review_split_l,noun_index_1+1,noun_index_2-1)
          step1_df[j,3]=as.character(servqual$Source[l])
          step1_df[j,4]=as.character(servqual$Organization[l])
          step1_df[j,5]=as.character(review_split_l)
          step1_df[j,6]=str_replace(str_extract(noun1,"\\b\\w+/NN\\b"),"/NN","")
          step1_df[j,7]=str_replace(str_extract(forward,"\\b\\w+/NNS\\b"),"/NNS","")
          step1_df[j,8]=str_replace(str_extract(forward,"\\b\\w+/NNP\\b"),"/NNP","")
          step1_df[j,9]=str_replace(str_extract(forward,"\\b\\w+/NNPS\\b"),"/NNPS","")
          step1_df[j,10]=str_replace(str_extract(forward,"\\b\\w+/VB\\b"),"/VB","")
          step1_df[j,11]=str_replace(str_extract(forward,"\\b\\w+/VBD\\b"),"/VBD","")
          step1_df[j,12]=str_replace(str_extract(forward,"\\b\\w+/VBG\\b"),"/VBG","")
          step1_df[j,13]=str_replace(str_extract(forward,"\\b\\w+/VBN\\b"),"/VBN","")
          step1_df[j,14]=str_replace(str_extract(forward,"\\b\\w+/VBP\\b"),"/VBP","")
          step1_df[j,15]=str_replace(str_extract(forward,"\\b\\w+/VBZ\\b"),"/VBZ","")
          step1_df[j,16]=str_replace(str_extract(forward,"\\b\\w+/RB\\b"),"/RB","")
          step1_df[j,17]=str_replace(str_extract(forward,"\\b\\w+/RBR\\b"),"/RBR","")
          step1_df[j,18]=str_replace(str_extract(forward,"\\b\\w+/RBS\\b"),"/RBS","")
          step1_df[j,19]=str_replace(str_extract(forward,"\\b\\w+/JJ\\b"),"/JJ","")
          step1_df[j,20]=str_replace(str_extract(forward,"\\b\\w+/JJR\\b"),"/JJR","")
          step1_df[j,21]=str_replace(str_extract(forward,"\\b\\w+/JJS\\b"),"/JJS","")
          step1_df[j,22]=str_replace(str_extract(forward,"\\b\\w+/CC\\b"),"/CC","")
          step1_df[j,23]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,6:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
          step1_df[j,25]=servqual$Rating[l]
          step1_df[j,26]=sentiment(review_split[k])$sentiment
          
          step1_df[is.na(step1_df)]=""
          
          if(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,7:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " ")==""){
            location_minus=str_locate_all(review_split_l,"\\b\\w+/NN\\b")
            location_start_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1]
            location_end_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1,2]
            noun1_back=str_sub(review_split_l,location_start_minus1,location_end_minus1)
            noun1_index_1minus=match(noun1_back,unlist(str_split(review_split_l," ")))
            
            if(t==1){
              backward=word(review_split_l,1,noun_index_1-1)
            }else {
              backward=word(review_split_l,noun1_index_1minus+1,noun_index_1-1)
            }
              
            step1_df[j,3]=as.character(servqual$Source[l])
            step1_df[j,4]=as.character(servqual$Organization[l])
            step1_df[j,5]=as.character(review_split_l)
            step1_df[j,6]=str_replace(str_extract(noun1,"\\b\\w+/NN\\b"),"/NN","")
            step1_df[j,7]=str_replace(str_extract(backward,"\\b\\w+/NNS\\b"),"/NNS","")
            step1_df[j,8]=str_replace(str_extract(backward,"\\b\\w+/NNP\\b"),"/NNP","")
            step1_df[j,9]=str_replace(str_extract(backward,"\\b\\w+/NNPS\\b"),"/NNPS","")
            step1_df[j,10]=str_replace(str_extract(backward,"\\b\\w+/VB\\b"),"/VB","")
            step1_df[j,11]=str_replace(str_extract(backward,"\\b\\w+/VBD\\b"),"/VBD","")
            step1_df[j,12]=str_replace(str_extract(backward,"\\b\\w+/VBG\\b"),"/VBG","")
            step1_df[j,13]=str_replace(str_extract(backward,"\\b\\w+/VBN\\b"),"/VBN","")
            step1_df[j,14]=str_replace(str_extract(backward,"\\b\\w+/VBP\\b"),"/VBP","")
            step1_df[j,15]=str_replace(str_extract(backward,"\\b\\w+/VBZ\\b"),"/VBZ","")
            step1_df[j,16]=str_replace(str_extract(backward,"\\b\\w+/RB\\b"),"/RB","")
            step1_df[j,17]=str_replace(str_extract(backward,"\\b\\w+/RBR\\b"),"/RBR","")
            step1_df[j,18]=str_replace(str_extract(backward,"\\b\\w+/RBS\\b"),"/RBS","")
            step1_df[j,19]=str_replace(str_extract(backward,"\\b\\w+/JJ\\b"),"/JJ","")
            step1_df[j,20]=str_replace(str_extract(backward,"\\b\\w+/JJR\\b"),"/JJR","")
            step1_df[j,21]=str_replace(str_extract(backward,"\\b\\w+/JJS\\b"),"/JJS","")
            step1_df[j,22]=str_replace(str_extract(backward,"\\b\\w+/CC\\b"),"/CC","")
            step1_df[j,23]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,7:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
            step1_df[j,25]=servqual$Rating[l]
            step1_df[j,26]=sentiment(review_split[k])$sentiment
            
            step1_df[is.na(step1_df)]=""
            
          
          }
          j=j+1
          
        }
        else{
          location_minus=str_locate_all(review_split_l,"\\b\\w+/NN\\b")
          location_start_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1]
          location_end_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1,2]
          noun1_back=str_sub(review_split_l,location_start_minus1,location_end_minus1)
          noun1_index_1minus=match(noun1_back,unlist(str_split(review_split_l," ")))
          
          backward=word(review_split_l,noun1_index_1minus+1,noun_index_1-1)
          step1_df[j,3]=as.character(servqual$Source[l])
          step1_df[j,4]=as.character(servqual$Organization[l])
          step1_df[j,5]=as.character(review_split_l)
          step1_df[j,6]=str_replace(str_extract(noun1,"\\b\\w+/NN\\b"),"/NN","")
          step1_df[j,7]=str_replace(str_extract(backward,"\\b\\w+/NNS\\b"),"/NNS","")
          step1_df[j,8]=str_replace(str_extract(backward,"\\b\\w+/NNP\\b"),"/NNP","")
          step1_df[j,9]=str_replace(str_extract(backward,"\\b\\w+/NNPS\\b"),"/NNPS","")
          step1_df[j,10]=str_replace(str_extract(backward,"\\b\\w+/VB\\b"),"/VB","")
          step1_df[j,11]=str_replace(str_extract(backward,"\\b\\w+/VBD\\b"),"/VBD","")
          step1_df[j,12]=str_replace(str_extract(backward,"\\b\\w+/VBG\\b"),"/VBG","")
          step1_df[j,13]=str_replace(str_extract(backward,"\\b\\w+/VBN\\b"),"/VBN","")
          step1_df[j,14]=str_replace(str_extract(backward,"\\b\\w+/VBP\\b"),"/VBP","")
          step1_df[j,15]=str_replace(str_extract(backward,"\\b\\w+/VBZ\\b"),"/VBZ","")
          step1_df[j,16]=str_replace(str_extract(backward,"\\b\\w+/RB\\b"),"/RB","")
          step1_df[j,17]=str_replace(str_extract(backward,"\\b\\w+/RBR\\b"),"/RBR","")
          step1_df[j,18]=str_replace(str_extract(backward,"\\b\\w+/RBS\\b"),"/RBS","")
          step1_df[j,19]=str_replace(str_extract(backward,"\\b\\w+/JJ\\b"),"/JJ","")
          step1_df[j,20]=str_replace(str_extract(backward,"\\b\\w+/JJR\\b"),"/JJR","")
          step1_df[j,21]=str_replace(str_extract(backward,"\\b\\w+/JJS\\b"),"/JJS","")
          step1_df[j,22]=str_replace(str_extract(backward,"\\b\\w+/CC\\b"),"/CC","")
          step1_df[j,23]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(step1_df[j,6:21]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
          step1_df[j,25]=servqual$Rating[l]
          step1_df[j,26]=sentiment(review_split[k])$sentiment
          
          step1_df[is.na(step1_df)]=""
          
          j=j+1
          
          
        }
        
        step1_df[is.na(step1_df)]=""
        
        }, error=function(e){})
        }
      }
      
    }, error=function(e){})
    }

  print(l)
})

write.csv(step1_df,"C:/Users/NEHA/Dropbox/Pabitra/Opinion mining/Files/Output/servqual_output1_manipal.csv")
rm(list=ls()) 

#########################################################
##including aspects after matching it with noun column
#########################################################

#importing file from previous step
step1_df=read.csv("C:/Users/NEHA/Dropbox/Pabitra/Opinion mining/Files/Output/servqual_output1_manipal.csv")

#list of aspect names mapped with service dimensions
list_dimension=list(c("infrastructure","computer","system","clean","quality","fake","comfort","facility","hospital","hygiene","maintainance","building","ventilation","beds","bedrooms"),c("assessment","billing","treatment","diagnosis","professionals","doctors","nurses","experience","treatment","process","happy","stupid","interest","reluctant","specialized","money","staff","knowledge","service","lazy"),c("busy","help","response","wait","talk","discuss","prompt","cooperative","wait","answer","explain","told","advise","interest"),c("trust","beleive","confidence","honest","cost","responsible"),c("friendliness","attention","polite","respect","care","concerned","treat","emotion"))
names(list_dimension)=c("Tangibles","Reliability","Responsiveness","Assurance","Empathy")

##extracting noun from each row and comparing its synonames with dimension list obtained above
for(j in 1:nrow(step1_df)){
  tryCatch({
    for (i in 1:length(list_dimension$Tangibles)) {
      tryCatch({
        tangible_test=list_dimension$Tangibles[i]
        if(str_detect(str_replace_all(as.String(str_detect(synonyms(step1_df$Noun[j],"NOUN"),tangible_test)),"\n"," "),"TRUE")){
          step1_df[j,24]=names(list_dimension[1])
          print(names(list_dimension[1]))
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Reliability)) {
      tryCatch({
        reliability_test=list_dimension$Reliability[i]
        if(str_detect(str_replace_all(as.String(str_detect(synonyms(step1_df$Noun[j],"NOUN"),reliability_test)),"\n"," "),"TRUE")){
          step1_df[j,24]=names(list_dimension[2])
          print(names(list_dimension[2]))
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Responsiveness)) {
      tryCatch({
        responsiveness_test=list_dimension$Responsiveness[i]
        if(str_detect(str_replace_all(as.String(str_detect(synonyms(step1_df$Noun[j],"NOUN"),responsiveness_test)),"\n"," "),"TRUE")){
          step1_df[j,24]=names(list_dimension[3])
          print(names(list_dimension[3]))
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Assurance)) {
      tryCatch({
        assurance_test=list_dimension$Assurance[i]
        if(str_detect(str_replace_all(as.String(str_detect(synonyms(step1_df$Noun[j],"NOUN"),assurance_test)),"\n"," "),"TRUE")){
          step1_df[j,24]=names(list_dimension[4])
          print(names(list_dimension[4]))
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Empathy)) {
      tryCatch({
        empathy_test=list_dimension$Empathy[i]
        if(str_detect(str_replace_all(as.String(str_detect(synonyms(step1_df$Noun[j],"NOUN"),empathy_test)),"\n"," "),"TRUE")){
          step1_df[j,24]=names(list_dimension[5])
          print(names(list_dimension[5]))
        }
      },error=function(e){})
    }
  },error=function(e){})
}


###further work- stemming the nouns and synonms to extract more dimensions from reviews

df[is.na(df)]=""
view(df)
write.csv(step1_df,"C:/Users/NEHA/Dropbox/Pabitra/Opinion mining/Files/Output/servqual_output2_manipal.csv")

rm(list=ls())                                   ##clear environment for next operation





##################################################################################################################
#####################################################PART 2#######################################################
##################################################################################################################
###################################################################################################################
##############################EXTRACTING SERVICE DIMENSIONS FROM ASPECTS FOR FURTHER MAPPING############################
###################################################################################################################

##dataframe consisting of service dimensions,sentiment score and rating
dimension_df=data.frame(matrix(nrow = 1300,ncol=9))
colnames(dimension_df)=c("Review_no","Review","A1:Tangibles","A2:Reliability","A3:REsponsiveness","A4:Assurance","A5:Empathy","Average Sentiment Score","Rating")
head(dimension_df)
dimension_df[is.na(dimension_df)]=""

##importing step1 file of code
step2_df=read.csv("C:/Users/NEHA/Dropbox/Pabitra/Opinion mining/Files/Output/servqual_output2_manipal.csv")


##extracting final matrix consisting dimensions in columns with avg rating and reviews. it does not include the last review details
k=1
for(i in 1:nrow(step1_df))
    {
  tryCatch(
    { 
      if(step1_df$Review[i]!=""){
      dummy_df=step1_df[which(step1_df$Review_no==paste("Review",k)):(which(step1_df$Review_no==paste("Review",k+1))-1),]     ##dummy_df is extracted from past matrix as reference and information is extracted
      dimension_df$Review_no[k]=dummy_df$Review_no[1]
      dimension_df$Review[k]=dummy_df$Review[1]
      dimension_df$Rating[k]=dummy_df$Rating[1]
      dimension_df$`A1:Tangibles`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Tangibles",]$`Sentiment_pos`))
      dimension_df$`A2:Reliability`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Reliability",]$`Sentiment_pos`))
      dimension_df$`A3:REsponsiveness`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Responsiveness",]$`Sentiment_pos`))
      dimension_df$`A4:Assurance`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Assurance",]$`Sentiment_pos`))
      dimension_df$`A5:Empathy`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Empathy",]$`Sentiment_pos`))
      print(k)
      k=k+1
      }
    },error=function(e){}
  )  
  }

dimension_df[is.na(dimension_df)]=""
#removing nan values generated in dataframe
dimension_df[dimension_df==NaN]=""

view(dimension_df)



write.csv(dimension_df2,"C:/Users/NEHA/Dropbox/Pabitra/Opinion mining/Files/Output/servqual_output3_manipal.csv")
rm(list=ls())                                   ##clear environment for next operation







