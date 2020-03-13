#ÇÁ·ÎÁ§Æ® ÄÚµå

library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(KoNLP)
useSejongDic()
library(rjson)
library(RTextTools)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(stringr)

#1. ÁÖÁ¦ ¼±Á¤ °úÁ¤
filepath = "D:/Twitter_201402/20140210170000-20140210180000.txt"
json_data = lapply(readLines(filepath1, encoding = "UTF-8"), fromJSON)  #ÇÑÁÙ¾¿ ÀĞ¾î json µ¥ÀÌÅÍ »ı¼º

only_text = c()
for(i in 1:length(json_data)){  #ÅØ½ºÆ® µ¥ÀÌÅÍ ÃßÃâ
  text = json_data[[i]]$text
  only_text[i] = enc2native(text)
}
#length_text = length(only_text)  #ÅØ½ºÆ® ¼ö

#(1)ÅäÇÈ ¸ğµ¨¸µ
only_noun = c()
l = 1
for(i in 1:10){
  if( nchar(only_text[i]) < 2 ){
    next
  }
  candi_noun = extractNoun(only_text[i])
  if( length(candi_noun) < 1){
    next
  }
  noun = c()

  k = 1
  for (j in 1:length(candi_noun)){
    token = gsub("[^¤¡-ÆR]", "", candi_noun[j])
    if( nchar(token) >= 2){
      noun[k] = enc2native(token)
      k = k + 1
    }
  }
  if( length(noun) < 1){
    next
  }
  noun = paste(noun, collapse=' ')
  only_noun[l] = noun
  l = l+1
}
#print(only_noun[1])  #È®ÀÎ

noun_corpus <- Corpus(VectorSource(only_noun)) 
inspect(noun_corpus)
matrix <- TermDocumentMatrix(noun_corpus)

#my_lda3 = LDA(matrix, 3)
#my_lda5 = LDA(matrix, 5)
my_lda10 = LDA(matrix, 10)  #ÀÌ¿ë
my_lda = my_lda10

my_topics <- tidy(my_lda, matrix = "beta")
my_topics[,2] = matrix$dimnames$Terms[as.integer(my_topics$term)]
my_topics

my_top_terms <- my_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#print(my_top_terms[my_top_terms$topic==1,])  #È®ÀÎ

my_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

my_beta_spread <- my_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
#my_beta_spread  #È®ÀÎ

#(2)Å°¿öµå ºóµµ¼ö ºñ±³
subject_text = c()  #ÁÖÁ¦ °ü·Ã ÅØ½ºÆ® ÃßÃâ
subject_text = only_text[grep("³ëÈ«Ã¶|À½ÁÖ¿îÀü", only_text)]
subject_text = only_text[grep("°£ÅëÁË|ÆóÁö", only_text)]
subject_text = only_text[grep("±è¿¬¾Æ|¿Ã¸²ÇÈ", only_text)]
subject_text = only_text[grep("¼ÒÆ®´ÏÄÚ¹Ù", only_text)]

length_sub = length(subject_text)  #Å°¿öµå Æ÷ÇÔ µ¥ÀÌÅÍ ¼ö
length_sub/length_text  #Å°¿öµå Æ÷ÇÔ µ¥ÀÌÅÍ ºñÀ²

#(3)subject_textÀÇ LDA È¥Àâµµ ºñ±³
only_noun2 = c()  #subject_text·Î »õ·Î¿î Topic Modeling »ı¼º
l2 = 1
for(i in 1:10){
  if( nchar(subject_text[i]) < 2 ){
    next
  }
  candi_noun2 = extractNoun(subject_text[i])
  if( length(candi_noun2) < 1){
    next
  }
  noun2 = c()
  k2 = 1
  for (j in 1:length(candi_noun2)){
    token2 = gsub("[^¤¡-ÆR]", "", candi_noun2[j])
    if( nchar(token2) >= 2){
      noun2[k] = enc2native(token2)
      k2 = k2 + 1
    }
  }
  if( length(noun2) < 1){
    next
  }
  noun2 = paste(noun2, collapse=' ')
  only_noun2[l2] = noun2
  l2 = l2+1
}
#print(only_noun2[1])  #È®ÀÎ

noun_corpus2 <- Corpus(VectorSource(only_noun2)) 
inspect(noun_corpus2)
matrix2 <- TermDocumentMatrix(noun_corpus2)
my_lda10 = LDA(matrix2, 10)
perplexity(my_lda10)  #È¥Àâµµ Ãâ·Â

#3. ºĞ¼® ÇÁ·Î¼¼½º

#(1)µ¥ÀÌÅÍ ÀüÃ³¸®
#¿Ã¸²ÇÈ
setwd("D:/Twitter_201402/")
filepath = "20140224.txt"
json_data = lapply(readLines(filepath, encoding = "UTF-8"), fromJSON)
 
only_text = c()
for(i in 1:length(json_data)){  #ÅØ½ºÆ® µ¥ÀÌÅÍ ÃßÃâ
  text = json_data[[i]]$text
  only_text[i] = enc2native(text)
}
#length(only_text)  #ÅØ½ºÆ® ¼ö

subject_text = c()  #ÁÖÁ¦ °ü·Ã ÅØ½ºÆ® ÃßÃâ
subject_text = only_text[grep("¿Ã¸²ÇÈ|±İ¸Ş´Ş|Àº¸Ş´Ş|µ¿¸Ş´Ş|
¼îÆ®Æ®·¢|½ºÇÇµå½ºÄÉÀÌÆÃ|ÇÇ°Ü|º¾½½·¹ÀÌ|½ºÅ°Á¡ÇÁ|ÄÃ¸µ|±è¿¬¾Æ|ÀÌ»óÈ­|ÀÌ½ÂÈÆ|½ÉÆÇ|¿À½É", only_text)]
#length(subject_text)  #Å°¿öµå Æ÷ÇÔ µ¥ÀÌÅÍ ¼ö
write.table(subject_text, "D:/¿Ã¸²ÇÈ/20140224.txt")  #ºĞ¼®À» À§ÇÑ ÅØ½ºÆ® µ¥ÀÌÅÍ ÀúÀå

#¿ùµåÄÅ
setwd("D:/Twitter_201407/")
filepath = "20140701.txt"
json_data = lapply(readLines(filepath, encoding = "UTF-8"), fromJSON) 

only_text = c()
for(i in 1:length(json_data)){
  text = json_data[[i]]$text
  only_text[i] = enc2native(text)
}
#length(only_text)

subject_text = c()
subject_text = only_text[grep("¿ùµåÄÅ|Ãà±¸|½Â¸®|¹«½ÂºÎ|ÆĞ¹è|
»ó´ëÆÀ|·©Å·|±â¼º¿ë|¼ÕÈï¹Î|±¸ÀÚÃ¶|¹ÚÁÖ¿µ|ÀÌÃ»¿ë|Á¤¼º·æ|½ÉÆÇ|¿À½É", only_text)]
#length(subject_text)
write.table(subject_text, "D:/¿ùµåÄÅ/¿ùµåÄÅ ±â°£/20140701.txt")

#¿öµå Å¬¶ó¿ìµå·Î È®ÀÎ
data <- readLines("D:/¿Ã¸²ÇÈ/20140221.txt")
#data <- readLines("D:/¿ùµåÄÅ/20140623.txt")
data <- sapply(data, extractNoun, USE.NAMES = F)
data_unlist <- unlist(data)
data_unlist = gsub(" ","",data_unlist)
data_unlist = gsub("[^¤¡-ÆR]","",data_unlist)
data_unlist = Filter(function(x){nchar(x)>=2}, data_unlist)
#head(data_unlist)
wordcount <- table(data_unlist)
#head(wordcount)
wordcount_top <-head(sort(wordcount, decreasing = T),100)
#wordcount_top
#wordcloud(names(wordcount_top), wordcount_top)
color <- brewer.pal(12, "Set3")
wordcloud(names(wordcount_top), wordcount_top, rot.per=0.35, min.freq = 3000, 
scale=c(5.8,0.3), random.order = FALSE, random.color = TRUE, colors = color)

#(2)Çà»çº° °ü½É ºĞ¼®
#³¯Â¥º° length(subject_text)/length(only_text)¸¦ ÃßÃâÇÑ µ¥ÀÌÅÍ¼Â ÀÌ¿ë
dfr1 <- read.csv("C:/Users/±èÈ²±Ô/Desktop/sns ÀÚ·á/¿Ã¸²ÇÈ1.csv")
#head(dfr1)  #È®ÀÎ
barplot(dfr1$°ü½É, names.arg=dfr1$³¯Â¥, 
main="¿Ã¸²ÇÈ Çà»ç¿¡ ´ëÇÑ °ü½É", xlab="³¯Â¥", ylab="¹éºĞÀ²")
abline(h=mean(dfr1$°ü½É), col="red", lty=3)

dfr2 <- read.csv("C:/Users/±èÈ²±Ô/Desktop/sns ÀÚ·á/¿ùµåÄÅ1.csv")
barplot(dfr2$°ü½É, names.arg=dfr2$³¯Â¥, 
main="¿ùµåÄÅ Çà»ç¿¡ ´ëÇÑ °ü½É", xlab="³¯Â¥", ylab="¹éºĞÀ²")
abline(h=mean(dfr2$°ü½É), col="red", lty=3)

dfr3 <- read.csv("C:/Users/±èÈ²±Ô/Desktop/sns ÀÚ·á/Ãà±¸1.csv")
barplot(dfr3$°ü½É, names.arg=dfr3$³¯Â¥, 
main="Ãà±¸ °æ±â¿¡ ´ëÇÑ °ü½É", xlab="³¯Â¥", ylab="¹éºĞÀ²")
abline(h=mean(dfr3$°ü½É), col="red", lty=3)

#(2)Çà»çº° °¨¼ºÁö¼ö ºĞ¼®
text = readLines("D:/¿Ã¸²ÇÈ/20140212.txt")
data = sapply(text, extractNoun, USE.NAMES = F)
word = unlist(data)
word = gsub(" ","",word)
word = gsub("[^¤¡-ÆR]", "", word)
word = Filter(function(x){nchar(x)>=2}, word)
write.table(word, "D:/¿Ã¸²ÇÈ/word.txt")

word = read.table("D:/¿Ã¸²ÇÈ/word.txt")
wordcount = table(word)
top = sort(wordcount, decreasing = T)
top10 = head(top, 10)
#top10  #È®ÀÎ
top200 = head(top, 200)
write.table(top200, "D:/¿Ã¸²ÇÈ/top200.txt")

pos.dic = readLines("C:/Users/±èÈ²±Ô/Desktop/sns ÀÚ·á/pos.txt")
neg.dic = readLines("C:/Users/±èÈ²±Ô/Desktop/sns ÀÚ·á/neg.txt")
#head(pos.dic)  #È®ÀÎ
#head(neg.dic)  #È®ÀÎ
#length(pos.dic)  #È®ÀÎ
#length(neg.dic)  #È®ÀÎ

#text = readLines("D:/¿Ã¸²ÇÈ/20140224.txt")
text = readLines("D:/¿ùµåÄÅ/20140627.txt")

count1 = c()
for (i in 1:length(pos.dic)) {
  count1[i] = length(grep(pos.dic[i], text))
}
count.pos <- sum(count1)

count2 = c()
for (i in 1:length(neg.dic)) {
  count2[i] = length(grep(neg.dic[i], text))
}
count.neg <- sum(count2)
c(count.pos, count.neg)  #±à/ºÎÁ¤ Áö¼ö È®ÀÎ
count.pos-count.neg  #°¨¼ºÁö¼ö »ı¼º

#°¨¼ºÁö¼ö Ç¥ÁØÈ­ °úÁ¤
olym <- read.csv("C:/Users/±èÈ²±Ô/Desktop/sns ÀÚ·á/¿Ã¸²ÇÈ2(ÃÖÁ¾).csv")
senti <- olym$°¨¼ºÁö¼ö
senti.temp <- scale(senti)
olym$°¨¼ºÁö¼ö <- senti.temp[,1]
#olym$°¨¼ºÁö¼ö  #È®ÀÎ
ggplot(olym, aes(x=olym$³¯Â¥, y=olym$°¨¼ºÁö¼ö))+geom_point()+geom_line()

word <- read.csv("C:/Users/±èÈ²±Ô/Desktop/sns ÀÚ·á/Ãà±¸2(ÃÖÁ¾).csv")
senti2 <- word$°¨¼ºÁö¼ö
senti.temp2 <- scale(senti2)
word$°¨¼ºÁö¼ö <- senti.temp2[,1]
#word$°¨¼ºÁö¼ö  #È®ÀÎ
ggplot(word, aes(x=word$³¯Â¥, y=word$°¨¼ºÁö¼ö))+geom_point()+geom_line()

#(3)¿Ã¸²ÇÈ ´Ùº¯¼ö ºĞ¼®
head(olym)
attach(olym)
oneway.test(°ü½É~¸Ş´Ş) 	  	#p-value=0.7527 > alpha=0.1, H0±â°¢X(º¯¼ö¿¡ µû¸¥ Â÷ÀÌ ¾øÀ½)
oneway.test(°ü½É~Á¾¸ñÀÎ±â)   	#0.05285
oneway.test(°ü½É~³²³à)  	  	#0.5452
oneway.test(°ü½É~¿À½É³í¶õ)  	#0.001714
oneway.test(°¨¼ºÁö¼ö~¸Ş´Ş)  	#0.9669
oneway.test(°¨¼ºÁö¼ö~Á¾¸ñÀÎ±â) 	#0.09836
oneway.test(°¨¼ºÁö¼ö~³²³à)  	#0.6268
oneway.test(°¨¼ºÁö¼ö~¿À½É³í¶õ)  	#0.02446

tt <- table(Á¾¸ñÀÎ±â, ¿À½É³í¶õ)
chisq.test(tt)

lm(°ü½É~Á¾¸ñÀÎ±â+¿À½É³í¶õ)
summary(lm(°ü½É~Á¾¸ñÀÎ±â+¿À½É³í¶õ))
lm(°¨¼ºÁö¼ö~Á¾¸ñÀÎ±â+¿À½É³í¶õ)
summary(lm(°¨¼ºÁö¼ö~Á¾¸ñÀÎ±â+¿À½É³í¶õ))
lm.result <- lm(°¨¼ºÁö¼ö~°ü½É)
summary(lm.result)
plot(°ü½É, °¨¼ºÁö¼ö); abline(lm.result)
detach(olym)

#(4)¿ùµåÄÅ(Ãà±¸ °æ±â) ´Ùº¯¼ö ºĞ¼®
head(word)
attach(word)
oneway.test(°ü½É~½ÂÆĞ) 	  	#p-value=0.1628 > alpha=0.1, H0±â°¢X(º¯¼ö¿¡ µû¸¥ Â÷ÀÌ ¾øÀ½)
oneway.test(°ü½É~Á¡¼öÂ÷)   	#0.7996
oneway.test(°ü½É~»ó´ëÆÀ·©Å·)  	#0.03894
oneway.test(°ü½É~¿À½É³í¶õ)  	#0.2389
oneway.test(°¨¼ºÁö¼ö~½ÂÆĞ)  	#0.2482
oneway.test(°¨¼ºÁö¼ö~Á¡¼öÂ÷) 	#0.713
oneway.test(°¨¼ºÁö¼ö~»ó´ëÆÀ·©Å·)	#0.1067
oneway.test(°¨¼ºÁö¼ö~¿À½É³í¶õ)  	#0.2645

tt <- table(½ÂÆĞ, Á¡¼öÂ÷); chisq.test(tt)  		#3.648e-06 > »óÈ£ÀÛ¿ëÈ¿°ú ÀÖÀ½
tt <- table(½ÂÆĞ, »ó´ëÆÀ·©Å·); chisq.test(tt)  	#0.000316 > »óÈ£ÀÛ¿ëÈ¿°ú ÀÖÀ½
tt <- table(½ÂÆĞ, ¿À½É³í¶õ); chisq.test(tt)  	#0.6965
tt <- table(Á¡¼öÂ÷, »ó´ëÆÀ·©Å·); chisq.test(tt)  	#0.8898
tt <- table(Á¡¼öÂ÷, ¿À½É³í¶õ); chisq.test(tt)  	#0.7981
tt <- table(»ó´ëÆÀ·©Å·, ¿À½É³í¶õ); chisq.test(tt)   #0.5374

lm(°ü½É~½ÂÆĞ+Á¡¼öÂ÷+»ó´ëÆÀ·©Å·)
summary(lm(°ü½É~½ÂÆĞ+Á¡¼öÂ÷+»ó´ëÆÀ·©Å·))
lm(°¨¼ºÁö¼ö~½ÂÆĞ+Á¡¼öÂ÷+»ó´ëÆÀ·©Å·)
summary(lm(°¨¼ºÁö¼ö~½ÂÆĞ+Á¡¼öÂ÷+»ó´ëÆÀ·©Å·))
lm.result <- lm(°¨¼ºÁö¼ö~°ü½É)
summary(lm.result)
plot(°ü½É, °¨¼ºÁö¼ö); abline(lm.result)
detach(word)



