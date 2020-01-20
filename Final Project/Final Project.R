#������Ʈ �ڵ�

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

#1. ���� ���� ����
filepath = "D:/Twitter_201402/20140210170000-20140210180000.txt"
json_data = lapply(readLines(filepath1, encoding = "UTF-8"), fromJSON)  #���پ� �о� json ������ ����

only_text = c()
for(i in 1:length(json_data)){  #�ؽ�Ʈ ������ ����
  text = json_data[[i]]$text
  only_text[i] = enc2native(text)
}
#length_text = length(only_text)  #�ؽ�Ʈ ��

#(1)���� �𵨸�
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
    token = gsub("[^��-�R]", "", candi_noun[j])
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
#print(only_noun[1])  #Ȯ��

noun_corpus <- Corpus(VectorSource(only_noun)) 
inspect(noun_corpus)
matrix <- TermDocumentMatrix(noun_corpus)

#my_lda3 = LDA(matrix, 3)
#my_lda5 = LDA(matrix, 5)
my_lda10 = LDA(matrix, 10)  #�̿�
my_lda = my_lda10

my_topics <- tidy(my_lda, matrix = "beta")
my_topics[,2] = matrix$dimnames$Terms[as.integer(my_topics$term)]
my_topics

my_top_terms <- my_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#print(my_top_terms[my_top_terms$topic==1,])  #Ȯ��

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
#my_beta_spread  #Ȯ��

#(2)Ű���� �󵵼� ��
subject_text = c()  #���� ���� �ؽ�Ʈ ����
subject_text = only_text[grep("��ȫö|���ֿ���", only_text)]
subject_text = only_text[grep("������|����", only_text)]
subject_text = only_text[grep("�迬��|�ø���", only_text)]
subject_text = only_text[grep("��Ʈ���ڹ�", only_text)]

length_sub = length(subject_text)  #Ű���� ���� ������ ��
length_sub/length_text  #Ű���� ���� ������ ����

#(3)subject_text�� LDA ȥ�⵵ ��

only_noun2 = c()  #subject_text�� ���ο� Topic Modeling ����
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
    token2 = gsub("[^��-�R]", "", candi_noun2[j])
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
#print(only_noun2[1])  #Ȯ��

noun_corpus2 <- Corpus(VectorSource(only_noun2)) 
inspect(noun_corpus2)
matrix2 <- TermDocumentMatrix(noun_corpus2)
my_lda10 = LDA(matrix2, 10)
perplexity(my_lda10)  #ȥ�⵵ ���

#3. �м� ���μ���

#(1)������ ��ó��
#�� ����
#�� ����
#�� û��

#�ø���
setwd("D:/Twitter_201402/")
filepath = "20140224.txt"
json_data = lapply(readLines(filepath, encoding = "UTF-8"), fromJSON)
 
only_text = c()
for(i in 1:length(json_data)){  #�ؽ�Ʈ ������ ����
  text = json_data[[i]]$text
  only_text[i] = enc2native(text)
}
#length(only_text)  #�ؽ�Ʈ ��

subject_text = c()  #���� ���� �ؽ�Ʈ ����
subject_text = only_text[grep("�ø���|�ݸ޴�|���޴�|���޴�|
��ƮƮ��|���ǵ彺������|�ǰ�|��������|��Ű����|�ø�|�迬��|�̻�ȭ|�̽���|����|����", only_text)]
#length(subject_text)  #Ű���� ���� ������ ��
write.table(subject_text, "D:/�ø���/20140224.txt")  #�м��� ���� �ؽ�Ʈ ������ ����

#������
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
subject_text = only_text[grep("������|�౸|�¸�|���º�|�й�|
�����|��ŷ|�⼺��|�����|����ö|���ֿ�|��û��|������|����|����", only_text)]
#length(subject_text)
write.table(subject_text, "D:/������/������ �Ⱓ/20140701.txt")

#���� Ŭ����� Ȯ��
data <- readLines("D:/�ø���/20140221.txt")
#data <- readLines("D:/������/20140623.txt")
data <- sapply(data, extractNoun, USE.NAMES = F)
data_unlist <- unlist(data)
data_unlist = gsub(" ","",data_unlist)
data_unlist = gsub("[^��-�R]","",data_unlist)
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

#�� ����

#(2)��纰 ���� �м�
#��¥�� length(subject_text)/length(only_text)�� ������ �����ͼ� �̿�
dfr1 <- read.csv("C:/Users/��Ȳ��/Desktop/sns �ڷ�/�ø���1.csv")
#head(dfr1)  #Ȯ��
barplot(dfr1$����, names.arg=dfr1$��¥, 
main="�ø��� ��翡 ���� ����", xlab="��¥", ylab="�����")
abline(h=mean(dfr1$����), col="red", lty=3)

dfr2 <- read.csv("C:/Users/��Ȳ��/Desktop/sns �ڷ�/������1.csv")
barplot(dfr2$����, names.arg=dfr2$��¥, 
main="������ ��翡 ���� ����", xlab="��¥", ylab="�����")
abline(h=mean(dfr2$����), col="red", lty=3)

dfr3 <- read.csv("C:/Users/��Ȳ��/Desktop/sns �ڷ�/�౸1.csv")
barplot(dfr3$����, names.arg=dfr3$��¥, 
main="�౸ ��⿡ ���� ����", xlab="��¥", ylab="�����")
abline(h=mean(dfr3$����), col="red", lty=3)

#(2)��纰 �������� �м�
text = readLines("D:/�ø���/20140212.txt")
data = sapply(text, extractNoun, USE.NAMES = F)
word = unlist(data)
word = gsub(" ","",word)
word = gsub("[^��-�R]", "", word)
word = Filter(function(x){nchar(x)>=2}, word)
write.table(word, "D:/�ø���/word.txt")

word = read.table("D:/�ø���/word.txt")
wordcount = table(word)
top = sort(wordcount, decreasing = T)
top10 = head(top, 10)
#top10  #Ȯ��
top200 = head(top, 200)
write.table(top200, "D:/�ø���/top200.txt")

pos.dic = readLines("C:/Users/��Ȳ��/Desktop/sns �ڷ�/pos.txt")
neg.dic = readLines("C:/Users/��Ȳ��/Desktop/sns �ڷ�/neg.txt")
#head(pos.dic)  #Ȯ��
#head(neg.dic)  #Ȯ��
#length(pos.dic)  #Ȯ��
#length(neg.dic)  #Ȯ��

#text = readLines("D:/�ø���/20140224.txt")
text = readLines("D:/������/20140627.txt")

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
c(count.pos, count.neg)  #��/���� ���� Ȯ��
count.pos-count.neg  #�������� ����

#�������� ǥ��ȭ ����
olym <- read.csv("C:/Users/��Ȳ��/Desktop/sns �ڷ�/�ø���2(����).csv")
senti <- olym$��������
senti.temp <- scale(senti)
olym$�������� <- senti.temp[,1]
#olym$��������  #Ȯ��
ggplot(olym, aes(x=olym$��¥, y=olym$��������))+geom_point()+geom_line()

word <- read.csv("C:/Users/��Ȳ��/Desktop/sns �ڷ�/�౸2(����).csv")
senti2 <- word$��������
senti.temp2 <- scale(senti2)
word$�������� <- senti.temp2[,1]
#word$��������  #Ȯ��
ggplot(word, aes(x=word$��¥, y=word$��������))+geom_point()+geom_line()

#(3)�ø��� �ٺ��� �м�
head(olym)
attach(olym)
oneway.test(����~�޴�) 	  	#p-value=0.7527 > alpha=0.1, H0�ⰢX(������ ���� ���� ����)
oneway.test(����~�����α�)   	#0.05285
oneway.test(����~����)  	  	#0.5452
oneway.test(����~���ɳ���)  	#0.001714
oneway.test(��������~�޴�)  	#0.9669
oneway.test(��������~�����α�) 	#0.09836
oneway.test(��������~����)  	#0.6268
oneway.test(��������~���ɳ���)  	#0.02446

tt <- table(�����α�, ���ɳ���)
chisq.test(tt)

lm(����~�����α�+���ɳ���)
summary(lm(����~�����α�+���ɳ���))
lm(��������~�����α�+���ɳ���)
summary(lm(��������~�����α�+���ɳ���))
lm.result <- lm(��������~����)
summary(lm.result)
plot(����, ��������); abline(lm.result)
detach(olym)

#(4)������(�౸ ���) �ٺ��� �м�
head(word)
attach(word)
oneway.test(����~����) 	  	#p-value=0.1628 > alpha=0.1, H0�ⰢX(������ ���� ���� ����)
oneway.test(����~������)   	#0.7996
oneway.test(����~�������ŷ)  	#0.03894
oneway.test(����~���ɳ���)  	#0.2389
oneway.test(��������~����)  	#0.2482
oneway.test(��������~������) 	#0.713
oneway.test(��������~�������ŷ)	#0.1067
oneway.test(��������~���ɳ���)  	#0.2645

tt <- table(����, ������); chisq.test(tt)  		#3.648e-06 > ��ȣ�ۿ�ȿ�� ����
tt <- table(����, �������ŷ); chisq.test(tt)  	#0.000316 > ��ȣ�ۿ�ȿ�� ����
tt <- table(����, ���ɳ���); chisq.test(tt)  	#0.6965
tt <- table(������, �������ŷ); chisq.test(tt)  	#0.8898
tt <- table(������, ���ɳ���); chisq.test(tt)  	#0.7981
tt <- table(�������ŷ, ���ɳ���); chisq.test(tt)   #0.5374

lm(����~����+������+�������ŷ)
summary(lm(����~����+������+�������ŷ))
lm(��������~����+������+�������ŷ)
summary(lm(��������~����+������+�������ŷ))
lm.result <- lm(��������~����)
summary(lm.result)
plot(����, ��������); abline(lm.result)
detach(word)


