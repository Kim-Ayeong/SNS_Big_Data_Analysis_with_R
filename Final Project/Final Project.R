#프로젝트 코드

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

#1. 주제 선정 과정
filepath = "D:/Twitter_201402/20140210170000-20140210180000.txt"
json_data = lapply(readLines(filepath1, encoding = "UTF-8"), fromJSON)  #한줄씩 읽어 json 데이터 생성

only_text = c()
for(i in 1:length(json_data)){  #텍스트 데이터 추출
  text = json_data[[i]]$text
  only_text[i] = enc2native(text)
}
#length_text = length(only_text)  #텍스트 수

#(1)토픽 모델링
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
    token = gsub("[^ㄱ-힣]", "", candi_noun[j])
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
#print(only_noun[1])  #확인

noun_corpus <- Corpus(VectorSource(only_noun)) 
inspect(noun_corpus)
matrix <- TermDocumentMatrix(noun_corpus)

#my_lda3 = LDA(matrix, 3)
#my_lda5 = LDA(matrix, 5)
my_lda10 = LDA(matrix, 10)  #이용
my_lda = my_lda10

my_topics <- tidy(my_lda, matrix = "beta")
my_topics[,2] = matrix$dimnames$Terms[as.integer(my_topics$term)]
my_topics

my_top_terms <- my_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#print(my_top_terms[my_top_terms$topic==1,])  #확인

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
#my_beta_spread  #확인

#(2)키워드 빈도수 비교
subject_text = c()  #주제 관련 텍스트 추출
subject_text = only_text[grep("노홍철|음주운전", only_text)]
subject_text = only_text[grep("간통죄|폐지", only_text)]
subject_text = only_text[grep("김연아|올림픽", only_text)]
subject_text = only_text[grep("소트니코바", only_text)]

length_sub = length(subject_text)  #키워드 포함 데이터 수
length_sub/length_text  #키워드 포함 데이터 비율

#(3)subject_text의 LDA 혼잡도 비교
only_noun2 = c()  #subject_text로 새로운 Topic Modeling 생성
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
    token2 = gsub("[^ㄱ-힣]", "", candi_noun2[j])
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
#print(only_noun2[1])  #확인

noun_corpus2 <- Corpus(VectorSource(only_noun2)) 
inspect(noun_corpus2)
matrix2 <- TermDocumentMatrix(noun_corpus2)
my_lda10 = LDA(matrix2, 10)
perplexity(my_lda10)  #혼잡도 출력

#3. 분석 프로세스

#(1)데이터 전처리
#올림픽
setwd("D:/Twitter_201402/")
filepath = "20140224.txt"
json_data = lapply(readLines(filepath, encoding = "UTF-8"), fromJSON)
 
only_text = c()
for(i in 1:length(json_data)){  #텍스트 데이터 추출
  text = json_data[[i]]$text
  only_text[i] = enc2native(text)
}
#length(only_text)  #텍스트 수

subject_text = c()  #주제 관련 텍스트 추출
subject_text = only_text[grep("올림픽|금메달|은메달|동메달|
쇼트트랙|스피드스케이팅|피겨|봅슬레이|스키점프|컬링|김연아|이상화|이승훈|심판|오심", only_text)]
#length(subject_text)  #키워드 포함 데이터 수
write.table(subject_text, "D:/올림픽/20140224.txt")  #분석을 위한 텍스트 데이터 저장

#월드컵
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
subject_text = only_text[grep("월드컵|축구|승리|무승부|패배|
상대팀|랭킹|기성용|손흥민|구자철|박주영|이청용|정성룡|심판|오심", only_text)]
#length(subject_text)
write.table(subject_text, "D:/월드컵/월드컵 기간/20140701.txt")

#워드 클라우드로 확인
data <- readLines("D:/올림픽/20140221.txt")
#data <- readLines("D:/월드컵/20140623.txt")
data <- sapply(data, extractNoun, USE.NAMES = F)
data_unlist <- unlist(data)
data_unlist = gsub(" ","",data_unlist)
data_unlist = gsub("[^ㄱ-힣]","",data_unlist)
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

#(2)행사별 관심 분석
#날짜별 length(subject_text)/length(only_text)를 추출한 데이터셋 이용
dfr1 <- read.csv("C:/Users/김황규/Desktop/sns 자료/올림픽1.csv")
#head(dfr1)  #확인
barplot(dfr1$관심, names.arg=dfr1$날짜, 
main="올림픽 행사에 대한 관심", xlab="날짜", ylab="백분율")
abline(h=mean(dfr1$관심), col="red", lty=3)

dfr2 <- read.csv("C:/Users/김황규/Desktop/sns 자료/월드컵1.csv")
barplot(dfr2$관심, names.arg=dfr2$날짜, 
main="월드컵 행사에 대한 관심", xlab="날짜", ylab="백분율")
abline(h=mean(dfr2$관심), col="red", lty=3)

dfr3 <- read.csv("C:/Users/김황규/Desktop/sns 자료/축구1.csv")
barplot(dfr3$관심, names.arg=dfr3$날짜, 
main="축구 경기에 대한 관심", xlab="날짜", ylab="백분율")
abline(h=mean(dfr3$관심), col="red", lty=3)

#(2)행사별 감성지수 분석
text = readLines("D:/올림픽/20140212.txt")
data = sapply(text, extractNoun, USE.NAMES = F)
word = unlist(data)
word = gsub(" ","",word)
word = gsub("[^ㄱ-힣]", "", word)
word = Filter(function(x){nchar(x)>=2}, word)
write.table(word, "D:/올림픽/word.txt")

word = read.table("D:/올림픽/word.txt")
wordcount = table(word)
top = sort(wordcount, decreasing = T)
top10 = head(top, 10)
#top10  #확인
top200 = head(top, 200)
write.table(top200, "D:/올림픽/top200.txt")

pos.dic = readLines("C:/Users/김황규/Desktop/sns 자료/pos.txt")
neg.dic = readLines("C:/Users/김황규/Desktop/sns 자료/neg.txt")
#head(pos.dic)  #확인
#head(neg.dic)  #확인
#length(pos.dic)  #확인
#length(neg.dic)  #확인

#text = readLines("D:/올림픽/20140224.txt")
text = readLines("D:/월드컵/20140627.txt")

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
c(count.pos, count.neg)  #긍/부정 지수 확인
count.pos-count.neg  #감성지수 생성

#감성지수 표준화 과정
olym <- read.csv("C:/Users/김황규/Desktop/sns 자료/올림픽2(최종).csv")
senti <- olym$감성지수
senti.temp <- scale(senti)
olym$감성지수 <- senti.temp[,1]
#olym$감성지수  #확인
ggplot(olym, aes(x=olym$날짜, y=olym$감성지수))+geom_point()+geom_line()

word <- read.csv("C:/Users/김황규/Desktop/sns 자료/축구2(최종).csv")
senti2 <- word$감성지수
senti.temp2 <- scale(senti2)
word$감성지수 <- senti.temp2[,1]
#word$감성지수  #확인
ggplot(word, aes(x=word$날짜, y=word$감성지수))+geom_point()+geom_line()

#(3)올림픽 다변수 분석
head(olym)
attach(olym)
oneway.test(관심~메달) 	  	#p-value=0.7527 > alpha=0.1, H0기각X(변수에 따른 차이 없음)
oneway.test(관심~종목인기)   	#0.05285
oneway.test(관심~남녀)  	  	#0.5452
oneway.test(관심~오심논란)  	#0.001714
oneway.test(감성지수~메달)  	#0.9669
oneway.test(감성지수~종목인기) 	#0.09836
oneway.test(감성지수~남녀)  	#0.6268
oneway.test(감성지수~오심논란)  	#0.02446

tt <- table(종목인기, 오심논란)
chisq.test(tt)

lm(관심~종목인기+오심논란)
summary(lm(관심~종목인기+오심논란))
lm(감성지수~종목인기+오심논란)
summary(lm(감성지수~종목인기+오심논란))
lm.result <- lm(감성지수~관심)
summary(lm.result)
plot(관심, 감성지수); abline(lm.result)
detach(olym)

#(4)월드컵(축구 경기) 다변수 분석
head(word)
attach(word)
oneway.test(관심~승패) 	  	#p-value=0.1628 > alpha=0.1, H0기각X(변수에 따른 차이 없음)
oneway.test(관심~점수차)   	#0.7996
oneway.test(관심~상대팀랭킹)  	#0.03894
oneway.test(관심~오심논란)  	#0.2389
oneway.test(감성지수~승패)  	#0.2482
oneway.test(감성지수~점수차) 	#0.713
oneway.test(감성지수~상대팀랭킹)	#0.1067
oneway.test(감성지수~오심논란)  	#0.2645

tt <- table(승패, 점수차); chisq.test(tt)  		#3.648e-06 > 상호작용효과 있음
tt <- table(승패, 상대팀랭킹); chisq.test(tt)  	#0.000316 > 상호작용효과 있음
tt <- table(승패, 오심논란); chisq.test(tt)  	#0.6965
tt <- table(점수차, 상대팀랭킹); chisq.test(tt)  	#0.8898
tt <- table(점수차, 오심논란); chisq.test(tt)  	#0.7981
tt <- table(상대팀랭킹, 오심논란); chisq.test(tt)   #0.5374

lm(관심~승패+점수차+상대팀랭킹)
summary(lm(관심~승패+점수차+상대팀랭킹))
lm(감성지수~승패+점수차+상대팀랭킹)
summary(lm(감성지수~승패+점수차+상대팀랭킹))
lm.result <- lm(감성지수~관심)
summary(lm.result)
plot(관심, 감성지수); abline(lm.result)
detach(word)



