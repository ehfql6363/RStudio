install.packages("stringr")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("RColorBrewer")
install.packages("multilinguer")
install.packages("hash")
install.packages("tau")
install.packages("Sejong")
install.packages("RSQLite")
install.packages("devtools")
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))
install.packages("readr")
library(stringr)
library(tidytext)
library(dplyr)
library(wordcloud2)
library(RColorBrewer)
library(multilinguer)
library(hash)
library(tau)
library(Sejong)
library(RSQLite)
library(devtools)
library(KoNLP)
library(readr)

setwd("C:/Users/ehfql/Desktop/대학교/4학년-1/빅데이터 처리/강의 자료/11주차")
Yoon <- readLines("Speech_Yoon.txt", encoding = "UTF-8")

#불필요한 문자 제거
#str_replace_all()
str_replace_all(string = Yoon, pattern = "[^가-힣]", replacement = " ")
#string : 처리할 텍스트
#pattern : 규칙 ([가-힣] : 가부터 힣까지의 모든 한글 + ^ : 반대)
#replacement : 바꿀 문자
Yoon1 <- Yoon %>%
  str_replace_all("[^가-힣]", " ")

#Yoon1에 있는 연속된 공백 제거
Yoon1 <- Yoon1 %>%
  str_squish()

#문자열 벡터를 tibble 구조로 바꾸기 -as.tibble()
Yoon1 <- as_tibble(Yoon1)

#연속
#Yoon1 <- Yoon %>%
#str_replace_all("[^가-힣]", " ") %>%
#str_squish() %>%
#as_tibble()

#토큰화
#문장 기준
# text %>%
#   unnest_tokens(input = value, 
#                 output = word, 
#                 token = "sentences")
text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text
text %>%
  unnest_tokens(input = value,
                out = word,
                token = "words") #characters : 문자 기준

#연설문 토큰화
word_space <- Yoon1 %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")
word_space

#빈도수 알아보기
word_space <- word_space %>%
  count(word, sort = T) # sort = T : 빈도수가 높은 순으로 단어 정렬
word_space

#한 글자만 있는 단어 지우기
word_space <- word_space %>%
  filter(str_count(word) > 1)

#한번에 작업 : 빈도 내림차순 정렬 후 두 글자 이상 단어 남기기
# word_space <- word_space %>%
#   count(word, sort = T) %>%
#   filter(str_count(word)>1)

#자주 사용되는 단어 추춘 top20
top20 <- word_space %>%
  head(20)
top20

#막대그래프 그리기
ggplot(top20, aes(x = reorder(word, n), y = n)) +  #단어 빈도순
  geom_col() + 
  coord_flip() #축 회전


#워드 클라우드
top50 <- word_space %>%
  head(50)
wordcloud2(top50, color = "random-light", shape = "circle")

#KoNLP 한글 형태소 분석
#형태소 사전 설정
useNIADic() #한 번만 실행

#형태소 분석기를 이용해 토큰화하기 - 명사 추출 : extractNoun() => 어근 추출
word_noun <- Yoon1 %>%
  unnest_tokens(input = value,
                output = word, 
                token = extractNoun)
word_noun

#단어 빈도수 구하기
word_noun <- word_noun %>%
  count(word, sort=T) %>%
  filter(str_count(word)>1)
word_noun

#자주 사용되는 단어 추출
top20_noun <- word_noun %>%
  head(20)
#그래프
ggplot(top20_noun, aes(x = reorder(word, n), y = n)) + 
  geom_col() + coord_flip()

#word_noun의 wordcloud2
top50_noun <- word_noun %>%
  head(50)
wordcloud2(top50_noun, color = "random-light", shape = "ractangle")


#TF-IDF
raw_speeches <- read_csv("speeches_presidents.csv")
raw_speeches

#기본적인 전처리
speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

#토큰화
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

#단어 빈도 구하기
frequency <- speeches %>%
  count(president, word) %>%
  filter(str_count(word)>1)
frequency

#TF-IDF 구하기
frequency <- frequency %>%
  bind_tf_idf(term = word, #단어
              document = president, #텍스트 구분 기준
              n = n) %>% #단어 빈도
  arrange(-tf_idf)
frequency

frequency %>% filter(president == "이명박")
frequency %>% filter(president == "노무현")
frequency %>% filter(president == "박근혜")
frequency %>% filter(president == "문재인")

#주요 단어 추출하여 막대그래프로 시각화
top10 <- frequency %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

top10 %>% print(n=Inf)

ggplot(top10, aes(x=reorder_within(word, tf_idf, president), y=tf_idf)) + geom_col() + #막대 그래프
  coord_flip() + #x축과 y축 회전
  geom_text(aes(label = n), hjust = -0.3) + #각각의 막대 옆에 정확한 숫자(빈도) 출력
  facet_wrap(~president, scales = "free") + #어느 대통령이 말한 단어인지 구분
  scale_x_reordered() + 
  labs(x = NULL)




