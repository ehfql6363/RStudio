library(dplyr)
library(readr)

setwd("C:/Users/ehfql/Desktop/대학교/4학년-1/빅데이터 처리/강의 자료/12주차")

dic <- read_csv("knu_sentiment_lexicon.csv")

#긍정 단어
dic %>%
  filter(polarity == 2) %>%
  arrange(word)

#부정 단어
dic %>%
  filter(polarity == -2) %>%
  arrange(word)

#중성 단어
dic %>%
  filter(polarity == 0) %>%
  arrange(word)

#긍정 부정 중성의 빈도수
dic$sentiment <- ifelse(dic$polarity >0, "긍정",
                        ifelse(dic$polarity<0,"부정", "중성"))
dic %>%
  group_by(sentiment) %>%
  summarise(n=n())



library(tidytext)
#간단한 구조로 감성분석
df <- tibble(sentence = c("디자인 예쁘고 마감도 좋아서 만족스럽다.", "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다."))
#토큰화(단어기준)
df <- df %>%
  unnest_tokens(input = sentence,
                output = word,
                token = "words",
                drop = F) #원문 보존을 위한 옵션

#감정사전 이용하여 단어의 감정 점수 매기기
df <- left_join(df,dic,by="word")

df$polarity <- ifelse(is.na(df$polarity), 0, df$polarity)

#각 문장별 감정 점수 합산
score_df <- df %>%
  group_by(sentence) %>%
  summarise(score = sum(polarity))

score_df


###기생충 아카데미 수상소식 관련 댓글 분석###
#1. 데이터 불러오기
install.packages("textclean")
library(dplyr)
library(stringr)
library(textclean)

raw_news_comment <- read_csv("news_comment_parasite.csv")

#2. 데이터 전처리
#고유번호 변수 만들기 : 댓글 내용 같아도 구별 가능하게
raw_news_comment <- mutate(raw_news_comment, id = row_number()) #row_number() : 행번호 생성 함수
raw_news_comment <- raw_news_comment[,c(6,1,2,3,4,5)] #id 변수를 첫 번째 자리로 이동

#HTML 특수문자 제거
news_comment <- raw_news_comment %>% mutate(reply = replace_html(reply))

#중복 공백 제거
news_comment$reply <- str_squish(news_comment$reply)

#특수문자와 두글자 미만 단어 포함.
#단어 기준으로 토큰화하고 원문 그대로 포함.
word_comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token =  "words",
                drop = F)

#감정점수 부여 
word_comment <- word_comment %>% left_join(dic, by = "word") #조인
word_comment$polarity <- ifelse(is.na(word_comment$polarity), 0, word_comment$polarity)

#자주 사용된 감정 단어 살펴보기
word_comment$sentiment <- ifelse(word_comment$polarity == 2, "긍정",
                                 ifelse(word_comment$polarity == -2, "부정", "중성"))
#긍정, 부정, 중성 건수 확인
table(word_comment$sentiment)

#긍정과 부정의 단어 그룹화
grp_word_comment <- word_comment %>% group_by(sentiment, word) %>%
  summarise(n=n())

#긍정과 부정의 상위 10개의 단어를 막대그래프로 그리기
top10_sentimen <- grp_word_comment %>%
  filter(sentiment != "중성") %>%
  slice_max(n, n = 10) #sclice_max() : 상위 n개의 행을 추출해 내림차순으로 정렬해주는 함수

library(ggplot2)
ggplot(top10_sentimen, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free")

#긍정의 문장과 부정의 문장 정도 확인
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity))

#감정 분류 및 막대 그래프
score_comment$sentiment <- ifelse(score_comment$score > 0, "긍정",
                                  ifelse(score_comment$score == 0, "중성", "부정"))
#sentiment 변 건수를 구한 후, 막대그래프 그래기
sen_score <- score_comment %>%
  group_by(sentiment) %>% summarise(n = n())

ggplot(sen_score, aes(x = sentiment, y = n, fill = sentiment)) + 
  geom_col() + geom_text(aes(label = n), vjust = 0.3) + 
  scale_x_discrete(limits = c("긍정", "부정", "중성"))


###의미망 분석 - 동시 출현 단어 분석###
#1. 데이터 셋 불러오기
library(readr)
news_comment <- read.csv("news_comment.csv")

library(dplyr)
library(stringr)
library(textclean)
library(tidytext)
library(KoNLP)
library(tidyr)

#토큰화 (품사별로)
comment_pos <- news_comment %>%
  unnest_tokens(input = reply,
                output = word, 
                token = SimplePos22,
                drop = F)

#필요 변수만 (word, reply)
comment_pos <- comment_pos[, -c(1)]

comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>% #품사 분리
  filter(str_detect(word, "/n|/pv|/pa")) %>% #명사, 형용사, 동사 추출
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"), #동사는 /또는 *$로 시작하는태그 자리에 "다"로 바꿈
                       str_remove(word, "/.*$"))) %>% # 태그 없애기
  filter(str_count(word) >= 2) %>% #단어는 두 글자 이상인 것만 가져오기
  arrange(id) #id 별로 정렬렬

#단어 동시 출현 빈도 구하기
install.packages("widyr")
library(widyr)

#pairwise-count()
pair <- comment_new %>%
  pairwise_count(item = word, feature = id, sort = T)

#네트워크 그래프 데이터 만들기
install.packages("tidygraph")
library(tidygraph)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

graph_comment

#네트워크 그래프 만들기
install.packages("ggraph")
library(ggraph)
ggraph(graph_comment) + 
  geom_edge_link() + #엣지
  geom_node_point() + #노드
  geom_node_text(aes(label = name)) #텍스트트

#그래프 다듬기
set.seed(1234)
ggraph(graph_comment, layout = "fr") + #레이아웃
  geom_edge_link(color = "skyblue", #엣지 색
                 alpha = 0.5) + #엣지 명암
  geom_node_point(color = "blue", #노드 색
                  size = 5) + #노드 크기
  geom_node_text(aes(label = name), #텍스트 
                 repel = T, #노드 밖 표시
                 size = 5) + #텍스트 크기
  theme_graph() #배경 삭제제


#동시출현 네트워크 데이터
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), #연결 중심성
         group = as.factor(group_infomap())) #커뮤니티

graph_comment




