#n-gram으로 토큰화
text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
#바이그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 2)
#트라이그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 3)

setwd("C:/Users/ehfql/Desktop/대학교/4학년-1/빅데이터 처리/강의 자료/12주차")
news_comment <- read.csv("news_comment.csv")
##12주차의 comment_new##
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

##13주차 n-gram##
line_comment <- comment_new %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

#바이그램으로 토큰화
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment

#연이어 사용된 단어쌍 살펴보기/네트워크 데이터 만들기
#1. 바이그램 분리
library(tidyr)
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_seprated

#2. 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()
pair_bigram

#네트워크는 연이어 나온 단어의 빈도수가 8이상인 것만 대상
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph()

#네트워크 그리기
set.seed(1234)
ggraph(graph_bigram, layout = "fr") + #레이아웃
  geom_edge_link(color = "skyblue", #엣지 색
                 alpha = 0.5) + #엣지 명암
  geom_node_point(color = "blue", #노드 색
                  size = 5) + #노드 크기
  geom_node_text(aes(label = name), #텍스트 
                 repel = T, #노드 밖 표시
                 size = 5) + #텍스트 크기
  theme_graph() #배경 삭제제

#유의어 처리 ex) 짝짝, 대단 자랑 등등
bigram_seprated <- bigram_seprated %>% #유의어 처리
  mutate(word1 = ifelse(str_detect(word1, "대단"), "대단", word1),
         word2 = ifelse(str_detect(word2, "대단"),"대단", word2),
         word1 = ifelse(str_detect(word1, "자랑"), "자랑", word1),
         word2 = ifelse(str_detect(word2, "자랑"),"자랑", word2),
         word1 = ifelse(str_detect(word1, "짝짝짝"), "짝짝짝", word1),
         word2 = ifelse(str_detect(word2, "짝짝짝"),"짝짝짝", word2)) %>%
  filter(word1 != word2) #같은 단어 연속 제거거

#연이은 단어쌍 분석 - ngram
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()
#빈도수가 8이상인 경우만으로 네트워크 데이터 만들기, 중심성과 커뮤니티 구하기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), #중심성
         group = as.factor(group_infomap())) #커뮤니티
graph_bigram

#그래프
ggraph(graph_bigram, layout = "fr") + #레이아웃
  geom_edge_link(color = "gray50", #edge 색
                 alpha = 0.5) + #edge 명암
  geom_node_point(aes(size = centrality, #노드 크기
                      color = group), #노드 색
                  show.legend = F)+ #범례 삭제
  scale_size(range = c(5,10)) +  #노드 크기 범위
  geom_node_text(aes(label = name), #텍스트 표시
                 repel = T, #노드 밖 표시
                 size = 5) + #텍스트 표시
  theme_graph() #배경 삭제



