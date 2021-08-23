library(tidyverse)
library(KoNLP)
library(showtext)
library(extrafont)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(ggthemes)
library(rgdal)

#--------------------------------------
# 데이터 불러오기 및 폰트 설정
#--------------------------------------

font_add_google("Noto Sans KR", "Noto Sans KR")
showtext_auto() 
manifesto <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSuYtw6eacgV8V4jv8FripM3QTVAru_UEfKtDNnv6AglATrI-KUKTQgXKBRgCQhjdKWZY6zZ51fAo6Z/pub?gid=0&single=true&output=csv")

#--------------------------------------
# 공약 카테고리 라벨링
#--------------------------------------

manifesto <- manifesto %>% mutate("category" = case_when(
  str_detect(공약,"대학") ~ "교육",
  str_detect(공약,"행정") ~ "행정",
  str_detect(공약,"인권") ~ "인권/복지",
  str_detect(공약,"3D") ~ "산업",
  str_detect(공약,"클러스터") ~ "산업",
  str_detect(공약,"R&D") ~ "산업",
  str_detect(공약,"첨단") ~ "산업",
  str_detect(공약,"안전") ~ "안전",
  str_detect(공약,"경찰") ~ "안전",
  str_detect(공약,"수해") ~ "안전",
  str_detect(공약,"교통") ~ "교통",
  str_detect(공약,"도로") ~ "교통",
  str_detect(공약,"철도") ~ "교통",
  str_detect(공약,"국도") ~ "교통",
  str_detect(공약,"허리경제권") ~ "교통",
  str_detect(공약,"회덕 IC") ~ "교통",
  str_detect(공약,"전철") ~ "교통",
  str_detect(공약,"대교") ~ "교통",
  str_detect(공약,"호선") ~ "교통",
  str_detect(공약,"노선") ~ "교통",
  str_detect(공약,"공항") ~ "교통",
  str_detect(공약,"KTX") ~ "교통",
  str_detect(공약,"GTX") ~ "교통",
  str_detect(공약,"환승") ~ "교통",
  str_detect(공약,"내륙선") ~ "교통",
  str_detect(공약,"전주역사") ~ "교통",
  str_detect(공약,"신안산선") ~ "교통",
  str_detect(공약,"무상") ~ "인권/복지",
  str_detect(공약,"관광") ~ "관광/문화",
  str_detect(공약,"리조트") ~ "관광/문화",
  str_detect(공약,"레포츠") ~ "관광/문화",
  str_detect(공약,"박물관") ~ "관광/문화",
  str_detect(공약,"한류") ~ "관광/문화",
  str_detect(공약,"문화") ~ "관광/문화",
  str_detect(공약,"익산 르네상스") ~ "관광/문화",
  str_detect(공약,"고도 르네상스") ~ "관광/문화",
  str_detect(공약,"올림픽") ~ "국제행사 지원",
  str_detect(공약,"람사르") ~ "국제행사 지원",
  str_detect(공약,"엑스포") ~ "국제행사 지원",
  str_detect(공약,"박람회") ~ "국제행사 지원",
  str_detect(공약,"아시안게임") ~ "국제행사 지원",
  str_detect(공약,"잼버리") ~ "국제행사 지원",
  str_detect(공약,"대회") ~ "국제행사 지원",
  str_detect(공약,"미세먼지") ~ "환경",
  str_detect(공약,"환경") ~ "환경",
  str_detect(공약,"공원") ~ "환경",
  str_detect(공약,"캠프페이지") ~ "환경",
  str_detect(공약,"클린") ~ "환경",
  str_detect(공약,"깨끗") ~ "환경",
  str_detect(공약,"자연") ~ "환경",
  str_detect(공약,"생태") ~ "환경",
  str_detect(공약,"수질") ~ "환경",
  str_detect(공약,"하수처리") ~ "환경",
  str_detect(공약,"공해") ~ "환경",
  str_detect(공약,"녹색") ~ "환경",
  str_detect(공약,"화력발전") ~ "환경",
  str_detect(공약,"탄소제로") ~ "환경",
  str_detect(공약,"정원") ~ "환경",
  str_detect(공약,"천연가스") ~ "환경",
  str_detect(공약,"원전 폐쇄") ~ "환경",
  str_detect(공약,"조력발전") ~ "환경",
  str_detect(공약,"교육") ~ "교육",
  str_detect(공약,"도청") ~ "행정",
  str_detect(공약,"집무실") ~ "행정",
  str_detect(공약,"국회") ~ "행정",
  str_detect(공약,"해양수산부") ~ "행정",
  str_detect(공약,"청원 통합") ~ "행정",
  str_detect(공약,"청주 통합") ~ "행정",
  str_detect(공약,"법원") ~ "행정",
  str_detect(공약,"자치") ~ "행정",
  str_detect(공약,"신도시") ~ "행정",
  str_detect(공약,"개성") ~ "남북",
  str_detect(공약,"통일") ~ "남북",
  str_detect(공약,"남북") ~ "남북",
  str_detect(공약,"사드") ~ "남북",
  str_detect(공약,"평화벨트") ~ "남북",
  str_detect(공약,"ICT") ~ "산업",
  str_detect(공약,"LED") ~ "산업",
  str_detect(공약,"전략소재") ~ "산업",
  str_detect(공약,"사물인터넷") ~ "산업",
  str_detect(공약,"벨트") ~ "산업",
  str_detect(공약,"단지") ~ "산업",
  str_detect(공약,"산업") ~ "산업",
  str_detect(공약,"산단") ~ "산업",
  str_detect(공약,"공단") ~ "산업",
  str_detect(공약,"자동차") ~ "산업",
  str_detect(공약,"조선업") ~ "산업",
  str_detect(공약,"일자리") ~ "산업",
  str_detect(공약,"생산") ~ "산업",
  str_detect(공약,"테크") ~ "산업",
  str_detect(공약,"밸리") ~ "산업",
  str_detect(공약,"농업") ~ "산업",
  str_detect(공약,"감귤") ~ "산업",
  str_detect(공약,"수산") ~ "산업",
  str_detect(공약,"발전 플랜트") ~ "산업",
  str_detect(공약,"플랜트 연구") ~ "산업",
  str_detect(공약,"촉진지구") ~ "산업",
  str_detect(공약,"의공학") ~ "산업",
  str_detect(공약,"세월호") ~ "인권/복지",
  str_detect(공약,"트라우마") ~ "인권/복지",
  str_detect(공약,"4.3") ~ "인권/복지",
  str_detect(공약,"고령 친화") ~ "인권/복지",
  str_detect(공약,"보육") ~ "인권/복지",
  str_detect(공약,"복지") ~ "인권/복지",
  str_detect(공약,"화합") ~ "인권/복지",
  str_detect(공약,"병원") ~ "의료",
  str_detect(공약,"의학") ~ "의료",
  str_detect(공약,"의료") ~ "의료",
  str_detect(공약,"메디컬") ~ "의료",
  str_detect(공약,"허브") ~ "산업",
  str_detect(공약,"해경") ~ "안전",
  str_detect(공약,"도심 재생") ~ "도시재생",
  str_detect(공약,"뉴딜") ~ "도시재생",
  str_detect(공약,"역세권") ~ "도시재생",
  str_detect(공약,"도시재생") ~ "도시재생",
  str_detect(공약,"균형 발전") ~ "도시재생",
  str_detect(공약,"특정지역") ~ "도시재생",
  str_detect(공약,"지역 발전") ~ "도시재생",
  str_detect(공약,"특화 개발") ~ "도시재생",
  TRUE ~ "기타"
))


#--------------------------------------
# 전체 키워드 양상 시각화 (by wordcloud)
#--------------------------------------

exNouns <- function(x){
  paste(extractNoun(as.character(x)),collapse = " ")
}

mani_nouns <- sapply(manifesto$공약,exNouns)

myCorpus <- Corpus(VectorSource(mani_nouns))
myCorpus2 <- tm_map(myCorpus,removePunctuation)
myCorpus2 <- tm_map(myCorpus,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))


myCorpus_term <- TermDocumentMatrix(myCorpus2, control = list(wordLengths = c(4, 32)))

term_df <- as.data.frame(as.matrix(myCorpus_term))
dim(term_df)

wordResult <- sort(rowSums(term_df),decreasing = T)

wordResult2 <- as.data.frame(wordResult)

myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult) # 데이터프레임 생성 
str(word.df)
pal <- brewer.pal(7,"Set2")

wordcloud(word.df$word, word.df$freq, scale = c(5, 1), min.freq = 4, random.order = FALSE,
          random.color = FALSE, colors = pal, family = "Noto Sans KR")

#--------------------------------------
# 전체 공약 테마 양상 시각화 (by bar chart)
#--------------------------------------

manifesto %>% 
  group_by(지역) %>% 
  count(category) %>% 
  ggplot(aes(x=category,y=n,group=지역))+geom_col() + coord_flip()+facet_wrap(~지역,ncol=4) +   
  labs(x = NULL, y = NULL, fill = "대",title = "지역별 공약 테마 분포",caption = "17-19대 공약 835개 대상", family = "Noto Sans KR")+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14),
        legend.position = "top",
        legend.key.width= unit(3, 'cm'),
        axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "white"), 
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
        panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
        panel.border = element_blank())

#--------------------------------------
# 산업 분류 라벨링
#--------------------------------------

manifesto %>% group_by(지역) %>% count(대,category)
manifesto %>% group_by(category) %>% count() %>% view()


manifesto <- manifesto %>% 
  mutate(category2 = case_when(
  str_detect(공약,"바이오") ~ "바이오",
  str_detect(공약,"제주형") ~ "바이오",
  str_detect(공약,"생명과학") ~ "바이오",
  str_detect(공약,"미생물") ~ "바이오",
  str_detect(공약,"BT") ~ "바이오",
  str_detect(공약,"감귤") ~ "농수산업",
  str_detect(공약,"수산") ~ "농수산업",
  str_detect(공약,"농업") ~ "농수산업",
  str_detect(공약,"해조류") ~ "농수산업",
  str_detect(공약,"농생명") ~ "농수산업",
  str_detect(공약,"어업") ~ "농수산업",
  str_detect(공약,"농식품") ~ "농수산업",
  str_detect(공약,"우주") ~ "우주항공",
  str_detect(공약,"항공") ~ "우주항공",
  str_detect(공약,"경남의 산업지도를 바꾸는 신성장 동력 산업 육성") ~ "우주항공",
  str_detect(공약,"반도체") ~ "반도체",
  str_detect(공약,"의료허브") ~ "의료산업",
  str_detect(공약,"의약") ~ "의료산업",
  str_detect(공약,"헬스") ~ "의료산업",
  str_detect(공약,"메디컬") ~ "의료산업",
  str_detect(공약,"의공학") ~ "의료산업",
  str_detect(공약,"의생명") ~ "의료산업",
  str_detect(공약,"의·과학") ~ "의료산업",
  str_detect(공약,"의료") ~ "의료산업",
  str_detect(공약,"항노화") ~ "의료산업",
  str_detect(공약,"네이처") ~ "의료산업",
  str_detect(공약,"고령 친화") ~ "의료산업",
  str_detect(공약,"첨단") ~ "첨단산업",
  str_detect(공약,"3D") ~ "기타 4차 산업",
  str_detect(공약,"자동차") ~ "자동차산업",
  str_detect(공약,"자율주행") ~ "자동차산업",
  str_detect(공약,"에너지") ~ "에너지",
  str_detect(공약,"풍력") ~ "에너지",
  str_detect(공약,"로봇") ~ "로봇/인공지능",
  str_detect(공약,"인공지능") ~ "로봇/인공지능",
  str_detect(공약,"관광") ~ "관광산업",
  str_detect(공약,"투어리즘") ~ "관광산업",
  str_detect(공약,"레저") ~ "관광산업",
  str_detect(공약,"고도 르네상스") ~ "관광산업",
  str_detect(공약,"익산 르네상스") ~ "관광산업",
  str_detect(공약,"영상") ~ "문화예술",
  str_detect(공약,"영화") ~ "문화예술",
  str_detect(공약,"문화") ~ "문화예술",
  str_detect(공약,"문화산업") ~ "문화예술",
  str_detect(공약,"콘텐츠") ~ "문화예술",
  str_detect(공약,"물산업") ~ "물산업",
  str_detect(공약,"조선") ~ "조선업",
  str_detect(공약,"해양플랜트") ~ "조선업",
  str_detect(공약,"소재") ~ "신소재",
  str_detect(공약,"4차") ~ "기타 4차 산업",
  TRUE ~ "기타"
))

#--------------------------------------
# 지역별 공약 데이터프레임 생성
#--------------------------------------


manifesto <- manifesto %>% mutate(keyword = extractNoun(공약))


mani_chbk <- manifesto %>% filter(지역 == "충북")
mani_chnm <- manifesto %>% filter(지역 == "충남")
mani_gybk <- manifesto %>% filter(지역 == "경북")
mani_gynm <- manifesto %>% filter(지역 == "경남")
mani_jnbk <- manifesto %>% filter(지역 == "전북")
mani_jnnm <- manifesto %>% filter(지역 == "전남")

mani_jeju <- manifesto %>% filter(지역 == "제주")
mani_gangwon <- manifesto %>% filter(지역 == "강원")
mani_busan <- manifesto %>% filter(지역 == "부산")
mani_ulsan <- manifesto %>% filter(지역 == "울산")
mani_gyung <- manifesto %>% filter(지역 == "경기")
mani_daejeon <- manifesto %>% filter(지역 == "대전")
mani_daegu <- manifesto %>% filter(지역 == "대구")
mani_incheon <- manifesto %>% filter(지역 == "인천")
mani_sejong <- manifesto %>% filter(지역 == "세종")
mani_gwangju <- manifesto %>% filter(지역 == "광주")

#--------------------------------------
# 전라북도
#--------------------------------------

jnbk_nouns <- sapply(mani_jnbk$공약,exNouns)

myCorpus_jnbk <- Corpus(VectorSource(jnbk_nouns))
myCorpus2_jnbk <- tm_map(myCorpus_jnbk,removePunctuation)
myCorpus2_jnbk <- tm_map(myCorpus_jnbk,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_jnbk <- TermDocumentMatrix(myCorpus2_jnbk, control = list(wordLengths = c(4, 32)))
term_df_jnbk <- as.data.frame(as.matrix(myCorpus_term_jnbk))

wordResult_jnbk <- sort(rowSums(term_df_jnbk),decreasing = T)

wordResult2_jnbk <- as.data.frame(wordResult_jnbk)

mani_jnbk %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  labs(x = "대",
       y = "순위",
       title = "전라북도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()
#--------------------------------------
# 전라남도
#--------------------------------------  

jnnm_nouns <- sapply(mani_jnnm$공약,exNouns)

myCorpus_jnnm <- Corpus(VectorSource(jnnm_nouns))
myCorpus2_jnnm <- tm_map(myCorpus_jnnm,removePunctuation)
myCorpus2_jnnm <- tm_map(myCorpus_jnnm,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_jnnm <- TermDocumentMatrix(myCorpus2_jnnm, control = list(wordLengths = c(4, 32)))
term_df_jnnm <- as.data.frame(as.matrix(myCorpus_term_jnnm))

wordResult_jnnm <- sort(rowSums(term_df_jnnm),decreasing = T)

wordResult2_jnnm <- as.data.frame(wordResult_jnnm)


jnnm_rank <- mani_jnnm %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

jnnm_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = jnnm_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = jnnm_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "전라남도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 경상남도
#--------------------------------------
gynm_nouns <- sapply(mani_gynm$공약,exNouns)

myCorpus_gynm <- Corpus(VectorSource(gynm_nouns))
myCorpus2_gynm <- tm_map(myCorpus_gynmremovePunctuation)
myCorpus2_gynm <- tm_map(myCorpus_gynm,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_gynm <- TermDocumentMatrix(myCorpus2_gynm, control = list(wordLengths = c(4, 32)))
term_df_gynm <- as.data.frame(as.matrix(myCorpus_term_gynm))

wordResult_gynm <- sort(rowSums(term_df_gynm),decreasing = T)

wordResult2_gynm <- as.data.frame(wordResult_gynm)

gynm_rank <- mani_gynm %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

gynm_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = gynm_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = gynm_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "경상남도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()


#--------------------------------------
# 경상북도
#--------------------------------------

gybk_nouns <- sapply(mani_gybk$공약,exNouns)

myCorpus_gybk <- Corpus(VectorSource(gybk_nouns))
myCorpus2_gybk <- tm_map(myCorpus_gybk,removePunctuation)
myCorpus2_gybk <- tm_map(myCorpus_gybk,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_gybk <- TermDocumentMatrix(myCorpus2_gybk, control = list(wordLengths = c(4, 32)))
term_df_gybk <- as.data.frame(as.matrix(myCorpus_term_gybk))

wordResult_gybk <- sort(rowSums(term_df_gybk),decreasing = T)

wordResult2_gybk <- as.data.frame(wordResult_gybk)
gybk_rank <- mani_gybk %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

gybk_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = gybk_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = gybk_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "경상북도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 충청남도
#--------------------------------------

chnm_nouns <- sapply(mani_chnm$공약,exNouns)

myCorpus_chnm <- Corpus(VectorSource(chnm_nouns))
myCorpus2_chnm <- tm_map(myCorpus_chnm,removePunctuation)
myCorpus2_chnm <- tm_map(myCorpus_chnm,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_chnm <- TermDocumentMatrix(myCorpus2_chnm, control = list(wordLengths = c(4, 32)))
term_df_chnm <- as.data.frame(as.matrix(myCorpus_term_chnm))

wordResult_chnm <- sort(rowSums(term_df_chnm),decreasing = T)

wordResult2_chnm <- as.data.frame(wordResult_chnm)

chnm_rank <- mani_chnm %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

chnm_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = chnm_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = chnm_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "충청남도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 충청북도
#--------------------------------------

chbk_nouns <- sapply(mani_chbk$공약,exNouns)

myCorpus_chbk <- Corpus(VectorSource(chbk_nouns))
myCorpus2_chbk <- tm_map(myCorpus_chbk,removePunctuation)
myCorpus2_chbk <- tm_map(myCorpus_chbk,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_chbk <- TermDocumentMatrix(myCorpus2_chbk, control = list(wordLengths = c(4, 32)))
term_df_chbk <- as.data.frame(as.matrix(myCorpus_term_chbk))

wordResult_chbk <- sort(rowSums(term_df_chbk),decreasing = T)

wordResult2_chbk <- as.data.frame(wordResult_chbk)

chbk_rank <- mani_chbk %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

chbk_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = chbk_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = chbk_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "충청북도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 부산
#--------------------------------------

bus_nouns <- sapply(mani_busan$공약,exNouns)

myCorpus_bus <- Corpus(VectorSource(bus_nouns))
myCorpus2_bus <- tm_map(myCorpus_bus,removePunctuation)
myCorpus2_bus <- tm_map(myCorpus_bus,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_bus <- TermDocumentMatrix(myCorpus2_chbk, control = list(wordLengths = c(4, 32)))
term_df_bus <- as.data.frame(as.matrix(myCorpus_term_bus))

wordResult_bus <- sort(rowSums(term_df_bus),decreasing = T)

wordResult2_bus <- as.data.frame(wordResult_bus)

busan_rank <- mani_busan %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

busan_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = busan_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = busan_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "부산광역시에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 울산
#--------------------------------------

ulsan_nouns <- sapply(mani_ulsan$공약,exNouns)

myCorpus_ulsa <- Corpus(VectorSource(ulsan_nouns))
myCorpus2_ulsa <- tm_map(myCorpus_ulsa,removePunctuation)
myCorpus2_ulsa <- tm_map(myCorpus_ulsa,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_ulsa <- TermDocumentMatrix(myCorpus2_ulsa, control = list(wordLengths = c(4, 32)))
term_df_ulsa <- as.data.frame(as.matrix(myCorpus_term_ulsa))

wordResult_ulsa <- sort(rowSums(term_df_ulsa),decreasing = T)

wordResult2_ulsa <- as.data.frame(wordResult_ulsa)

ulsan_rank <- mani_ulsan %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

ulsan_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = ulsan_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = ulsan_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "울산광역시에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 대전
#--------------------------------------

daejeon_nouns <- sapply(mani_daejeon$공약,exNouns)

myCorpus_daejeon <- Corpus(VectorSource(daejeon_nouns))
myCorpus2_daejeon <- tm_map(myCorpus_daejeon,removePunctuation)
myCorpus2_daejeon <- tm_map(myCorpus_daejeon,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_daejeon <- TermDocumentMatrix(myCorpus2_daejeon, control = list(wordLengths = c(4, 32)))
term_df_daejeon <- as.data.frame(as.matrix(myCorpus_term_daejeon))

wordResult_daejeon <- sort(rowSums(term_df_daejeon),decreasing = T)

wordResult2_daejeon <- as.data.frame(wordResult_daejeon)

daejeon_rank <- mani_daejeon %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

daejeon_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = daejeon_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = daejeon_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "대전광역시에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 대구
#--------------------------------------

daegu_nouns <- sapply(mani_daegu$공약,exNouns)

myCorpus_daegu <- Corpus(VectorSource(daegu_nouns))
myCorpus2_daegu <- tm_map(myCorpus_daegu,removePunctuation)
myCorpus2_daegu <- tm_map(myCorpus_daegu,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_daegu <- TermDocumentMatrix(myCorpus2_daegu, control = list(wordLengths = c(4, 32)))
term_df_daegu <- as.data.frame(as.matrix(myCorpus_term_daegu))

wordResult_daegu <- sort(rowSums(term_df_daegu),decreasing = T)

wordResult2_daegu <- as.data.frame(wordResult_daegu)

daegu_rank <- mani_daegu %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

daegu_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = daegu_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = daegu_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "대구광역시에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 광주
#--------------------------------------

gwangju_nouns <- sapply(mani_gwangju$공약,exNouns)

myCorpus_gwangju <- Corpus(VectorSource(gwangju_nouns))
myCorpus2_gwangju <- tm_map(myCorpus_gwangju,removePunctuation)
myCorpus2_gwangju <- tm_map(myCorpus_gwangju,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_gwangju <- TermDocumentMatrix(myCorpus2_gwangju, control = list(wordLengths = c(4, 32)))
term_df_gwangju <- as.data.frame(as.matrix(myCorpus_term_gwangju))

wordResult_gwangju <- sort(rowSums(term_df_gwangju),decreasing = T)

wordResult2_gwangju <- as.data.frame(wordResult_gwangju)


gwangju_rank <- mani_gwangju %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

gwangju_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = gwangju_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = gwangju_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "광주광역시에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 경기
#--------------------------------------

gyungi_nouns <- sapply(mani_gyung$공약,exNouns)

myCorpus_gyungi <- Corpus(VectorSource(gyungi_nouns))
myCorpus2_gyungi <- tm_map(myCorpus_gyungi,removePunctuation)
myCorpus2_gyungi <- tm_map(myCorpus_gyungi,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_gyungi <- TermDocumentMatrix(myCorpus2_gyungi, control = list(wordLengths = c(4, 32)))
term_df_gyungi <- as.data.frame(as.matrix(myCorpus_term_gyungi))

wordResult_gyungi <- sort(rowSums(term_df_gyungi),decreasing = T)

wordResult2_gyungi <- as.data.frame(wordResult_gyungi)

gyung_rank <- mani_gyung %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

gyung_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = gyung_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = gyung_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "경기도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 인천
#--------------------------------------

incheon_nouns <- sapply(mani_incheon$공약,exNouns)

myCorpus_incheon <- Corpus(VectorSource(incheon_nouns))
myCorpus2_incheon <- tm_map(myCorpus_incheon,removePunctuation)
myCorpus2_incheon <- tm_map(myCorpus_incheon,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_incheon <- TermDocumentMatrix(myCorpus2_incheon, control = list(wordLengths = c(4, 32)))
term_df_incheon <- as.data.frame(as.matrix(myCorpus_term_incheon))

wordResult_incheon <- sort(rowSums(term_df_incheon),decreasing = T)

wordResult2_incheon <- as.data.frame(wordResult_incheon)

incheon_rank <- mani_incheon %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

incheon_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = incheon_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = incheon_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "인천광역시에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

mani_incheon %>% filter()
#--------------------------------------
# 강원
#--------------------------------------
gangwon_nouns <- sapply(mani_gangwon$공약,exNouns)

myCorpus_gangwon <- Corpus(VectorSource(gangwon_nouns))
myCorpus2_gangwon <- tm_map(myCorpus_gangwon,removePunctuation)
myCorpus2_gangwon <- tm_map(myCorpus_gangwon,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_gangwon <- TermDocumentMatrix(myCorpus2_gangwon, control = list(wordLengths = c(4, 32)))
term_df_gangwon <- as.data.frame(as.matrix(myCorpus_term_gangwon))

wordResult_gangwon <- sort(rowSums(term_df_gangwon),decreasing = T)

wordResult2_gangwon <- as.data.frame(wordResult_gangwon)

gangwon_rank <- mani_gangwon %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

gangwon_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = gangwon_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = gangwon_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "강원도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 세종
#--------------------------------------
sejong_nouns <- sapply(mani_sejong$공약,exNouns)

myCorpus_sejong <- Corpus(VectorSource(sejong_nouns))
myCorpus2_sejong <- tm_map(myCorpus_sejong,removePunctuation)
myCorpus2_sejong<- tm_map(myCorpus_sejong,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_sejong <- TermDocumentMatrix(myCorpus2_sejong, control = list(wordLengths = c(4, 32)))
term_df_sejong <- as.data.frame(as.matrix(myCorpus_term_sejong))

wordResult_sejong <- sort(rowSums(term_df_sejong),decreasing = T)

wordResult2_sejong <- as.data.frame(wordResult_sejong)

sejong_rank <- mani_sejong %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

sejong_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = sejong_rank %>% filter(대 == 18),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = sejong_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "세종특별자치시에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()

#--------------------------------------
# 제주
#--------------------------------------

jeju_nouns <- sapply(mani_jeju$공약,exNouns)

myCorpus_jeju <- Corpus(VectorSource(jeju_nouns))
myCorpus2_jeju <- tm_map(myCorpus_jeju,removePunctuation)
myCorpus2_jeju<- tm_map(myCorpus_jeju,removeWords,c( "인천","경기","강원","대전","충남","충북","세종","부산","대구","울산","경북","경남","광주","전남","전북","제주"))
myCorpus_term_jeju <- TermDocumentMatrix(myCorpus2_jeju, control = list(wordLengths = c(4, 32)))
term_df_jeju<- as.data.frame(as.matrix(myCorpus_term_jeju))

wordResult_jeju <- sort(rowSums(term_df_jeju),decreasing = T)

wordResult2_jeju <- as.data.frame(wordResult_jeju)

jeju_rank <- mani_jeju %>% group_by(대) %>% 
  count(category) %>% 
  mutate(my_ranks = order(order(n, decreasing=TRUE))) %>% arrange(대,my_ranks) 

jeju_rank %>% 
  ggplot(aes(x=대,y=my_ranks,group=category))+geom_line(aes(color = category, alpha = 1), size = 2) +
  geom_point(aes(color = category), size = 4) + 
  geom_point(color = "#FFFFFF", size = 1) +
  geom_text(data = jeju_rank %>% filter(대 == 17),
            aes(label = category, x = 16.95) , hjust = 1, family = "Noto Sans KR", color = "#000000", size = 4) +
  geom_text(data = jeju_rank %>% filter(대 == 19),
            aes(label = category, x = 19.05) , hjust = .1, family = "Noto Sans KR", color = "#000000", size = 4) +
  scale_y_reverse(breaks = 1:10) +
  scale_alpha(guide = 'none')+
  scale_x_continuous(breaks = 17:19, minor_breaks = 17:19, expand = c(.05, .05))+
  theme(legend.position = "none")+
  labs(x = "대",
       y = "순위",
       title = "제주도에서 가장 많이 나온 공약 분야는?",
       subtitle = "17-19대 공약 순위 추이 알아보기") + my_ggplot()
