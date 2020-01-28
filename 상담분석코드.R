



library(tm)
library(stringr)
library(tidyverse)
library(tidytext)
library(KoNLP)
library(wordcloud2)
library(reshape2)
library(igraph)
library(gridExtra)

getwd()
rm(list = ls())



##### 말뭉치 만들기 #####
mytextlocation <- "C:/Users/hwang/Desktop/datadong/test1"
mypaper <- VCorpus(DirSource(mytextlocation))
# mypaper[[1]]$content[1]
# 깨짐
mypaper[[1]]$content[1]
# readLines encoding UTF-8과 paste를 이용해서 content를 한문장으로 바꾸기
for (i in 1:length(mypaper)){
  
  a <- paste('test1/text', i + 1, '.txt', sep = '')
  
  b <- readLines(file(a), encoding = "UTF-8")
  
  c <- paste(b, collapse = ' ')
  
  mypaper[[i]]$content <- c
}

mypaper[[14]]$content[1]
# mypaper[[1]]$content
# mypaper[[2]]$content
# mypaper[[3]]$content



##### 변형도구 만들기 #####
q <- function(myobject, oldexp, newexp){
  newobject <- tm_map(myobject,
                      content_transformer(function(x, pattern) 
                        gsub(pattern, newexp, x)),oldexp)
  newobject
}

# 상담자와 내담자 목록을 통일시켜 분류하기
# 주의
# text14번(mypaper[[13]])이 다 상1, 내1으로만 표기됨
# text9번 (mypaper[[8]])이 상담자1, 내담자1 등으로 표기됨

mydigits <- lapply(mypaper, function(x) (
  
  str_extract_all(x, "\\b상[[:digit:]]{1,}|\\b상담자[[:digit:]]{1,}")
  
))

table(unlist(mydigits))

z <- q(mypaper, "상[[:digit:]]{1,}|상[[:digit:]]{1,}:|상[[:digit:]]{1,} :|
       상담자[[:digit:]]{1,}|상담자[[:digit:]]{1,}:|상담자[[:digit:]]{1,} :", 
       "상담:상담자왈")

z <- q(z, "내[[:digit:]]{1,}|내[[:digit:]]{1,}:|내[[:digit:]]{1,} :|
       내담자[[:digit:]]{1,}|내담자[[:digit:]]{1,}:|내담자[[:digit:]]{1,} :", 
       "내담:내담자왈")


for (i in 1:length(z)) {
  
  z[[i]]$content <- unlist(strsplit(z[[i]]$content, "\\b상담:|\\b내담:"))
  
}

z[[1]]$content[2]
z[[1]]$content[3]




##### 전처리 #####
### 특수문자 확인
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:punct:]]{1,}")
  
))
table(unlist(check))

# 먼저 괄호표현 제거
check <- lapply(z, function(x) (
  
  str_extract_all(x, "〈[[:print:]]{1,100}")
  
))
table(unlist(check))
# 꺽쇠, 대괄호 속 표현은 설명글이므로 제거
z <- q(z, "\\[[[:print:]]{1,50}\\]|〈[[:print:]]{1,50}〉", "")


# 소괄호 제거
check <- lapply(z, function(x) (
  
  str_extract_all(x, "\\([[:print:]]{1,100}")
  
))
table(unlist(check))
# 소괄호 안의 표현은 상대방의 말이므로 제거함
# 원래는 괄호 속 문장도 따로 빼서 상대방 말풍선에 집어넣으려 했으나 일단 보류
# 주의할 점은 (웃음), (음) 등 괄호 속 괄호 때문에 오류가 날 수 있으므로
# 2문자 이하의 짧은 괄호 먼저 제거해야 한다.

z <- q(z, "\\([[:print:]]{1,2}\\)", "")
z <- q(z, "\\(흐흐\\)", "")
# (흐흐)라는 표현이 자꾸 새어나와서 이렇게 제거

# 다음으로 괄호 속 중간 길이 표현제거
z <- q(z, "\\([[:print:]]{3,9}\\)", "")

# 다음으로 괄호 속 두 번째 중간 길이 표현제거
z <- q(z, "\\([[:print:]]{10,30}\\)", "")

# 마지막으로 괄호 속 긴 길이 표현제거
z <- q(z, "\\([[:print:]]{31,100}\\)", "")


# 작은따옴표는 어떻게 봐야할 지 정확히 모르겠다.
# 다만, 본인의 생각을 말하는 혼잣말 같은 표현과 명사 하이라이팅 표현의 모습이 강하므로
# 설명문보다 상담자의 말로 보는 것이 맞는 듯 하다.
# 삭제하고 싶다면 아래 순서대로 제거해주면 된다.
# 또 작은 따옴표가 홀수개임. 지우면 하나가 남음. 오타인듯
# z <- q(z, "'[[:print:]]{3,6}'", "")
# z <- q(z, "'[[:print:]]{7,20}'", "")
# z <- q(z, "'[[:print:]]{21,25}'", "")
# z <- q(z, "'[[:print:]]{26,30}'", "")
# z <- q(z, "'[[:print:]]{31,100}'", "")
# z <- q(z, "'[[:print:]]{100,200}'", "")
# 다른 작은따옴표 표현
# z <- q(z, "‘[[:print:]]{1,30}’", "")
# z <- q(z, "‘[[:print:]]{30,80}’", "")

# 큰따옴표는 누군가의 말을 인용한 부분이 많다.
# 이것 또한 상담자의 말로 인식하겠다.
# z <- q(z, '"[[:print:]]{1,50}"', "")
# 다른 표현
# z <- q(z, "“[[:print:]]{1,80}”", "")

# 방해요소 제거
check <- lapply(z, function(x) (
  
  str_extract_all(x, "ㅎ{1,}")
  
))
table(unlist(check))

z <- q(z, "ㅎ{1,}", " ")
z <- q(z, "쓰읍", " ")
z <- q(z, "-중략-", " ")
z <- q(z, "숑숑[[:alpha:]]{1}", "씨")

# 남은 특수문자 표현 제거
# 말줄임표 및 물음표, 내담자 이름표현 등 밖에 없으므로 제거
# 다만 앞뒤 두 단어가 붙는 경우를 피하기 위해 공백으로 변형
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:punct:]]{1,}")
  
))
table(unlist(check))

z <- q(z, "[[:punct:]]{1,}", " ")


# 마지막으로 내담자:, 상담자: 표현을 만들기 
z <- q(z, "상담자왈", "상담자:")
z <- q(z, "내담자왈", "내담자:")

z[[1]]$content[2]
z[[1]]$content[3]



##### 숫자표현 제거 #####
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:digit:]]{1,}")
  
))
table(unlist(check))

# 빈도표에서 00은 내담자를 호칭하는 표현이므로 무시할 수 있다.
# 그 외 숫자 1, 2, 3, 4는 빈도가 높다.
# 살펴보니 1은 '1학년'이라는 표현이 많이 나오는 것을 확인할 수 있다.
# 1 표현의 36%가 '1학년'이라는 표현인데
# 특정 시점에 대한 고유명사로써 생각할 수 있으므로 '1학년'이라는 표현은 살려두도록 한다.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "1[[:print:]]{1,2}")
  
))
table(unlist(check))

z <- q(z, "1학년", "일학년")

# 아울러 다른 숫자들은 빈도가 낮아 확인이 필요 없어보인다.
# 어느정도 빈도수가 높은 표현들은 점수, 학번, 날짜, 시간 등 무시할만한 표현들이다.
# 하지만 9의 경우 총 16번 나타난 표현 중 14번이라는 아주 높은 확률의 표현이
# '9시'인 것으로 나타났다.
# 학생의 입장에서 9시 수업에 대한 얘기가 
# 다른 시간대 수업보다 이야깃거리로 더 많이 오르내리는 것은 쉽게 공감이 되는 부분이다.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "9[[:print:]]{1}")
  
))
table(unlist(check))
# 이렇게 보니까 9의 빈도수가 다르게 나타난다...
# 살짝 당황스럽지만 앞선 이유로 '9시'의 빈도는 유의한 것으로 보고 살려두도록 한다.

z <- q(z, "9시", "아홉시")

# 둘을 제외한 나머지 숫자표현 제거
z <- tm_map(z, removeNumbers)



##### 둘 이상의 공백 제거 #####
space2 <- lapply(z, function(x) (
  
  str_extract_all(x, "\\b[[:alpha:]]{1,}[[:space:]]{2}[[:alpha:]]{1,}\\b")
  
))
table(unlist(space2))

space3 <- lapply(z, function(x) (
  
  str_extract_all(x, "\\b[[:alpha:]]{1,}[[:space:]]{3,}[[:alpha:]]{1,}\\b")
  
))
table(unlist(space3))

# 공백이 2개인 경우는 거의 공백 하나가 두 개로 잘못 들어간 경우라고 판단할 수 있다.
# 하지만 3개 이상의 공백이 있는 경우는 다르다.
# 예컨대 극복, 대변, 목소리 등 완전한 명사형을 부수는 경우가 있다.
# 따라서 일반적인 띄어쓰기가 잘못 들어갔다고 보기는 어렵다.
# 그렇다고 3 이상의 공백을 붙여버리면, 명사형이 전혀 아닌 것을 명사로 취급한다.
# 아래의 예를 보면 확인할 수 있다.
# SimplePos09("하면좋겠다")
# SimplePos09("해서제가")
# 이렇게 잘못 만들어지는 명사형이 제대로 된 명사를 망치는 경우보다 빈도가 높다.
# 따라서 몇 개의 명사를 버리더라도
# 3 이상의 공백은 공백처리를 하는 것이 좋을 것 같다.

z <- tm_map(z, stripWhitespace)

z[[1]]$content[2]
z[[1]]$content[3]



##### 말뭉치 작업 #####

# 필요한 단어 목록은 다음과 같다.
# 전체, 내담자, 상담자 말뭉치 (3개)
# 내담자 성별 말뭉치 (2개)
# 개별축어록 (14개) : 내담자 호소문제는 안 해도 됨
# 5회기 이하 전체, 내담자, 상담자, 내담자 성별 말뭉치 (5개)
# 6회기 이상 전체, 내담자, 상담자, 내담자 성별 말뭉치 (5개)

# 2. 남자, 10
# 3. 여자, 7
# 4. 남자, 4
# 5. 남자, 3
# 6. 남자, 2
# 7. 여자, 4
# 8. 여자, 1
# 9. 여자, 5
# 10. 남자, 2
# 11. 남자, 2
# 12. 남자, 5
# 13. 여자, 2
# 14. 여자, 2
# 15. 여자, 8

m <- "남자"
f <- "여자"
hey <- data.frame(sex  = c(m,f,m,m,m,f,f,f,m,m,m,f,f,f),
                  time = c(10,7,4,3,2,4,1,5,2,2,5,2,2,8))

hey

for (i in 1:length(z)) {
  z[[i]]$sex <- hey[i,1]
  z[[i]]$time <- hey[i,2]
}


### 상담자 및 내담자 언어 분리
counselor <- z
client <- z

for (i in 1:length(z)) {
  
  count_counselor = NULL
  count_client = NULL
  
  for (j in 1:length(z[[i]]$content)) {
    
    if ( substr(z[[i]]$content[j], 1, 4) == "상담자:" ) {
      count_counselor <- c(count_counselor, z[[i]]$content[j])
    } else if ( substr(z[[i]]$content[j], 1, 4) == "내담자:" ) {
      count_client <- c(count_client, z[[i]]$content[j])
    }
  }
  
  counselor[[i]]$content <- count_counselor
  client[[i]]$content <- count_client
}


# 문장 구분자로 쓰인 상담자: , 내담자: 표현 제거
z <- q(z, "상담자:|내담자:", "")
counselor <- q(counselor, "상담자:", "")
client <- q(client, "내담자:", "")


# 그리고 simpleposs09로 명사 구분해준 것 바로 넣는다.
# 각 회기별 데이터에 nouns라는 형태로 집어넣어 준다.
for (i in 1:length(z)) {
  
  ga <- SimplePos09(z[[i]]$content)
  z[[i]]$nouns <- ga %>% 
    melt() %>% 
    as_tibble() %>% 
    mutate(noun = str_match(value, '([가-힣]+)/N')[,2]) %>% 
    na.omit() %>% 
    count(noun, sort = TRUE)
  
  na <- SimplePos09(client[[i]]$content)
  client[[i]]$nouns <- na %>% 
    melt() %>%
    as_tibble() %>% 
    mutate(noun = str_match(value, '([가-힣]+)/N')[,2]) %>% 
    na.omit() %>% 
    count(noun, sort = TRUE)
  
  da <- SimplePos09(counselor[[i]]$content)
  counselor[[i]]$nouns <- da %>% 
    melt() %>% 
    as_tibble() %>% 
    mutate(noun = str_match(value, '([가-힣]+)/N')[,2]) %>% 
    na.omit() %>% 
    count(noun, sort = TRUE)
  
}

# 기존에 사용했던 전체 데이터의 빈도표에 대하여 전처리를 한다.


### 빈도표 정제
# 다음의 단어들이 정리될 필요성을 느꼈다.
# 재미
# 재수
# 방학
# 미안
# 무섭다
# 전공
# 수업
# 고민
# 답답하다
# 대학(교)
#
# 위 단어들이 사용 빈도보다 빈도표에 표기된 숫자가 적다.
# 분산되어 있는 표현들을 하나로 취합해주기 위한 툴을 만들었다.
# 다만 밑에 보류 중인 항목들에 대해서는 논의가 필요하다.
# 형용사로 나오는 재미, 무서움, 힘듬 등을 명사로 넣어줘야 하나?
# 아울러 전공%이라는 표현은 앞선 전처리에서 공백을 넣어줘서
# 전공이 들어간 합성어를 명사 2개로 쪼개야 할 것으로 보인다.
#
# 고쳐야할 항목 *보류*
# 재밌 -> 재미 있 (공백주의)
# 무서
# 무섭 -> 무섭다
# 나쁘
# 나쁜 -> 나쁘다
# 힘이 들
# 힘들% -> 힘들다
# 전공% -> 전공 %

# 이 외 밑에 항목들은 표현을 하나로 취합하여 빈도수가 낮게 나타나거나
# 분산되어 나타나는 형상을 줄여보았다.
# 단어 선택 기준은 14개 축어록을 훑어보면서
# 전반적으로 많이 나타나는 단어들을 찾아보았다.
# 그 단어가 전체 축어록 중 표현의 빈도수가 어느 정도 높지만,
# 빈도표에서는 낮게 나타나는 것을 선정했다.
#
# 통일 항목
# 친구들%  -> 친구들
# 가족%    -> 가족
# 얘기
# 얘기%    -> 이야기
# 불안%    -> 불안
# 재수%    -> 재수
# 방학%    -> 방학
# 미안%    -> 미안
# 수업%    -> 수업
# 고민%    -> 고민
# 답답%    -> 답답
# 대학%    -> 대학
#
# * 중요
# 대학원과 대학교는 엄연히 다르다.
# 대학%의 표현을 그냥 대학으로 고쳐주면 안되므로 
# 대학 위에 '원'자는 제외해주는 작업이 필요하다
# 대학생도 마찬가지로 대학과는 다른 의미를 갖는다.
# 따라서 '생'자가 붙는 것도 제외해주는 작업이 필요하다
# 그 작업은 한글 [[:alpha:]]{1,} 표현인 [가-힣]에서 제거해주는 식으로
# [가-샛,샞-웍,웓-힣] 으로 할 수 있다.
# 
# 
# 또한 답답, 미안, 대학은 각각 답답함, 미안함, 대학교로 쓰여야하지만,
# 바꿔주는 단어가 후자로 들어가면 오류를 불러 일으킨다.
# 따라서 우선 답답, 미안, 대학의 표현으로 쓰고
# 모든 작업이 마무리 되고 나서 답답함, 미안함, 대학교의 표현으로 바꾸어주도록 한다.
#
# '불안'과 같은 감정명사의 카운트를 잘 측정해낼 수 있었다는 점이 가장 좋았고,
# 재밌다, 무섭다, 힘들다 와 같은 동사/형용사를 봐야한다는 함의도 얻을 수 있었다.
# 동사/형용사 부분에 대한 취합도 어느 정도 필요할 것으로 보인다.
# 우선 명사만 가지고 진행해보도록 하자.

k <- data.frame(col1 = c("친구들[가-힣]",
                         "가족[[:alpha:]]{1,}",
                         "\\b얘기",
                         "불안[[:alpha:]]{1,}",
                         "재수[[:alpha:]]{1,}",
                         "방학[[:alpha:]]{1,}",
                         "미안[[:alpha:]]{1,}",
                         "수업[[:alpha:]]{1,}",
                         "고민[[:alpha:]]{1,}",
                         "답답[[:alpha:]]{1,}",
                         "\\b씨[[:alpha:]]{1,}",
                         "\\b생각[[:alpha:]]{1,}",
                         "대학[가-샛,샞-웍,웓-힣]"),
                
                col2 = c("친구들",
                         "가족",
                         "이야기",
                         "불안",
                         "재수",
                         "방학",
                         "미안",
                         "수업",
                         "고민",
                         "답답",
                         "씨",
                         "생각",
                         "대학"),
                stringsAsFactors = FALSE)

# 호칭 "씨"와 "생각"은 새어나가는 단어의 카운트가 너무 큰 것들이 많아서
# 워드클라우드를 그릴 때 모양이 이상해진다.
# 물론 밑에서 제거 될 표현이긴 하지만
# '생각해', '생각나' 같은 표현들을 일일이 제거하기 전에
# 한꺼번에 '생각'으로 취합해서 정리해주는 편이 낫다고 생각한다.
#
# 또한 상담자 표현에서 '답답함'이 없고, 
# 내담자 표현에서는 '미안함'이 없다.
# 이 외에도 각 회기마다 없는 단어가 무조건 있다.
# k$col1의 표현들을 k$col2의 명사로 바꿔주는 작업에서
# 빈도표에 k$col2에 명사가 없으면 삽입해주는 작업이 필요하다.
# 빈도표에 k$col2에 있는 명사가 존재하지 않다면 에러가 나기 때문이다.

for (i in 1:length(z)){
  
  for (j in 1:nrow(k)) {
    
    if (!(k$col2[j] %in% z[[i]]$nouns$noun)) {
      z[[i]]$nouns <- rbind(z[[i]]$nouns, c(k$col2[j], 0))
    }
    
    if (!(k$col2[j] %in% client[[i]]$nouns$noun)) {
      client[[i]]$nouns <- rbind(client[[i]]$nouns, c(k$col2[j], 0))
    }
    
    if (!(k$col2[j] %in% counselor[[i]]$nouns$noun)) {
      counselor[[i]]$nouns <- rbind(counselor[[i]]$nouns, c(k$col2[j], 0))
    }
    
  }
  
}


# 명사 카운트 열인 nouns$n의 class가 문자형으로 되어있기 때문에
# 합이나 정렬에 오작동을 일으키는 원인이 된다.
# 따라서 n을 숫자형으로 고쳐줘야 한다.
for (i in 1:length(z)) {
  z[[i]]$nouns$n <- as.numeric(z[[i]]$nouns$n)
  client[[i]]$nouns$n <- as.numeric(client[[i]]$nouns$n)
  counselor[[i]]$nouns$n <- as.numeric(counselor[[i]]$nouns$n)
}


# 그리고 나서 단어 정리 작업
# 예를 들어 '생각해'이라는 단어가 있다면 '생각'이라는 명사 카운트에 더해주고
# '생각해'가 있는 행은 지워버리는 작업이다.
# 전체, 내담자, 상담자 말뭉치 모두 돌려준다.

for (p in 1:length(z)) {
  trash1 <- NULL
  trash2 <- NULL
  trash3 <- NULL
  
  for (i in 1:nrow(k)) {
    
    for (j in 1:nrow(z[[p]]$nouns)) {
      
      if (str_detect(z[[p]]$nouns$noun[j], k$col1[i])) {
        
        trash1 <- c(trash1, j)
        
        plus <- z[[p]]$nouns[j, 2]
        
        z[[p]]$nouns[which(z[[p]]$nouns$noun == k$col2[i]),][2] <-
          z[[p]]$nouns[which(z[[p]]$nouns$noun == k$col2[i]),][2] + plus
        
      }
      
    }
    
    for (j in 1:nrow(client[[p]]$nouns)) {
      
      if (str_detect(client[[p]]$nouns$noun[j], k$col1[i])) {
        
        trash2 <- c(trash2, j)
        
        plus <- client[[p]]$nouns[j, 2]
        
        client[[p]]$nouns[which(client[[p]]$nouns$noun == k$col2[i]),][2] <-
          client[[p]]$nouns[which(client[[p]]$nouns$noun == k$col2[i]),][2] + plus
        
      }
      
    }
    
    for (j in 1:nrow(counselor[[p]]$nouns)) {
      
      if (str_detect(counselor[[p]]$nouns$noun[j], k$col1[i])) {
        
        trash3 <- c(trash3, j)
        
        plus <- counselor[[p]]$nouns[j, 2]
        
        counselor[[p]]$nouns[which(counselor[[p]]$nouns$noun == k$col2[i]),][2] <-
          counselor[[p]]$nouns[which(counselor[[p]]$nouns$noun == k$col2[i]),][2] + plus
        
      }
      
    }
    
    
  }
  
  z[[p]]$nouns <- z[[p]]$nouns[-c(trash1), ]
  z[[p]]$nouns <- z[[p]]$nouns %>% 
    arrange(desc(n))
  
  
  client[[p]]$nouns <- client[[p]]$nouns[-c(trash2), ]
  client[[p]]$nouns <- client[[p]]$nouns %>% 
    arrange(desc(n))
  
  counselor[[p]]$nouns <- counselor[[p]]$nouns[-c(trash3), ]
  counselor[[p]]$nouns <- counselor[[p]]$nouns %>% 
    arrange(desc(n))
  
}



### 명사 전처리
# nchar가 1인 명사
# 길이가 1인 명사 중 표현 빈도가 20 이상인 단어에 대해서 정제

# 다음 단어들은 근거를 두고 제외한다.
# 일 : 알바 등 노동의 의미보다 상황을 나타내는 의미로 해석됨.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "\\b일[[:print:]]{1,5}")
  
))
table(unlist(check))

# 앞 : front보다 앞으로, ~앞서, 이전을 의미하는 등의 표현이 강함
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:print:]]{1,5}앞[[:print:]]{1,10}")
  
))
table(unlist(check))

# 밖 : 야외의 의미보다 '~ 밖에'의 조사 표현이 강함
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:print:]]{1,5}밖[[:print:]]{1,5}")
  
))
table(unlist(check))

# 나 : 상담이 본인에 대한 얘기를 하는 거기 때문에 
# 내담자 중심의 표현인 '나'가 많이 나올 수 밖에 없다.
# 아래에서 생각을 제외하는 것과 같은 이유로 삭제한다.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "\\b나[[:print:]]{1,5}")
  
))
table(unlist(check))

# 다음 단어들을 제외한 전체 11개의 1글자 단어를 살려두도록 한다.
# '춤'같은 경우 빈도수가 20은 안 되지만 
# 슈퍼비전보고서 10번 파일에서 자주 사용됐다.
# 아마 내담자가 춤을 전공한 것 같은데,
# 추후에 TF-IDF 등을 사용한다면 필요한 단어라고 생각해서 포함하도록 한다.

oneword <- c("말","씨","남","집","화","형","돈","술","책","옷","춤") 


for (i in 1:length(z)) {
  z[[i]]$nouns <- z[[i]]$nouns %>% 
    filter(noun %in% oneword |
             nchar(noun) != 1)
  
  client[[i]]$nouns <- client[[i]]$nouns %>% 
    filter(noun %in% oneword |
             nchar(noun) != 1)
  
  counselor[[i]]$nouns <- counselor[[i]]$nouns %>% 
    filter(noun %in% oneword |
             nchar(noun) != 1)
}



### 제거 단어들
# 대부분의 단어는 영어에서 tm library의 불용단어 목록을 참고하여 
# 관사나 지시대명사 등과 같은 표현을 불용단어로 선별해 제거했다.
# stopwords("en")

except <- c("생각", "이야기", "사람", "말", "씨", "뭔가", "사실", "그거", "그때", "그것", "진짜", "거기", "정도", "상황", "때문", "한번", "그걸", "부분", "긍까", "자기", "같애", "이거", "말하", "동안", "이러", "이번", "저희", "가지", "저번", "이걸", "이것", "다음", "이후", "그쪽", "그런거", "같긴", "아무것", "어떻하지")

# 먼저 내담자 호칭의 표현 '씨'는 회의를 거쳐 제거단어로 둔다.
#
# 주관적으로 제거한 단어는 아래와 같다.
# 최고 빈도 수인 '생각'에 대해서는
# '생각'이라는 단어 자체가 추상적인 정리 표현뿐만 아니라
# 느끼다, 기억하다 등 다른 표현으로 대체되는 표현이 크다.
# 또한 상담 영역에서 내담자의 의견을 듣기 위한 질문의 대답으로써
# 본인의 주관적인 대답을 표현할 때 상투적으로 쓰이는 표현이기 때문에
# 형용사의 형태로 생각된다. 따라서 제거했다.
# 생각이 사용된 문장을 보면 다음과 같다.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:print:]]{1,10}생각[[:print:]]{1,10}")
  
))
table(unlist(check))

# '이야기' 또한 상담 내용이라는 파일의 특성으로 인해
# 빈도수가 높을 수 밖에 없는 상투적인 표현이라고 생각해서 제거하였다.
# 아울러 '상황'은 너무 추상적인 표현이라고 생각한다.
#
# '말' 같은 경우 말한다는 표현과 '이야기', '대화'와 같은 의미의 표현이다.
# 어떤 의미로 많이 쓰였는지 정확하게 말하기가 힘들다.
# 정말 애매하지만 '이야기'와 같은 맥락으로 제거해야겠다고 판단한다.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:print:]]{1,10} 말[[:print:]]{1,10}")
  
))
table(unlist(check))

# 가장 고민스러운 단어는 '사람'인데 
# 영어로 따지면 person 또는 people의 의미와
# him, her과 같은 인칭대명사가 같이 쓰이고,
# '다른 사람', '어떤 사람'과 같은 약간 someone?의 표현도 많다.
# 영어에서의 불용단어 stopwords("SMART") 에서는
# who와 someone, somebody 같은 표현은 제거되지만,
# person, people과 같은 단어는 제거되지 않는다.
# '사람'의 경우도 '말'처럼 많은 표현이 섞여나와서 고민스러웠지만,
# somebody와 같은 표현이 많다고 느끼기 때문에 제거하기로 했다.
# '사람'이 사용된 문장을 살펴보고자 하면 다음을 보면 된다.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:print:]]{1,10}사람[[:print:]]{1,10}")
  
))
table(unlist(check))

# 하지만 '사람들'의 경우 완전히 다른 모습인데,
# 영어에서 they가 아닌 person 또는 people의 의미가 짙고,
# 어떻게 보면 person을 의미하는 '사람'을 지운 대가로
# '사람들'은 살려두도록 했다.
check <- lapply(z, function(x) (
  
  str_extract_all(x, "[[:print:]]{1,10}사람들[[:print:]]{1,10}")
  
))
table(unlist(check))
# 여기에 대해서는 완전 개인적인 견해이므로 추가적인 논의가 필요해 보인다.


for (i in 1:length(z)) {
  z[[i]]$nouns <- z[[i]]$nouns %>% 
    filter(!noun %in% except)
  
  client[[i]]$nouns <- client[[i]]$nouns %>% 
    filter(!noun %in% except)
  
  counselor[[i]]$nouns <- counselor[[i]]$nouns %>% 
    filter(!noun %in% except)
}


# 끝으로 미안, 답답, 대학 3개 단어에 대해서
# 미안함, 답답함, 대학교로 바꿔주기로 했던 작업을 진행해보자

for (i in 1:length(z)) {
  for (j in 1:nrow(z[[i]]$nouns)) {
    if (z[[i]]$nouns$noun[j] == "미안") {
      z[[i]]$nouns$noun[j] <- "미안함"
    } else if (z[[i]]$nouns$noun[j] == "답답") {
      z[[i]]$nouns$noun[j] <- "답답함"
    } else if (z[[i]]$nouns$noun[j] == "대학") {
      z[[i]]$nouns$noun[j] <- "대학교"
    }
  }
  
  for (j in 1:nrow(client[[i]]$nouns)) {
    if (client[[i]]$nouns$noun[j] == "미안") {
      client[[i]]$nouns$noun[j] <- "미안함"
    } else if (client[[i]]$nouns$noun[j] == "답답") {
      client[[i]]$nouns$noun[j] <- "답답함"
    } else if (client[[i]]$nouns$noun[j] == "대학") {
      client[[i]]$nouns$noun[j] <- "대학교"
    }
  }
  
  for (j in 1:nrow(counselor[[i]]$nouns)) {
    if (counselor[[i]]$nouns$noun[j] == "미안") {
      counselor[[i]]$nouns$noun[j] <- "미안함"
    } else if (counselor[[i]]$nouns$noun[j] == "답답") {
      counselor[[i]]$nouns$noun[j] <- "답답함"
    } else if (counselor[[i]]$nouns$noun[j] == "대학") {
      counselor[[i]]$nouns$noun[j] <- "대학교"
    }
  }
}


# 이렇게 하면 모든 단어 정제가 끝나므로 원하는 회기별 분류를 실시한다.
# 각 회기별 단어 빈도 z[[i]]$nouns 를 제외한 
# 필요한 15개의 분류를 만든다.
# 하나씩 분류해도 되겠지만, 파일이 많아지는 경우에 대비해서 for문을 돌린다.

all <- NULL
all_client <- NULL
client_male <- NULL
client_female <- NULL
all_counselor <- NULL

lower5all <- NULL
lower5counselor <- NULL
lower5client_all <- NULL
lower5client_male <- NULL
lower5client_female <- NULL

upper6all <- NULL
upper6counselor <- NULL
upper6client_all <- NULL
upper6client_male <- NULL
upper6client_female <- NULL


for (i in 1:length(z)) {
  if (z[[i]]$time <= 5) {
    
    lower5all <- bind_rows(lower5all, z[[i]]$nouns)
    lower5counselor <- bind_rows(lower5counselor, counselor[[i]]$nouns)
    lower5client_all <- bind_rows(lower5client_all, client[[i]]$nouns)
    
    all <- bind_rows(all, z[[i]]$nouns)
    all_client <- bind_rows(all_client, client[[i]]$nouns)
    all_counselor <- bind_rows(all_counselor, counselor[[i]]$nouns)
    
    if (z[[i]]$sex == '여자') {
      lower5client_female <- bind_rows(lower5client_female, client[[i]]$nouns)
      client_female <- bind_rows(client_female, client[[i]]$nouns)
    } else {
      lower5client_male <- bind_rows(lower5client_male, client[[i]]$nouns)
      client_male <- bind_rows(client_male, client[[i]]$nouns)
    }
    
  } else {
    
    upper6all <- bind_rows(upper6all, z[[i]]$nouns)
    upper6counselor <- bind_rows(upper6counselor, counselor[[i]]$nouns)
    upper6client_all <- bind_rows(upper6client_all, client[[i]]$nouns)
    
    all <- bind_rows(all, z[[i]]$nouns)
    all_client <- bind_rows(all_client, client[[i]]$nouns)
    all_counselor <- bind_rows(all_counselor, counselor[[i]]$nouns)
    
    if (z[[i]]$sex == '여자') {
      upper6client_female <- bind_rows(upper6client_female, client[[i]]$nouns)
      client_female <- bind_rows(client_female, client[[i]]$nouns)
    } else {
      upper6client_male <- bind_rows(upper6client_male, client[[i]]$nouns)
      client_male <- bind_rows(client_male, client[[i]]$nouns)
    }
  }
}

# 15개에 대해서 각각을 빈도수로 재정렬해주면 된다.
A <- list(all, all_client, client_male, client_female, all_counselor,
          lower5all, lower5counselor, lower5client_all, lower5client_male,
          lower5client_female, upper6all, upper6counselor, upper6client_all,
          upper6client_male, upper6client_female)

for (i in 1:length(A)) {
  A[[i]] <- A[[i]] %>% 
    group_by(noun) %>% 
    summarise(n = sum(n)) %>% 
    arrange(desc(n))
}

all <- A[[1]]
all_client <- A[[2]]
client_male <- A[[3]]
client_female <- A[[4]]
all_counselor <- A[[5]]

lower5all <- A[[6]]
lower5counselor <- A[[7]]
lower5client_all <- A[[8]]
lower5client_male <- A[[9]]
lower5client_female <- A[[10]]

upper6all <- A[[11]]
upper6counselor <- A[[12]]
upper6client_all <- A[[13]]
upper6client_male <- A[[14]]
upper6client_female <- A[[15]]

# 이렇게 해서 나온 명사의 빈도표가
# 이전에 발표자료용을 만들기 위해 진행했던 결과와 크게 다르지 않다.
# 정말 다행이다.
# 그러면 이제 원하는 회기별, 분류별 워드클라우드 및 빈도표를 그려본다.


##### 구름띄우기 #####
# 워드클라우드와 빈도표를 그리는 것은 viewer창의 크기에 직접적인 영향을 받는다.
# 따라서 아래 그림을 그릴 때는 다음과 같이 viewer창을 조절했다.
# 1. 워드클라우드가 나타나는 크기는 모두 동일하게 했으며
#    그 안에서 minSize와 size를 조정하여 원을 그렸다.
# 2. 전체, 내담자, 상담자 빈도표는 100개,
#    나머지는 30개에 맞춰 크기를 조절했다.
# 3. 개별축어록 빈도표는 viewer에서만 나타나도록 했다.
# 그림에 대한 코드는 아래와 같다.

# 1
### 전체 데이터
wordcloud2(data = all, 
           minSize = 3, 
           rotateRatio = 0,
           backgroundColor = "#F8F8F8",
           size = .7,
           gridSize = 10, 
           ellipticity = 1 
)

# 전체 빈도표
grid.arrange(
  tableGrob(rowid_to_column(all, "순위")[c(1:25),], rows = NULL),
  tableGrob(rowid_to_column(all, "순위")[c(26:50),], rows = NULL),
  tableGrob(rowid_to_column(all, "순위")[c(51:75),], rows = NULL),
  tableGrob(rowid_to_column(all, "순위")[c(76:100),], rows = NULL),
  nrow = 1
)

# 2
### 내담자 전체
wordcloud2(data = all_client, 
           minSize = 4, 
           rotateRatio = 0,
           size = .8,
           gridSize = 10, 
           ellipticity = 1 
)

# 내담자 빈도표
grid.arrange(
  tableGrob(rowid_to_column(all_client, "순위")[c(1:25),], rows = NULL),
  tableGrob(rowid_to_column(all_client, "순위")[c(26:50),], rows = NULL),
  tableGrob(rowid_to_column(all_client, "순위")[c(51:75),], rows = NULL),
  tableGrob(rowid_to_column(all_client, "순위")[c(76:100),], rows = NULL),
  nrow = 1
)

# 3
### 내담자 남성 
wordcloud2(data = client_male, 
           minSize = 4, 
           rotateRatio = 0,
           size = .9,
           gridSize = 10, 
           ellipticity = 1 
)

# 내담자 남성 빈도표
grid.arrange(
  tableGrob(rowid_to_column(client_male, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(client_male, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 4
### 내담자 여성
wordcloud2(data = client_female, 
           minSize = 3, 
           rotateRatio = 0,
           size = .9,
           gridSize = 10, 
           ellipticity = 1 
)

# 내담자 여성 빈도표
grid.arrange(
  tableGrob(rowid_to_column(client_female, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(client_female, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 5
### 상담자 데이터
wordcloud2(data = all_counselor, 
           minSize = 4, 
           rotateRatio = 0,
           size = .7,
           gridSize = 10, 
           ellipticity = 1 
)

# 상담자 빈도표
grid.arrange(
  tableGrob(rowid_to_column(all_counselor, "순위")[c(1:25),], rows = NULL),
  tableGrob(rowid_to_column(all_counselor, "순위")[c(26:50),], rows = NULL),
  tableGrob(rowid_to_column(all_counselor, "순위")[c(51:75),], rows = NULL),
  tableGrob(rowid_to_column(all_counselor, "순위")[c(76:100),], rows = NULL),
  nrow = 1
)

# 6
### 5회기 이하 전체
wordcloud2(data = lower5all, 
           minSize = 4, 
           rotateRatio = 0,
           size = .7,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(lower5all, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(lower5all, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 7
### 5회기 이하 상담자
wordcloud2(data = lower5counselor, 
           minSize = 4, 
           rotateRatio = 0,
           size = .7,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(lower5counselor, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(lower5counselor, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 8
### 5회기 이하 내담자 전체
wordcloud2(data = lower5client_all, 
           minSize = 5, 
           rotateRatio = 0,
           size = .7,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(lower5client_all, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(lower5client_all, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 9
### 5회기 이하 내담자 남성
wordcloud2(data = lower5client_male, 
           minSize = 5, 
           rotateRatio = 0,
           size = .8,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(lower5client_male, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(lower5client_male, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 10
### 5회기 이하 내담자 여성
wordcloud2(data = lower5client_female, 
           minSize = 4, 
           rotateRatio = 0,
           size = .8,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(lower5client_female, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(lower5client_female, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 11
### 6회기 이상 전체
wordcloud2(data = upper6all, 
           minSize = 3, 
           rotateRatio = 0,
           size = 1,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(upper6all, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(upper6all, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 12
### 6회기 이상 상담자
wordcloud2(data = upper6counselor, 
           minSize = 6, 
           rotateRatio = 0,
           size = .9,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(upper6counselor, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(upper6counselor, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 13
### 6회기 이상 내담자 전체
wordcloud2(data = upper6client_all, 
           minSize = 3, 
           rotateRatio = 0,
           size = .9,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(upper6client_all, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(upper6client_all, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 14
### 6회기 이상 내담자 남성
wordcloud2(data = upper6client_male, 
           minSize = 3, 
           rotateRatio = 0,
           size = .7,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(upper6client_male, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(upper6client_male, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)

# 15
### 6회기 이상 내담자 여성
wordcloud2(data = upper6client_female, 
           minSize = 2, 
           rotateRatio = 0,
           size = .9,
           gridSize = 10, 
           ellipticity = 1 
)

# 빈도표
grid.arrange(
  tableGrob(rowid_to_column(upper6client_female, "순위")[c(1:15),], rows = NULL),
  tableGrob(rowid_to_column(upper6client_female, "순위")[c(16:30),], rows = NULL),
  nrow = 1
)



##### 개별 축어록 빈도표 Top15 #####

# 개별 축어록에 대한 TOP 15개 단어 빈도표를 뽑는 코드는 아래와 같다.
# 코드의 길이를 줄이기 위해서 assign함수를 이용하여 Viewer를 바로 뽑았다.
for (i in 1:length(z)) {
  assign(paste('file', i + 1, sep = ''),
         grid.arrange(
           tableGrob(rowid_to_column(z[[i]]$nouns, "순위")[c(1:15),], rows = NULL)
         ))
}



