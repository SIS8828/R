# chap09_FormalInformal

##########################################
##  Chapter09. 정형/비정형 데이터 처리     
##########################################

## 1.1 Oracle 정형 데이터 처리 

# Oracle 연동을 위한 R패키지 설치.

# 1) 패키지 설치 
#   - RJDBC 패키지를 사용하기 위해서는 우선 java를 설치해야 한다.

install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")

# 2) 패키지 로딩 
library(DBI)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_202")
library(rJava)
library(RJDBC)

# 3) Oracle 연동 

##################
#  Oracle 11g
##################

# driver
drv <- JDBC("oracle.jdbc.driver.OracleDriver", "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")

# db 연동
conn <- dbConnect(drv, "jdbc:oracle:thin:@//localhost:1521/xe", "scott", "tiger")


# 1. 모든 레코드 검색 
query <- "select * from test_table" 
dbGetQuery(conn, query)

# 2. 정렬 조회 - 나이 컬럼을 기준으로 내림차순 정렬 
query <- "select * from test_table order by age desc"
dbGetQuery(conn, query)

# 3. 레코드 삽입 
query <- "insert into test_table values('kang','1234','강감찬',35)"
dbSendUpdate(conn, query)


# 4. 조건 검색 - 나이가 35세 이상인 레코드 조회 
query <- "select * from test_table where age >= 35"
result <- dbGetQuery(conn,query)
result
View(result)


# 5. 레코드 수정 : 데이터 '강감찬'의 나이를 40으로 수정.
query <- "update test_table set age = 40 where name='강감찬'"
dbSendUpdate(conn, query)


# 6. 레코드 삭제 - 데이터 '홍길동' 레코드 삭제 
query <- "delete from test_table where name='홍길동'"
dbSendUpdate(conn, query)


# 7. db 연결 종료 
dbDisconnect(conn)



# 2. 비정형 데이터 처리(텍스트 마이닝 분석)

# - 텍스트 마이닝(Text mining): 문자로 된 데이터에서 가치 있는 정보를 얻어 내는 분석 기법.


# 2.1 토픽 분석 
#   - 텍스트 데이터를 대상으로 단어를 추출하고, 이를 단어 사전과 비교하여 단어의 출현 빈도수를 분석하는 과정을 의미.
#   - 또한 단어구름(word cloud) 패키지를 적용하여 분석 결과를 시각화하는 과정도 포함.

# 패키지 설치
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP") # Korean Natural Language Processing


# 패키지 로드 
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_202")

library(memoise)
library(KoNLP)
library(rJava)


# useNIADic()



# 데이터 불러오기 
txt <- readLines("C:/workspaces/R/data/hiphop.txt")
head(txt)

library(stringr)

# 특수 문자 제거 
txt <- str_replace_all(txt, "\\W", " ")
head(txt)

# extractNoun() - 명사 추출 함수 
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다.")
# [1] "대한" "민국" "영토" "한반도와" "부속도서"

# 가사에서 명사 추출 
nouns <- extractNoun(txt)
nouns

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성 
wordcount <- table(unlist(nouns))
wordcount

tail(wordcount)

# 데이터 프레임으로 변환 
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
View(df_word)


# 변수명 수정 
install.packages("dplyr")
library(dplyr)
df_word <- rename(df_word, word=Var1, frequency=Freq)
# 혹은 names(df_word) <- c("word", "frequency")
head(df_word)

# 두 글자 이상 단어 추출 
df_word <- filter(df_word, nchar(word) >= 2)

# 빈도수가 가장 많은 상위 20개 단어 추출 
top_20 <- df_word %>% arrange(desc(frequency)) %>% head(20)

top_20


# 텍스트 마이닝의 시각화 
#  - Wordcloud:단어의 빈도를 구름 모양으로 표현한 그래프.

# 패키지 설치
install.packages("wordcloud")

# 패키지 로드
library(wordcloud)
library(RColorBrewer)


pal <- brewer.pal(8,"Dark2") # Dark2 색상 목록에서 8개 색상 추출 

set.seed(1234)
wordcloud(words=df_word$word,     # 단어 
          freq=df_word$frequency, # 빈도수 
          scale=c(4, 0.3),        # 단어 크기 범위  
          min.freq=2,             # 최소 단어 빈도 
          max.words=200,          # 표현 단어 수 
          random.order=F,         # 고빈도 단어 중앙 배치 
          rot.per=0.1,            # 회전 단어 비율 
          colors=pal              # 색깔 목록 
          )



pal <- brewer.pal(9, "Blues")[5:9]  # 색상 목록 생성 
set.seed(1234)                      # 난수 고정 


wordcloud(words=df_word$word,     # 단어 
          freq=df_word$frequency, # 빈도수 
          scale=c(4, 0.3),        # 단어 크기 범위  
          min.freq=2,             # 최소 단어 빈도 
          max.words=200,          # 표현 단어 수 
          random.order=F,         # 고빈도 단어 중앙 배치 
          rot.per=0.1,            # 회전 단어 비율 
          colors=pal              # 색깔 목록 
)

# 예) 국정원 트윗 텍스트 마이닝 
# 데이터 로드
twitter <- read.csv("C:/workspaces/R/data/twitter.csv",
         header = T, stringsAsFactors = F, fileEncoding = "UTF-8")

View(twitter)


# 변수명 수정
twitter <- rename(twitter, no=번호, id=계정이름, date=작성일, tw=내용 )
head(twitter)


# 특수 문자 제거 
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

# 트윗에서 명사 추출 
nouns <- extractNoun(twitter$tw)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성 
wordcount <- table(unlist(nouns))

# 벡터형의 data를 데이터프레임으로 변환 
df_word <- as.data.frame(wordcount, stringsAsFactors = F)


# 변수명 수정 
df_word <- rename(df_word, word=Var1, freq=Freq)
head(df_word)

# 두 글자 이상 단어만 추출
df_word <- filter(df_word, nchar(word) >= 2)

# 상위 20 개 추출 
top20 <- df_word %>% arrange(desc(freq)) %>% head(20)
top20


# 단어 빈도 막대 그래프
install.packages("ggplot2")
library(ggplot2)

order <- arrange(top20, freq)$word

ggplot(data=top20, aes(x=word, y=freq)) +
  geom_col() +
  ylim(0, 1200) + 
  coord_flip() +
  scale_x_discrete(limit=order) +   # 빈도순 막대 정렬 
  geom_text(aes(label=freq), hjust= -0.3)


# 워드 클라우드 
pal <- brewer.pal(8, "Dark2")

set.seed(1234)

wordcloud(words = df_word$word,
          freq=df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = 0.1,
          scale = c(6, 0.2),
          colors = pal)


# 예) facebook 텍스트 마이닝 

# 패키지 설치 
install.packages("slam")
install.packages("tm") # 영문 텍스트 마이닝 
remove.packages("tm")

install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos = NULL)
library(slam)
library(tm) # tm 패키지는 slam 패키지에 의존적임.

library(KoNLP) # 세종사전 / NIA 사전 
library(wordcloud) # RColorBrewer() 함수 제공 

# 텍스트 자료 가져오기 
facebook <- file("C:/workspaces/R/data/facebook_bigdata.txt", encoding = "UTF-8")
facebook_data <- readLines(facebook) # 줄단위 데이터 생성 
head(facebook_data)
str(facebook_data)

# Corpus() 함수 : 벡터 데이터를 대상으로 자료집(documents) 생성.
facebook_corpus <- Corpus(VectorSource(facebook_data))
facebook_corpus
inspect(facebook_corpus)


# 단어 추가와 단어 추출 
# (1) 세종 사전 사용 및 단어 추가 
install.packages("curl")
library(curl)
useSejongDic() # 세종 사전 불러오기(370957 words)

# (2) 세종 사전에 없는 단어 추가 
mergeUserDic(data.frame(c("자바", "R 프로그래밍", "페이스북"), C("ncn"))) # ncn:명사지시코드


# 단어추출 사용자 함수 정의
# (1) 사용자 정의 함수 작성


exNouns <- function(x) {
  paste(extractNoun(as.character(x)), collapse =  " ")
}

# (2) exNouns 함수 이용 단어 추출
facebook_nouns <- sapply(facebook_corpus,exNouns)
facebook_nouns[1]

# 추출된 단어 대상 전처리
# (1)추출된 단어 이용 자료집 생성
myCourpus<- Corpus(VectorSource(facebook_nouns))

# (2) 데이터 전처리
myCorpus<-tm_map(myCourpus,removePunctuation) # 문장부호 제거
myCorpus<-tm_map(myCourpus,removeNumbers) # 수치 제거
myCorpus<-tm_map(myCourpus,tolower) # 소문자변경
myCorpus<-tm_map(myCourpus,removeWords,stopwords('english')) 
# 불용어 제거 : for, very, and, of, are 등
inspect(myCorpus[1:5])

# 단어선별
# 전처리된 단어집을 대상으로 일반문서로 변환
myCorputfacebook_txt<-tm_map(myCorpus, PlainTextDocument)

Sys.setlocale('LC_ALL','C')

# 단어길이 2개 이상인 단어만 선별하여 matrix 자료 구조로 변경
myCorputfacebook_txt<-TermDocumentMatrix(myCorputfacebook_txt[1:76], control = list(wordLengths=c(2,Inf)))

myCorputfacebook_txt

myCorputfacebook.df <- as.data.frame(as.matrix(myCorputfacebook_txt))

dim(myCorputfacebook.df)





















