##default path : "C:/Users/samsung/Documents/dt/BA_seminar"
library(rvest)
library(xml2)

# "C:/Users/samsung/Documents/dt/BA_seminar/moviedaily"
setwd("moviedaily/") ## '현재경로'에서 moviedaily 폴더(하위 경로)로 이동
filename<-readLines("filenames.txt")
datalist<-list()
titleset<-c()
for (i in 1:length(filename)){
  data=read.csv(filename[i],header = T)
  coln=data[1,]
  colname = c()
  for (j in 1:length(coln)){
    colname=c(colname,as.character(coln[,j]))
  }
  data = data[-1,]
  title = colnames(data)[1]
  colnames(data)= colname
  data = data[1:14,]
  datalist[[i]]<-data
  titleset<-c(titleset,title)
}
names(datalist)<-titleset
#moviedaily 전처리
library("stringr")
for (i in 1:length(datalist)){ 
  if(str_detect(datalist[[i]][,1],"/")==TRUE){
    datalist[[i]]$요일<-weekdays(as.Date(datalist[[i]][,1],format="%m/%d/%Y"))
  }else{
    datalist[[i]]$요일<-weekdays(as.Date(datalist[[i]][,1]))
  }
}

wednesday<-list()
for (i in 1:length(datalist)){
  if (datalist[[i]]$요일[1]=="수요일"){
    wednesday[[names(datalist)[i]]]<-datalist[[i]]
  }
  
}

x<-strsplit(names(tb), split=" ")
x0<-list()
for (i in 1:length(names(tb))){
  x1<-x[[i]][1][1]
  x0<-append(x0,x1)
}
x0<-unlist(x0)  

# "C:/Users/samsung/Documents/dt/BA_seminar"
setwd("../") ## 위에서 이동한 working directory 에서 이전 상위폴더로 이동(to default path in this proj )








url<-"https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=142384&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="
#100페이지까지 총 1000개의 리뷰
page<-c(1:100)
alltxt<-c()
for (i in page){
  url1<-paste0(url,i)
  html<-read_html(url1)
  description<-html_nodes(html,css=".score_reple")%>%html_nodes("p")%>%html_text()
  postdate<-html_nodes(html,css=".score_reple")%>%html_nodes("dt")%>%html_nodes("em")%>%html_text(trim=T)
  postdate<-postdate[c(2,4,6,8,10,12,14,16,18,20)]
  txt = cbind(postdate,description)
  alltxt<-rbind(alltxt,txt)
  print(i)
}
final_dat = data.frame(alltxt, stringsAsFactors = F)

###############################################################################################
################################댓글의 트렌드를 보려고 함######################################
###############################################################################################
library(pspline)
tb <- table(final_dat$postdate)#tb<-table(final_dat[,'postdate'])

#날짜 데이터에서 시간 빼내기
x<-strsplit(names(tb), split=" ")
x0<-list()
for (i in 1:length(names(tb))){
  x1<-x[[i]][1][1]
  x0<-append(x0,x1)
}
x0<-unlist(x0)  

x <-as.Date(x0, format = "%Y.%m.%d")
y <- as.numeric(tb)
plot(x, y, pch = 19, cex = 0.5)
fit <- sm.spline(x = as.integer(x), y = y, cv = TRUE)


# zero frequecy
head(x,1)
as.Date(as.integer(x[1]), origin = "1970-01-01")
xx <- as.Date(as.integer(min(x)):as.integer(max(x)),origin = "1970-01-01")#0인 날까지 포함
yy <- rep(0, length(xx))
yy[xx%in%x] <-y
plot(xx,yy, pch = 19, cex = 0.5)
# penalized spline function 
fit<-sm.spline(xx,yy,cv = TRUE)
points(fit$x, fit$ysmth, type = 'l', lty = 2, lwd = 1.5, col = 'blue')

# local polynomial function
xxint <- as.integer(xx)
rdata = data.frame(y = yy, x = xxint)
fit<-loess(y~x,data = rdata, span = 0.5, normalize = FALSE)
plot(fit, pch = 19, cex = 0.5)
points(fit$x,fit$fitted, type = 'l', lty = 2, lwd = 1.5, col = 'blue')

# K fold cross validation 
k.fold = 5
idx <-sample(1:5, length(xxint), replace = TRUE)

k = 1
rdata.tr <- rdata[idx != k, ]
rdata.va <- rdata[idx == k, ]
fit<-loess(y~x,data = rdata.tr, span = 0.1, normalize = FALSE)
fit.y<-predict(fit, newdata = rdata.va)
mean((fit.y-rdata.va$y)^2, na.rm = T)

# loop 

# loop 2
k.fold = 10
idx <-sample(1:k.fold, length(xxint), replace = TRUE)#replace복원 추출하겠다.
span.var <- seq(0.02, 0.5, by  = 0.01)
valid.mat <- NULL #span이 어떤 게 적당한지 
for (j in 1:length(span.var))
{
  valid.err <- c() #kfold for문 돌리기
  for (k in 1:k.fold)
  {
    rdata.tr <- rdata[idx != k, ]
    rdata.va <- rdata[idx == k, ]
    fit<-loess(y~x,data = rdata.tr, 
               span = span.var[j], normalize = FALSE)
    fit.y<-predict(fit, newdata = rdata.va)
    valid.err[k] <- mean((fit.y-rdata.va$y)^2, na.rm = T)
  }
  valid.mat <- cbind(valid.mat, valid.err)
}

# check
boxplot(valid.mat)
lines(colMeans(valid.mat), col = "blue", lty = 2)

###################################################################
#########################비선형 모델 선택##########################
###################################################################

# model decision
span.par<- span.var[which.min(colMeans(valid.mat))]
fit<-loess(y~x,data = rdata, 
           span = span.par, normalize = FALSE) 
#밸리데이션 에러가 가장 작은 값을 span값으로 쓴거임.
plot(xx,yy,  pch = 19, cex = 0.5)
points(xx,fit$fitted, type = 'l', lty = 2, lwd = 1.5, col = 'blue')
#x는 0을 포함하는 값,y는 loess적합한 값으로 둠

####################################################################
####################################################################
###########################텍스트 마이닝############################
####################################################################
####################################################################

#install.packages("KoNLP")
library(KoNLP)
final_dat[10,5]
a <- gsub(pattern = "<[/?A-Za-z]*>", #문자열만 뽑아오겠다.
          replace = "", final_dat[10,5])
a
# deletion tag
extractNoun(a, autoSpacing = T)
dat_tmp <- final_dat
for (i in 1:nrow(final_dat))
{
  dat_tmp[i,2]<-   gsub(pattern = "<[/|A-Za-z]*>", 
                        replace = "", final_dat[i,2])
}


library(tm)
##############################################################################
###########tm package is based on Eng. Addition option is required############
##############################################################################
text = dat_tmp[,2]
text<-iconv(text,"UTF-8")
#Vectorsource를 사용하여 말뭉치로 변환 
cps = Corpus(VectorSource(text)) #coupus=말뭉치#말뭉치가 몇개인지
#cps = Corpus(DataframeSource(data.frame(text)))

#TermDocumentMatrix 함수를 사용하여 단어가 문서에 출현하는 빈도수를 행렬로 저장
dtm = tm::DocumentTermMatrix(cps, 
                             control = list(tokenize = extractNoun, 
                                            removeNumber = T,
                                            wordLengths=c(1,10),
                                            removePunctuation = T))

str(dtm)

# matrix class
rmat <- as.matrix(dtm)

# sparseMatrix
library(Matrix)
rmat <-spMatrix(dtm$nrow,dtm$ncol, i=dtm$i, j=dtm$j, x=dtm$v)

wcount<-colSums(rmat) #각 단어에 대해 총 몇번 언급되었는지
wname <- dtm$dimnames$Terms #dim은 document와 term으로 나뉨. 고로 wname은 단어에 대한 정보
#wname <- repair_encoding(dtm$dimnames$Terms)#Encoding(wname)='UTF-8'

colnames(rmat)<- wname

sort.var <- sort(wcount,decreasing = T)[100] #언급된 횟수의 임계치
#query=영화이름
query='공조'
idx <- !( grepl(query, wname)|(wcount<=sort.var)|grepl('영화',wname)|grepl('너무',wname)|grepl('정말',wname)|grepl('진짜',wname)|grepl('ㅋㅋㅋ',wname)|grepl('ㅋㅋㅋㅋ',wname)|grepl('ㅋㅋ',wname)|grepl('ㅋ',wname)|grepl('ㅎㅎ',wname))
#언급된 총 횟수가 24보다 작거나 같은 단어이거나 '아임리얼'이 포함된 단어를 제외함
wname.rel <- wname[idx]
wcount.rel <- wcount[idx] #아임리얼 제외, 언급된 횟수 24보다 작은 것 제외한 단어에 대한 언급수

library(wordcloud)
wordcloud(wname.rel,freq = wcount.rel)
pal <- brewer.pal(9, "Set1") #컬러 설정
wordcloud(wname.rel,freq = wcount.rel, colors = pal)

dtm = as.matrix(dtm)
##################################
#interest.word.c<-c(grep("생과일",wname.rel),grep("착즙",wname.rel),grep("물한방울",wname.rel),grep("요거트",wname.rel),grep("유산균",wname.rel),grep("카스피해",wname.rel),grep("스무디",wname.rel),grep("키즈",wname.rel),grep("건강한",wname.rel),grep("딸기",wname.rel),grep("스트로베리",wname.rel),grep("당근",wname.rel),grep("집에서",wname.rel),grep("만들어",wname.rel),grep("만든",wname.rel))
#interest.word.name<-wname.rel[interest.word.c]
#word.c <- order(wcount.rel[interest.word.c], decreasing=TRUE)
#freq.words <- rmat[word.c, ]


#co.matrix<-freq.words%*%t(freq.words)

#colnames(co.matrix)=interest.word.name
#rownames(co.matrix)=interest.word.name

word.order<-order(wcount.rel,decreasing=TRUE)
freq.words <- rmat[word.order[1:50], ]


co.matrix<-freq.words%*%t(freq.words)

colnames(co.matrix)=wname.rel[word.order[1:50]]
rownames(co.matrix)=wname.rel[word.order[1:50]]

co.matrix


library(qgraph)
pdf.options(family="Korea1deb")
par(family="Apple SD Gothic Neo")
qgraph(co.matrix, labels=rownames(co.matrix),
       diag=FALSE, layout='spring', threshold=0.5,
       vsize=log(diag(co.matrix)) * 2)

