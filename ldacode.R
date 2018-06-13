
################################################################1.데이터 임포트
library(stringr)
library(gtools)
library(ggplot2)
library(reshape2)
library(lda)
library(arules)
library(dplyr)
library(igraph)
library(arulseVis)
###data import
set1<-read.table("제4회 Big Data Competition-분석용데이터-01.고객DEMO.txt",sep=",",header=T)
set1$HOM_PST_NO<-na.replace(set1$HOM_PST_NO,999) ## 거주지 na =999
set1<-as.data.frame(lapply(set1,as.factor))
#set2
set2<-read.table("제4회 Big Data Competition-분석용데이터-02.쇼핑업종 상품구매.txt",sep=",",header=T)
#set3
set3<-read.table("제4회 Big Data Competition-분석용데이터-03.쇼핑외 업종 상품구매.txt",sep=",",header=T)
#set4
set4<-readLines("제4회 Big Data Competition-분석용데이터-04.쇼핑업종 상품분류.txt",encoding="UTF-8")
set4<-str_replace_all(set4,",","\t");write(set4,"set4.txt")
set4<-read.table("set4.txt",sep="\t",header=T)
set4<-as.data.frame(lapply(set4,str_replace_all,pattern="\t",replace="/"))


## set2 merge
set2<-cbind(set2,iteminfo=paste0(set2$BIZ_UNIT,"_",set2$PD_S_C))
set4<-cbind(set4,iteminfo=paste0(set4$BIZ_UNIT,"_",set4$PD_S_C))
newset2<-merge(set2,set4[,-c(1:2)],by="iteminfo")
newset2<-cbind(newset2,"ID_DT"=paste0("ID",newset2$ID,"_",newset2$DE_DT))

# 업종별 구분
newset2_a01<-newset2[newset2$BIZ_UNIT=="A01",]
newset2_a02<-newset2[newset2$BIZ_UNIT=="A02",]
newset2_a03<-newset2[newset2$BIZ_UNIT=="A03",]
newset2_a04<-newset2[newset2$BIZ_UNIT=="A04",]
newset2_a05<-newset2[newset2$BIZ_UNIT=="A05",]

#업종별 아이디별 이용금액
id.biz<-group_by(newset2,ID,BIZ_UNIT)
id.biz.money<-summarise(id.biz,"BUY_AM_SUM"=sum(BUY_AM))

#id.biz.count<-recast(newset2,newset2$BIZ_UNIT~newset2$DE_DT)


####################################################################2. LDA


#######################################################A01
#######################################################
set.seed(1234)
set=newset2_a03
k=10
itemname<-sort(unique(as.character(set$PD_M_NM)))
lda_model<-ldafunction(set,10)
result_lda<-ldaresult_list(lda_model,itemname)

##theta table
theta<-result_lda$theta
rownames(theta)<-sort(unique(set$ID))

idx<-rownames(theta)[c(1,500,3000)]
theta_id<-cbind(as.data.frame(theta[rownames(theta) %in% idx,]),"client"=factor(idx))
theta_id<-melt(theta_id,id.var="client",variable.name ="topic")
ggplot(theta_id,aes(topic,value,fill=topic))+geom_bar(stat ="identity")+coord_flip()+facet_grid(.~client)+guides(fill=F)+ylab("theta")

##id별 토픽 부여
id.topic<-ifelse(theta>=0.3,1,0)
apply(id.topic,2,sum)
# 각 토픽 설명을 위함



topicN<-6
##phi
phi<-result_lda$phi
phi_topic<-data.frame(value=sort(phi[topicN,],decreasing = T)[1:10])
ggplot(phi_topic,aes(reorder(rownames(phi_topic),value),value,fill=value))+geom_bar(stat="identity")+coord_flip()+xlab("Item(TopN30)")+ylab("Phi")

##lift  
lift<-result_lda$lift
lift_topic<-data.frame(lift=sort(lift[topicN,],decreasing = T)[1:10])
ggplot(lift_topic,aes(reorder(rownames(lift_topic),lift),lift,fill=lift))+geom_bar(stat="identity")+coord_flip()+xlab("Item(TopN30)")+ylab("Lift")


##각 토픽 별 나이대 분포
prdtable<-data.frame()
for (i in 1:k){
  t<-id.topic[id.topic[,i]==1,]
  AGE<-set1[set1$ID %in% rownames(t),3]
  t<-data.frame(AGE,"Topic"=i)
  prdtable<-rbind(prdtable,t)
}
prd<-reshape2::recast(prdtable,AGE~prdtable$Topic)
prd<-melt(prd);
p<-unlist(tapply(prd$value, prd$variable, function(x) x/sum(x)))
prd<-cbind(prd,p)
ggplot(prd,aes(variable,p,fill=AGE))+geom_bar(stat="identity")

##각 토픽별 성별 분포
prdtable<-data.frame()
for (i in 1:k){
  t<-id.topic[id.topic[,i]==1,]
  GENDER<-set1[set1$ID %in% rownames(t),2]
  t<-data.frame(GENDER,"Topic"=i)
  prdtable<-rbind(prdtable,t)
}
prd<-reshape2::recast(prdtable,GENDER~prdtable$Topic)
prd<-melt(prd);p<-unlist(tapply(prd$value, prd$variable, function(x) x/sum(x)));prd<-cbind(prd,p)
ggplot(prd,aes(variable,p,fill=GENDER))+geom_bar(stat="identity")

##각 토픽별 이용금액 boxplot
#prdtable<-data.frame()
#for (i in 1:k){
#  t<-id.topic[id.topic[,i]==1,]
#  t<-set1[set1$ID %in% rownames(t),];t<-merge(t,id.biz.money[id.biz.money$BIZ_UNIT=="A01",-2],by="ID")
#  money<-newset1[newset1$ID %in% rownames(t),5]
#  t<-data.frame(money,"Topic"=i)
#  prdtable<-rbind(prdtable,t)
#}
#ggplot(prdtable,aes(factor(Topic),money,col=factor(Topic)))+geom_boxplot() + coord_flip()+geom_jitter()
#tapply(prdtable$money, prdtable$Topic, median)


##토픽별 id information
topic_id_info<-id.topic[id.topic[,topicN]==1,]
topic_id_info<-set1[set1$ID %in% rownames(topic_id_info),]
#topic_id_info<-merge(t,id.biz.money[id.biz.money$BIZ_UNIT==biz,-2],by="ID")


######################################################################3.Market basket

####################장바구니
table(apply(id.topic,1,sum))#몇개의 토픽이 선택되었는가 빈도
topic_cN<-apply(id.topic,1,sum)

#c개의 토픽이 선택된 아이디
t=c(6) ## 해당된 토픽이 선택된 아이디 추출
c=length(t)
ct_id<-data.frame(id.topic[,t])
ct_id<-rownames(id.topic[apply(ct_id,1,sum)==c,])

#t토픽을 택한 id basekt
basket_set<-set[set$ID %in% ct_id,]

###market basket
basket<-tapply(as.character(basket_set$PD_M_NM),as.character(basket_set$ID),unique)
basket<-sapply(basket,list)
basket_tran<-as(basket,"transactions")
summary(basket_tran)
basket_crossmat<-crossTable(basket_tran)


itemFrequencyPlot(basket_tran,topN=30)
rule<-apriori(basket_tran,parameter = list(support=0.3,confidence=0.8))
rule<-sort(rule,decreasing=TRUE,by="lift")
inspect(rule)



rule_label<-labels(rule[1:40])
rulemat<-sapply(rule_label,strsplit," => ",USE.NAMES = F)
rulemat<-do.call("rbind",rulemat)
ruleg<-graph.edgelist(rulemat,directed = T)
plot.igraph(ruleg,edge.arrow.size=0.4,vertex.label.color="black",vertex.color="orange")







rules_before <-apriori(basket_tran, parameter=list(supp=0.2,confidence=0.8), 
                       appearance = list(default="lhs",rhs="우유"),
                       control = list(verbose=F))
rules_before <-sort(rules_before , decreasing=TRUE,by="lift")
inspect(rules_before[1:20])



##########################function


# 모델 생성을 위한 전처리
pr<-function(data,name){
  tab<-table(data[,1])
  line<-paste(data[,2],data[,3],sep=":")
  line<-tapply(line,data[,1],identity)
  line<-Map(c,tab,line)
  line<-lapply(line,function(x) paste(x,collapse = " "))
  line<-paste(line,collapse = "\n")
  loc<-paste0(name,".dat")
  outfile=file(loc)
  writeLines(line,outfile)
  close(outfile)
}


## LDA 모델 생성
ldafunction<-function(data,k){
  itemfreq<-recast(data,data$ID~data$PD_M_NM)
  itemfreq2<-melt(itemfreq,id=names(itemfreq)[1])
  itemfreq2<-itemfreq2[order(itemfreq2[,1]),]
  itemname<-as.character(unique(itemfreq2$variable))
  w<-length(itemname)
  alpha<-1/k
  beta<-1/w
  itemfreq2$variable<-rep(1:length(unique(itemfreq2$variable)),length(unique(itemfreq2[,1])))
  itemfreq2<-itemfreq2[itemfreq2$value!=0,]
  itemname<-data.frame("품목"=itemname)
  write.table(itemname,"itemname.txt")
  pr(itemfreq2,"쇼핑")
  shop<-read.documents("쇼핑.dat")
  item_name<-read.vocab("itemname.txt")
  lda<-lda.collapsed.gibbs.sampler(shop,k,item_name,500,alpha,beta)
  return(lda)
}



# LDA모델 분석 테이블_리스트
ldaresult_list<-function(lda,itemname){
  resultlist<-list()  
  
  theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별) , id vs topic
  for (i in 1:nrow(theta)) theta[i,] = theta[i,]/sum(theta[i,])
  k<-ncol(theta)
  
  phi<-lda$topics   # item vs topic
  for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  
  
  p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도
  w<-ncol(phi)-1
  p<-p[-1]
  phi<-phi[,-1]
  lift = matrix(0,nrow=k,ncol=w)
  
  colnames(lift)<-itemname
  
  for (i in 1:k){
    lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
    if(length(lift[i,p==0])!=0){
      lift[i,p==0]=0
    }
  }
  
  
  topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
  for (i in 1:k){
    sorted=sort(lift[i,],decreasing = T)[1:2]
    topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
  }
  colnames(theta)<-paste0(c(1:k),".",topic_name)
  
  #colnames(theta)<-paste0("Topic",c(1:k))
  resultlist[[1]]<-theta
  resultlist[[2]]<-phi
  resultlist[[3]]<-p
  resultlist[[4]]<-lift
  names(resultlist)<-c("theta","phi","p","lift")
  return(resultlist)
}




################################################################1.데이터 임포트
install.packages("stringr");library(stringr)
install.packages("gtools");library(gtools)
install.packages("ggplot2");library(ggplot2)
install.packages("reshape2");library(reshape2)
install.packages("lda");library(lda)
install.packages("arules");library(arules)
install.packages("dplyr");library(dplyr)
library(igraph)
#install.packages("arulesViz");library(arulseVis)
###data import
set1<-read.table("제4회 Big Data Competition-분석용데이터-01.고객DEMO.txt",sep=",",header=T)
set1$HOM_PST_NO<-na.replace(set1$HOM_PST_NO,999) ## 거주지 na =999
set1<-as.data.frame(lapply(set1,as.factor))
#set2
set2<-read.table("제4회 Big Data Competition-분석용데이터-02.쇼핑업종 상품구매.txt",sep=",",header=T)
#set3
set3<-read.table("제4회 Big Data Competition-분석용데이터-03.쇼핑외 업종 상품구매.txt",sep=",",header=T)
#set4
set4<-readLines("제4회 Big Data Competition-분석용데이터-04.쇼핑업종 상품분류.txt",encoding="UTF-8")
set4<-str_replace_all(set4,",","\t");write(set4,"set4.txt")
set4<-read.table("set4.txt",sep="\t",header=T)
set4<-as.data.frame(lapply(set4,str_replace_all,pattern="\t",replace="/"))


## set2 merge
set2<-cbind(set2,iteminfo=paste0(set2$BIZ_UNIT,"_",set2$PD_S_C))
set4<-cbind(set4,iteminfo=paste0(set4$BIZ_UNIT,"_",set4$PD_S_C))
newset2<-merge(set2,set4[,-c(1:2)],by="iteminfo")
newset2<-cbind(newset2,"ID_DT"=paste0("ID",newset2$ID,"_",newset2$DE_DT))

# 업종별 구분
newset2_a01<-newset2[newset2$BIZ_UNIT=="A01",]
newset2_a02<-newset2[newset2$BIZ_UNIT=="A02",]
newset2_a03<-newset2[newset2$BIZ_UNIT=="A03",]
newset2_a04<-newset2[newset2$BIZ_UNIT=="A04",]
newset2_a05<-newset2[newset2$BIZ_UNIT=="A05",]

#업종별 아이디별 이용금액
id.biz<-group_by(newset2,ID,BIZ_UNIT)
id.biz.money<-summarise(id.biz,"BUY_AM_SUM"=sum(BUY_AM))

#id.biz.count<-recast(newset2,newset2$BIZ_UNIT~newset2$DE_DT)


####################################################################2. LDA


#######################################################A01
#######################################################
set.seed(1234)
set=newset2_a03
k=10
itemname<-sort(unique(as.character(set$PD_M_NM)))
lda_model<-ldafunction(set,10)
result_lda<-ldaresult_list(lda_model,itemname)

##theta table
theta<-result_lda$theta
rownames(theta)<-sort(unique(set$ID))

idx<-rownames(theta)[c(1,500,3000)]
theta_id<-cbind(as.data.frame(theta[rownames(theta) %in% idx,]),"client"=factor(idx))
theta_id<-melt(theta_id,id.var="client",variable.name ="topic")
ggplot(theta_id,aes(topic,value,fill=topic))+geom_bar(stat ="identity")+coord_flip()+facet_grid(.~client)+guides(fill=F)+ylab("theta")

##id별 토픽 부여
id.topic<-ifelse(theta>=0.3,1,0)
apply(id.topic,2,sum)
# 각 토픽 설명을 위함



topicN<-6
##phi
phi<-result_lda$phi
phi_topic<-data.frame(value=sort(phi[topicN,],decreasing = T)[1:10])
ggplot(phi_topic,aes(reorder(rownames(phi_topic),value),value,fill=value))+geom_bar(stat="identity")+coord_flip()+xlab("Item(TopN30)")+ylab("Phi")

##lift  
lift<-result_lda$lift
lift_topic<-data.frame(lift=sort(lift[topicN,],decreasing = T)[1:10])
ggplot(lift_topic,aes(reorder(rownames(lift_topic),lift),lift,fill=lift))+geom_bar(stat="identity")+coord_flip()+xlab("Item(TopN30)")+ylab("Lift")


##각 토픽 별 나이대 분포
prdtable<-data.frame()
for (i in 1:k){
  t<-id.topic[id.topic[,i]==1,]
  AGE<-set1[set1$ID %in% rownames(t),3]
  t<-data.frame(AGE,"Topic"=i)
  prdtable<-rbind(prdtable,t)
}
prd<-reshape2::recast(prdtable,AGE~prdtable$Topic)
prd<-melt(prd);
p<-unlist(tapply(prd$value, prd$variable, function(x) x/sum(x)))
prd<-cbind(prd,p)
ggplot(prd,aes(variable,p,fill=AGE))+geom_bar(stat="identity")

##각 토픽별 성별 분포
prdtable<-data.frame()
for (i in 1:k){
  t<-id.topic[id.topic[,i]==1,]
  GENDER<-set1[set1$ID %in% rownames(t),2]
  t<-data.frame(GENDER,"Topic"=i)
  prdtable<-rbind(prdtable,t)
}
prd<-reshape2::recast(prdtable,GENDER~prdtable$Topic)
prd<-melt(prd);p<-unlist(tapply(prd$value, prd$variable, function(x) x/sum(x)));prd<-cbind(prd,p)
ggplot(prd,aes(variable,p,fill=GENDER))+geom_bar(stat="identity")

##각 토픽별 이용금액 boxplot
#prdtable<-data.frame()
#for (i in 1:k){
#  t<-id.topic[id.topic[,i]==1,]
#  t<-set1[set1$ID %in% rownames(t),];t<-merge(t,id.biz.money[id.biz.money$BIZ_UNIT=="A01",-2],by="ID")
#  money<-newset1[newset1$ID %in% rownames(t),5]
#  t<-data.frame(money,"Topic"=i)
#  prdtable<-rbind(prdtable,t)
#}
#ggplot(prdtable,aes(factor(Topic),money,col=factor(Topic)))+geom_boxplot() + coord_flip()+geom_jitter()
#tapply(prdtable$money, prdtable$Topic, median)


##토픽별 id information
topic_id_info<-id.topic[id.topic[,topicN]==1,]
topic_id_info<-set1[set1$ID %in% rownames(topic_id_info),]
#topic_id_info<-merge(t,id.biz.money[id.biz.money$BIZ_UNIT==biz,-2],by="ID")


######################################################################3.Market basket

####################장바구니
table(apply(id.topic,1,sum))#몇개의 토픽이 선택되었는가 빈도
topic_cN<-apply(id.topic,1,sum)

#c개의 토픽이 선택된 아이디
t=c(6) ## 해당된 토픽이 선택된 아이디 추출
c=length(t)
ct_id<-data.frame(id.topic[,t])
ct_id<-rownames(id.topic[apply(ct_id,1,sum)==c,])

#t토픽을 택한 id basekt
basket_set<-set[set$ID %in% ct_id,]

###market basket
basket<-tapply(as.character(basket_set$PD_M_NM),as.character(basket_set$ID),unique)
basket<-sapply(basket,list)
basket_tran<-as(basket,"transactions")
summary(basket_tran)
basket_crossmat<-crossTable(basket_tran)


itemFrequencyPlot(basket_tran,topN=30)
rule<-apriori(basket_tran,parameter = list(support=0.3,confidence=0.8))
rule<-sort(rule,decreasing=TRUE,by="lift")
inspect(rule)



rule_label<-labels(rule[1:40])
rulemat<-sapply(rule_label,strsplit," => ",USE.NAMES = F)
rulemat<-do.call("rbind",rulemat)
ruleg<-graph.edgelist(rulemat,directed = T)
plot.igraph(ruleg,edge.arrow.size=0.4,vertex.label.color="black",vertex.color="orange")







rules_before <-apriori(basket_tran, parameter=list(supp=0.2,confidence=0.8), 
                       appearance = list(default="lhs",rhs="우유"),
                       control = list(verbose=F))
rules_before <-sort(rules_before , decreasing=TRUE,by="lift")
inspect(rules_before[1:20])



##########################function


# 모델 생성을 위한 전처리
pr<-function(data,name){
  tab<-table(data[,1])
  line<-paste(data[,2],data[,3],sep=":")
  line<-tapply(line,data[,1],identity)
  line<-Map(c,tab,line)
  line<-lapply(line,function(x) paste(x,collapse = " "))
  line<-paste(line,collapse = "\n")
  loc<-paste0(name,".dat")
  outfile=file(loc)
  writeLines(line,outfile)
  close(outfile)
}


## LDA 모델 생성
ldafunction<-function(data,k){
  itemfreq<-recast(data,data$ID~data$PD_M_NM)
  itemfreq2<-melt(itemfreq,id=names(itemfreq)[1])
  itemfreq2<-itemfreq2[order(itemfreq2[,1]),]
  itemname<-as.character(unique(itemfreq2$variable))
  w<-length(itemname)
  alpha<-1/k
  beta<-1/w
  itemfreq2$variable<-rep(1:length(unique(itemfreq2$variable)),length(unique(itemfreq2[,1])))
  itemfreq2<-itemfreq2[itemfreq2$value!=0,]
  itemname<-data.frame("품목"=itemname)
  write.table(itemname,"itemname.txt")
  pr(itemfreq2,"쇼핑")
  shop<-read.documents("쇼핑.dat")
  item_name<-read.vocab("itemname.txt")
  lda<-lda.collapsed.gibbs.sampler(shop,k,item_name,500,alpha,beta)
  return(lda)
}



# LDA모델 분석 테이블_리스트
ldaresult_list<-function(lda,itemname){
  resultlist<-list()  
  
  theta<-t(lda$document_sums)# document_sum : 특정 토픽에 존재하는 아이템 구매 빈도(id별) , id vs topic
  for (i in 1:nrow(theta)) theta[i,] = theta[i,]/sum(theta[i,])
  k<-ncol(theta)
  
  phi<-lda$topics   # item vs topic
  for(i in 1:k) phi[i,] = phi[i,]/sum(phi[i,])  
  
  p=colSums(lda$topics)/sum(lda$topics)  ## 단일 품목의 support 특정상품빈도/전체빈도
  w<-ncol(phi)-1
  p<-p[-1]
  phi<-phi[,-1]
  lift = matrix(0,nrow=k,ncol=w)
  
  colnames(lift)<-itemname
  
  for (i in 1:k){
    lift[i,p!=0]=phi[i,p!=0]/p[p!=0]
    if(length(lift[i,p==0])!=0){
      lift[i,p==0]=0
    }
  }
  
  
  topic_name<-c()  ##lift 값 기준 크기순 2개의 아이템
  for (i in 1:k){
    sorted=sort(lift[i,],decreasing = T)[1:2]
    topic_name<-append(topic_name,paste(names(sorted),collapse = "-"))
  }
  colnames(theta)<-paste0(c(1:k),".",topic_name)
  
  #colnames(theta)<-paste0("Topic",c(1:k))
  resultlist[[1]]<-theta
  resultlist[[2]]<-phi
  resultlist[[3]]<-p
  resultlist[[4]]<-lift
  names(resultlist)<-c("theta","phi","p","lift")
  return(resultlist)
}



