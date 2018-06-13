delta<-function(x,a){
  if(x<(-a/2)){
    output=0;
  }else if (x>=(-a/2)&x<(a/2)){
    output=1/a*(x+a/2);
  }else{
    output=0;
  }
  return(output)
} 

x<-seq(-100,100,0.01)
result<-sapply(x,delta,a=100)
plot(x,result,type="p",cex=.1)
