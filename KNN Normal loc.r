require(class)

misprobf<-NULL
for(delta in seq(-2,2,by=0.1))
{
    misprob<-NULL
    for(isimul in 1:1000)
    {

      n<-100
	x<-cbind(rnorm(n,0,1),rnorm(n,0,1))
      y<-cbind(rnorm(n,delta,1),rnorm(n,0,1))
	train <- rbind(x, y)
      m<-100
	z1<-cbind(rnorm(m,0,1),rnorm(m,0,1))
	z2<-cbind(rnorm(m,delta,1),rnorm(m,0,1))
	test <- rbind(z1, z2)
	cl <- factor(c(rep("Y",100), rep("Z",100)))
	ls<-knn(train, test, cl, k = 5, prob=FALSE)
	prob <- 1-sum(ls==cl)/length(cl)
	misprob<-c(misprob,prob)
   }
  misprobf<-rbind(misprobf,mean(misprob))
  allprob<-cbind(delta,misprobf)
}

delta<-seq(-2,2,by=0.1)
plot(delta, misprobf, type="l", xlab=expression(delta), ylab="Probability of misclassification")
write.table(allprob, file="c:/users/solabukky/desktop/5NN normal loc.txt", row.names=F)
