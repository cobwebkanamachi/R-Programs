#runnable on Azure Machine Learning, but not complete(tuned results not same original article)
#http://tjo.hatenablog.com/entry/2013/12/24/190000
#random forest contour plot
# data xors: https://github.com/ozt-ca/tjo.hatenablog.samples/blob/master/r_samples/public_lib/jp/xor_simple.txt
# data xorc: https://github.com/ozt-ca/tjo.hatenablog.samples/blob/master/r_samples/public_lib/jp/xor_complex.txt
# please make new dataset and load local file to the dataset.
# then connect dataset output to R exec script input.
xors <- maml.mapInputPort(1)
xorc <- maml.mapInputPort(2)
require("randomForest")
xors$label<-as.factor(xors$label-1)
xorc$label<-as.factor(xorc$label-1)
#
xors.rf<-tuneRF(xors[,-3],xors[,3],ntreeTry=50, stepFactor=0.05, 
    improve=0.05, trace=TRUE, plot=TRUE, dobest=FALSE)
xors.rf<-randomForest(label~.,xors,mtry=4)

xorc.rf<-tuneRF(xorc[,-3],xorc[,3],ntreeTry=50, stepFactor=0.05, 
    improve=0.05, trace=TRUE, plot=TRUE, dobest=FALSE)
xorc.rf<-randomForest(label~.,xorc,mtry=4)
print (xors.rf)
print (xorc.rf)
#
px<-seq(from=-3,to=3,length.out=300)
py<-seq(from=-3,to=3,length.out=300)
pgrid<-expand.grid(px,py)
names(pgrid)<-c("x","y")

plot(xors[1:50,-3],col="blue",pch=19,cex=3,xlim=c(-3,3),ylim=c(-3,3))
points(xors[51:100,-3],col="red",pch=19,cex=3)
par(new=T)
out<-predict(xors.rf,newdata=pgrid)
contour(px,py,array(out,dim=c(length(px),length(py))),xlim=c(-3,3),ylim=c(-3,3),col="purple",lwd=3,drawlabels=F,levels=0.5)

plot(xorc[1:50,-3],col="blue",pch=19,cex=3,xlim=c(-3,3),ylim=c(-3,3))
points(xors[51:100,-3],col="red",pch=19,cex=3)
par(new=T)
out2<-predict(xorc.rf,newdata=pgrid)
contour(px,py,array(out2,dim=c(length(px),length(py))),xlim=c(-3,3),ylim=c(-3,3),col="purple",lwd=3,drawlabels=F,levels=0.5)

table(xors$label,predict(xors.rf,xors[,-3]))
table(xorc$label,predict(xorc.rf,xorc[,-3]))
plot(xors.rf)
plot(xorc.rf)

#http://pegasus.cc.ucf.edu/~xsu/CLASS/STA5703/R-chp11-Bagging-RF.R
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/seq.html
