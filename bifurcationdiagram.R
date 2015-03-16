#http://www.r-bloggers.com/dynamical-systems-mapping-chaos-with-r/
#http://blog.hackingisbelieving.org/2012/04/r-multicore.html
# run under Azure Machine Learning Studio.
# on windows mc.cores=1 only(">1 is not available" message said)
require(parallel) 
testit <- function(x)
{
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 # The cpu usage should be negligible
}
testit(3.7)

mci.mclapply <- function(x, u) {
  mclapply(x, 
    function(x) {
      g <- x * exp( -(u * x)^2 / 2)
      mean(g) / sqrt(2 * pi) + 0.5
    },mc.cores=1
  )
}

bifurcation<-function(from=3,to=4,res=500,
x_o=runif(1,0,1),N=500,reps=500,cores=4)
{
r_s<-seq(from=from,to=to,length.out=res)
r<-numeric(res*reps)
for(i in 1:res)
r[((i-1)*reps+1):(i*reps)]<-r_s[i]
 
x<-array(dim=N)
 
iterate<-mclapply(1:(res*reps),
mc.cores=1,
function(k){
x[1]<-runif(1,0,1)
for(i in 2:N)
x[i]<-r[k]*x[i-1]*(1-x[i-1])
 
return(x[N])
})
 
plot(r,iterate,pch=15,cex=0.1)
 
return(cbind(r,iterate))
}
 
#warning: Even in parallel with 4 cores, this is by no means fast code!
bi<-bifurcation()
png('chaos.png',width=1000,height=850)
par(bg='black',col='green',col.main='green',cex=1)
plot(bi,col='green',xlab='R',ylab='n --> inf',main='',pch=15,cex=0.2)
dev.off()
