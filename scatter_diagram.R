#http://qh73xe.jimdo.com/%E3%83%97%E3%83%AD%E3%83%83%E3%83%88/%E3%83%91%E3%83%83%E3%82%B1%E3%83%BC%E3%82%B8ggplot/geom-point/
#http://cse.fra.affrc.go.jp/okamura/bayes/mcmc.pdf
#http://stackoverflow.com/questions/19025717/how-to-plot-frequency-with-ggplot
#http://stat.biopapyrus.net/ggplot/geom-point.html
#http://www.calvin.edu/~rpruim/talks/SC11/Seattle/RatSC11/R/Chap-RIntro.R

library(ggplot2)

if(1){
  B <- 2000
  x1 <- x2 <- rep(NA,B+1)
  x1[1] <- -2
  x2[1] <- 1
  for (i in 1:B)
  {
    x1[i+1] <- rnorm(1,1+0.7*(x2[i]-2),
                     sqrt(1-0.7^2))
    x2[i+1] <- rnorm(1,2+0.7*(x1[i+1]-1),
                     sqrt(1-0.7^2))
  }
  df <- data.frame(x=x1, y=x2)
  p<-ggplot(df,aes(x=x,y=y))
  p<-p+geom_point()
  plot(p)
}else{
  # 乱数を利用してサンプルデータを作成
  x <- sort(runif(1000, 1, 10))
  y <- rnorm(1000, 50, 10) * sort(runif(1000,1, 10))
  # ggplot に代入するためのデータフレームを作成
  df <- data.frame(x=x, y=y)
  # 作成したデータフレームを確認
  head(df)
  ##          x        y
  ## 1 1.017262 49.10280
  ## 2 1.020018 41.57822
  ## 3 1.021907 66.32804
  ## 4 1.037068 60.73542
  ## 5 1.050855 34.18668
  ## 6 1.085932 67.09626
  p<-ggplot(df,aes(x=x,y=x))
  p<-p+geom_point()
  plot(p)                       
}
