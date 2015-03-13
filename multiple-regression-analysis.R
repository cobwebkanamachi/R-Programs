library('forecast')
data(attitude)
attitude[1,]
round(cor(attitude),2)
pairs(attitude,panel=panel.smooth,attitude)
lm1<-lm(rating ~ ., data = attitude)
summary(lm1)
lm2<-step(lm1)
summary(lm2)
plot(lm2,which = 1:4)

#jp http://www1.doshisha.ac.jp/~mjin/R/14.html の末尾の回帰診断図実現する。
#https://stat.ethz.ch/R-manual/R-patched/library/stats/html/plot.lm.html にある、
#plotのcook’s distanceの記述より、plotに適切なオプション（which = 1:4）を付与する
#ということがわかる（## Cook‘s distances instead of Residual-Leverage plotの条）。
#En execute and realize above url's regression diagnostic on Azure Machine Learning
#then, I see Cook's distance not in the output plot, so finally plot's argument
# "which = 1:4" make Azure to output diagnostic graphics plot.
