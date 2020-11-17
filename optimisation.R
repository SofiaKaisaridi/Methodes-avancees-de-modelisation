bw <- MASS::birthwt

bw <- bw %>% 
  mutate(
    lp1 = -1.5 + smoke + 1.5 * ht + ui,
    lp2 = -1 + .5 * smoke + ht + ui,
    p1 = plogis(lp1),
    p2 = plogis(lp2)
  )

install.packages("pROC")
library("pROC")

r1<-roc(response=bw$low, predictor=bw$p1, auc = TRUE, plot=TRUE)
r1

r1$sensitivities
r1$specificities

s1<-smooth(r1, n=100)
s1

plot(s1,col="red")
s1$model
s1$sensitivities
s1$specificities


r2<-roc(response=bw$low, predictor=bw$p2, auc = TRUE, plot=TRUE)
r2

r1$sensitivities
r1$specificities
?auc

s2<-smooth(r2, n=500)
s2

plot(s2,col="red")
s2$model
s2$sensitivities
s2$specificities

