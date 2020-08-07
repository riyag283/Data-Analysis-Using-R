setwd("C:/Users/Riya/Desktop/DA-lab2/espenIPL")
data <- read.csv(file = 'batting.csv')
head(data)

player <- read.csv(file = 'batting.csv')[c(FALSE,TRUE),]
plyteam <- read.csv(file = 'batting.csv')[c(TRUE,FALSE),]
plyteam <- plyteam[-1,]
pteam <- plyteam$ï..Batting.averages
player$team <- pteam
pdata <- as.data.frame(player)

pdata[pdata=="-"] <- NA

stat1 <- as.data.frame(pdata[,c(1,7,9)])

library(ggplot2)
library(dplyr)
#ggplot(stat1, aes(x=stat1$x.5)+
# geom_point(y=stat1$x.7))

colnames(pdata) <- c("player","matches","inns","no","runs","hs","avg","bf","sr","100s","50s","0","4s","6s","team")
ggplot(pdata,aes(x=runs,y=avg))+geom_point()
  #+geom_smooth(method="lm")
ggplot(pdata,aes(x=avg))+geom_bar()

library(stringr)
pdata$hs_clean <- stringr::str_replace(pdata$hs,'\\*','')

#pdata$score <- as.numeric(pdata$hs_clean) * 1
w1<-8.5
w2<-5
w3<-2
w4<-7.5
w5<-5
w6<-7
w7<-6
w8<-4
w9<-12.5
score <- as.numeric(pdata$hs_clean) * w1 + as.numeric(pdata$runs)/as.numeric(pdata$bf) * w2 +
  as.numeric(pdata$sr) * w7 + as.numeric(pdata$`100s`) * w3 + as.numeric(pdata$'50s') * w4 +
  as.numeric(pdata$`4s`) * w5 + as.numeric(pdata$`6s`) * w6 + as.numeric(pdata$no) * w8 +
  as.numeric(pdata$avg) * w9

#batScore = batStrikeRate ∗ batAverage
batscore <- as.numeric(pdata$runs) * as.numeric(pdata$sr) * 0.01

pdata$batscore <- as.numeric(batscore) + as.numeric(score)

order(pdata)
r <- pdata[
  order( pdata[,17], pdata[,2] , decreasing = TRUE),
  ]
r2 <- head(r,n=15)
ggplot(r2,aes(x=runs,y=sr,color=no))+geom_point()
r2$matches <- stringr::str_replace(r2$matches,'\\ ','')
r2$matches <- as.numeric(r2$matches)
r2$inns <- stringr::str_replace(r2$inns,'\\ ','')
r2$inns <- as.numeric(r2$inns)
r2$no <- stringr::str_replace(r2$no,'\\ ','')
r2$no <- as.numeric(r2$no)
r2$runs <- stringr::str_replace(r2$runs,'\\ ','')
r2$runs <- as.numeric(r2$runs)
r2$hs_clean <- stringr::str_replace(r2$hs_clean,'\\ ','')
r2$hs_clean <- as.numeric(r2$hs_clean)
r2$avg <- stringr::str_replace(r2$avg,'\\ ','')
r2$avg <- as.numeric(r2$avg)
r2$bf <- stringr::str_replace(r2$bf,'\\ ','')
r2$bf <- as.numeric(r2$bf)
r2$sr <- stringr::str_replace(r2$sr,'\\ ','')
r2$sr <- as.numeric(r2$sr)
r2$`100s` <- stringr::str_replace(r2$`100s`,'\\ ','')
r2$`100s` <- as.numeric(r2$`100s`)
r2$'50s' <- stringr::str_replace(r2$'50s','\\ ','')
r2$'50s' <- as.numeric(r2$'50s')
r2$'0' <- stringr::str_replace(r2$'0','\\ ','')
r2$'0' <- as.numeric(r2$'0')
r2$'4s' <- stringr::str_replace(r2$'4s','\\ ','')
r2$'4s' <- as.numeric(r2$'4s')
r2$'6s' <- stringr::str_replace(r2$'6s','\\ ','')
r2$'6s' <- as.numeric(r2$'6s')

r2

#******************************************************************
X <- r2$runs
Y <- r2$avg
cov(X,Y)
cor(X,Y)
sd(X)
sd(Y)

library(MASS)
plotstat <- cbind(X, Y)

# divide plot area as 2-by-2 array
par(mfrow = c(2, 2))

# plot datasets
plot(plotstat, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation Graph")

library(reshape2)

molten.r2<-melt(r2,id = c("runs","bf"),value.name ="player",na.rm=TRUE)
#recasted.r2<-acast(molten.r2,batscore~variable)

molten.r2<-head(molten.r2,n=15)

# scatter plot runs vs bf
ggplot(aes(x=runs,y=bf),data=molten.r2)+geom_point(alpha=1/2)
ggplot(aes(x=runs,y=bf),data=molten.r2)+geom_point()+xlim(400,500)
# Jittering refers to adding small amount of random noise to data.
ggplot(aes(x=runs,y=bf),data=molten.r2)+geom_jitter(alpha=1/2)
#using coord_trans for visualization.
ggplot(aes(x=runs,y=bf),data=molten.r2)+geom_point(alpha=1/2)+
  coord_trans(y = "sqrt")

# tibble are modern data frames that keep features that have stood the test of time
# creating a tibble
run_groups<-group_by(molten.r2,runs)
molten.r2.bf_by_runs<-summarise(run_groups,
                        bf_mean=mean(bf),
                        bf_median=median(bf),
                        n=n())
molten.r2.bf_by_runs<-arrange(molten.r2.bf_by_runs,runs)
molten.r2.bf_by_runs
ggplot(aes(runs,bf_mean),data=molten.r2.bf_by_runs) +
  geom_line()

# scatter plot with the tibble.
ggplot(aes(x=runs,y=bf),data=molten.r2)+
  geom_point(alpha=1/2,
             position=position_jitter(h=0),
             color='red')+
  coord_trans(y = "sqrt")+
  geom_line(stat='summary',fun.y=mean)+
  geom_line(stat='summary',fun.y=median,linetype=2,color='blue')+
  geom_line(stat='summary',fun.y=quantile,fun.args=list(probs=0.9),color='blue')

maxavg <- head(pdata[
  order( pdata[,7], pdata[,2] , decreasing = TRUE),
  ],n=20)
ggplot(maxavg, aes(runs,inns), y=player) + 
  geom_line(aes(color=team, group=team))

ratings <- head(pdata[
  order( pdata[,17], pdata[,2] , decreasing = TRUE),
  ],n=20)

hist(r2$hs_clean, main="Distribution of Player Ratings",
  col = c("blue", "red", "pink", "green","orange"))

boxplot(r2$inns,r2$no,r2$hs,r2$avg,r2$sr,r2$`4s`,r2$`6s`)

#ggplot(r2 , aes(x=player,y=value[17]),) +
 # geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=5)+
  #theme(axis.text.x = element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") + ylab("No. of Live Births") +
  #ggtitle("LIVE BIRTHS BY AGE OF THE MOTHER & BIRTH ORDER(RURAL) - 2016") 

barplot(r2$runs,r2$bf)
