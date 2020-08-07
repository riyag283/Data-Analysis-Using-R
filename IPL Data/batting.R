library(corrplot)
library(Hmisc)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)

setwd("C:/Users/Riya/Desktop/DA-lab2/espenIPL")

# importing and cleaning data
player <- read.csv(file = 'batting.csv')[c(FALSE,TRUE),]
plyteam <- read.csv(file = 'batting.csv')[c(TRUE,FALSE),]
plyteam <- plyteam[-1,]
pteam <- plyteam$ï..Batting.averages
player$team <- pteam
pdata <- as.data.frame(player)
colnames(pdata) <- c("player","matches","inns","no","runs","hs","avg","bf","sr","100s","50s","0","4s","6s","team")
pdata[pdata=="-"] <- NA
pdata$hs_clean <- stringr::str_replace(pdata$hs,'\\*','')

# checking the structure of the imported data
str(pdata)

# Giving weights to the attributes
w1<-8.5
w2<-5
w3<-2
w4<-7.5
w5<-5
w6<-7
w7<-6
w8<-4
w9<-12.5

# Calulating scores
score <- as.numeric(pdata$hs_clean) * w1 + as.numeric(pdata$runs)/as.numeric(pdata$bf) * w2 +
  as.numeric(pdata$sr) * w7 + as.numeric(pdata$`100s`) * w3 + as.numeric(pdata$'50s') * w4 +
  as.numeric(pdata$`4s`) * w5 + as.numeric(pdata$`6s`) * w6 + as.numeric(pdata$no) * w8 +
  as.numeric(pdata$avg) * w9

#batScore = batStrikeRate ∗ batAverage
bs <- as.numeric(pdata$runs) * as.numeric(pdata$sr) * 0.01

pdata$batscore <- as.numeric(bs) + as.numeric(score)

r2 <- as.data.frame(pdata)

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
r2$sr <- stringr::str_replace(r2$sr ,'\\ ','')
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

r2$hs <- r2$hs_clean
r2 <- select(r2, -'hs_clean')

ggplot(r2,aes(x=runs,y=sr,color=no))+geom_point()

# Desciptive Analysis

# Number of players
pdata %>% summarise(pdata_count = n())

# Number of teams
team_count = length(unique(r2$team))
team_count

# Which player wins with maximum runs
max_runs = r2[which.max(r2$runs),] 
max_runs %>% select('player','runs')

maxavg <- head(r2[
  order( r2[,7], r2[,2] , decreasing = TRUE),
  ],n=50)
ggplot(maxavg, aes(runs,inns), y=player) + 
  geom_line(aes(color=team, group=team))

ratings <- head(pdata[
  order( pdata[,16], pdata[,2] , decreasing = TRUE),
  ],n=20)


hist(r2$hs, main="Distribution of Player Ratings",
     col = c("blue", "red", "pink", "green","orange","violet"))

# plotting box plot
stat3 <- as.data.frame(r2[,c(1,15,5)])
ggplot(data = stat3, aes(x=team,y=runs, color=team)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Runs of all the teams',y='Runs',x='Teams')

stat4 <- as.data.frame(r2[,c(1,15,9)])
ggplot(data = stat4, aes(x=team,y=sr, color=team)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Strike rates of all the teams',y='SR',x='Teams')

# Fetching the top 15 players
r2 <- r2[
  order( r2[,16], r2[,2] , decreasing = TRUE),
  ]
r2 <- head(r2,n=15)
str(r2)
rm(pdata)
pdata <- r2

# Removing extras
rm(player,plyteam,r2,stat3,stat4)

# Selecing useful columns
stat1 <- as.data.frame(pdata[,c(1,7,9)])

# Visualizing the data

# Getting descriptive stats
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

descriptive <- function(coln){
  cat("Mean: ",mean(coln,na.rm=TRUE),"\n")
  cat("Median: ",median(coln,na.rm=TRUE),"\n")
  #cat("Mode: ",getmode(coln),"\n")
  cat("MAX: ",max(coln,na.rm=TRUE),"\n")
  cat("MIN: ",min(coln,na.rm=TRUE),"\n")
  cat("Mean: ",mean(coln,na.rm=TRUE),"\n")
  cat("Range: ",range(coln,na.rm=TRUE),"\n")
  cat("Variance: ",var(coln,na.rm=TRUE),"\n")
  cat("Standard Deviation: ",sd(coln,na.rm=TRUE),"\n")
  #cat("Scale: ",scale(coln),"\n")
  summary(coln)
}

descriptive(pdata$runs)
descriptive(pdata$hs)
descriptive(pdata$avg)
descriptive(pdata$bf)
descriptive(pdata$sr)

# Ranking of Players by sr
ggplot(data=stat1,aes(x=reorder(player,sr),y=sr))+
  geom_bar(stat='identity',aes(fill=sr))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Strike Rate")+
  labs(title = 'Ranking of Players by Strike Rate',
       y='SR',x='Players')+ 
  geom_hline(yintercept = mean(stat1$sr),size = 1, color = 'blue')

# Ranking of Players by avg
ggplot(data=stat1,aes(x=reorder(player,avg),y=avg))+
  geom_bar(stat='identity',aes(fill=avg))+
  geom_text(aes(label=avg), vjust=1.6, color="white", size=3.5)+
  theme_classic() + 
  scale_fill_gradient(name="Average")+
  labs(title = 'Ranking of Players by Average',
       y='Average',x='Players')+ 
  geom_hline(yintercept = mean(stat1$avg),size = 1,color='orange')

# Correlation plot

# Between Runs and Balls Faced
stat2 <- as.data.frame(pdata[,c(1,5,8)])
res = cor(stat2[,-1])
res
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Calucating p-value to see whether the correlation is significant
# the smaller the p-value, the more significant the correlation
res2 <- rcorr(as.matrix(stat2[,-1]))
res2

# Runs and Average
X1 <- pdata$runs
Y1 <- pdata$avg
cov(X1,Y1)
cor(X1,Y1)

# Runs and Balls faced
X2 <- pdata$runs
Y2 <- pdata$bf
cov(X2,Y2)
cor(X2,Y2)

# SR and HS
X3 <- pdata$sr
Y3 <- pdata$hs
cov(X3,Y3)
cor(X3,Y3)

# SR and Average
X4 <- pdata$'matches'
Y4 <- pdata$'inns'
cov(X4,Y4)
cor(X4,Y4)

library(MASS)
plotstat <- cbind(X1,Y1)

# divide plot area as 2-by-2 array
par(mfrow = c(2, 2))
plot(plotstat, col = 'steelblue', pch = 20, xlab = 'Runs', ylab = 'Average', 
     main = "Correlation Graph")

plotstat2 <- cbind(X2,Y2)
plot(plotstat2, col = 'orange', pch = 20, xlab = 'Runs', ylab = 'bf', 
     main = "Correlation Graph")

plotstat3 <- cbind(X3,Y3)
plot(plotstat3, col = 'red', pch = 20, xlab = 'sr', ylab = 'hs', 
     main = "Correlation Graph")

plotstat4 <- cbind(X4,Y4)
plot(plotstat4, col = 'darkgreen', pch = 20, xlab = 'matches', ylab = 'inns', 
     main = "Correlation Graph")

res <- cor(as.matrix(plotstat))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res <- cor(as.matrix(plotstat2))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res <- cor(as.matrix(plotstat3))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res <- cor(as.matrix(plotstat4))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

par(mfrow = c(1,1))

library(reshape2)
molten.r<-melt(pdata,id = c("runs","bf"),value.name ="player",na.rm=TRUE)
#recasted.r2<-acast(molten.r2,batscore~variable)
molten.r<-head(molten.r,n=15)

# scatter plot runs vs bf
ggplot(aes(x=runs,y=bf),data=molten.r)+geom_point(alpha=1/2)
ggplot(aes(x=runs,y=bf),data=molten.r)+geom_point()+xlim(400,500)
# Jittering refers to adding small amount of random noise to data.
ggplot(aes(x=runs,y=bf),data=molten.r)+geom_jitter(alpha=1/2)
#using coord_trans for visualization.
ggplot(aes(x=runs,y=bf),data=molten.r)+geom_point(alpha=1/2)+
  coord_trans(y = "sqrt")

# tibble are modern data frames that keep features that have stood the test of time
# creating a tibble
run_groups<-group_by(molten.r,runs)
molten.r.bf_by_runs<-summarise(run_groups,
                                bf_mean=mean(bf),
                                bf_median=median(bf),
                                n=n())
molten.r.bf_by_runs<-arrange(molten.r.bf_by_runs,runs)
molten.r.bf_by_runs
ggplot(aes(runs,bf_mean),data=molten.r.bf_by_runs) +
  geom_line()

# scatter plot with the tibble.
ggplot(aes(x=runs,y=bf),data=molten.r)+
  geom_point(alpha=1/2,
             position=position_jitter(h=0),
             color='red')+
  coord_trans(y = "sqrt")+
  geom_line(stat='summary',fun.y=mean)+
  geom_line(stat='summary',fun.y=median,linetype=2,color='blue')+
  geom_line(stat='summary',fun.y=quantile,fun.args=list(probs=0.9),color='blue')

