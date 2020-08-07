setwd("C:/Users/Riya/Desktop/DA-lab2/espenIPL")

player <- read.csv(file = 'bowling.csv')[c(FALSE,TRUE),]
plyteam <- read.csv(file = 'bowling.csv')[c(TRUE,FALSE),]
plyteam <- plyteam[-1,]
pteam <- plyteam$ï..Bowling.averages
player$team <- pteam
pdata <- as.data.frame(player)

rm(player,plyteam)

pdata[pdata=="-"] <- NA
pdata <- na.omit(pdata, cols = c(3))
  
library(ggplot2)
library(corrplot)
library(dplyr)

colnames(pdata) <- c("player","matches","inns","overs","mdns","runs","wkts","bbi","avg","econ","sr","4","5","ct","st","team")
dim(pdata)

tonum <- function(coln){
  temp <- stringr::str_replace(coln,'\\ ','')
  temp <- as.numeric(temp)
  return(temp)
}

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
pdata$matches <- tonum(pdata$matches)
descriptive(pdata$matches)
pdata$inns <- tonum(pdata$inns)
descriptive(pdata$inns)
pdata$overs <- tonum(pdata$overs)
descriptive(pdata$overs)
pdata$mdns <- tonum(pdata$mdns)
descriptive(pdata$mdns)
pdata$runs <- tonum(pdata$runs)
descriptive(pdata$runs)
pdata$wkts <- tonum(pdata$wkts)
descriptive(pdata$wkts)
pdata$avg <- tonum(pdata$avg)
descriptive(pdata$avg)
pdata$econ <- tonum(pdata$econ)
pdata$sr <- tonum(pdata$sr)
pdata$ct <- tonum(pdata$ct)

ggplot(data = pdata, aes(x=inns,y=runs)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

ggplot(data=pdata, aes(x=inns, y=runs, fill=team)) +
  geom_bar(stat="identity")

# Plotting Runs
plot(pdata$runs)

# Giving weights to the attributes
w1 <- 5 #econ
w2 <- 4 #sr
w3 <- 6 #wkts
w4 <- 7 #mdns
w5 <- 2 #overs
w6 <- 2 #ct 

# Calulating scores
pdata$score <- as.numeric(pdata$econ)/w1 + as.numeric(pdata$sr)*w2 + 
  as.numeric(pdata$wkts)*w3 + as.numeric(pdata$mdns)*w4 +
  as.numeric(pdata$overs)*w5 + as.numeric(pdata$ct)*w6

# Desciptive Analysis

# Number of players
pdata %>% summarise(pdata_count = n())

# Number of teams
team_count = length(unique(pdata$team))
team_count

# Which player wins with lowest economy
max_runs = pdata[which.min(pdata$econ),] 
max_runs %>% select('player','econ')

maxavg <- head(pdata[
  order( pdata[,7], pdata[,2] , decreasing = TRUE),
  ],n=20)
ggplot(maxavg, aes(runs,inns), y=player) + 
  geom_line(aes(color=team, group=team))

ratings <- head(pdata[
  order( pdata[,17], pdata[,2] , decreasing = TRUE),
  ],n=20)

stat1 <- as.data.frame(pdata[,c(1,16,9)])
ggplot(data = stat1, aes(x=team,y=avg, color=team)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Average of all the teams',y='Average',x='Teams')

# Fetching the top 15 players
pdata <- pdata[
  order( pdata[,17], pdata[,2] , decreasing = TRUE),
  ]
pdata <- head(pdata,n=15)
str(pdata)

stat1 <- as.data.frame(pdata[,c(1,6)])
# Ranking of Players by runs
ggplot(data=stat1,aes(x=reorder(player,runs),y=runs))+
  geom_bar(stat='identity',aes(fill=runs))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Runs")+
  labs(title = 'Ranking of Players by Runs',
       y='Runs',x='Players')+ 
  geom_hline(yintercept = mean(stat1$runs),size = 1, color = 'blue')

# Correlation Plots
library(Hmisc)
# Between wickects and Ecomony
stat2 <- as.data.frame(pdata[,c(1,7,10)])
res = cor(stat2[,-1])
res
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Calucating p-value to see whether the correlation is significant
# the smaller the p-value, the more significant the correlation
res2 <- rcorr(as.matrix(stat2[,-1]))
res2

# Wickects and Economy
X1 <- pdata$wkts
Y1 <- pdata$econ
cov(X1,Y1)
cor(X1,Y1)

# Runs and Overs
X2 <- pdata$runs
Y2 <- pdata$overs
cov(X2,Y2)
cor(X2,Y2)

# SR and Average
X3 <- pdata$sr
Y3 <- pdata$avg
cov(X3,Y3)
cor(X3,Y3)

# SR and Inns
X4 <- pdata$sr
Y4 <- pdata$inns
cov(X4,Y4)
cor(X4,Y4)

library(MASS)
plotstat <- cbind(X1,Y1)
# divide plot area as 2-by-2 array
par(mfrow = c(2, 2))
plot(plotstat, col = 'steelblue', pch = 20, xlab = 'Wickets', ylab = 'Economy', 
     main = "Correlation Graph")

plotstat2 <- cbind(X2,Y2)
plot(plotstat2, col = 'darkorange', pch = 20, xlab = 'Runs', ylab = 'Overs', 
     main = "Correlation Graph")

plotstat3 <- cbind(X3,Y3)
plot(plotstat3, col = 'red', pch = 20, xlab = 'sr', ylab = 'Average', 
     main = "Correlation Graph")

plotstat4 <- cbind(X4,Y4)
plot(plotstat4, col = 'darkgreen', pch = 20, xlab = 'sr', ylab = 'inns', 
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
