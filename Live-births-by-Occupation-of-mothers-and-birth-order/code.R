library(reshape2)
library(ggplot2)
library(dplyr)
library(stats)

data11 <- read.csv("C:/Users/Riya/Desktop/DA-lab1/CRS-2011.csv")
data12 <- read.csv("C:/Users/Riya/Desktop/DA-lab1/CRS-2012.csv")
data13 <- read.csv("C:/Users/Riya/Desktop/DA-lab1/CRS-2013.csv")
data14 <- read.csv("C:/Users/Riya/Desktop/DA-lab1/CRS-2014.csv")
data15 <- read.csv("C:/Users/Riya/Desktop/DA-lab1/CRS-2015.csv")
data16 <- read.csv("C:/Users/Riya/Desktop/DA-lab1/CRS-2016.csv")

a1 <- melt(data11[12:21,1:15],c="All.Areas")
b1 <- melt(data11[23:32,1:15],c="All.Areas")

a <- data11 %>%
  filter(Area == "Urban" & All.Areas != "TOTAL")
b <- data11 %>%
  filter(Area == "Rural" & All.Areas != "TOTAL")

ggplot(a1,aes(x=All.Areas,y=log10(value),fill=variable),) +geom_bar(stat=
      "identity", position = position_dodge(),color = "black") +theme(axis.text.x =
      element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") +
      ylab("No. of Live Births") +ggtitle("LIVE BIRTHS BY OCCUPATION OF THE MOTHER & BIRTH ORDER (URBAN) - 2011")

c1 <- melt(data13[12:21,1:15],c="All.Areas")
d1 <- melt(data13[23:32,1:15],c="All.Areas")

c <- data13 %>%
  filter(Area == "Urban" & ï..All.Areas != "TOTAL")
d <- data13 %>%
  filter(Area == "Rural" & ï..All.Areas != "TOTAL")

ggplot(d1,aes(x=ï..All.Areas,y=log10(value),fill=variable),) +geom_bar(stat=
 "identity", position = position_dodge(),color = "black") +theme(axis.text.x =
  element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") +ylab("No. of Live Births")+
  ggtitle("LIVE BIRTHS BY OCCUPATION OF THE MOTHER & BIRTH ORDER (RURAL) - 2013")
ggplot(c1,aes(x=ï..All.Areas,y=log10(value),fill=variable),) +geom_bar(stat=
  "identity", position = position_dodge(),color = "black") +theme(axis.text.x =
  element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") +ylab("No. of Live Births")+
  ggtitle("LIVE BIRTHS BY OCCUPATION OF THE MOTHER & BIRTH ORDER (URBAN) - 2013")

summary(data14)
summary(data15)

ggplot(c, aes(x = ï..All.Areas, group = 1)) +
  geom_line(aes(y = log10(d$X2), colour = "Urban Birth Order 2")) +
  geom_line(aes(y = log10(c$X2), colour = "Rural Birth Order 2")) +
  geom_line(aes(y = log10(d$X1), colour = "Rural Birth Order 1")) +
  geom_line(aes(y = log10(c$X1), colour = "Urban Birth Order 1")) +ylab("Number of Live Births")+
  theme(axis.text.x = element_text(angle=60,
  vjust=0.6))+xlab("Occupation of mother")+ggtitle(" LIVE BIRTHS BY OCCUPATION OF THE MOTHER(RURAL & URBAN) - 2013")

ggplot(d1 , aes(x=ï..All.Areas,y=log10(value),color = variable),)+
  geom_point(stat="identity") +theme(axis.text.x = element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") +
  ylab("No. of Live Births") +ggtitle("LIVE BIRTHS BY OCCUPATION OF THE MOTHER & BIRTH ORDER (RURAL) - 2013")

e <- data15 %>%
  filter(Area == "Urban" & ALL.AREAS != "TOTAL")
f <- data15 %>%
  filter(Area == "Rural" & ALL.AREAS != "TOTAL")
ggplot(e, aes(x = ALL.AREAS, group = 1)) +
  geom_line(aes(y = log10(e$X5), colour = "Urban Birth Order 4")) +
  geom_line(aes(y = log10(f$X4), colour = "Rural Birth Order 4")) +
  geom_line(aes(y = log10(e$X3), colour = "Rural Birth Order 3")) +
  geom_line(aes(y = log10(f$X3), colour = "Urban Birth Order 3")) +
  geom_line(aes(y = log10(e$X2), colour = "Urban Birth Order 2")) +
  geom_line(aes(y = log10(f$X2), colour = "Rural Birth Order 2")) +
  geom_line(aes(y = log10(e$X1), colour = "Rural Birth Order 1")) +
  geom_line(aes(y = log10(f$X1), colour = "Urban Birth Order 1")) +ylab("Number of Live Births")+
  theme(axis.text.x = element_text(angle=60,vjust=0.6))+xlab("Occupation of mother")+
  ggtitle(" LIVE BIRTHS BY OCCUPATION OF THE MOTHER(RURAL & URBAN) - 2015")

e1 <- melt(data13[12:21,1:17],c="ï..All.Areas")
ggplot(e1 , aes(x=ï..All.Areas,y=log10(value)),) +
  geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=5) + 
  theme(axis.text.x = element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") + ylab("No. of Live Births") +
  ggtitle("LIVE BIRTHS BY AGE OF THE MOTHER & BIRTH ORDER(URBAN) - 2015")

h1 <- melt(data16[23:32,1:17],c="All.Areas")
ggplot(h1 , aes(x=All.Areas,y=value),) +
  geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=5) + 
  theme(axis.text.x = element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") + ylab("No. of Live Births") +
  ggtitle("LIVE BIRTHS BY AGE OF THE MOTHER & BIRTH ORDER(RURAL) - 2016")

# total of all years - rural
w1 <- melt(data11[23:32,16])
w2 <- melt(data12[23:32,16])
w3 <- melt(data13[23:32,16])
w4 <- melt(data14[23:32,16])
w5 <- melt(data15[23:32,16])
w6 <- melt(data16[23:32,16])

ggplot(w1 , aes(x=ï..All.Areas,y=log10(value),color = variable),)+
  geom_point(stat="identity") +theme(axis.text.x = element_text(angle=60, vjust=0.6)) + xlab("Occupation of Mother") +
  ylab("No. of Live Births") +ggtitle("LIVE BIRTHS BY OCCUPATION OF THE MOTHER & BIRTH ORDER (RURAL) - 2013")

hh<-c("Prof. Technical & Related workers",
      + "Administrative, Executive & Managerial Workers",
      + "Clerical & Related Workers",
      + "Sales Workers",
      + "Serivce Workers",
      + "Farmers, Fisherman, etc. & related workers",
      + "Production & other related workers",
      + "Workers, occupation are not elsewhere classified",
      + "Non Workers",
      + "Not Stated")
w <- c("2011"=w1,"2012"=w2,"2013"=w3,"2014"=w4,"2015"=w5,"2016"=w6)

ww <- data.frame(w)

data <- data.frame(group=hh,value=c(w1,w2,w3,w4,w5,w6))
data

summary(data)
# rural for year 2011
mean(data$value.value)
meadian(data$value.value)
mode(data$value.value)

ggplot(data, aes(x="", y=value.value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

ggplot(data, aes(x="", y=value.value.1, fill=group),
       aes(x="", y=value.value.2, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

total <- c(sum(w1),sum(w2),sum(w3),sum(w4),sum(w5),sum(w6))
plot(c("2011","2012","2013","2014","2015","2016"),log10(total))

