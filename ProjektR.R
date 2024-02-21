getwd()
setwd("C:/Users/pirak/OneDrive/Dokumenty/Programowanie w R")
getwd()
setwd("C:/Users/mateu/OneDrive/Pulpit/PROJEKT R")

#importowanie danch z pliku xls
library(readxl)
#dane dotyczace PKB państw świata 
GPD <- read_excel("GPD.xls")
View(GPD)
dane <- GPD
#dane dotyczące PKB per capita danego panstwa
GDPpc <- read_excel("GDPpc.xls")
pc <- GDPpc
#usuwam niepotrzebne wiersze(obróbka danych)
dane <- dane[-1,]
dane <- dane[-1,]
dane <- dane[,-66]
dane[1,]

pc <- pc[,-c(5:34)]
pc <- pc[-1,]
pc <- pc[-1,]
pc <- pc[,-36]
r <- c(1990:2020)
#ustawienie nazwy kolumn w tabeli pc
colnames(pc)[1:65] <- c("Country Name", "Country Code", "Indicator Name","Indicator Code",r)
ramkapc <- cbind(pc$`Country Name`,pc$`Country Code`,pc$`2020`)
ramkapc <- as.data.frame(ramkapc)
ramkapc <- ramkapc[-1,]

#wektor zawierajacy lata od 1960 do 2020
w <- c(1960:2020)
w

#zamiana wektora wartosci liczbowych na tekstowe
paste(w, sep=" ", collapse=NULL)
#ustawienie nazwy kolumn w tabeli dane z wykorzytsaniem utworzonego wczesniej wektora
colnames(dane) [1:65] <- c("Country Name", "Country Code", "Indicator Name","Indicator Code",w)
dane <- dane[-1,]
#utworzenie ramki danych z wybranych kolumn zaimportowanych danych
ramka <- cbind(dane$`Country Name`,dane$`Country Code`,dane$`2020`)
#polecenie as.data.frame służy do zamiany objektu na ramke danych
ramka <- as.data.frame(ramka)
ramka
#usuwamy PKB ?wiata by wykres by? bardziej czytelny
ramka <- ramka[-260,]
dane2 <- dane[-260,]
ramka

#mapa dla pkb poszczegolnych panstw w 2020 roku
library(plotly)
fig <- plot_ly(ramka, type='choropleth', locations=ramka$V2, z=dane2$`2020`, text=ramka$V1, colorscale="Blues")
fig

#wykres pkb na osobe w 2020 roku
oso <- plot_ly(ramkapc, type='choropleth', locations=pc$`Country Code`, z=pc$`2020`, text=pc$`Country Name`, colorscale="8")
oso

#agregacja danch aby przedstawic je w wydognej do obrobki formie
#co pozwoli na wykorzystanie ich w tworzeniu wykresow
ag <- aggregate(as.numeric(dane["2019"][!is.na(dane["2019"])]),list(dane["Country Name"][!is.na(dane["2019"])]),mean)
#posortowane danych poleceniem order malejaco
ag2 <- ag[order(ag$x, decreasing = TRUE), ]
ag2 [10:252,]

# zaladowanie bibliotegi ggplot2 pozwalajacej na tworzenie wykresow
library(ggplot2)

#wykres dla PKB Polskiw w latach 1990-2020
ye <- c(1990:2020)
pl <- dane[-c(1:190),]
pl2 <- pl[-c(2:76),]
pl3 <- pl2[,-c(5:34)]
pl4 <- pl3[,-(1:4)]
pkbp <- as.numeric(pl4)

data2 <- data.frame(
  x=ye,
  y=pkbp
)

ggplot(data2, aes(x=ye, y=pkbp)) + 
  labs(title="PKB Polski w latach 1990-2020", y="GDP (mld,USD)",
       x="Lata",)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_gradient(low="blue", high="red")+
  geom_bar(colour="black", fill="#728ac6", width=.8, stat="identity")


# utorzenie danych zawartych w wykresie
data <- data.frame(
  KRAJ=ag2$Group.1[c(14,18,22,27,32)],
  GDP=ag2$x[c(14,18,22,27,32)]/1000000000000
)
ag2$Group.1[c(14,18,22,27,32)]
str(ramka2)
# Barplot
ggplot(data, aes(x=KRAJ, y=GDP)) +
  #tytul i podpisy osi wykresu
  labs(title="Państwa z najwyższym PKB w roku 2020", y="GDP (bln)",
       x="Kraj",)+
  #wysrodkowanie podpisow wykresu
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_gradient(low="blue", high="red")+
  geom_bar(colour="black", fill="#728ac6", width=.8, stat="identity")

#wyodrebnienie danych dla danego roku z GDP per capita
top5pc <- aggregate(as.numeric(pc["2019"][!is.na(pc["2019"])]),list(pc["Country Name"][!is.na(pc["2019"])]),mean)
top5pc
#sortowanie malejaco danych
top5pc <- top5pc[order(top5pc$x, decreasing = TRUE), ]
top5pc

top5pc$Group.1[2]
# top 5 per capita
data <- data.frame(
  Państwo=top5pc$Group.1[c(2,3,4,5,8)],
  GDPpercapita=top5pc$x[c(2,3,4,5,8)])

str(ramka2)
# Państwa top 5 PKB per capita
ggplot(data, aes(x=Państwo, y=GDPpercapita)) + 
  labs(title="Państwa z najwyższym PKB per capita w roku 2019", y="GDP(US$)",
       x="Państwo",)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_gradient(low="blue", high="red")+
  geom_bar(colour="black", fill="#728ac6", width=.8, stat="identity")


length(as.numeric(dane["2020"][!is.na(dane["2020"])]))
length(as.list(dane["Country Name"][!is.na(dane["2020"])]))

wg <- c(1:61)     
wg
wd <- c(1:266)

ramka2 <- dane[,5:65]
as.numeric(ramka2[3,3])
ramka2
#zamianna typu danych w ramce na numeric
ramka2[ , wg] <- apply(ramka2[ , wg], 2,  function(x) as.numeric(as.character(x)))
ramka2
dane[,3]
ramka3 <- cbind(dane[,3],ramka2)

#usuwanie nieporzadanych danych
ramka3[167,62]
dane[240,65]
ramka4 <- cbind(dane$`Country Name`,ramka3)
ramka4

#utworzenie funkcji pozwalajacej na przedwtawnie wartosci PKB na swiecie w wybranych latach
GDPWORLD <- function(y) {
  
  world <- ramka4[y][260,]
  world
}
c(1980:2020)

swiat <- GDPWORLD(c("1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988",
           "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997",
           "1998", "1999","2000", "2001", "2002", "2003", "2004", "2005", "2006",
           "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
           "2016", "2017", "2018", "2019","2020"))
swiat
str(swiat)
#procentowa zmiana PKB z roku 2019 do 2020
(as.numeric(swiat["2003"])-as.numeric(swiat["2002"]))/as.numeric(swiat["2002"])*100
((as.numeric(swiat["2003"])/as.numeric(swiat["2002"])) - 1 ) *100

#wyodrebnienie samych wartosci ze struktury swiat
swiatwar <- as.numeric(swiat)
swiatwar
#utworzene ramki danych do wykresu zmian PKB na wiecie
data3 <- data.frame(
  ROK=c(1980:2020),
  GDP=swiatwar/1000000000000
)
#utworzenie wkresu przedstawiajacego zmiany PKB swiata w zakresie czasu
ggplot(data3, aes(x=ROK, y=GDP)) + 
  labs(title="Zmiany w PKB na świecie w latach 1980-2020", y="GDP (bln)",
       x="ROK",)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_gradient(low="blue", high="red")+
  geom_bar(colour="black", fill="#728ac6", width=.8, stat="identity")


library(tidyverse)
library(tidyquant)
library(scales)
library(tibbletime)



data3 <- data.frame(
  ROK=c(1980:2020),
  GDP=swiatwar/1000000000000
)
# Wykres liniowy przedstawiający zmiany PKB na swiecie
ggplot(data3,aes(x=ROK,y=GDP))+ 
  geom_line(color="royalblue") + 
  scale_y_continuous(label=scales::percent, breaks=seq(-.04,.05,.01),
                     sec.axis=dup_axis())+
  theme_minimal()+
  labs(x="rok",y="PKB",title="Zmiany w PKB na świecie ",
       subtitle="w latach 1980-2020")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  theme(plot.title=element_text(face="bold",size=16),
        plot.subtitle=element_text(face="italic",size=12),
        plot.caption=element_text(hjust=0,size=8))




GDPdanyrok <- aggregate(ramka3["1980"], by=list('1980'=ramka3$`Indicator Name`), FUN=sum, na.rm=TRUE)
GDPdanyrok


GDPyear <- function(y) {
  
  year <- aggregate(ramka3[y], by=list('y'=ramka3$`Indicator Name`), FUN=sum, na.rm=TRUE)
  year
}

GDPyear(c("1997","2019", "2020"))



pl <- dane[-c(1:190),]
pl
pl2 <- pl[-c(2:76),]
pl3 <- pl2[,-c(5:34)]
pl <- dane[-c(1:190),]
pl
pl[2,]
pl2 <- pl[-c(2:76),]
pl3 <- pl2[,-c(5:34)]
pl4 <- pl3[,-c(5:34)]
pl4
as.numeric(pl4[[5]])/3795000



