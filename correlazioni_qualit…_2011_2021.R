library(readxl)
library(DescTools)
library(e1071)
library(dplyr)
library(ggplot2)
library(corrplot)

COR_2011 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2011.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2012 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2012.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2013 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2013.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2014 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2014.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2015 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2015.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2016 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2016.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2017 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2017.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2018 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2018.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2019 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2019.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2020 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2020.csv", 
                       header=TRUE, sep=";", dec=".")
COR_2021 <- read.table("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2021.csv", 
                       header=TRUE, sep=";", dec=".")
#nrow(COR_2011)
# ncol(COR_2011)
COR<- COR_2011

## CORRELAZIONE MEDIA
for (i in 1:nrow(COR_2011))
{
  for (j in 1:nrow(COR_2011))
  {
    COR[i,j]<-((COR_2011[i,j]+COR_2012[i,j]+COR_2013[i,j]+COR_2014[i,j]+COR_2015[i,j]+COR_2016[i,j]+COR_2017[i,j]+COR_2018[i,j]+COR_2019[i,j]+COR_2020[i,j]+COR_2021[i,j])/11)
  }
}
COR<-data.frame(Qualità=COR$Qualità.2011,COR[,2:10])
write.table(COR,file="C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/COR_MEDIA_qualità.csv",
            sep=";", dec=".", quote=FALSE, row.names=FALSE)

COR<- as.matrix(COR)


corrplot(COR, type="lower", method="number",tl.cex=0.8,tl.offset=0.1,number.cex = 0.6, cl.cex = 0.3, cl.ratio = 0.08)
corrplot(COR, type="lower",tl.cex=0.8, cl.cex = 0.6, cl.ratio = 0.06, tl.col='darkblue',number.digits=2, bg='lightblue')
