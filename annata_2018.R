library(readxl)
library(DescTools)
library(e1071)
library(dplyr)
library(ggplot2)
library(corrplot)

RESA <- read_xlsx("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/annata_2018.xlsx")

PROD<-data.frame(RESA[,4:26])
PROD<- na.omit(PROD)

for (i in 1:nrow(PROD))
{
  if (PROD$T_vendemmia[i] == 'precoce')
    PROD$T_vendemmia[i] = 1
  if (PROD$T_vendemmia[i] == 'media')
    PROD$T_vendemmia[i] = 2
  if (PROD$T_vendemmia[i] == 'tardiva')
    PROD$T_vendemmia[i] = 3
  
  if (PROD$Esposizione[i] == 'Nord - Ovest')
    PROD$Esposizione[i] = 1
  if (PROD$Esposizione[i] == 'Nord')
    PROD$Esposizione[i] = 2
  if (PROD$Esposizione[i] == 'Nord - Est')
    PROD$Esposizione[i] = 3
  if (PROD$Esposizione[i] == 'Ovest')
    PROD$Esposizione[i] = 4
  if (PROD$Esposizione[i] == 'Est')
    PROD$Esposizione[i] = 5
  if (PROD$Esposizione[i] == 'Sud - Est')
    PROD$Esposizione[i] = 6
  if (PROD$Esposizione[i] == 'Sud')
    PROD$Esposizione[i] = 7
  if (PROD$Esposizione[i] == 'Sud - Ovest')
    PROD$Esposizione[i] = 8
  
  if (PROD$Altitudine[i] == 'inferiore a 250 m s.l.m')
    PROD$Altitudine[i] = 150
  if (PROD$Altitudine[i] == '250 - 300 m s.l.m')
    PROD$Altitudine[i] = 250
  if (PROD$Altitudine[i] == '350 - 400 m s.l.m')
    PROD$Altitudine[i] = 350
  if (PROD$Altitudine[i] == '400 - 450 m s.l.m')
    PROD$Altitudine[i] = 400
  if (PROD$Altitudine[i] == '500 -600 m s.l.m')
    PROD$Altitudine[i] = 500
  if (PROD$Altitudine[i] == '600 - 700 m s.l.m')
    PROD$Altitudine[i] = 600
  
  if (PROD$Tessitura[i] == 'argillosa')
    PROD$Tessitura[i] = 1
  if (PROD$Tessitura[i] == 'medio impasto')
    PROD$Tessitura[i] = 2
  if (PROD$Tessitura[i] == 'sabbiosa')
    PROD$Tessitura[i] = 3
  if (PROD$Tessitura[i] == 'alberese e galestro')
    PROD$Tessitura[i] = 4
  
  if (PROD$Pietrosita[i] == 'bassa')
    PROD$Pietrosita[i] = 1
  if (PROD$Pietrosita[i] == 'media')
    PROD$Pietrosita[i] = 2
  if (PROD$Pietrosita[i] == 'grossa')
    PROD$Pietrosita[i] = 3
  
  if (PROD$Calcare[i] == 'assente')
    PROD$Calcare[i] = 0
  if (PROD$Calcare[i] == 'medio')
    PROD$Calcare[i] = 1
  if (PROD$Calcare[i] == 'elevato')
    PROD$Calcare[i] = 2
  
  if (PROD$pH[i] == '< 7.5')
    PROD$pH[i] = 1
  if (PROD$pH[i] == '> 7.5')
    PROD$pH[i] = 2
  
  if (PROD$Trattamenti[i] == 'manuali')
    PROD$Trattamenti[i] = 1
  if (PROD$Trattamenti[i] == 'mezzi meccanici tradizionali')
    PROD$Trattamenti[i] = 2
  if (PROD$Trattamenti[i] == 'mezzi meccanici con sistemi di agricoltura di precisione')
    PROD$Trattamenti[i] = 3
  
  if (PROD$Tipo_vendemmia[i] == 'vendemmia manuale')
    PROD$Tipo_vendemmia[i] = 1
  if (PROD$Tipo_vendemmia[i] == 'vendemmia meccanica')
    PROD$Tipo_vendemmia[i] = 2
  
  if (PROD$Rilevamento_meteo[i] == 'Si')
    PROD$Rilevamento_meteo[i] = 1
  if (PROD$Rilevamento_meteo[i] == 'No')
    PROD$Rilevamento_meteo[i] = 0
  
  if (PROD$Metodo[i] == 'Biologico')
    PROD$Metodo[i] = 2
  if (PROD$Metodo[i] == 'non biologico')
    PROD$Metodo[i] = 0
  if (PROD$Metodo[i] == 'in conversione')
    PROD$Metodo[i] = 1
  
  if (PROD$Irrigazione[i] == 'assente')
    PROD$Irrigazione[i] = 0
  if (PROD$Irrigazione[i] == 'irrigazione')
    PROD$Irrigazione[i] = 1
  
}
PROD$Qualità=as.numeric(PROD$Qualità)
PROD$Struttura=as.numeric(PROD$Struttura)
PROD$Acidità=as.numeric(PROD$Acidità)
PROD$Colore=as.numeric(PROD$Colore)
PROD$Gradi=as.numeric(PROD$Gradi)

PROD$IW=as.numeric(PROD$IW)
PROD$IH=as.numeric(PROD$IH)
PROD$IF=as.numeric(PROD$IF)
PROD$IFN=as.numeric(PROD$IFN)
PROD$T_vendemmia=as.numeric(PROD$T_vendemmia)
PROD$Resa=as.numeric(PROD$Resa)

PROD$Altitudine=as.numeric(PROD$Altitudine)
PROD$Esposizione=as.numeric(PROD$Esposizione)
PROD$Densità=as.numeric(PROD$Densità)
PROD$Tessitura=as.numeric(PROD$Tessitura)
PROD$Pietrosita=as.numeric(PROD$Pietrosita)
PROD$Calcare=as.numeric(PROD$Calcare)
PROD$pH=as.numeric(PROD$pH)

PROD$Trattamenti=as.numeric(PROD$Trattamenti)
PROD$Tipo_vendemmia=as.numeric(PROD$Tipo_vendemmia)
PROD$Rilevamento_meteo=as.numeric(PROD$Rilevamento_meteo)
PROD$Metodo=as.numeric(PROD$Metodo)
PROD$Irrigazione=as.numeric(PROD$Irrigazione)



# MATRICE DI CORRELAZIONE
COV<-cov(PROD)
COR<-cor(PROD)
corrplot(cor(PROD), type="lower", method="number",tl.cex=0.6,tl.offset=0.1,number.cex = 0.6, cl.cex = 0.3, cl.ratio = 0.08)
corrplot(cor(PROD), type="lower",tl.cex=0.6,tl.col='darkblue',number.digits=2, bg='lightblue')
plot(PROD)

write.table(COR,file="C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/annata_COR_2018.csv",
            sep=";", dec=".", quote=FALSE, row.names=FALSE)

