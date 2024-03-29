library(readxl)
library(DescTools)
library(e1071)
library(dplyr)
library(ggplot2)
library(corrplot)

Q <- read_xlsx("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/qualità_2011.xlsx")
AZ <- Q[,3:12]
AZ <- na.omit(AZ)

for (i in 1:nrow(AZ))
{
  if (AZ$Fatturato[i] == 'oltre 1.000.000 euro')
    AZ$Fatturato[i] = 1000
  if (AZ$Fatturato[i] == 'tra 500.001 e 1.000.000 euro')
    AZ$Fatturato[i] = 500
  if (AZ$Fatturato[i] == 'tra 250.001 e 500.000 euro')
    AZ$Fatturato[i] = 250
  if (AZ$Fatturato[i] == 'tra 100.001 e 250.000 euro')
    AZ$Fatturato[i] = 100
  if (AZ$Fatturato[i] == 'tra 50.001 e 100.000 euro')
    AZ$Fatturato[i] = 50
  if (AZ$Fatturato[i] == 'tra 10.000 e 50.000 euro')
    AZ$Fatturato[i] = 10
  
  if (AZ$Vendemmia[i] == 'vendemmia manuale')
    AZ$Vendemmia[i] = 1
  if (AZ$Vendemmia[i] == 'vendemmia meccanica')
    AZ$Vendemmia[i] = 2
  
  if (AZ$Trattamenti[i] == 'manuali')
    AZ$Trattamenti[i] = 1
  if (AZ$Trattamenti[i] == 'mezzi meccanici tradizionali')
    AZ$Trattamenti[i] = 2
  if (AZ$Trattamenti[i] == 'mezzi meccanici con sistemi di agricoltura di precisione')
    AZ$Trattamenti[i] = 3
  
  if (AZ$Metodo[i] == 'Biologico')
    AZ$Metodo[i] = 2
  if (AZ$Metodo[i] == 'non biologico')
    AZ$Metodo[i] = 0
  if (AZ$Metodo[i] == 'in conversione')
    AZ$Metodo[i] = 1
  
  if (AZ$Rilevamento_meteo[i] == 'Si')
    AZ$Rilevamento_meteo[i] = 1
  if (AZ$Rilevamento_meteo[i] == 'No')
    AZ$Rilevamento_meteo[i] = 0
  
  if (AZ$Irrigazione[i] == 'assente')
    AZ$Irrigazione[i] = 0
  if (AZ$Irrigazione[i] == 'irrigazione')
    AZ$Irrigazione[i] = 1
  
}
#trasformo in var.numeriche per fare la correlazione di più variabili
AZ$`Qualità 2011`=as.numeric(AZ$`Qualità 2011`)
AZ$Fatturato=as.numeric(AZ$Fatturato)
AZ$`Vigneti totali`=as.numeric(AZ$`Vigneti totali`)
AZ$Etichette=as.numeric(AZ$Etichette)
AZ$Lavoratori=as.numeric(AZ$Lavoratori)
AZ$Trattamenti=as.numeric(AZ$Trattamenti)
AZ$Vendemmia=as.numeric(AZ$Vendemmia)
AZ$Metodo=as.numeric(AZ$Metodo)
AZ$Rilevamento_meteo=as.numeric(AZ$Rilevamento_meteo)
AZ$Irrigazione=as.numeric(AZ$Irrigazione)

# MATRICE DI CORRELAZIONE
COV<-cov(AZ)
COR<-cor(AZ)
corrplot(cor(AZ), type="lower", method="number",tl.cex=0.6)
corrplot(cor(AZ), type="lower",tl.cex=0.1,tl.col='darkblue',number.digits=2, bg='lightblue')
plot(AZ)

write.table(COR,file="C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/Risultati/qualità_COR_2011.csv",
            sep=";", dec=".", quote=FALSE, row.names=FALSE)

