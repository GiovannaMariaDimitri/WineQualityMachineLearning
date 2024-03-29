## LIBRARY
library(readxl)
library(DescTools)
library(e1071)
library(dplyr)
library(ggplot2)
library(corrplot)

AZIENDE <- read_xlsx("C:/Users/Alberto/Desktop/AGRITECH/CHIANTI/ANALISI/ANALISI_TESI/AZIENDE VITIVINICOLE.xlsx")
AZIENDE <- na.omit(AZIENDE)
AZ <- AZIENDE[,2:10]

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
corrplot(COR, type="lower", method="number",tl.cex=0.8,tl.offset=0.1,number.cex = 0.6, cl.cex = 0.3, cl.ratio = 0.08)
corrplot(COR, type="lower",tl.cex=0.8, cl.cex = 0.8, cl.ratio = 0.08,   tl.col='darkblue',number.digits=2, bg='lightblue')
plot(AZ)

## -- FUNCTIONS -----
normalize_dataset = function(dataset)
{
  for(i in 1:ncol(dataset))
  {
    minvar = min(dataset[,i])
    maxvar = max(dataset[,i])
    for(j in 1:nrow(dataset))
    {
      dataset[j,i] = (dataset[j,i] - minvar)/(maxvar - minvar)
    }
  }
  return(dataset)
}
denormalize_dataset = function (norm_dataset, true_dataset)
{
  for(i in 1:ncol(norm_dataset))
  {
    min = min(true_dataset[,i])
    max = max(true_dataset[,i])
    for(j in 1:nrow(norm_dataset))
    {
      norm_dataset[j,i] = norm_dataset[j,i]*(max-min)+min
    }
  }
  return(norm_dataset)
}


# CLUSTER
AZ<-AZ[,1:4]
norm_AZ<-normalize_dataset(AZ)
kc_AZ <- kmeans(norm_AZ ,3)

c_AZ 
print(kc_AZ $size)
print(kc_AZ $cluster)
centers<-data.frame(Center=kc_AZ $centers)
centers=denormalize_dataset(centers, AZ)
print(kc_AZ $centers)

cluster_kc_AZ<-data.frame(AZIENDE$Nome , Cluster=kc_AZ $cluster)
top_cluster_kc_AZ<-arrange(cluster_kc_AZ ,desc(cluster_kc_AZ$Cluster))
Clust_kc_AZ_1 <- filter(cluster_kc_AZ, kc_AZ$cluster == 1)
Clust_kc_AZ_2 <- filter(cluster_kc_AZ, kc_AZ$cluster == 2)
Clust_kc_AZ_3 <- filter(cluster_kc_AZ, kc_AZ$cluster == 3)

plot(AZ$Fatturato, col=kc_AZ $cluster,
     cex=2,
     lwd=3,
     xlab="",
     ylab="FATTURATO" )

plot(AZ$`Vigneti totali`, col=kc_AZ $cluster,
     cex=2,
     lwd=3,
     xlab="",
     ylab="VIGNETI")

plot(AZ$Lavoratori, col=kc_AZ $cluster,
     cex=2,
     lwd=3,
     xlab="",
     ylab="LAVORATORI")

plot(AZ$Etichette, col=kc_AZ $cluster,
     cex=2,
     lwd=3,
     xlab="",
     ylab="ETICHETTE")







