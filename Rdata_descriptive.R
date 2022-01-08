library(data.table)
library(dplyr)
library(readxl)
library(openxlsx)
library(cluster)
library(factoextra)
library(purr)
library(e1071)

Combined <- read_excel("C:/Users/Wojtek/Desktop/DANE MGR/Combined.xlsx", 
                       sheet = "opisowe", range = "A1:L29")
Dane <- data.frame(Combined[,3:12],row.names = Combined$Kod)
Dane

########################STATYSTYKI PODSTAWOWE##########################
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



i=0
Granice <- data.frame(Zmienna=colnames(Dane))
for (i in 1:10){
Granice$Srednia[i] = mean(Dane[,i])
Granice$OdchStandardowe[i] = sd(Dane[,i])
Granice$Gorna[i] = Granice$Srednia[i]+3*Granice$OdchStandardowe[i]
Granice$Dolna[i] = if_else(Granice$Srednia[i]-3*Granice$OdchStandardowe[i]>0,Granice$Srednia[i]-3*Granice$OdchStandardowe[i],0)
Granice$Modalna[i] = Mode(Dane[,i])
Granice$Mediana[i] = median(Dane[,i])
Granice$Kwartyl1[i] = quantile(Dane[,i],0.25)
Granice$Kwartyl3[i] = quantile(Dane[,i],0.75)
Granice$RozstÍp[i] = max(Dane[,i])-min(Dane[,i])
Granice$Odch∆wiartkowe[i] = Granice$Kwartyl3[i]-Granice$Kwartyl1[i]
Granice$KlasWspZmienn[i] = Granice$OdchStandardowe[i]/Granice$Srednia[i]
Granice$PozWspZmienn[i] = Granice$Odch∆wiartkowe[i]/Granice$Mediana[i]
Granice$Skosnosc[i] = skewness(Dane[,i],type=3)
}
Statystyki <- transpose(Granice[,2:13])
colnames(Statystyki) <- colnames(Dane)

library(openxlsx)
write.xlsx(Granice,"Statystyki2.xlsx", colNames = TRUE)
#############################################HISTOGRAMY############################
l_klas <- round(sqrt(28),0)
#u≥oøenie labelkÛw - las
attach(Dane)

przedzia≥ <- seq(min(X1),max(X1),by=(max(X1)-min(X1))/l_klas)
par(mar=c(5,4.2,1,1))
hist(X1,
     main = "",
     border = "white",
     col = "#74bf0b",
     xlab = "WartoúÊ cechy X1",
     ylab = "Liczba paÒstw",
     ylim = c(0,8),
     xlim = c(10771,33228),
     las= 1,
     breaks = przedzia≥,
     xaxt = "n")
axis(1,at = przedzia≥,labels = round(przedzia≥,2))

#xaxt wywala x z bazowego histu, a axis dodaje labele z granicami przedzia≥Ûw