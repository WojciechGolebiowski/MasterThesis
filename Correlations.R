library(corrplot)
library(Hmisc)
Korelacje2 <- cor(Dane,method = "pearson")
Korelacje2
#KORELOGRAM
par(mar=c(0,0,0,0))
pairs(Dane,
      main = "",
      cex = 3.5,
      cex.labels = 4,
      cex.axis = 1.45,
      col = "blue",
      gap = 0.35,
      las = 1,
      pch = 20)
###################################KORELACJE BEZ D£ ¯YCIA:

corrplot(Korelacje2,
         mar=c(1,1,0.5,3),
         method = "color",
         tl.col="black",
         diag=TRUE,
         tl.cex = 1,
         addgrid.col="black",
         outline=FALSE,
         col = colorRampPalette(c("#ff0008", "#fceb2b", "#15b509"))(10))

#KORELACJE DLA TYLKO ISTOTNYCH ZMIENNYCH

kor_test2 <- rcorr(as.matrix(Dane))
p_mat2 <- kor_test2$P

corrplot(Korelacje2,
         mar=c(1,1,0.5,3),
         method = "number",
         tl.col="black",
         number.cex = 0.75,
         diag=TRUE,
         tl.cex = 1,
         addgrid.col="black",
         outline=FALSE,
         col = colorRampPalette(c("#ff0008", "#fceb2b", "#15b509"))(20),
         p.mat = p_mat2, sig.level = 0.05, insig = "blank")


# SHAPIRO WILK ------------------------------------------------------------
#jeœli wiêksze od 0.5 to normalne
shapiro.test(Dane$X1) #1
shapiro.test(Dane$X2)
shapiro.test(Dane$X3) #1
shapiro.test(Dane$X4) #1
shapiro.test(Dane$X5)
shapiro.test(Dane$X6) #1
shapiro.test(Dane$X7)
shapiro.test(Dane$X8)
shapiro.test(Dane$X9)
shapiro.test(Dane$X10)


# Spearman ----------------------------------------------------------------
Spear <- cor(Dane,method = "spearman")
Spear

corrplot(Spear,
         mar=c(1,1,0.5,3),
         method = "color",
         tl.col="black",
         diag=TRUE,
         tl.cex = 1,
         addgrid.col="black",
         outline=FALSE,
         col = colorRampPalette(c("#ff0008", "#fceb2b", "#15b509"))(10))


spear_test <- rcorr(as.matrix(Dane),type = "spearman")
sp_mat2 <- spear_test$P
#o-value ni¿sze ni¿ 0.05 oznacza ¿e istotne cor.test
corrplot(Spear,
         mar=c(1,1,0.5,3),
         method = "number",
         tl.col="black",
         number.cex = 0.7,
         diag=TRUE,
         tl.cex = 1,
         addgrid.col="black",
         outline=FALSE,
         col = colorRampPalette(c("#ff0008", "#fceb2b", "#15b509"))(10),
         p.mat = sp_mat2, sig.level = 0.05, insig = "blank")


# spear dla HDI i m -------------------------------------------------------

library(readxl)
Temp <- read_excel("C:/Users/Wojtek/Desktop/DANE MGR/Combined.xlsx", 
                       sheet = "HDI UE")
HDI <- data.frame(Temp[,3:5],row.names = Temp$Kraj)

HDIm <- cor(HDI$HDI,HDI$M,method = "spearman")
HDIm
HDIm2 <- cor(HDI$HDI,HDI$mi,method = "spearman")
HDIm2

cor.test(HDI$HDI,HDI$M,method = "spearman")
cor.test(HDI$HDI,HDI$mi,method = "spearman")
