x = c(1.2, 9, 3.14)
y = list(TRUE, 1, "a")
dim(mtcars)
colSums(is.na(mtcars)) #mencari apakah ada data yang valuenya kosong
str(mtcars) #melihat struktur data


b=matrix(c(2,4,3,1,5,7), nrow=3)
c=matrix(c(7,4,2), nrow=3)
d=matrix(c(6,2),nrow=1)
e=cbind(b,c)
f=rbind(b,d)
colnames(e)=c("alpha","gamma","beta")
sub_mtcars=mtcars[ ,1:3]
sub_mtcars2=mtcars[1:3, ]
install.packages("foreign")
install.packages("REdaS")
install.packages("psych")
install.packages("MVN")
install.packages("biotools")
install.packages("MASS")
install.packages("caret")
install.packages("rrcov")
install.packages("klaR")
install.packages("cluster")

?integer
rm(list=ls())
x=c(1,2,3,4,5)
y=c(1,2,3,4,5)
z=list(TRUE,3.56,"alpha",2+4i,3)
w=rbind(x,y)
q=cbind(x,y)

getwd()
#setwd() buat ganti default
hbat <- read.csv("D:/Kharismawan/Workshop Statistika Multivariat dengan R - 110317/hbat.csv",header=TRUE) #import data
str(hbat)
library(car)
scatterplotMatrix(hbat[8:15])
library(MVN) # tes asumsi normalitas
uniNorm(hbat[8:20],type="SW",desc=TRUE) #shapiro-wilk
uniNorm(hbat[8:20],type="Lillie",desc=FALSE) # Kolmogorov-Smirnov
library(biotools)
boxM(hbat[8:20],grouping=hbat$x1) #uji heteroskedastisitas
#karena p value kecil (padahal harus diatas 0,05), maka perseberan antar variabel independen tidak sama, maka hapus yang normalitasnya NO pas di normalitas
#Kita akan Data clean
cleancol=c(10,11,12,13,16,17,20) #hapus kolom ke-segini
clean_hbat=hbat[ ,cleancol] # subset dari hbat yang udah sebagian dibuang
boxM(clean_hbat,grouping=hbat$x1)
#sudah diatas 0,05 maka H0 diterima, ada homoskedastisitas di clean_hbat
library(MASS) #analisis diskriminan
hbat.lda=lda(x1~x8+x9+x10+x11+x14+x15+x18,data=hbat)
hbat.lda
library(caret) #melihat fungsi diskriminan
hbat.lda.predict=train(x1~x8+x9+x10+x11+x14+x15+x18,method="lda",data=hbat) #latih data
confusionMatrix(hbat$x1, predict(hbat.lda.predict)) #coba prediksi / melihat fungsi diskriminan
library(rrcov) 
Wilks.test(x1~x8+x9+x10+x11+x14+x15+x18,data=hbat) #seberapa baik variabel independen berkontribusi terhadap model
hbat.lda.values=predict(hbat.lda)
ldahist(hbat.lda.values$x[,1],g=hbat$x1) #untuk grup 1
ldahist(hbat.lda.values$x[,2],g=hbat$x1) #untuk grup 2
library(klaR)
partimat(x1~x6+x9+x11, data=hbat, method="lda")
plot(hbat.lda.values$x[,1], hbat.lda.values$x[,2],col=hbat$x1)
newdata2=data.frame(type=hbat[,3], lda=hbat.lda.values$x) 
ggplot(newdata2) + geom_point(aes(lda.LD1, lda.LD2, colour=type, shape=type), size=2.5)


#ANALISIS FAKTOR


library(foreign)
newdata=read.spss(file="Stock Price.sav", to.data.frame = TRUE, use.value.labels = TRUE)
newdata_z=as.data.frame(scale(newdata[2:24])) #NORMALISASI DATA
head(newdata_z)                              
colSums(is.na(newdata_z))

library(REdaS)
KMOS(newdata_z,use="complete.obs") #kecukupan sampel
#karena as_sell <0,5 maka harus didrop;
newdata_z=newdata_z[c(-5)]
KMOS(newdata_z,use="complete.obs")
#pas dicoba lagi semuanya oke setelah drop yang variabel 0,5!!

bart_spher(newdata_z, use="complete.obs") #meguji validitas dan kesesuaian data yang kita punya
#karena p value <0,5 maka valid!!

fact_stock=princomp(na.omit(newdata_z),cor=TRUE) #membuat model faktor analisis na omit buat drop

plot(fact_stock,type="lines") #ngeplot berapa faktor yang dihasilkan, yang melebihi 1 maka diambil (biasanya...)

fact_stock$loadings

library(psych)
princ_stock=principal(na.omit(newdata_z),nfactors=5,rotate="varimax") #motong jadi berapa faktor, maksimisasi variance of squared loadnis 
princ_stock
princ_stock$communality
print(princ_stock$loadings, cutoff=0.5) #hasil penggabungan jadi 5 attribut aja


# CLUSTERING

wine=read.table("wine.data", header = TRUE, sep=",")
head(wine)

#PAKE CARA AGLOROMERATIVE ALIAS WARD
d=dist(wine,method="euclidian")
fit = hclust(d,method="ward.D")
plot(fit)
group=cutree(fit,k=5)
rect.hclust(fit,k=5,border="red")


#PAKE CARA K MEANS
wine_stand<-scale(wine[-1])
k.means.fit=kmeans(wine_stand,3)
k.means.fit$centers

#CARA MELIHAT JUMLAH CLUSTER YANG IDEAL (LIAT ELBOWNYA YANG PALING PARAH)
wssplot<-function(data, nc=15, seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)}
  plot(1:nc,wss,type="b",xlab="Numbers of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine_stand,nc=6)

library(cluster)
clusplot(wine_stand, k.means.fit$cluster, main='2D representation of the cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)