dir()
# beolvasás
m1904 = read.table("201904.txt",head=T,sep="\t", dec=",")
str(m1904)
m1904csak = m1904[,c(2:4,6,11,13)]
colnames(m1904csak)=c("lh","rp","w","wi","cs","s")


# idõ
m1904ido = m1904[,1]
m1904iP = strptime(m1904ido, "%Y.%m.%d %H:%M")

plot(m1904iP, m1904csak$lh , type="l")

# xts
library(xts)
m1904.xts = xts(m1904csak, m1904iP)
plot(m1904.xts$lh)

filename = "201904.txt"
aktual = read.table(filename, head=T, sep="\t", dec=",", na.strin = "-999,9")[,c(2:4,6,11,13)]
colnames(aktual)=c("lh","rp","w","wi","cs","s")
aktido = as.POSIXct(strptime(m1904ido, "%Y.%m.%d %H:%M"))
akt.xts = xts(aktual, aktido)
telj.xts = akt.xts

# fileall = c("201905.txt","201906.txt","201907.txt")
fileall = dir(patt="^2")[-1]

for(filename in fileall) {
   aktual = read.table(filename, head=T, sep="\t", dec=",", na.strin = "-999,9")[,c(1:4,6,11,13)]
   aktidochr = aktual[,1]
   aktual = aktual[,-1]
   colnames(aktual)=c("lh","rp","w","wi","cs","s")
   ## Számformátum alakítás
   aktual$s = as.numeric(sub(" ","",sub(",",".",aktual$s)))
   aktido = as.POSIXct(strptime(aktidochr, "%Y.%m.%d %H:%M"))
   akt.xts = xts(aktual, aktido)
   telj.xts = c(telj.xts, akt.xts)
}

# plot hõmérséklet
plot(telj.xts$lh)
# napi átlag
napi = apply.daily(telj.xts$lh, mean)
havi = apply.monthly(telj.xts$lh, mean)

plot(napi)

index(telj.xts)[diff(index(telj.xts)) > 600]