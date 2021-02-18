dir()
# beolvasás
m2001 = read.table("202001.txt",head=T,sep="\t", dec=",", skip=17)
str(m2001)
m2001csak = m2001[,c(2:4,6,11,13)]
colnames(m2001csak)=c("lh","rp","w","wi","cs","s")


# idõ
m2001ido = m2001[,1]
m2001iP = strptime(m2001ido, "%Y.%m.%d %H:%M")

plot(m2001iP, m2001csak$lh , type="l")

# xts
library(xts)
m2001.xts = xts(m2001csak, m2001iP)
plot(m2001.xts$lh)

filename = "202001.txt"
aktual = read.table(filename, head=F, sep="\t", dec=",", skip=17, na.strin = "-999,9")[,c(1:4,6,11,13)]
colnames(aktual)=c("ido","lh","rp","w","wi","cs","s")
aktido = as.POSIXct(strptime(aktual[,1], "%Y.%m.%d %H:%M"))
akt.xts = xts(aktual[,-1], aktido)
telj.xts = akt.xts

# fileall = c("201905.txt","201906.txt","201907.txt")
fileall = dir(patt="^2")[-1]

for(filename in fileall) {
   aktual = read.table(filename, head=T, sep="\t", dec=",", skip=17, na.strin = "-999,9")[,c(1:4,6,11,13)]
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
napi = apply.daily(telj.xts$lh, mean, na.rm=T)
havi = apply.monthly(telj.xts$lh, mean, na.rm=T)

plot(napi)
plot(havi)

index(telj.xts)[diff(index(telj.xts)) > 600]

write.table(round(napi, 2), "napi.txt", sep = "\t", dec = ",", row.name = F)
write.table(round(havi, 2), "havi.txt", sep = "\t", dec = ",", row.name = F)
