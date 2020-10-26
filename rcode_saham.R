setwd("c:/RMFR/materi_26102020/rcode/")
saham <- read.table("saham.txt",header=TRUE)
price <- lm(price~pe+eps+roi+roe+bv, data=saham)
summary(price)

# Uji asumsi multikolinieritas 
library(car)
vif(price)

# Uji asumsi heteroskedastisitas
library(lmtest)
bptest(price, studentize=FALSE, data=saham)

# Uji asumsi autokorelasi
library(lmtest)
dwtest(price)
bgtest(price, order=6)

# diagnosa kenormalan residual
par(mfrow=c(2,2))
plot(price,which=c(1:4))




