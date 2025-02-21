library(readxl)
Excel_trafic_routier <- read_excel("data/raw/Excel - trafic routier.xlsx", col_names = TRUE, col_types = NULL) 

View(Excel_trafic_routier)

#création de la série temporelle
trafic_routier <- ts(Excel_trafic_routier[,2], start=c(2001,01),end=c(2024,09),frequency=12)

# graphique chronologique de la série
plot(trafic_routier,main="Trafic routier mensuel sur routes nationales",xlab="t",ylab="milliards de voitures-kilomètres")

# décomposition de la série temporelle pour isoler sa tendance, saisonnalité et aléatoire
decomp.x=decompose(trafic_routier,type="multiplicative")
decomp.x$figure
plot(decomp.x)

#transformation en log pour corriger l'éventuelle croissance e la saisonnalité
log_tr <- log(trafic_routier
              )
# corrélogrammes
layout(matrix(1:2,1,2))
acf(c(log_tr),lag.max= NULL, type = c("correlation"),plot = TRUE, col="cyan", main=element_blank())
acf(c(log_tr),lag.max= NULL, type = c("partial"),plot = TRUE, col="red",main=element_blank())


#transformation de la série pour obtenir ses variations
log_tr_diff1=diff(log_tr,lag=1,differences=1)

#transformation pour supprimmer les variations saisonnières de la série
log_tr_diff1_12=diff(log_tr_diff1,lag=12,differences=1)

#ACF et PACF de toutes les séries
layout(matrix(1:6,3,2))
acf(c(log_tr),lag.max= NULL, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(log_tr_diff1),lag.max= NULL, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(log_tr_diff1_12),lag.max= NULL, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(log_tr),lag.max= NULL, type = c("partial"),plot = TRUE, col="red", main=element_blank())
acf(c(log_tr_diff1),lag.max= NULL, type = c("partial"),plot = TRUE, col="red", main=element_blank())
acf(c(log_tr_diff1_12),lag.max= NULL, type = c("partial"),plot = TRUE, col="red", main=element_blank())
