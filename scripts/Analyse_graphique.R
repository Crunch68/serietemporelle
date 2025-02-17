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

# corrélogrammes
layout(matrix(1:2,1,2))
acf(c(trafic_routier),lag.max= NULL, type = c("correlation"),plot = TRUE, col="cyan", main=element_blank())
acf(c(trafic_routier),lag.max= NULL, type = c("partial"),plot = TRUE, col="red",main=element_blank())
