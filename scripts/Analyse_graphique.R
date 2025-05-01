library(readxl)
Excel_trafic_routier <- read_excel("data/raw/Excel - trafic routier.xlsx", col_names = TRUE, col_types = NULL) 

View(Excel_trafic_routier)

# Création de la série temporelle
trafic_routier <- ts(Excel_trafic_routier[,2], start=c(2001,01),end=c(2024,09),frequency=12)

# Graphique chronologique de la série
plot(trafic_routier,main="Trafic routier mensuel sur routes nationales",xlab="t",ylab="milliards de voitures-kilomètres")

# Décomposition de la série temporelle pour isoler sa tendance, saisonnalité et aléatoire
decomp.x=decompose(trafic_routier,type="multiplicative")
decomp.x$figure
plot(decomp.x)

# Corrélogrammes
layout(matrix(1:2,1,2))
acf(c(trafic_routier),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan", main=element_blank())
acf(c(trafic_routier),lag.max= 36, type = c("partial"),plot = TRUE, col="red",main=element_blank())


# Transformation de la série pour obtenir ses variations
trafic_routier_diff1=diff(trafic_routier,lag=1,differences=1)

# Transformation pour supprimmer les variations saisonnières de la série
trafic_routier_diff1_12=diff(trafic_routier_diff1,lag=12,differences=1)

# ACF et PACF de toutes les séries
layout(matrix(1:6,3,2))
acf(c(trafic_routier),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(trafic_routier_diff1),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(trafic_routier_diff1_12),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(trafic_routier),lag.max= 36, type = c("partial"),plot = TRUE, col="red", main=element_blank())
acf(c(trafic_routier_diff1),lag.max= 36, type = c("partial"),plot = TRUE, col="red", main=element_blank())
acf(c(trafic_routier_diff1_12),lag.max= 36, type = c("partial"),plot = TRUE, col="red", main=element_blank())