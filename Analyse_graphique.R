library(readxl)
Excel_trafic_routier <- read_excel("data/raw/Excel - trafic routier.xlsx", col_names = TRUE, col_types = NULL) 

View(Excel_trafic_routier)

trafic_routier <- ts(Excel_trafic_routier[,2], start=c(2001,01),end=c(2024,09),frequency=12)


plot(trafic_routier,main="Trafic routier mensuel sur routes nationales",xlab="t",ylab="milliards de voitures-kilomètres")

