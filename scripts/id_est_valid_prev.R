# Librairies utiles
library(readxl)
library(tsoutliers)
library(forecast)
library(trend)
library(lmtest)
library(urca)
library(Metrics)
library(TSstudio)
library(CADFtest)
library(FinTS)
library(tseries)
library(DescTools)

# Séries utiles
Excel_trafic_routier <- read_excel("data/raw/Excel - trafic routier.xlsx", col_names = TRUE, col_types = NULL) 

trafic_routier <- ts(Excel_trafic_routier[,2], start=c(2001,01),end=c(2024,09),frequency=12)
trafic_routier_diff1=diff(trafic_routier,lag=1,differences=1)



# 1 - Procédure de test de stationnarité sur la série brute
  # Test de Cox & Stuart
    cs.test(trafic_routier) #rejet de H0, la série a une tendance
    
  # Tests seront donc effectués sur des modèles avec tendance
  
  # Test ADF
    # Sélection du lag optimal
      # Basé sur les critères AIC, MAIC, BIC, HQC
        T <- length(trafic_routier)
        pmax <- as.integer(12*(T/100)^(0.25))
        adf <- CADFtest(trafic_routier,criterion="MAIC",type="trend",max.lag.y=pmax)
        summary(adf) # Convergence vers un lag 13
    adftest <- ur.df(trafic_routier, type = c("trend"), lags = 13)
    summary(adftest) # Présence d'au moins une racine unitaire
        
  # Test de Phillips-Perron
    pptest <- ur.pp(trafic_routier, type="Z-tau", model="trend", lags="short")
    summary(pptest) # Série ne possède pas de racine unitaire (-7.1286 < -3.427043)
  
  # Test KPSS
    testKPSStau <- ur.kpss(trafic_routier, type="tau")
    summary(testKPSStau) # Série n'est pas stationnaire (0.171>0.146)

# Tests sont contradictoires, ce qui peut être causé par la présence d'une racine saisonnière
  


# 2 - Tests de stationnarité sur la série en niveau désaisonnalisée
  # Désaisonnalisation
  t_r<-decompose(trafic_routier) 
  plot(t_r)
  plot(t_r$trend)
    # Cette méthode de désaisonnalisation est ici préférée pour retirer les valeurs aberrantes exogènes correspondant au Covid et pouvant fausser les tests
  
  # Suppression des valeurs ommises (à cause des moyennes mobiles)
  trafic_routier_trend<-na.omit(t_r$trend)
  plot(trafic_routier_trend, main = "Série en niveau désaisonnalisée")
  
  # Test ADF
    # Sélection du lag optimal
      # Basée sur les critères AIC, MAIC, BIC, HQC
      T <- length(trafic_routier_trend)
      pmax <- as.integer(12*(T/100)^(0.25))
      adf <- CADFtest(trafic_routier_trend, criterion="AIC", type="trend", max.lag.y=pmax)
      summary(adf) # Convergence vers un lag 15 (avec l'ajout de la méthode GTOS)
  
    adftest <- ur.df(trafic_routier_trend,type=c("trend"),lags=15)
    summary(adftest) # Présence d'au moins une racine unitaire (-2.806 > -3.42)
  
  # Test de Phillips-Perron
  pptest <- ur.pp(trafic_routier_trend, type="Z-tau", model="trend", lags="short")
  summary(pptest) # Série  possède au moins une racine unitaire (-2.4854 < -3.42769) 
  
  # Test KPSS
  testKPSStau <- ur.kpss(trafic_routier_trend, type="tau")
  summary(testKPSStau) # Série n'est pas stationnaire (0.4633 > 0.146)

# Convergence des tests sur le fait que la série en niveau désaisonnalisée ne soit pas I(O)


  
# 3 - Tests de stationnarité sur la série en différences premières désaisonnalisées
trafic_routier_diff1_trend<-diff(trafic_routier_trend)
acf(c(trafic_routier_diff1_trend),lag.max=36) # Série stationnaire
pacf(c(trafic_routier_diff1_trend),lag.max=36) # Série stationnaire

  # Test de Cox-Stuart
  cs.test(trafic_routier_diff1_trend) # Absence de tendance
  plot(trafic_routier_diff1_trend)
  summary(trafic_routier_diff1_trend) # Moyenne centréé en 0
  
  # Tests ne seront plus effectués sur des modèles sans tendance (ni constante pour l'ADF)

  # Test ADF
    # Sélection du lag optimal
      # Basée sur les critères AIC, MAIC, BIC, HQC
      T <- length(trafic_routier_diff1_trend)
      pmax <- as.integer(12*(T/100)^(0.25))
      adf <- CADFtest(trafic_routier_diff1_trend, criterion="AIC", type="none", max.lag.y=pmax)
      summary(adf) # Convergence vers un lag 15 (avec l'ajout de méthode GTOS)

    adftest <- ur.df(trafic_routier_diff1_trend,type=c("none"),lags=15)
    summary(adftest) # Absence de racine unitaire (-4.9242 > -1.95)

  # Test de Phillips-Perron
  pptest <- ur.pp(trafic_routier_diff1_trend, type="Z-tau", model="constant", lags="short")
  summary(pptest) # Absence de racine unitaire (-5.2461 < -2.872317)

  # Test KPSS
  testKPSSmu <- ur.kpss(trafic_routier_diff1_trend, type="mu")
  summary(testKPSSmu) # Série est stationnaire (0.0981 < 0.463)

# Convergence des tests : série en niveau désaisonnalisée est I(1)
  # Or, pour que la série en niveau soit stationnaire il faut qu'elle soit différenciée à l'ordre 1 et 12
    # Donc, la série en niveau possède 1 racine unitaire classique et 1 racine unitaire saisonnière
      # On estime donc : d=1 et D=1


# 4 - Procédure d'estimation automatique d'un modèle SARIMA
  # Estimation du modèle
    modele_sarima<-auto.arima(trafic_routier, seasonal=TRUE, approximation=TRUE, d=1, D=1)
    
    summary(modele_sarima) # Modèle : (1,1,2)(0,1,2)[12]
    coeftest(modele_sarima) # Toutes les variables sont significatives
  
  # Estimation des résidus
  checkresiduals(modele_sarima) # Présence de valeurs aberrantes correspondant au Covid
  acf(c(modele_sarima$residuals),lag.max=36) # Résidus auto-corrélés
  pacf(c(modele_sarima$residuals),lag.max=36) # Résidus auto-corrélés
  jarque.bera.test(residuals(modele_sarima)) # Résidus très anormalement distribués (p-value très inférieure à 5%)
  ArchTest(residuals(modele_sarima), lags = 12, demean = TRUE) # Résidus très hétéroscédastiques (p-value très inférieure à 5%)

# Le modèle ainsi estimé n'est pas validé
  # Nous allons donc tenté de retirer les valeurs aberrantes


  
# 5 - Estimation automatique d'un modèle SARIMAX (en tenant compte des outliers)
  # Détection automatique des outliers
  outliers_detected <- tso(trafic_routier, types = c("AO", "LS", "TC"),maxit.iloop = 50)

  # Définir les outliers manuels
  outliers_manuel_indices <- c(134, 136, #février 2012 a été un mois exceptionnellement froid
                             231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246) #Covid 
  
  # Fusionner outliers automatiques + manuels
    # Extraire indices automatiques
    if (nrow(outliers_detected$outliers) > 0) {
      indices_outliers_auto <- outliers_detected$outliers$ind
    } else {
      indices_outliers_auto <- integer(0)  # vide
    }
    
    # Fusionner sans doublons
    indices_total <- sort(unique(c(indices_outliers_auto, outliers_manuel_indices)))
  
  # Créer la matrice de dummies
  if (length(indices_total) > 0) {
    
    dummy_matrix <- matrix(0, nrow = length(trafic_routier), ncol = length(indices_total))
    colnames(dummy_matrix) <- paste0("dummy_", seq_len(length(indices_total)))
    
    for (i in seq_along(indices_total)) {
      pos <- indices_total[i]
      if (!is.na(pos) && pos >= 1 && pos <= length(trafic_routier)) {
        dummy_matrix[pos, i] <- 1
      } else {
        warning(paste(" Outlier détecté hors limites : position", pos))
      }
    }
    
  } else {
    dummy_matrix <- NULL
  }
  
  # Visualisation rapide des dummies
  if (!is.null(dummy_matrix)) {
    matplot(dummy_matrix, type = "l", main = "Variables dummies pour outliers (auto + manuels)", ylab = "Dummy", col = 1:ncol(dummy_matrix))
  }
  
  # Estimation automatique du modèle SARIMA corrigé
    modele_sarima_auto_corrected <- auto.arima(trafic_routier, seasonal = TRUE, d=1, D=1, xreg = dummy_matrix, approximation = TRUE)
  
  summary(modele_sarima_auto_corrected) # Modèle : (0,1,1)(0,1,1)[12]
    # AIC et BIC se sont très nettement améliorés
  coeftest(modele_sarima_auto_corrected) # Ensemble des variables sont significatives
    
  # Diagnostic des résidus
  checkresiduals(modele_sarima_auto_corrected) # Plus de valeurs réellement aberrantes
  acf(c(modele_sarima_auto_corrected$residuals),lag.max=36) # Résidus auto-corrélés
  pacf(c(modele_sarima_auto_corrected$residuals),lag.max=36) # Résidus auto-corrélés  
  jarque.bera.test(residuals(modele_sarima_auto_corrected)) # Résidus normaux (valeur supérieure à 5%)
  ArchTest(residuals(modele_sarima_auto_corrected), lags = 12, demean = TRUE) # Résidus hétéroscédastiques (p-value < 0.05)

# Le modèle n'est pas validé mais représente une très nette amélioration
  
  
  
# 6 - Estimation manuelle d'un modèle SARIMAX
  # Amélioration du modèle automatique
  fixed_vector <- c(
    # AR(1–35)
    rep(0, 21),  # ar1–ar21 (forcés à 0)
    NA,      # ar22 (estimé)
    rep(0, 11), # ar23-ar33 (forcés à 0)
    NA,      # ar34
    NA,      # ar35
     # MA(1)
    NA, 
    # SMA(1)
    NA,     
    # xreg : tous à estimer
    rep(NA, 18))
  
  modele_sarima_corrected<- Arima(trafic_routier, order=c(35,1,1), seasonal=list(order=c(0,1,1),period=12), fixed=fixed_vector, xreg=dummy_matrix, include.constant = TRUE, transform.pars = TRUE)
  
  summary(modele_sarima_corrected) # Modèle : ARIMA(35,1,1)(0,1,1)[12] (mais avec uniquement un AR(22, 34 et 35))
    # Meilleurs AIC et BIC
  coeftest(modele_sarima_corrected) # Ensemble des variables sont significatives
  
  # Diagnostic des résidus
  checkresiduals(modele_sarima_corrected)
  acf(c(modele_sarima_corrected$residuals), lag.max=36) # Résidus bruit blanc
  pacf(c(modele_sarima_corrected$residuals), lag.max=36) # Résidus bruit blanc
  jarque.bera.test(residuals(modele_sarima_corrected)) # Résidus normaux (valeur supérieure à 5%)
  ArchTest(residuals(modele_sarima_corrected), lags = 12, demean = TRUE) # Résidus hétéroscédastiques (p-value < 0.05)

# Le modèle est validé, malgré la légère hétéroscédasticité des résidus que nous ne savons pas traiter
    
  

# 7 - Prévision sur les 11 derniers mois
  # Tronquage des 11 derniers mois de la série
  n_total <- length(trafic_routier)
  horizon <- 11
  
  # Partie estimation (jusqu'à t - 11)
  y_train <- window(trafic_routier, end = time(trafic_routier)[n_total - horizon])
  
  # Partie validation (vraies valeurs à comparer)
  y_test <- window(trafic_routier, start = time(trafic_routier)[n_total - horizon + 1])
  
  # Extraire les xreg correspondants
  xreg_train <- dummy_matrix[1:(n_total - horizon), ]
  xreg_test <- dummy_matrix[(n_total - horizon + 1):n_total, ]
  
  # Estimation du modèle SARIMAX sur y_train
  model <- Arima(
    y_train,
    order = c(35, 1, 1),
    seasonal = list(order = c(0, 1, 1), period = 12),
    fixed = fixed_vector,
    xreg = xreg_train)
  
  summary(model)
  coeftest(model) # Toutes les variables sont significatives
  
  # Estimation des résidus
  checkresiduals(model)
  acf(c(model$residuals),lag.max=36) # Résidus bruit blanc
  pacf(c(model$residuals),lag.max=36) # Résidus bruit blanc
  jarque.bera.test(residuals(model)) # Résidus normaux
  ArchTest(residuals(model), lags = 12, demean = TRUE) # Résidus hétéroscédastiques (p-value < 0.05)
  
  # Prévision sur les 11 derniers mois
  forecast_obj <- forecast(model, h = horizon, xreg = xreg_test)
  
  # Graphique : Observé vs Prédicton + IC
    # Zoom sur les 12 derniers mois
    start_zoom <- time(trafic_routier)[n_total - 11]
    end_zoom <- time(trafic_routier)[n_total + horizon - horizon]
    
    # Réprésentation graphique
    plot(trafic_routier,
      main = "Prévision sur les 11 derniers mois (zoom)",
      xlab = "Temps",
      ylab = "Valeur",
      col = "black",
      xlim = c(start_zoom, end_zoom),
      ylim = c(3.5, 5))
    
    lines(forecast_obj$mean, col = "red", lwd = 2)
    lines(forecast_obj$upper[, 2], col = "blue", lty = 2)
    
    lines(forecast_obj$lower[, 2], col = "blue", lty = 2)
    
    legend("topleft",
           legend = c("Observations", "Prévisions", "Intervalle 95%"),
           col = c("black", "red", "blue"),
           lty = c(1, 1, 2),
           bty = "n")
  
  # Évaluation de la prévision
  cat("Évaluation de la prévision sur 11 mois :")
  cat("RMSE  :", rmse(forecast_obj$mean, tail(trafic_routier, n = 11))) # Ecarts absolus moyens entre les prévisions et la réalité sont très faibles (0.0663)
  cat("MAPE  :", mape(forecast_obj$mean, tail(trafic_routier, n = 11))) # Ecarts en pourcentages moyens entre les prévisions et la réalité sont très faibles (0.0134 ou 1,34%)
  cat("TheilU:", TheilU(forecast_obj$mean, tail(trafic_routier, n = 11))) # Très proche de 0 : modèle beaucoup plus performant que le modèle naïf où la valeur future est égale à celle passée (0.0156)

# Prévision correcte (valeurs réelles sont toutes dans l'intervalle de confiance à 95% et les écarts moyens sont très faibles)



# 8 - Prévision sur les 12 prochains mois
  horizon <- 12
  
  # Utilise dynamiquement le même nombre de colonnes que dummy_matrix
  future_dummy <- matrix(0, nrow = horizon, ncol = ncol(dummy_matrix))
  colnames(future_dummy) <- colnames(dummy_matrix)  # en cas de noms utilisés
  
  # Vérification avant forecast
  stopifnot(ncol(future_dummy) == ncol(dummy_matrix))
  
  # Prévision
  forecasts <- forecast(
    modele_sarima_corrected,
    xreg = future_dummy,
    h = horizon)
  
  # Définir les bornes de l’axe des abscisses pour inclure les 12 mois futurs
  start_time <- start(trafic_routier)
  end_time <- time(tail(trafic_routier, 1)) + 12 / frequency(trafic_routier)
  
  # Tracé complet avec dézoom
  plot(trafic_routier,
    main = "Prévision sur les 12 prochains mois",
    xlab = "Temps",
    ylab = "Valeur",
    xlim = c(start_time[1], end_time),
    col = "black")
  
  # Ajouter les lignes de prévision et intervalles de confiance
  lines(forecasts$mean, col = "red", lwd = 2)
  lines(forecasts$upper[, 2], col = "blue", lty = 2)
  lines(forecasts$lower[, 2], col = "blue", lty = 2)
  
  # Légende
  legend("topleft",
         legend = c("Prévision", "Intervalle 95%"),
         col = c("red", "blue"),
         lty = c(1, 2),
         bty = "n")