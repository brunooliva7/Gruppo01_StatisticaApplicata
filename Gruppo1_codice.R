#GRUPPO 1 STATISTICA APPLICATA 
#COMPONENTI: Elisa Lombardi, Bruno Oliva, Christian Risi, Giacomo Rossino

library(corrplot)

dati <- read.csv(file.choose())


#1 : Analisi Preliminare, statistica Descrittiva e Analisi di Correlazione

#1.1 Statistica descrittiva

print(" STATISTICHE DESCRITTIVE ")
print(summary(dati)) 
deviazioni <- sapply(dati, sd) 
print(deviazioni)

hist(dati$y_IQ, freq=FALSE, main="Istogramma Image Quality (IQ)", 
     xlab="Image Quality (IQ)", ylab="Densità", col="lightblue")
lines(density(dati$y_IQ), lwd=2, col="red") 

windows(width = 14, height = 7)
par(mfrow=c(2,4))           
vars <- names(dati)         
for(i in 1:length(vars)) {                 
  boxplot(dati[[i]], main=vars[i], col="lightblue", varwidth=TRUE, notch=TRUE) 
}
par(mfrow=c(1,1))
dev.off()

print("ANALISI DI CORRELAZIONE")

cor_matrix <- cor(dati)

round(cor_matrix, 2)

# Apri una finestra grande
windows(width = 10, height = 10) 

# Genera il grafico misto (numeri sotto, ellissi sopra)
corrplot.mixed(cor_matrix, 
               lower = "number", 
               upper = "ellipse", 
               tl.pos = "lt",    # Posiziona le etichette di testo
               number.cex = 0.9, # Grandezza dei numeri
               tl.col = "black") # Colore del testo
#GENERAZIONE PRIMO FILE SCATTER SEMPLICE 
png("Scatter_X_vs_Y.png", width = 1200, height = 600, res = 120)

par(mfrow = c(2, 4)) 

# Ciclo che va dalla colonna 2 alla colonna 8 (le tue X)
for(i in 2:ncol(dati)) {
  # dati[, i] prende i valori della colonna X attuale
  # dati[, 1] prende i valori della colonna Y (la prima)
  plot(dati[, i], dati[, 1], 
       pch = 19,                         
       col = rgb(0, 0, 1, 0.5),          
       xlab = names(dati)[i],            # Usa in automatico il nome della colonna i
       ylab = names(dati)[1],            # Usa in automatico il nome della colonna 1
       main = paste(names(dati)[1], "vs", names(dati)[i]))
  # 2. Calcola la retta di regressione lineare semplice per questa specifica colonna
  modello_semplice <- lm(dati[, 1] ~ dati[, i])
  
  # 3. Disegna la linea rossa sopra il grafico corrente prima di passare al successivo
  abline(modello_semplice, col = "red", lwd = 2)
}

par(mfrow = c(1, 1)) 
dev.off() 


# ------------------------------------------------------------------------------
# IMMAGINE 2: X vs X 
# ------------------------------------------------------------------------------
png("Scatter_X_vs_X.png", width = 1000, height = 1000, res = 120)

# Incrocia solo le colonne da 2 all'ultima
pairs(dati[, 2:ncol(dati)], 
      pch = 15,                          
      col = rgb(1, 0, 0, 0.4),           
      main = "Verifica Disegno Sperimentale ortogonale (X vs X)")

dev.off() 

print("Immagini generate con successo. Il problema delle lunghezze è stato aggirato!")
# In questo punto definiamo formalmente le strutture dei modelli che vogliamo 
# testare. Usiamo l'operatore I() per indicare a R di calcolare i termini quadratici 
# (elevati al quadrato) emersi dall'analisi precedente.

# --- MODELLO A: Modello puramente Lineare (Benchmark) ---
# Ipotizza che tutti e 7 i regressori abbiano un impatto esclusivamente lineare.
formula_Modello_A <- y_IQ ~ x1_ISO + x2_T + x3_MP + x4_CF + x5_F + x6_GSI + x7_UA


# --- MODELLO B: Modello Strutturale Parsimonioso (Scelta del Gruppo) ---
# Include i regressori significativi, esclude le ridondanze hardware (x4 e x5)
# e include l'effetto parabolico dell'altezza di volo (x7^2).
formula_Modello_B <- y_IQ ~ x1_ISO + x2_T + x3_MP + x6_GSI + x7_UA + I(x7_UA^2)


# --- MODELLO C: Modello Polinomiale Esteso ---
# Include TUTTI i termini quadratici che nella tabella descrittiva univariata 
formula_Modello_C <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x4_CF + x5_F + x6_GSI + I(x6_GSI^2) + x7_UA + I(x7_UA^2)


#MODELLO D---: incluse le curve quadratiche ma esclusa la ridondanza 
formula_Modello_D <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x4_CF + x6_GSI + I(x6_GSI^2) + x7_UA + I(x7_UA^2)


#MODELLO E----: proviamo ad inserire x5 invece di x4 
formula_Modello_E <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x5_F + x6_GSI + I(x6_GSI^2) + x7_UA + I(x7_UA^2)


# Stampa di verifica per confermare la corretta memorizzazione delle formule
print("--- Formule dei Modelli Definite con Successo ---")
print(formula_Modello_A)
print(formula_Modello_B)
print(formula_Modello_C)
print(formula_Modello_D)
print(formula_Modello_E)

print("====================================================================")
print("--- PUNTO 4: STIMA EMPIRICA DEI MODELLI E INTERVALLI DI CONFIDENZA ---")
print("====================================================================")

# 4.1 Esecuzione della stima OLS per i tre modelli candidati
stima_Modello_A <- lm(formula_Modello_A, data = dati)
stima_Modello_B <- lm(formula_Modello_B, data = dati)
stima_Modello_C <- lm(formula_Modello_C, data = dati)
stima_Modello_D <- lm(formula_Modello_D, data = dati)
stima_Modello_E <- lm(formula_Modello_E, data = dati)

# 4.2 Visualizzazione dei Summary per il confronto metrico
print("--- STIMA COMPLETA: MODELLO A (LINEARE BASE) ---")
print(summary(stima_Modello_A))

print("--- STIMA COMPLETA: MODELLO B (STRUTTURALE OPT - SCELTA GRUPPO) ---")
summary_B <- summary(stima_Modello_B)
print(summary_B)

print("--- STIMA COMPLETA: MODELLO C (POLINOMIALE ESTESO) ---")
print(summary_C <- summary(stima_Modello_C))

print("--- STIMA COMPLETA: MODELLO D (POLINOMIALE SENZA CORRELAZIONI) ---")
print(summary_D <- summary(stima_Modello_D))

print("--- STIMA COMPLETA: MODELLO E (CON FOCALE, SENZA CROP FACTOR) ---")
print(summary(stima_Modello_E))

#--CONFRONTO TRAMITE BIC E AIC PER TROVARE IL MODELLO MIGLIORE----
# Estrazione dell'AIC per tutti i modelli candidati
aic_valori <- c(
  Modello_A = AIC(stima_Modello_A),
  Modello_B = AIC(stima_Modello_B),
  Modello_C = AIC(stima_Modello_C),
  Modello_D = AIC(stima_Modello_D),
  Modello_E = AIC(stima_Modello_E)
)
#Estrazione del BIC per tutti i modelli candidati
bic_valori <- c(
  Modello_A = BIC(stima_Modello_A),
  Modello_B = BIC(stima_Modello_B),
  Modello_C = BIC(stima_Modello_C),
  Modello_D = BIC(stima_Modello_D),
  Modello_E = BIC(stima_Modello_E)
)

print("--- VALORI AIC (Il modello migliore ha il valore più BASSO) ---")
print(round(aic_valori, 2))

print("--- VALORI BIC (Il modello migliore ha il valore più BASSO) ---")
print(round(bic_valori, 2))

# 4.4 DETERMINAZIONE DEGLI INTERVALLI DI CONFIDENZA DEI PARAMETRI (95%)
print("====================================================================")
print("--- DETERMINAZIONE INTERVALLI DI CONFIDENZA (LIVELLO 0.95) ---")
print("====================================================================")

print("--- INTERVALLI DI CONFIDENZA - MODELLO A ---")
print(confint(stima_Modello_A, level = 0.95))

print("--- INTERVALLI DI CONFIDENZA - MODELLO B ---")
print(confint(stima_Modello_B, level = 0.95))

print("--- INTERVALLI DI CONFIDENZA - MODELLO C ---")
print(confint(stima_Modello_C, level = 0.95))

print("--- INTERVALLI DI CONFIDENZA - MODELLO D ---")
print(confint(stima_Modello_D, level = 0.95))

print("--- INTERVALLI DI CONFIDENZA - MODELLO E ---")
print(confint(stima_Modello_E, level = 0.95))

print("=== FINE PUNTO 4 ===")


# ====================================================================
# --- PUNTO 5: COEFFICIENTE DI DETERMINAZIONE E DIAGNOSTICA DEI RESIDUI ---
# ====================================================================

# 5.1 Estrazione e confronto del Coefficiente di Determinazione
# Creiamo una tabella riassuntiva dei modelli principali
r_squared_valori <- c(
  Modello_A = summary(stima_Modello_A)$r.squared,
  Modello_B = summary(stima_Modello_B)$r.squared,
  Modello_C = summary(stima_Modello_B)$r.squared,
  Modello_D = summary(stima_Modello_D)$r.squared,
  Modello_E = summary(stima_Modello_B)$r.squared
)

adj_r_squared_valori <- c(
  Modello_A = summary(stima_Modello_A)$adj.r.squared,
  Modello_B = summary(stima_Modello_B)$adj.r.squared,
  Modello_C = summary(stima_Modello_B)$adj.r.squared,
  Modello_D = summary(stima_Modello_D)$adj.r.squared,
  Modello_E = summary(stima_Modello_B)$adj.r.squared
)

print("--- Multiple R-squared ---")
print(round(r_squared_valori, 5))

print("--- Adjusted R-squared (OTTIMALE PER IL CONFRONTO) ---")
print(round(adj_r_squared_valori, 5))

# 5.2 Grafici Diagnostici sui Residui del Modello Scelto (es. Modello D)
# Il comando plot() su un oggetto 'lm' genera automaticamente 4 grafici.

# --- Diagnostica Modello A ---
print("Generazione dei grafici diagnostici per il Modello A...")
png("Diagnostica_Residui_Modello_A.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_A) 

par(mfrow = c(1, 1)) 
dev.off()
print("Grafici diagnostici salvati con successo in 'Diagnostica_Residui_Modello_A.png'")


# --- Diagnostica Modello B ---
print("Generazione dei grafici diagnostici per il Modello B...")
png("Diagnostica_Residui_Modello_B.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_B) 

par(mfrow = c(1, 1)) 
dev.off()
print("Grafici diagnostici salvati con successo in 'Diagnostica_Residui_Modello_B.png'")


# --- Diagnostica Modello C ---
print("Generazione dei grafici diagnostici per il Modello C...")
png("Diagnostica_Residui_Modello_C.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_C) 

par(mfrow = c(1, 1)) 
dev.off()
print("Grafici diagnostici salvati con successo in 'Diagnostica_Residui_Modello_C.png'")

#--- Diagnostica Modello D ---
print("Generazione dei grafici diagnostici per il Modello D...")

png("Diagnostica_Residui_Modello_D.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2)) # Divide la finestra grafica in una griglia 2x2
plot(stima_Modello_D) # Genera i 4 plot diagnostici standard
par(mfrow = c(1, 1)) # Ripristina la finestra grafica standard
dev.off()


print("Grafici diagnostici salvati con successo in 'Diagnostica_Residui_Modello_D.png'")



# --- Diagnostica Modello E ---
print("Generazione dei grafici diagnostici per il Modello E...")
png("Diagnostica_Residui_Modello_E.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_E) 

par(mfrow = c(1, 1)) 
dev.off()
print("Grafici diagnostici salvati con successo in 'Diagnostica_Residui_Modello_E.png'")

print("--- Esecuzione Regressione Stepwise (Backward) ---")

# Eseguiamo l'algoritmo partendo dal Modello C
# direction = "backward" impone a R di partire da questo modello "pieno" e procedere per sottrazione
modello_stepwise_backward <- step(stima_Modello_C, direction = "backward")

print("--- Risultato Modello Ottimale Scelto dalla Stepwise ---")
# Visualizziamo le statistiche del modello "vincitore"
summary(modello_stepwise_backward)

print("AIC del modello Backward:")
AIC(modello_stepwise_backward)

print("--- Esecuzione Regressione Stepwise (Forward) ---")

# 1. Definiamo il modello base "vuoto" (solo intercetta)
modello_base <- lm(y_IQ ~ 1, data = dati) # Assicurati che il nome del dataframe sia corretto

# 2. Eseguiamo la stepwise in avanti
# scope = list(...) dice all'algoritmo il recinto in cui può muoversi:
# dal modello vuoto fino alla formula completa del Modello C
modello_stepwise_forward <- step(modello_base, 
                                 scope = list(lower = modello_base, upper = formula(stima_Modello_C)), 
                                 direction = "forward")

print("--- Risultato Modello Ottimale Scelto dalla Stepwise Forward ---")
summary(modello_stepwise_forward)

print("AIC del modello Forward:")
AIC(modello_stepwise_forward)
