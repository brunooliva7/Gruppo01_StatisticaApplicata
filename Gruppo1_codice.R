#GRUPPO 1 STATISTICA APPLICATA 
#COMPONENTI: Elisa Lombardi, Bruno Oliva, Christian Risi, Giacomo Rossino

library(corrplot)

<<<<<<< HEAD
dati <- read.csv(file.choose())
=======
dati <-  read.csv(file.choose())
>>>>>>> 20669ace2417ea4d6783aeff8bb2e750c8dc20dc

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
# (pag. 6 della relazione) erano risultati significativi (x2^2, x6^2, x7^2),
# mantenendo anche gli altri regressori per verificarne l'effetto combinato.
formula_Modello_C <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x4_CF + x5_F + x6_GSI + I(x6_GSI^2) + x7_UA + I(x7_UA^2)


# Stampa di verifica per confermare la corretta memorizzazione delle formule
print("--- Formule dei Modelli Definite con Successo ---")
print(formula_Modello_A)
print(formula_Modello_B)
print(formula_Modello_C)

print("====================================================================")
print("--- PUNTO 4: STIMA EMPIRICA DEI MODELLI E INTERVALLI DI CONFIDENZA ---")
print("====================================================================")

# 4.1 Esecuzione della stima OLS per i tre modelli candidati
stima_Modello_A <- lm(formula_Modello_A, data = dati)
stima_Modello_B <- lm(formula_Modello_B, data = dati)
stima_Modello_C <- lm(formula_Modello_C, data = dati)

# 4.2 Visualizzazione dei Summary per il confronto metrico
print("--- STIMA COMPLETA: MODELLO A (LINEARE BASE) ---")
print(summary(stima_Modello_A))

print("--- STIMA COMPLETA: MODELLO B (STRUTTURALE OPT - SCELTA GRUPPO) ---")
summary_B <- summary(stima_Modello_B)
print(summary_B)

print("--- STIMA COMPLETA: MODELLO C (POLINOMIALE ESTESO) ---")
print(summary_C <- summary(stima_Modello_C))
