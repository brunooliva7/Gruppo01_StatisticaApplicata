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

cor_matrix <- cor(dati)

round(cor_matrix, 2)

windows(width = 10, height = 10) 
corrplot.mixed(cor_matrix, 
               lower = "number", 
               upper = "ellipse", 
               tl.pos = "lt",    
               number.cex = 0.9, 
               tl.col = "black")  
png("Scatter_X_vs_Y.png", width = 1200, height = 600, res = 120)

par(mfrow = c(2, 4)) 

for(i in 2:ncol(dati)) {
  plot(dati[, i], dati[, 1], 
       pch = 19,                                 
       col = rgb(0, 0, 1, 0.5),          
       xlab = names(dati)[i],             
       ylab = names(dati)[1],             
       main = paste(names(dati)[1], "vs", names(dati)[i]))
  
  modello_semplice <- lm(dati[, 1] ~ dati[, i])
  abline(modello_semplice, col = "red", lwd = 2)
}

par(mfrow = c(1, 1)) 
dev.off() 


# ------------------------------------------------------------------------------
# IMMAGINE 2: X vs X 
# ------------------------------------------------------------------------------
png("Scatter_X_vs_X.png", width = 1000, height = 1000, res = 120)

pairs(dati[, 2:ncol(dati)], 
      pch = 15,                                  
      col = rgb(1, 0, 0, 0.4),            
      main = "Verifica Disegno Sperimentale ortogonale (X vs X)")

dev.off() 


# --- MODELLO A: Modello puramente Lineare (Benchmark) ---
formula_Modello_A <- y_IQ ~ x1_ISO + x2_T + x3_MP + x4_CF + x5_F + x6_GSI + x7_UA

# --- MODELLO B: Modello Strutturale Parsimonioso (Scelta del Gruppo) ---
formula_Modello_B <- y_IQ ~ x1_ISO + x2_T + x3_MP + x6_GSI + x7_UA + I(x7_UA^2)

# --- MODELLO C: Modello Polinomiale Esteso ---
formula_Modello_C <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x4_CF + x5_F + x6_GSI + I(x6_GSI^2) + x7_UA + I(x7_UA^2)

#MODELLO D---: incluse le curve quadratiche ma esclusa la ridondanza 
formula_Modello_D <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x4_CF + x6_GSI + I(x6_GSI^2) + x7_UA + I(x7_UA^2)

#MODELLO E----: proviamo ad inserire x5 invece di x4 
formula_Modello_E <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x5_F + x6_GSI + I(x6_GSI^2) + x7_UA + I(x7_UA^2)


print("--- Formule dei Modelli Definite con Successo ---")
print(formula_Modello_A)
print(formula_Modello_B)
print(formula_Modello_C)
print(formula_Modello_D)
print(formula_Modello_E)

print("====================================================================")
print("--- PUNTO 4: STIMA EMPIRICA DEI MODELLI E INTERVALLI DI CONFIDENZA ---")
print("====================================================================")

stima_Modello_A <- lm(formula_Modello_A, data = dati)
stima_Modello_B <- lm(formula_Modello_B, data = dati)
stima_Modello_C <- lm(formula_Modello_C, data = dati)
stima_Modello_D <- lm(formula_Modello_D, data = dati)
stima_Modello_E <- lm(formula_Modello_E, data = dati)


print("--- STIMA COMPLETA: MODELLO A (LINEARE BASE) ---")
print(summary(stima_Modello_A))

print("--- STIMA COMPLETA: MODELLO B (STRUTTURALE) ---")
summary_B <- summary(stima_Modello_B)
print(summary_B)

print("--- STIMA COMPLETA: MODELLO C (POLINOMIALE ESTESO) ---")
print(summary_C <- summary(stima_Modello_C))

print("--- STIMA COMPLETA: MODELLO D (POLINOMIALE SENZA CORRELAZIONI) ---")
print(summary_D <- summary(stima_Modello_D))

print("--- STIMA COMPLETA: MODELLO E ---")
print(summary(stima_Modello_E))

#--CONFRONTO TRAMITE BIC E AIC PER TROVARE IL MODELLO MIGLIORE----
aic_valori <- c(
  Modello_A = AIC(stima_Modello_A),
  Modello_B = AIC(stima_Modello_B),
  Modello_C = AIC(stima_Modello_C),
  Modello_D = AIC(stima_Modello_D),
  Modello_E = AIC(stima_Modello_E)
)

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


# ====================================================================
# --- PUNTO 5: COEFFICIENTE DI DETERMINAZIONE E DIAGNOSTICA DEI RESIDUI ---
# ====================================================================

# 5.1 Estrazione e confronto del Coefficiente di Determinazione
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

# --- Diagnostica Modello A ---
print("Generazione dei grafici diagnostici per il Modello A...")
png("Diagnostica_Residui_Modello_A.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_A) 

par(mfrow = c(1, 1)) 
dev.off()


# --- Diagnostica Modello B ---
png("Diagnostica_Residui_Modello_B.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_B) 

par(mfrow = c(1, 1)) 
dev.off()


# --- Diagnostica Modello C ---
png("Diagnostica_Residui_Modello_C.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_C) 

par(mfrow = c(1, 1)) 
dev.off()

#--- Diagnostica Modello D ---

formula_Modello_D_finale <- y_IQ ~ x1_ISO + x2_T + I(x2_T^2) + x3_MP + x4_CF + x6_GSI + I(x6_GSI^2) + I(x7_UA^2)
stima_Modello_D_finale <- lm(formula_Modello_D_finale,data = dati)

png("Diagnostica_Residui_Modello_D_finale.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2)) 
plot(stima_Modello_D_finale) 
par(mfrow = c(1, 1)) 
dev.off()


# --- Diagnostica Modello E ---
png("Diagnostica_Residui_Modello_E.png", width = 1000, height = 800, res = 120)

par(mfrow = c(2, 2)) 
plot(stima_Modello_E) 

par(mfrow = c(1, 1)) 
dev.off()


# --- Esecuzione Regressione Stepwise (Backward) ---
modello_stepwise_backward <- step(stima_Modello_C, direction = "backward")

print("--- Risultato Modello Ottimale Scelto dalla Stepwise ---")
summary(modello_stepwise_backward)

print("AIC del modello Backward:")
AIC(modello_stepwise_backward)

modello_base <- lm(y_IQ ~ 1, data = dati) 

modello_stepwise_forward <- step(modello_base, 
                                 scope = list(lower = modello_base, upper = formula(stima_Modello_C)), 
                                 direction = "forward")

print("--- Risultato Modello Ottimale Scelto dalla Stepwise Forward ---")
summary(modello_stepwise_forward)

print("AIC del modello Forward:")
AIC(modello_stepwise_forward)