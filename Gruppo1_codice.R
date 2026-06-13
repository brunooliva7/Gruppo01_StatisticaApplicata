#GRUPPO 1 STATISTICA APPLICATA 
#COMPONENTI: Elisa Lombardi, Bruno Oliva, Christian Risi, Giacomo Rossino

library(corrplot)

dati <- dati <- read.csv(file.choose())

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
  boxplot(dati[[i]], main=vars[i], col=, varwidth=TRUE, notch=TRUE) 
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
      col = rgb(1, 0, 0, 0.4))

dev.off() 

# ==============================================================================
# PUNTO 2b: VERIFICA STATISTICA DELLA REGRESSIONE POLINOMIALE (Y vs X e X^2)
# ==============================================================================

# Creiamo una tabella vuota per salvare i risultati
tabella_poli <- data.frame(
  Variabile = character(),
  R_quadro = numeric(),
  P_value_Lineare = numeric(),
  P_value_Quadratico = numeric(),
  Curva_Significativa = character()
)

# Ciclo sulle variabili indipendenti (colonne da 2 a 8)
for(i in 2:8) {
  x_name <- names(dati)[i]
  X_var <- dati[, i]
  Y_var <- dati$y_IQ
  
  # Calcolo del modello polinomiale di grado 2
  modello_poli <- lm(Y_var ~ X_var + I(X_var^2))
  sommario <- summary(modello_poli)
  
  # Estrazione dei p-value (il p-value del termine X^2 è nella riga 3, colonna 4)
  pval_lin <- sommario$coefficients[2, 4]
  pval_quad <- sommario$coefficients[3, 4]
  
  # Aggiungiamo i risultati alla tabella
  tabella_poli <- rbind(tabella_poli, data.frame(
    Variabile = x_name,
    R_quadro = round(sommario$r.squared, 4),
    P_value_Lineare = round(pval_lin, 4),
    P_value_Quadratico = round(pval_quad, 4),
    Curva_Significativa = ifelse(pval_quad < 0.05, "SI", "NO") # Test al 5%
  ))
}

# Stampiamo la tabella finale sulla console
print("--- RISULTATI REGRESSIONE POLINOMIALE (GRADO 2) ---")
print(tabella_poli)


