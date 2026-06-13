#GRUPPO 1 STATISTICA APPLICATA 
#COMPONENTI: Elisa Lombardi, Bruno Oliva, Christian Risi, Giacomo Rossino

library(corrplot)

setwd("C:/Users/bruno/Desktop/Gruppo1_StatisticaApplicata/Dataset_N1.csv")
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
#1.2 Analisi di Correlazione
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

# ==============================================================================
# STEP 3 CORRETTO: SCATTER PLOT (X vs Y)
# ==============================================================================
graphics.off() 

# 1. Crea un file PNG gigante nella tua cartella documenti/lavoro
png("Miei_Scatter_Plot.png", width = 1600, height = 800, res = 120)

# 2. Imposta la griglia
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
nomi_x <- names(dati)[-1] 

# 3. Disegna (invisibilmente, dentro il file)
for(variabile in nomi_x) {
  plot(dati[[variabile]], dati$y, 
       main = paste("y vs", variabile), 
       xlab = variabile, 
       ylab = "y", 
       pch = 16, 
       col = adjustcolor("darkblue", alpha.f = 0.5))
  
  abline(lm(dati$y ~ dati[[variabile]]), col = "red", lwd = 2)
}

# 4. SALVA E CHIUDE IL FILE (Fondamentale!)
dev.off() 

cat("Finito! Cerca il file 'Miei_Scatter_Plot.png' sul tuo PC!\n")
