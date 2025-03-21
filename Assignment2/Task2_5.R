# Notwendige Bibliothek laden
library(forecast)

# Vorherige Plots löschen und Layout zurücksetzen
graphics.off()  # Schließt alle vorherigen Plots
par(mfrow=c(1,1))  # Setzt Layout zurück

# Modellparameter festlegen
p <- 0  # Keine nicht-saisonalen AR-Komponenten
d <- 0  # Keine Differenzierung
q <- 1  # Nicht-saisonaler MA(1)-Prozess

# Saisonale Komponenten
P <- 0  # Keine saisonalen AR-Komponenten
D <- 0  # Keine saisonale Differenzierung
Q <- 1  # Saisonaler MA(1)-Prozess
Period <- 12  # Saisonalität: 12 Perioden (z.B. monatliche Daten mit jährlicher Wiederholung)

# MA-Koeffizienten festlegen
theta <- 0.4  # Nicht-saisonaler MA(1)-Koeffizient
Theta <- -0.8  # Saisonaler MA(1)-Koeffizient mit Lag 12

# Simulierte Zeitreihe generieren
n <- 200  # Anzahl der Beobachtungen
sim_data <- arima.sim(model = list(order = c(p,d,q), 
                                   seasonal = list(order = c(P,D,Q), period = Period), 
                                   ma = theta, 
                                   seasonal.ma = Theta), 
                      n = n)

# Layout für die Plots festlegen: Zeitreihe oben, ACF und PACF unten nebeneinander
layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(1.5, 1.5))

# Zeitreihe plotten
plot.ts(sim_data, main="Simulated ARIMA(0,0,1) × (0,0,1)[12], θ_1 = 0.4, Θ_2 = -0.8", 
        ylab="Value", xlab="Zeit", col="black", lwd=1.2)

# Plot ACF (left)
acf(sim_data, main = "Autocorrelation (ACF)", col = "black", lwd = 2)

# Plot PACF (right)
pacf(sim_data, main = "Partial Autocorrelation (PACF)", col = "brown", lwd = 2)
# Layout zurücksetzen
par(mfrow=c(1,1))
