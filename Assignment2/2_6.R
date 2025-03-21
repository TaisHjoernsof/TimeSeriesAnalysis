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
P <- 1  # Saisonaler AR(1)-Prozess
D <- 0  # Keine saisonale Differenzierung
Q <- 0  # Keine saisonale MA-Komponente
Period <- 12  # Saisonalität: 12 Perioden (z.B. monatliche Daten mit jährlicher Wiederholung)

# MA- und AR-Koeffizienten festlegen
theta <- -0.4  # Nicht-saisonaler MA(1)-Koeffizient
Phi <- 0.7  # Saisonaler AR(1)-Koeffizient mit Lag 12

# Simulierte Zeitreihe generieren
n <- 200  # Anzahl der Beobachtungen
sim_data <- arima.sim(model = list(order = c(p,d,q), 
                                   seasonal = list(order = c(P,D,Q), period = Period), 
                                   ma = theta, 
                                   seasonal.ar = Phi), 
                      n = n)

# Layout für die Plots festlegen: Zeitreihe oben, ACF und PACF unten nebeneinander
layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(1.5, 1.5))

# Zeitreihe plotten
plot.ts(sim_data, main="Simulated ARIMA(0,0,1) × (1,0,0)[12], θ_1 = -0.4, Φ_1 = 0.7", 
        ylab="Value", xlab="Zeit", col="black", lwd=1.2)

# Plot ACF (left)
acf(sim_data, main = "Autocorrelation (ACF)", col = "black", lwd = 2)

# Plot PACF (right)
pacf(sim_data, main = "Partial Autocorrelation (PACF)", col = "brown", lwd = 2)
# Layout zurücksetzen
par(mfrow=c(1,1))