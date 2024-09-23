# Data suku bunga
tahun <- c(1998:2024)
suku_bunga <- c(49.32416667, 23.1275, 12.54666667, 16.61583333, 14.9475, 9.9425, 7.445, 9.12, 11.83333333, 8.604166667,
                8.666666667, 7.145833333, 6.5, 6.583333333, 5.770833333, 7.020833333, 7.520833333, 7.520833333, 6,
                4.5625, 5.057692308, 5.625, 4.25, 3.520833333, 4, 5.8125, 6)

# Menampilkan data
data <- data.frame(Tahun = tahun, Suku_Bunga = suku_bunga)
print(data)

# Menghitung estimasi menggunakan Metode Jackknife
deltat <- 1
y <- numeric(length(suku_bunga) - 1)

for (t in 1:(length(suku_bunga) - 1)) {
  y[t] <- suku_bunga[t + 1]
}

print(y)

X <- matrix(NA, nrow = length(suku_bunga) - 1, ncol = 2)

for (t in 1:(length(suku_bunga) - 1)) {
  X[t, 1] <- suku_bunga[t]
  X[t, 2] <- deltat
}

print(X)

# Membuat model regresi
model <- lm(y ~ X, data = data)

# Menghitung residual
residuals <- resid(model)

# Uji normalitas residual
shapiro_test <- shapiro.test(residuals)
ks_test <- ks.test(residuals, "pnorm")

# Uji homoskedastisitas (Breusch-Pagan test)
library(lmtest)
bp_test <- bptest(model)

# Uji autokorelasi residual (Ljung-Box test)
ljung_box_test <- Box.test(residuals, lag = 1, type = "Ljung-Box")

# Uji Durbin-Watson
library(lmtest)
dw_test <- durbinWatsonTest(model)

# Menampilkan hasil uji
print("Shapiro-Wilk test for normality:")
print(shapiro_test)

print("\nBreusch-Pagan test for homoskedasticity:")
print(bp_test)

print("\nLjung-Box test for autocorrelation of residuals:")
print(ljung_box_test)

print("\nDurbin-Watson test for autocorrelation in residuals:")
print(dw_test)
