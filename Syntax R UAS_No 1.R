

# ============================================================
# 1. PACKAGE
# ============================================================

library(readxl)
library(tseries)
library(forecast)
library(FinTS)
library(rugarch)
library(xts)


# ============================================================
# 2. IMPORT DATA
# ============================================================

# Pilih file Excel Data Harga Saham APEX
data <- read_excel(file.choose())

# Mengubah Date menjadi format tanggal
data$Date <- as.Date(data$Date)

# Mengurutkan data berdasarkan tanggal
data <- data[order(data$Date), ]


# ============================================================
# 3. PEMERIKSAAN DATA
# ============================================================

# Struktur data
str(data)

# Statistik deskriptif harga
summary(data)

# Memeriksa missing value
colSums(is.na(data))

# Memeriksa tanggal duplikat
sum(duplicated(data$Date))

# Memeriksa harga <= 0
sum(data$Close <= 0)


# ============================================================
# 4. MEMBENTUK LOG RETURN
# ============================================================

# Return logaritmik:
# r_t = ln(P_t / P_(t-1))

data$Return <- c(
  NA,
  diff(log(data$Close))
)

# Menghapus NA pertama
data_return <- data[!is.na(data$Return), ]

# Menyimpan return sebagai vector
return_apex <- data_return$Return

# Membuat objek xts dengan tanggal
return_xts <- xts(
  return_apex,
  order.by = data_return$Date
)

# Statistik deskriptif return
summary(return_apex)


# ============================================================
# 5. PLOT HARGA SAHAM
# ============================================================

plot(
  data$Date,
  data$Close,
  type = "l",
  main = "Harga Penutupan Harian Saham APEX",
  xlab = "Tanggal",
  ylab = "Harga Penutupan"
)

# ============================================================
# 6. PLOT RETURN
# ============================================================

plot(
  data_return$Date,
  return_apex,
  type = "l",
  main = "Return Harian Saham APEX",
  xlab = "Tanggal",
  ylab = "Log Return"
)

abline(
  h = 0,
  lty = 2
)


# ============================================================
# 7. PLOT KUADRAT RETURN
# ============================================================

plot(
  data_return$Date,
  return_apex^2,
  type = "l",
  main = "Kuadrat Return Harian Saham APEX",
  xlab = "Tanggal",
  ylab = "Return^2"
)

# ============================================================
# 8. UJI STASIONERITAS RETURN
# ============================================================

adf.test(return_apex)

# ============================================================
# 9. IDENTIFIKASI ACF DAN PACF RETURN
# ============================================================

par(mfrow = c(2,1))

acf(
  return_apex,
  main = "ACF Return Saham APEX"
)

pacf(
  return_apex,
  main = "PACF Return Saham APEX"
)

par(mfrow = c(1,1))


# ============================================================
# 10. IDENTIFIKASI MODEL MEAN
# ============================================================

model_mean <- auto.arima(
  return_apex,
  seasonal = FALSE,
  stationary = TRUE,
  max.d = 0,
  stepwise = FALSE,
  approximation = FALSE,
  ic = "aic"
)

summary(model_mean)


# ============================================================
# 11. MEMBENTUK MODEL MEAN BERDASARKAN HASIL IDENTIFIKASI
# ============================================================
# Berdasarkan hasil pengolahan APEX:
# ARIMA(3,0,0) dengan zero mean

model_ar3 <- Arima(
  return_apex,
  order = c(3,0,0),
  include.mean = FALSE
)
summary(model_ar3)

# ============================================================
# 12. RESIDUAL MODEL MEAN
# ============================================================

resid_mean <- residuals(model_ar3)


# ============================================================
# 13. ACF DAN PACF RESIDUAL
# ============================================================

par(mfrow = c(2,1))

acf(
  resid_mean,
  main = "ACF Residual ARIMA(3,0,0)"
)

pacf(
  resid_mean,
  main = "PACF Residual ARIMA(3,0,0)"
)

par(mfrow = c(1,1))


# ============================================================
# 14. ACF DAN PACF KUADRAT RESIDUAL
# ============================================================

par(mfrow = c(2,1))

acf(
  resid_mean^2,
  main = "ACF Kuadrat Residual"
)

pacf(
  resid_mean^2,
  main = "PACF Kuadrat Residual"
)

par(mfrow = c(1,1))


# ============================================================
# 15. UJI ARCH-LM
# ============================================================

# Uji ARCH-LM lag 1
ARCH_LM_1 <- ArchTest(
  resid_mean,
  lags = 1
)

ARCH_LM_1


# Uji ARCH-LM lag 5
ARCH_LM_5 <- ArchTest(
  resid_mean,
  lags = 5
)

ARCH_LM_5


# Uji ARCH-LM lag 10
ARCH_LM_10 <- ArchTest(
  resid_mean,
  lags = 10
)

ARCH_LM_10


# ============================================================
# 16. SPESIFIKASI MODEL ARCH/GARCH
# ============================================================
# Menggunakan:
# - Mean equation = AR(3)
# - Distribusi Normal
#
# Kandidat model:
# ARCH(1)
# GARCH(1,1)
# GARCH(1,2)
# GARCH(2,1)


# ------------------------------------------------------------
# A. ARCH(1)
# ------------------------------------------------------------

spec_arch1 <- ugarchspec(
  mean.model = list(
    armaOrder = c(3,0),
    include.mean = FALSE
  ),
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,0)
  ),
  distribution.model = "norm"
)

fit_arch1 <- ugarchfit(
  spec = spec_arch1,
  data = return_xts,
  solver = "hybrid"
)


# ------------------------------------------------------------
# B. GARCH(1,1)
# ------------------------------------------------------------

spec_garch11 <- ugarchspec(
  mean.model = list(
    armaOrder = c(3,0),
    include.mean = FALSE
  ),
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  distribution.model = "norm"
)

fit_garch11 <- ugarchfit(
  spec = spec_garch11,
  data = return_xts,
  solver = "hybrid"
)


# ------------------------------------------------------------
# C. GARCH(1,2)
# ------------------------------------------------------------

spec_garch12 <- ugarchspec(
  mean.model = list(
    armaOrder = c(3,0),
    include.mean = FALSE
  ),
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,2)
  ),
  distribution.model = "norm"
)

fit_garch12 <- ugarchfit(
  spec = spec_garch12,
  data = return_xts,
  solver = "hybrid"
)


# ------------------------------------------------------------
# D. GARCH(2,1)
# ------------------------------------------------------------

spec_garch21 <- ugarchspec(
  mean.model = list(
    armaOrder = c(3,0),
    include.mean = FALSE
  ),
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(2,1)
  ),
  distribution.model = "norm"
)

fit_garch21 <- ugarchfit(
  spec = spec_garch21,
  data = return_xts,
  solver = "hybrid"
)


# ============================================================
# 17. MELIHAT OUTPUT ESTIMASI
# ============================================================

show(fit_arch1)

show(fit_garch11)

show(fit_garch12)

show(fit_garch21)


# ============================================================
# 18. PERBANDINGAN MODEL
# ============================================================

# Fungsi untuk mengambil ukuran model
get_ic <- function(fit) {
  
  ic <- infocriteria(fit)
  
  c(
    LogLik = as.numeric(likelihood(fit)),
    AIC = as.numeric(ic[1]),
    BIC = as.numeric(ic[2])
  )
}


# Membuat tabel perbandingan
comparison <- rbind(
  
  ARCH1 = get_ic(fit_arch1),
  
  GARCH11 = get_ic(fit_garch11),
  
  GARCH12 = get_ic(fit_garch12),
  
  GARCH21 = get_ic(fit_garch21)
)

comparison


# ============================================================
# 19. URUTKAN MODEL BERDASARKAN AIC
# ============================================================

comparison_AIC <- comparison[
  order(comparison[, "AIC"]),
]

comparison_AIC


# ============================================================
# 20. URUTKAN MODEL BERDASARKAN BIC/SIC
# ============================================================

comparison_BIC <- comparison[
  order(comparison[, "BIC"]),
]

comparison_BIC


# ============================================================
# 21. MENENTUKAN MODEL TERPILIH
# ============================================================
# Berdasarkan hasil pengolahan APEX sebelumnya,
# GARCH(1,1) memiliki AIC dan BIC terendah
# di antara model dengan distribusi Normal.
#
# Karena itu digunakan sebagai model final.

fit_final <- fit_garch11


# ============================================================
# 22. OUTPUT MODEL FINAL
# ============================================================

show(fit_final)


# ============================================================
# 23. MENGAMBIL STANDARDIZED RESIDUAL
# ============================================================

z_final <- residuals(
  fit_final,
  standardize = TRUE
)

z_final <- as.numeric(z_final)


# ============================================================
# 24. PLOT STANDARDIZED RESIDUAL
# ============================================================

plot(
  data_return$Date,
  z_final,
  type = "l",
  main = "Standardized Residual GARCH(1,1)",
  xlab = "Tanggal",
  ylab = "Standardized Residual"
)

abline(
  h = 0,
  lty = 2
)

# ============================================================
# 25. UJI NORMALITAS RESIDUAL
# ============================================================
# Materi mencantumkan normalitas error sebagai bagian evaluasi.

jarque.bera.test(z_final)


# ============================================================
# 26. UJI KEACAKAN RESIDUAL
# ============================================================
# H0 : tidak terdapat autokorelasi residual

Box.test(
  z_final,
  lag = 12,
  type = "Ljung-Box"
)


# ============================================================
# 27. UJI KEACAKAN RESIDUAL KUADRAT
# ============================================================
# H0 : tidak terdapat autokorelasi residual kuadrat

Box.test(
  z_final^2,
  lag = 12,
  type = "Ljung-Box"
)


# ============================================================
# 28. UJI ARCH-LM SETELAH PEMODELAN
# ============================================================
# H0 : tidak terdapat lagi ARCH effect

ARCH_LM_FINAL <- ArchTest(
  z_final,
  lags = 12
)

ARCH_LM_FINAL


# ============================================================
# 29. ACF STANDARDIZED RESIDUAL
# ============================================================

acf(
  z_final,
  main = "ACF Standardized Residual GARCH(1,1)"
)


# ============================================================
# 30. PACF STANDARDIZED RESIDUAL
# ============================================================

pacf(
  z_final,
  main = "PACF Standardized Residual GARCH(1,1)"
)


# ============================================================
# 31. ACF KUADRAT STANDARDIZED RESIDUAL
# ============================================================

acf(
  z_final^2,
  main = "ACF Kuadrat Standardized Residual GARCH(1,1)"
)


# ============================================================
# 32. MENGAMBIL CONDITIONAL VOLATILITY
# ============================================================

volatility_final <- sigma(fit_final)


# ============================================================
# 33. PLOT CONDITIONAL VOLATILITY
# ============================================================

plot(
  data_return$Date,
  volatility_final,
  type = "l",
  main = "Conditional Volatility Saham APEX - GARCH(1,1)",
  xlab = "Tanggal",
  ylab = "Conditional Volatility"
)


# ============================================================
# 34. RINGKASAN CONDITIONAL VOLATILITY
# ============================================================
summary(volatility_final)

# Nilai volatilitas rata-rata
mean(volatility_final)

# Nilai volatilitas minimum
min(volatility_final)

# Nilai volatilitas maksimum
max(volatility_final)

# Tanggal ketika volatilitas maksimum terjadi
data_return$Date[
  which.max(volatility_final)
]

# ============================================================
# 35. NILAI PARAMETER MODEL FINAL
# ============================================================

coef(fit_final)

# ============================================================
# 36. KOEFISIEN DAN P-VALUE
# ============================================================

fit_final@fit$matcoef

