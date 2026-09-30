# ============================================================
# ANALISIS DINAMIS INFLASI, BI-RATE, DAN KURS
# MENGGUNAKAN VAR/VECM
# ============================================================


# ============================================================
# 1. PACKAGE
# ============================================================

library(readxl)
library(tseries)
library(urca)
library(vars)
library(zoo)


# ============================================================
# 2. IMPORT DATA
# ============================================================

data <- read_excel(file.choose())

str(data)
head(data)
tail(data)


# ============================================================
# 3. PEMBERSIHAN DAN PEMBENTUKAN TANGGAL
# ============================================================

# Mengisi tahun yang kosong
data$Tahun <- na.locf(data$Tahun)

# Mengubah tahun menjadi integer
data$Tahun <- as.integer(data$Tahun)

# Pemetaan nama bulan Indonesia
bulan_num <- c(
  "Januari" = 1,
  "Februari" = 2,
  "Maret" = 3,
  "April" = 4,
  "Mei" = 5,
  "Juni" = 6,
  "Juli" = 7,
  "Agustus" = 8,
  "September" = 9,
  "Oktober" = 10,
  "November" = 11,
  "Desember" = 12
)

data$Bulan_Num <- bulan_num[data$Bulan]

# Membentuk tanggal
data$Date <- as.Date(
  paste(
    data$Tahun,
    data$Bulan_Num,
    "01",
    sep = "-"
  )
)

# Mengurutkan data berdasarkan tanggal
data <- data[order(data$Date), ]


# ============================================================
# 4. PEMERIKSAAN DATA
# ============================================================

str(data)
summary(data)

# Pemeriksaan missing value
colSums(
  is.na(
    data[, c("Inflasi", "Bi-Rate", "Kurs")]
  )
)

# Pemeriksaan duplikasi tanggal
sum(duplicated(data$Date))

# Periode data
min(data$Date)
max(data$Date)

# Jumlah observasi
nrow(data)


# ============================================================
# 5. VISUALISASI DATA LEVEL
# ============================================================

par(mfrow = c(3, 1))

plot(
  data$Date,
  data$Inflasi,
  type = "l",
  main = "Inflasi Bulanan",
  xlab = "Tanggal",
  ylab = "Inflasi"
)

plot(
  data$Date,
  data$`Bi-Rate`,
  type = "l",
  main = "BI-Rate",
  xlab = "Tanggal",
  ylab = "BI-Rate"
)

plot(
  data$Date,
  data$Kurs,
  type = "l",
  main = "Nilai Tukar Rupiah terhadap USD",
  xlab = "Tanggal",
  ylab = "Kurs"
)

par(mfrow = c(1, 1))


# ============================================================
# 6. MEMBENTUK TIME SERIES
# ============================================================

inflasi <- ts(
  data$Inflasi,
  start = c(2011, 1),
  frequency = 12
)

bi_rate <- ts(
  data$`Bi-Rate`,
  start = c(2011, 1),
  frequency = 12
)

kurs <- ts(
  data$Kurs,
  start = c(2011, 1),
  frequency = 12
)


# ============================================================
# 7. UJI STASIONERITAS LEVEL
# H0: deret memiliki unit root / tidak stasioner
# H1: deret stasioner
# ============================================================

adf_inflasi <- adf.test(inflasi)
adf_bi_rate <- adf.test(bi_rate)
adf_kurs <- adf.test(kurs)

adf_inflasi
adf_bi_rate
adf_kurs

# Ringkasan uji ADF level
adf_level <- data.frame(
  Variabel = c("Inflasi", "BI-Rate", "Kurs"),
  Statistik = c(
    as.numeric(adf_inflasi$statistic),
    as.numeric(adf_bi_rate$statistic),
    as.numeric(adf_kurs$statistic)
  ),
  P_Value = c(
    adf_inflasi$p.value,
    adf_bi_rate$p.value,
    adf_kurs$p.value
  )
)

adf_level$Keputusan <- ifelse(
  adf_level$P_Value < 0.05,
  "Tolak H0",
  "Gagal menolak H0"
)

adf_level


# ============================================================
# 8. FIRST DIFFERENCE
# ============================================================

d_inflasi <- diff(inflasi)
d_bi_rate <- diff(bi_rate)
d_kurs <- diff(kurs)


# ============================================================
# 9. UJI STASIONERITAS FIRST DIFFERENCE
# H0: deret memiliki unit root / tidak stasioner
# H1: deret stasioner
# ============================================================

adf_d_inflasi <- adf.test(d_inflasi)
adf_d_bi_rate <- adf.test(d_bi_rate)
adf_d_kurs <- adf.test(d_kurs)

adf_d_inflasi
adf_d_bi_rate
adf_d_kurs

# Ringkasan uji ADF first difference
adf_diff <- data.frame(
  Variabel = c("Delta Inflasi", "Delta BI-Rate", "Delta Kurs"),
  Statistik = c(
    as.numeric(adf_d_inflasi$statistic),
    as.numeric(adf_d_bi_rate$statistic),
    as.numeric(adf_d_kurs$statistic)
  ),
  P_Value = c(
    adf_d_inflasi$p.value,
    adf_d_bi_rate$p.value,
    adf_d_kurs$p.value
  )
)

adf_diff$Keputusan <- ifelse(
  adf_diff$P_Value < 0.05,
  "Tolak H0",
  "Gagal menolak H0"
)

adf_diff


# ============================================================
# 10. VISUALISASI FIRST DIFFERENCE
# ============================================================

par(mfrow = c(3, 1))

plot(
  d_inflasi,
  type = "l",
  main = "First Difference Inflasi",
  ylab = "Delta Inflasi"
)

plot(
  d_bi_rate,
  type = "l",
  main = "First Difference BI-Rate",
  ylab = "Delta BI-Rate"
)

plot(
  d_kurs,
  type = "l",
  main = "First Difference Kurs",
  ylab = "Delta Kurs"
)

par(mfrow = c(1, 1))


# ============================================================
# 11. IDENTIFIKASI ORDE INTEGRASI
# ============================================================

orde_integrasi <- data.frame(
  Variabel = c("Inflasi", "BI-Rate", "Kurs"),
  Orde = c("I(0)", "I(1)", "I(1)")
)

orde_integrasi


# ============================================================
# 12. DATA VARIABEL I(1) UNTUK UJI KOINTEGRASI
# ============================================================

I1_data <- cbind(
  BI_Rate = bi_rate,
  Kurs = kurs
)


# ============================================================
# 13. PEMILIHAN LAG UNTUK UJI KOINTEGRASI
# ============================================================

lag_I1 <- VARselect(
  I1_data,
  lag.max = 12,
  type = "const"
)

lag_I1$selection
lag_I1$criteria

# Lag VAR level yang dipilih untuk Johansen
K_johansen <- as.integer(
  lag_I1$selection["SC(n)"]
)

K_johansen


# ============================================================
# 14. UJI KOINTEGRASI JOHANSEN - TRACE TEST
# H0: rank <= r
# H1: rank > r
# ============================================================

johansen_trace <- ca.jo(
  I1_data,
  type = "trace",
  ecdet = "const",
  K = K_johansen,
  spec = "transitory"
)

summary(johansen_trace)

# Ringkasan Trace Test pada taraf 5%
trace_result <- data.frame(
  Hipotesis = rownames(johansen_trace@cval),
  Statistik = johansen_trace@teststat,
  Critical_5pct = johansen_trace@cval[, "5pct"]
)

trace_result$Keputusan <- ifelse(
  trace_result$Statistik > trace_result$Critical_5pct,
  "Tolak H0",
  "Gagal menolak H0"
)

trace_result


# ============================================================
# 15. UJI KOINTEGRASI JOHANSEN - MAXIMUM EIGENVALUE
# H0: rank = r
# H1: rank > r
# ============================================================

johansen_eigen <- ca.jo(
  I1_data,
  type = "eigen",
  ecdet = "const",
  K = K_johansen,
  spec = "transitory"
)

summary(johansen_eigen)

# Ringkasan Maximum Eigenvalue Test pada taraf 5%
eigen_result <- data.frame(
  Hipotesis = rownames(johansen_eigen@cval),
  Statistik = johansen_eigen@teststat,
  Critical_5pct = johansen_eigen@cval[, "5pct"]
)

eigen_result$Keputusan <- ifelse(
  eigen_result$Statistik > eigen_result$Critical_5pct,
  "Tolak H0",
  "Gagal menolak H0"
)

eigen_result


# ============================================================
# 16. DATA STASIONER UNTUK VAR
# ============================================================

VAR_data <- na.omit(
  cbind(
    Inflasi = inflasi,
    dBI_Rate = diff(bi_rate),
    dKurs = diff(kurs)
  )
)

head(VAR_data)
summary(VAR_data)


# ============================================================
# 17. PEMILIHAN LAG VAR
# ============================================================

lag_VAR <- VARselect(
  VAR_data,
  lag.max = 12,
  type = "const"
)

lag_VAR$selection
lag_VAR$criteria

# Lag final menggunakan SC/BIC
p_VAR <- as.integer(
  lag_VAR$selection["SC(n)"]
)

p_VAR


# ============================================================
# 18. ESTIMASI VAR FINAL
# ============================================================

VAR_final <- VAR(
  VAR_data,
  p = p_VAR,
  type = "const"
)

summary(VAR_final)


# ============================================================
# 19. UJI STABILITAS VAR
# ============================================================

roots_VAR <- roots(VAR_final)

roots_VAR

all(
  Mod(roots_VAR) < 1
)

plot(stability(VAR_final))


# ============================================================
# 20. UJI AUTOKORELASI RESIDUAL
# H0: tidak terdapat autokorelasi residual
# H1: terdapat autokorelasi residual
# ============================================================

serial_test <- serial.test(
  VAR_final,
  lags.pt = 12,
  type = "PT.asymptotic"
)

serial_test


# ============================================================
# 21. UJI NORMALITAS RESIDUAL
# H0: residual berdistribusi normal multivariat
# H1: residual tidak berdistribusi normal multivariat
# ============================================================

normality_test <- normality.test(
  VAR_final,
  multivariate.only = FALSE
)

normality_test


# ============================================================
# 22. UJI ARCH RESIDUAL
# H0: tidak terdapat ARCH effect
# H1: terdapat ARCH effect
# ============================================================

arch_test <- arch.test(
  VAR_final,
  lags.multi = 12,
  multivariate.only = FALSE
)

arch_test


# ============================================================
# 23. IMPULSE RESPONSE FUNCTION
# ============================================================

# Urutan variabel untuk orthogonalized IRF
VAR_data

# Seluruh impulse-response
irf_all <- irf(
  VAR_final,
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

plot(irf_all)


# ============================================================
# 24. IRF SHOCK PERUBAHAN BI-RATE -> INFLASI
# ============================================================

irf_dbi_inflasi <- irf(
  VAR_final,
  impulse = "dBI_Rate",
  response = "Inflasi",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

plot(irf_dbi_inflasi)


# ============================================================
# 25. IRF SHOCK PERUBAHAN KURS -> INFLASI
# ============================================================

irf_dkurs_inflasi <- irf(
  VAR_final,
  impulse = "dKurs",
  response = "Inflasi",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

plot(irf_dkurs_inflasi)


# ============================================================
# 26. IRF SHOCK INFLASI -> PERUBAHAN BI-RATE
# ============================================================

irf_inflasi_dbi <- irf(
  VAR_final,
  impulse = "Inflasi",
  response = "dBI_Rate",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

plot(irf_inflasi_dbi)


# ============================================================
# 27. IRF SHOCK INFLASI -> PERUBAHAN KURS
# ============================================================

irf_inflasi_dkurs <- irf(
  VAR_final,
  impulse = "Inflasi",
  response = "dKurs",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

plot(irf_inflasi_dkurs)


# ============================================================
# 28. FORECAST ERROR VARIANCE DECOMPOSITION
# ============================================================

fevd_VAR <- fevd(
  VAR_final,
  n.ahead = 12
)

fevd_VAR
plot(fevd_VAR)


# ============================================================
# 29. GRANGER CAUSALITY
# ============================================================

causality(
  VAR_final,
  cause = "Inflasi"
)

causality(
  VAR_final,
  cause = "dBI_Rate"
)

causality(
  VAR_final,
  cause = "dKurs"
)


# ============================================================
# 30. RINGKASAN MODEL
# ============================================================

cat("\n============================================\n")
cat("RINGKASAN ANALISIS VAR/VECM\n")
cat("============================================\n")

cat("Jumlah observasi :", nrow(data), "\n")
cat("Periode          :", format(min(data$Date), "%Y-%m"),
    "s/d", format(max(data$Date), "%Y-%m"), "\n")

cat("\nOrde Integrasi:\n")
print(orde_integrasi)

cat("\nLag Johansen :", K_johansen, "\n")
cat("Lag VAR      :", p_VAR, "\n")

cat("\nRoots VAR:\n")
print(roots_VAR)

cat("\nVAR Stabil :", all(Mod(roots_VAR) < 1), "\n")

cat("\n============================================\n")

