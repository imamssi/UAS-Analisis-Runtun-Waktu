
# ============================================================
# ANALISIS ERROR CORRECTION MODEL (ECM)
# PDB DAN PENGELUARAN KONSUMSI RUMAH TANGGA
# DATA TRIWULANAN 2000-2025
# ============================================================


# ============================================================
# 1. PACKAGE
# ============================================================

library(readxl)
library(urca)
library(tseries)
library(zoo)
library(aTSA)


# ============================================================
# 2. IMPORT DATA
# ============================================================

data <- read_excel(file.choose())

# Menormalkan nama kolom
names(data) <- trimws(names(data))

str(data)
names(data)
head(data)
tail(data)


# ============================================================
# 3. PEMBENTUKAN PERIODE
# ============================================================

# Mengisi Tahun yang kosong dengan tahun sebelumnya
data$Tahun <- na.locf(data$Tahun)

# Mengubah Tahun menjadi integer
data$Tahun <- as.integer(data$Tahun)


# Mengubah nama triwulan menjadi angka
periode_num <- c(
  "Triwulan I" = 1,
  "Triwulan II" = 2,
  "Triwulan III" = 3,
  "Triwulan IV" = 4
)

data$Periode_Num <- unname(
  periode_num[data$Periode]
)


# Pemeriksaan label periode
if (any(is.na(data$Periode_Num))) {
  stop(
    "Terdapat label periode triwulan yang tidak dikenali."
  )
}


# Membentuk tanggal
data$Date <- as.Date(
  paste0(
    data$Tahun,
    "-",
    c(
      "01-01",
      "04-01",
      "07-01",
      "10-01"
    )[data$Periode_Num]
  )
)


# Mengurutkan berdasarkan waktu
data <- data[order(data$Date), ]

rownames(data) <- NULL


# ============================================================
# 4. PEMERIKSAAN DATA
# ============================================================

str(data)
summary(data)

cat(
  "Jumlah observasi:",
  nrow(data),
  "\n"
)

cat(
  "Periode awal:",
  format(min(data$Date)),
  "\n"
)

cat(
  "Periode akhir:",
  format(max(data$Date)),
  "\n"
)

cat(
  "Missing PDB:",
  sum(is.na(data$PDB)),
  "\n"
)

cat(
  "Missing Konsumsi:",
  sum(
    is.na(
      data$`Pengeluaran Konsumsi Rumah Tangga`
    )
  ),
  "\n"
)

cat(
  "Tanggal duplikat:",
  sum(duplicated(data$Date)),
  "\n"
)


# Pemeriksaan missing value
if (
  anyNA(data$PDB) ||
  anyNA(data$`Pengeluaran Konsumsi Rumah Tangga`)
) {
  stop(
    "Terdapat missing value pada variabel utama."
  )
}


# Pemeriksaan tanggal duplikat
if (any(duplicated(data$Date))) {
  stop(
    "Terdapat tanggal duplikat."
  )
}


# ============================================================
# 5. VISUALISASI DATA LEVEL
# ============================================================

par(mfrow = c(2, 1))

plot(
  data$Date,
  data$PDB,
  type = "l",
  main = "Produk Domestik Bruto (PDB)",
  xlab = "Periode",
  ylab = "PDB"
)

plot(
  data$Date,
  data$`Pengeluaran Konsumsi Rumah Tangga`,
  type = "l",
  main = "Pengeluaran Konsumsi Rumah Tangga",
  xlab = "Periode",
  ylab = "Konsumsi Rumah Tangga"
)

par(mfrow = c(1, 1))


# ============================================================
# 6. MEMBENTUK TIME SERIES
# ============================================================

pdb <- ts(
  data$PDB,
  start = c(2000, 1),
  frequency = 4
)

konsumsi <- ts(
  data$`Pengeluaran Konsumsi Rumah Tangga`,
  start = c(2000, 1),
  frequency = 4
)


# ============================================================
# 7. TINGKAT SIGNIFIKANSI
# ============================================================

# Alpha 10% digunakan untuk penetapan orde integrasi
alpha_integrasi <- 0.10


# ============================================================
# 8. UJI STASIONERITAS LEVEL
#
# Pemilihan lag menggunakan AIC
#
# H0: deret memiliki unit root / tidak stasioner
# H1: deret stasioner
# ============================================================

adf_pdb_level <- ur.df(
  pdb,
  type = "drift",
  lags = 8,
  selectlags = "AIC"
)

adf_konsumsi_level <- ur.df(
  konsumsi,
  type = "drift",
  lags = 8,
  selectlags = "AIC"
)

cat("\n============================================\n")
cat("ADF LEVEL - PDB\n")
cat("============================================\n")

print(
  summary(adf_pdb_level)
)


cat("\n============================================\n")
cat("ADF LEVEL - KONSUMSI RUMAH TANGGA\n")
cat("============================================\n")

print(
  summary(adf_konsumsi_level)
)


# ============================================================
# 9. FIRST DIFFERENCE
# ============================================================

d_pdb <- diff(pdb)

d_konsumsi <- diff(konsumsi)


# ============================================================
# 10. UJI STASIONERITAS FIRST DIFFERENCE
#
# Pemilihan lag menggunakan AIC
#
# H0: deret memiliki unit root / tidak stasioner
# H1: deret stasioner
# ============================================================

adf_pdb_diff <- ur.df(
  d_pdb,
  type = "drift",
  lags = 8,
  selectlags = "AIC"
)

adf_konsumsi_diff <- ur.df(
  d_konsumsi,
  type = "drift",
  lags = 8,
  selectlags = "AIC"
)

cat("\n============================================\n")
cat("ADF FIRST DIFFERENCE - PDB\n")
cat("============================================\n")

print(
  summary(adf_pdb_diff)
)


cat("\n============================================\n")
cat("ADF FIRST DIFFERENCE - KONSUMSI RUMAH TANGGA\n")
cat("============================================\n")

print(
  summary(adf_konsumsi_diff
  )
)


# ============================================================
# 11. RINGKASAN HASIL UJI ADF
# ============================================================

ambil_hasil_urdf <- function(
    uji,
    variabel,
    alpha = 0.10
) {
  
  kolom_cv <- paste0(
    100 * alpha,
    "pct"
  )
  
  statistik <- as.numeric(
    uji@teststat[1]
  )
  
  nilai_kritis <- as.numeric(
    uji@cval[1, kolom_cv]
  )
  
  data.frame(
    Variabel = variabel,
    Statistik_ADF = statistik,
    Critical_Value = nilai_kritis,
    Alpha = alpha,
    Keputusan = ifelse(
      statistik < nilai_kritis,
      "Tolak H0",
      "Gagal menolak H0"
    )
  )
}


# ADF LEVEL

adf_level_ringkas <- rbind(
  
  ambil_hasil_urdf(
    adf_pdb_level,
    "PDB",
    alpha_integrasi
  ),
  
  ambil_hasil_urdf(
    adf_konsumsi_level,
    "Konsumsi Rumah Tangga",
    alpha_integrasi
  )
)

cat("\n============================================\n")
cat("HASIL ADF LEVEL\n")
cat("============================================\n")

print(
  adf_level_ringkas
)


# ADF FIRST DIFFERENCE

adf_diff_ringkas <- rbind(
  
  ambil_hasil_urdf(
    adf_pdb_diff,
    "Delta PDB",
    alpha_integrasi
  ),
  
  ambil_hasil_urdf(
    adf_konsumsi_diff,
    "Delta Konsumsi",
    alpha_integrasi
  )
)

cat("\n============================================\n")
cat("HASIL ADF FIRST DIFFERENCE\n")
cat("============================================\n")

print(
  adf_diff_ringkas
)


# ============================================================
# 12. IDENTIFIKASI ORDE INTEGRASI
# ============================================================

orde_integrasi <- data.frame(
  
  Variabel = c(
    "PDB",
    "Konsumsi Rumah Tangga"
  ),
  
  Orde = ifelse(
    
    adf_level_ringkas$Keputusan ==
      "Gagal menolak H0" &
      adf_diff_ringkas$Keputusan ==
      "Tolak H0",
    
    "I(1)",
    
    NA_character_
  )
)


cat("\n============================================\n")
cat("ORDE INTEGRASI\n")
cat("============================================\n")

print(
  orde_integrasi
)


# Pemeriksaan syarat I(1)

if (any(is.na(orde_integrasi$Orde))) {
  
  stop(
    paste0(
      "Syarat I(1) belum terpenuhi pada alpha = ",
      alpha_integrasi,
      ". Analisis ECM dihentikan."
    )
  )
}


# ============================================================
# 13. REGRESI JANGKA PANJANG
#
# Model:
#
# Konsumsi_t = beta_0 + beta_1 PDB_t + u_t
# ============================================================

long_run <- lm(
  konsumsi ~ pdb
)


cat("\n============================================\n")
cat("REGRESI JANGKA PANJANG\n")
cat("============================================\n")

print(
  summary(long_run)
)


cat("\nKOEFISIEN REGRESI JANGKA PANJANG\n")

print(
  coef(long_run)
)


cat("\nINTERVAL KEPERCAYAAN 95%\n")

print(
  confint(long_run)
)


# ============================================================
# 14. MEMBENTUK RESIDUAL REGRESI JANGKA PANJANG
# ============================================================

residual_long_run <- residuals(
  long_run
)


cat("\n============================================\n")
cat("RINGKASAN RESIDUAL REGRESI JANGKA PANJANG\n")
cat("============================================\n")

print(
  summary(residual_long_run)
)


# Visualisasi residual
plot(
  residual_long_run,
  type = "l",
  main = "Residual Regresi Jangka Panjang",
  xlab = "Periode",
  ylab = "Residual"
)

abline(
  h = 0,
  lty = 2
)


# ============================================================
# 15. UJI KOINTEGRASI ENGLE-GRANGER
#
# H0: tidak terdapat kointegrasi
# H1: terdapat kointegrasi
#
# coint.test() digunakan karena merupakan prosedur
# khusus Engle-Granger dan memberikan p-value
# berdasarkan critical value kointegrasi.
# ============================================================

uji_EG <- coint.test(
  konsumsi,
  pdb,
  d = 0,
  nlag = NULL,
  output = TRUE
)


cat("\n============================================\n")
cat("HASIL UJI ENGLE-GRANGER\n")
cat("============================================\n")

print(
  uji_EG
)


# ============================================================
# 16. MENGAMBIL HASIL TYPE 1
#
# Type 1:
# Model tanpa tren
#
# Sesuai dengan hubungan jangka panjang:
#
# Konsumsi_t = beta_0 + beta_1 PDB_t + u_t
# ============================================================


# Mengambil baris pertama secara posisi
# agar tidak tergantung pada nama baris
hasil_EG <- data.frame(
  
  Lag = as.numeric(
    uji_EG[1, "lag"]
  ),
  
  Statistik_EG = as.numeric(
    uji_EG[1, "EG"]
  ),
  
  P_Value = as.numeric(
    uji_EG[1, "p.value"]
  )
)


cat("\n============================================\n")
cat("RINGKASAN UJI ENGLE-GRANGER TYPE 1\n")
cat("============================================\n")

print(
  hasil_EG
)


# ============================================================
# 17. KEPUTUSAN KOINTEGRASI
# ============================================================

p_value_EG <- hasil_EG$P_Value


# Nilai p-value pada aTSA dibatasi:
# 0.01 berarti p-value <= 0.01
# 0.10 berarti p-value >= 0.10
#
# Oleh karena itu, bila p-value keluaran = 0.10,
# keputusan pada alpha 10% adalah gagal menolak H0.

ada_kointegrasi <- (
  p_value_EG < alpha_integrasi
)


cat("\n============================================\n")
cat("KEPUTUSAN KOINTEGRASI\n")
cat("============================================\n")


if (ada_kointegrasi) {
  
  cat(
    "Tolak H0 pada alpha = ",
    alpha_integrasi,
    ".\n",
    sep = ""
  )
  
  cat(
    "Terdapat bukti kointegrasi.\n"
  )
  
} else {
  
  cat(
    "Gagal menolak H0 pada alpha = ",
    alpha_integrasi,
    ".\n",
    sep = ""
  )
  
  cat(
    "Tidak terdapat bukti yang cukup untuk menyatakan adanya kointegrasi.\n"
  )
}


# ============================================================
# 18. PEMBENTUKAN ECM
#
# ECM hanya dibentuk jika terdapat bukti kointegrasi.
# ============================================================

if (ada_kointegrasi) {
  
  
  # ==========================================================
  # 18A. ERROR CORRECTION TERM
  # ==========================================================
  
  ECT <- residual_long_run
  
  
  # ==========================================================
  # 18B. PEMBENTUKAN DATA ECM
  # ==========================================================
  
  ECM_data <- data.frame(
    
    dKonsumsi =
      as.numeric(
        diff(konsumsi)
      ),
    
    dPDB =
      as.numeric(
        diff(pdb)
      ),
    
    ECT_lag1 =
      as.numeric(
        head(ECT, -1)
      )
  )
  
  
  # ==========================================================
  # 18C. ESTIMASI ECM TINGKAT PERTAMA
  #
  # Model:
  #
  # Delta Konsumsi_t =
  # alpha_0 +
  # alpha_1 Delta PDB_t +
  # alpha_2 ECT_(t-1) +
  # epsilon_t
  # ==========================================================
  
  ECM_model <- lm(
    dKonsumsi ~ dPDB + ECT_lag1,
    data = ECM_data
  )
  
  
  cat("\n============================================\n")
  cat("HASIL ESTIMASI ECM\n")
  cat("============================================\n")
  
  print(
    summary(ECM_model)
  )
  
  
  cat("\nKOEFISIEN ECM\n")
  
  print(
    coef(ECM_model)
  )
  
  
  cat("\nKOEFISIEN DAN P-VALUE ECM\n")
  
  print(
    summary(ECM_model)$coefficients
  )
  
  
  # ==========================================================
  # 18D. KOEFISIEN ERROR CORRECTION TERM
  # ==========================================================
  
  koef_ECT <- coef(
    ECM_model
  )["ECT_lag1"]
  
  
  pvalue_ECT <- summary(
    ECM_model
  )$coefficients[
    "ECT_lag1",
    "Pr(>|t|)"
  ]
  
  
  cat(
    "\nKoefisien ECT = ",
    koef_ECT,
    "\n",
    sep = ""
  )
  
  
  cat(
    "P-value ECT = ",
    pvalue_ECT,
    "\n",
    sep = ""
  )
  
  
  # ==========================================================
  # 18E. DIAGNOSTIC CHECKING
  # ==========================================================
  
  resid_ECM <- residuals(
    ECM_model
  )
  
  
  # ----------------------------------------------------------
  # Uji autokorelasi residual
  #
  # H0: tidak terdapat autokorelasi residual
  # H1: terdapat autokorelasi residual
  # ----------------------------------------------------------
  
  ljung_box <- Box.test(
    resid_ECM,
    lag = 4,
    type = "Ljung-Box"
  )
  
  
  cat("\n============================================\n")
  cat("UJI LJUNG-BOX\n")
  cat("============================================\n")
  
  print(
    ljung_box
  )
  
  
  # ----------------------------------------------------------
  # Uji normalitas residual
  #
  # H0: residual berdistribusi normal
  # H1: residual tidak berdistribusi normal
  # ----------------------------------------------------------
  
  jarque_bera <- jarque.bera.test(
    resid_ECM
  )
  
  
  cat("\n============================================\n")
  cat("UJI JARQUE-BERA\n")
  cat("============================================\n")
  
  print(
    jarque_bera
  )
  
  
  # ----------------------------------------------------------
  # ACF residual
  # ----------------------------------------------------------
  
  acf(
    resid_ECM,
    main = "ACF Residual ECM"
  )
  
  
  # ----------------------------------------------------------
  # Plot residual
  # ----------------------------------------------------------
  
  plot(
    resid_ECM,
    type = "l",
    main = "Residual ECM",
    xlab = "Periode",
    ylab = "Residual"
  )
  
  abline(
    h = 0,
    lty = 2
  )
  
  
  # ==========================================================
  # 18F. RINGKASAN HASIL AKHIR
  # ==========================================================
  
  cat(
    "\n============================================\n"
  )
  
  cat(
    "HASIL UTAMA ANALISIS ECM\n"
  )
  
  cat(
    "============================================\n"
  )
  
  
  cat(
    "\nAlpha integrasi = ",
    alpha_integrasi,
    "\n",
    sep = ""
  )
  
  
  cat(
    "\nOrde Integrasi:\n"
  )
  
  print(
    orde_integrasi
  )
  
  
  cat(
    "\nKoefisien Regresi Jangka Panjang:\n"
  )
  
  print(
    coef(long_run)
  )
  
  
  cat(
    "\nHasil Engle-Granger Type 1:\n"
  )
  
  print(
    hasil_EG
  )
  
  
  cat(
    "\nKoefisien ECM:\n"
  )
  
  print(
    coef(ECM_model)
  )
  
  
  cat(
    "\nKoefisien ECT = ",
    koef_ECT,
    "\n",
    sep = ""
  )
  
  
  cat(
    "P-value ECT = ",
    pvalue_ECT,
    "\n",
    sep = ""
  )
  
  
  cat(
    "Ljung-Box p-value = ",
    ljung_box$p.value,
    "\n",
    sep = ""
  )
  
  
  cat(
    "Jarque-Bera p-value = ",
    jarque_bera$p.value,
    "\n",
    sep = ""
  )
  
  
} else {
  
  
  # ==========================================================
  # 19. JIKA TIDAK TERDAPAT KOINTEGRASI
  # ==========================================================
  
  cat(
    "\n============================================\n"
  )
  
  cat(
    "HASIL AKHIR ANALISIS\n"
  )
  
  cat(
    "============================================\n"
  )
  
  
  cat(
    "\nTidak terdapat bukti kointegrasi pada alpha = ",
    alpha_integrasi,
    ".\n",
    sep = ""
  )
  
  
  cat(
    "ECM tidak dibentuk sebagai model final.\n"
  )
  
  
  cat(
    "Hubungan jangka panjang tidak didukung oleh\n"
  )
  
  cat(
    "uji Engle-Granger pada taraf signifikansi yang digunakan.\n"
  )
}


# ============================================================
# 20. SELESAI
# ============================================================


