# ============================================================
# ANALISIS ERROR CORRECTION MODEL (ECM)
# PDB DAN PENGELUARAN KONSUMSI RUMAH TANGGA
# Data triwulanan 2000-2025
# ============================================================


# ============================================================
# 1. PACKAGE
# ============================================================
library(readxl)
library(urca)
library(tseries)
library(zoo)


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
data$Tahun <- na.locf(data$Tahun)
data$Tahun <- as.integer(data$Tahun)

periode_num <- c(
  "Triwulan I" = 1,
  "Triwulan II" = 2,
  "Triwulan III" = 3,
  "Triwulan IV" = 4
)

data$Periode_Num <- unname(
  periode_num[data$Periode]
)

if (any(is.na(data$Periode_Num))) {
  stop("Terdapat label periode triwulan yang tidak dikenali.")
}

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

if (
  anyNA(data$PDB) ||
  anyNA(data$`Pengeluaran Konsumsi Rumah Tangga`)
) {
  stop(
    "Terdapat missing value pada variabel utama."
  )
}

if (any(duplicated(data$Date))) {
  stop("Terdapat tanggal duplikat.")
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
# Alpha 10% digunakan untuk penetapan orde integrasi.
alpha_integrasi <- 0.10


# ============================================================
# 8. UJI STASIONERITAS LEVEL
# Pemilihan lag menggunakan AIC
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

summary(adf_pdb_level)
summary(adf_konsumsi_level)


# ============================================================
# 9. FIRST DIFFERENCE
# ============================================================
d_pdb <- diff(pdb)
d_konsumsi <- diff(konsumsi)


# ============================================================
# 10. UJI STASIONERITAS FIRST DIFFERENCE
# Pemilihan lag menggunakan AIC
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

summary(adf_pdb_diff)
summary(adf_konsumsi_diff)


# ============================================================
# 11. RINGKASAN UJI ADF
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

cat("\nHASIL ADF LEVEL\n")
print(adf_level_ringkas)


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

cat("\nHASIL ADF FIRST DIFFERENCE\n")
print(adf_diff_ringkas)


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

cat("\nORDE INTEGRASI\n")
print(orde_integrasi)


# Pemeriksaan syarat I(1)
if (any(is.na(orde_integrasi$Orde))) {
  
  stop(
    paste0(
      "Syarat I(1) belum terpenuhi pada alpha = ",
      alpha_integrasi,
      ". ECM tidak dibentuk."
    )
  )
}


# ============================================================
# 13. REGRESI JANGKA PANJANG
# Model:
# Konsumsi_t = beta_0 + beta_1 PDB_t + u_t
# ============================================================
long_run <- lm(
  konsumsi ~ pdb
)

summary(long_run)

coef(long_run)

confint(long_run)


# ============================================================
# 14. MEMBENTUK ERROR CORRECTION TERM (ECT)
# ============================================================
ECT <- residuals(long_run)

summary(ECT)

plot(
  ECT,
  type = "l",
  main = "Error Correction Term (ECT)",
  xlab = "Periode",
  ylab = "ECT"
)

abline(
  h = 0,
  lty = 2
)


# ============================================================
# 15. UJI KOINTEGRASI ENGLE-GRANGER
# ADF terhadap residual regresi jangka panjang
#
# H0: residual memiliki unit root
#     atau tidak terdapat kointegrasi
#
# H1: residual stasioner
#     atau terdapat kointegrasi
# ============================================================
adf_ect <- ur.df(
  ECT,
  type = "none",
  lags = 8,
  selectlags = "AIC"
)

summary(adf_ect)


# ============================================================
# 16. KEPUTUSAN UJI KOINTEGRASI
# ============================================================
stat_ect <- as.numeric(
  adf_ect@teststat[1]
)

critical_ect_5 <- as.numeric(
  adf_ect@cval[1, "5pct"]
)

critical_ect_10 <- as.numeric(
  adf_ect@cval[1, "10pct"]
)

hasil_kointegrasi <- data.frame(
  
  Statistik_ADF = stat_ect,
  
  Critical_Value_5pct =
    critical_ect_5,
  
  Critical_Value_10pct =
    critical_ect_10,
  
  Keputusan_5pct = ifelse(
    stat_ect < critical_ect_5,
    "Tolak H0",
    "Gagal menolak H0"
  ),
  
  Keputusan_10pct = ifelse(
    stat_ect < critical_ect_10,
    "Tolak H0",
    "Gagal menolak H0"
  )
)

cat("\nHASIL UJI KOINTEGRASI\n")
print(hasil_kointegrasi)


# Keputusan mengikuti alpha integrasi
ada_kointegrasi <- if (
  alpha_integrasi == 0.05
) {
  
  stat_ect < critical_ect_5
  
} else {
  
  stat_ect < critical_ect_10
  
}


# ============================================================
# 17. PEMBENTUKAN DATA ECM
# ============================================================
if (ada_kointegrasi) {
  
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
  # 18. ESTIMASI ECM TINGKAT PERTAMA
  #
  # Model:
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
  
  summary(ECM_model)
  
  coef(ECM_model)
  
  summary(
    ECM_model
  )$coefficients
  
  
  # ==========================================================
  # 19. KOEFISIEN ERROR CORRECTION TERM
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
  
  koef_ECT
  pvalue_ECT
  
  
  # ==========================================================
  # 20. DIAGNOSTIC CHECKING
  # ==========================================================
  resid_ECM <- residuals(
    ECM_model
  )
  
  
  # ----------------------------------------------------------
  # Uji autokorelasi residual
  # H0: tidak terdapat autokorelasi residual
  # H1: terdapat autokorelasi residual
  # ----------------------------------------------------------
  ljung_box <- Box.test(
    resid_ECM,
    lag = 4,
    type = "Ljung-Box"
  )
  
  ljung_box
  
  
  # ----------------------------------------------------------
  # Uji normalitas residual
  # H0: residual berdistribusi normal
  # H1: residual tidak berdistribusi normal
  # ----------------------------------------------------------
  jarque_bera <- jarque.bera.test(
    resid_ECM
  )
  
  jarque_bera
  
  
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
  # 21. RINGKASAN HASIL UTAMA
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
    "\nAlpha penetapan orde integrasi = ",
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
    "\nHasil Uji Kointegrasi:\n"
  )
  
  print(
    hasil_kointegrasi
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
  
  cat(
    "\n============================================\n"
  )
  
  cat(
    "HASIL UJI KOINTEGRASI\n"
  )
  
  cat(
    "============================================\n"
  )
  
  cat(
    "Tidak terdapat bukti kointegrasi pada alpha = ",
    alpha_integrasi,
    ".\n",
    sep = ""
  )
  
  cat(
    "ECM tidak dibentuk sebagai model final.\n"
  )
}


# ============================================================
# SELESAI
# ============================================================

