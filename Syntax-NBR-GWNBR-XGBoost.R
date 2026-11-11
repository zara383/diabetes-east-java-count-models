#LOAD PACKAGE
library(readxl)
library(dplyr)
library(openxlsx)
library(MASS)
library(lmtest)
library(sf)
library(spdep)
library(GWmodel)
library(ggplot2)
library(xgboost)
library(shapviz)
library(scales)
library(ggspatial)
library(viridis)

select <- dplyr::select
filter <- dplyr::filter
# ============================================================
# 1. PILIH FILE
# ============================================================
file_excel <- file.choose()
file_zip <- file.choose()

# 3.BACA EXCEL
data <- read_excel(file_excel)
colnames(data) <- c("Kabupaten", "Y", "X1", "X2", "X3", "X4", "X5", "Jumlah_Penduduk")

names(data)
str(data)
length(unique(data$Kabupaten)) == nrow(data)

#CEK MISSING VALUE & CEK DUPLIKAT
if (any(colSums(is.na(data)) > 0)) {
  stop("Terdapat Missing Value pada data Excel!")}

# 5. BUAT FOLDER HASIL
path_folder <- "C:/!ZAHRA/.SKRIPSI/OLAHDATA/HASIL_GWNBR_XGBOOST_JATIM_2025(4)"
sub_folders <- c(
  "01_Deskriptif",
  "02_Poisson_NBR",
  "03_Heterogenitas",
  "04_GWNBR",
  "05_XGBoost",
  "06_SHAP",
  "07_Leave_Surabaya_Out",
  "08_Perbandingan",
  "Data_Proses"
)

dir.create(path_folder, recursive = TRUE, showWarnings = FALSE)
sapply(file.path(path_folder, sub_folders), dir.create, showWarnings = FALSE)

folder_data <- file.path(path_folder, "Data_Proses")
zip_dir <- file.path(folder_data, "shapefile")
dir.create(zip_dir, showWarnings = FALSE)

# 6. EKSTRAK & BACA SHAPEFILE
unzip(file_zip, exdir = zip_dir)
shp_files <- list.files(zip_dir,pattern = "\\.shp$",recursive = TRUE,full.names = TRUE)

if (length(shp_files) == 0) {
  stop("Tidak ditemukan file .shp di dalam file ZIP.")}

shp_file <- shp_files[1]
geo      <- st_read(shp_file, quiet = TRUE)

# Hapus fitur level provinsi bila terikut
candidate_name <- names(geo)[grepl("kab|kota|name|nama|wilayah", names(geo), ignore.case = TRUE)]
if (length(candidate_name) > 0) {
  geo$Kab_Temp <- trimws(as.character(geo[[candidate_name[1]]]))
  geo <- geo[!grepl("Jawa Timur", geo$Kab_Temp, ignore.case = TRUE), ]
  geo$Kab_Temp <- NULL}

# Ensure geometri valid
geo <- st_make_valid(geo)

# 5. GABUNGKAN DATA BERDASARKAN NAMA KABUPATEN/KOTA
data_bersih <- data
# Tentukan kolom nama wilayah pada shapefile
candidate_name <- names(geo)[grepl("kab|kota|name|nama|wilayah", names(geo), ignore.case = TRUE)]

if (length(candidate_name) == 0) {stop("Tidak ditemukan kolom nama Kabupaten/Kota pada shapefile.")}

nama_shp <- candidate_name[1]
cat("Kolom nama wilayah shapefile yang digunakan:",nama_shp, "\n")

# Buat nama wilayah pada shapefile
geo$Kabupaten <- trimws(as.character(geo[[nama_shp]]))

# Fungsi normalisasi nama wilayah
normalisasi_nama <- function(x) {x <- trimws(as.character(x))
x <- sub("^Kabupaten\\s+","",x,ignore.case = TRUE)
x <- gsub("\\s+"," ",x)
return(x)}

# Buat key untuk proses pencocokan
geo$KEY_WILAYAH <- normalisasi_nama(geo$Kabupaten)
data_bersih$KEY_WILAYAH <- normalisasi_nama(data_bersih$Kabupaten)

# Cek apakah semua wilayah data ditemukan di shapefile
wilayah_tidak_ditemukan <- setdiff(data_bersih$KEY_WILAYAH,geo$KEY_WILAYAH)

if (length(wilayah_tidak_ditemukan) > 0) {
  print(data.frame( Wilayah_Tidak_Ditemukan = wilayah_tidak_ditemukan))
  stop("Ada Kabupaten/Kota pada data yang tidak ditemukan pada shapefile.")}

# Pastikan tidak ada nama wilayah ganda
if (anyDuplicated(geo$KEY_WILAYAH) > 0) {
  print(geo %>%st_drop_geometry() %>%
          filter(duplicated(KEY_WILAYAH) | duplicated(KEY_WILAYAH, fromLast = TRUE)) %>%
          select(Kabupaten, KEY_WILAYAH))
  stop("Terdapat nama wilayah ganda pada shapefile.")}


if (anyDuplicated(data_bersih$KEY_WILAYAH) > 0) {
  print(data_bersih %>%
          filter(duplicated(KEY_WILAYAH) | duplicated(KEY_WILAYAH, fromLast = TRUE)) %>%
          select(Kabupaten, KEY_WILAYAH))
  stop("Terdapat nama wilayah ganda pada data.")}

# Join berdasarkan KEY_WILAYAH
geo_data <- geo %>%left_join(data_bersih %>%select(-Kabupaten),by = "KEY_WILAYAH")

# Rapikan kolom
geo_data <- geo_data %>%select(-KEY_WILAYAH)

# Cek hasil pasangan wilayah dan Y
print(geo_data %>%st_drop_geometry() %>%select(Kabupaten,Y,X1,X2,X3,X4,X5))
# Validasi jumlah wilayah
stopifnot(nrow(geo_data) == 38)
stopifnot(length(unique(geo_data$Kabupaten)) == 38)
stopifnot(!any(is.na(geo_data$Y)))

# 6. SIMPAN FILE HASIL (.GPKG & .XLSX)
file_gpkg <- file.path(folder_data, "Jawa_Timur_Data_Analisis.gpkg")

# Buang kolom 'fid' bawaan shapefile (bentrok dengan kolom kunci GPKG)
geo_data <- geo_data[, !tolower(names(geo_data)) %in% "fid"]

gc()
if (file.exists(file_gpkg)) file.remove(file_gpkg)

st_write(geo_data,dsn = file_gpkg,layer = "Jawa_Timur_Data",driver = "GPKG",quiet = TRUE)

# ============================================================
# TAHAP 1: STATISTIK DESKRIPTIF, VIF (MANUAL), SENTROID, & PETA
# ============================================================
folder_out_01 <- file.path(path_folder, "01_Deskriptif")

# Ambil data numerik tanpa geometri
df_numeric <- geo_data %>% st_drop_geometry() %>% select(Y, X1, X2, X3, X4, X5, Jumlah_Penduduk)

# A. STATISTIK DESKRIPTIF (Satu-satu eksplisit)
options(scipen = 999)
stat_deskriptif <- data.frame(
  Variabel = colnames(df_numeric),
  Mean     = round(sapply(df_numeric, function(x) mean(x, na.rm = TRUE)), 3),
  SD       = round(sapply(df_numeric, function(x) sd(x, na.rm = TRUE)), 3),
  Min      = round(sapply(df_numeric, function(x) min(x, na.rm = TRUE)), 3),
  Max      = round(sapply(df_numeric, function(x) max(x, na.rm = TRUE)), 3))

# B. UJI MULTIKOLINIERITAS (VIF MANUAL)
x_vars <- c("X1", "X2", "X3", "X4", "X5")
vif_values <- numeric(length(x_vars))
names(vif_values) <- x_vars

for (i in seq_along(x_vars)) {
  var_target  <- x_vars[i]
  var_others  <- x_vars[-i]
  formula_vif <- as.formula(paste(var_target, "~", paste(var_others, collapse = " + ")))
  fit_vif     <- lm(formula_vif, data = df_numeric)
  
  r_squared     <- summary(fit_vif)$r.squared
  vif_values[i] <- 1 / (1 - r_squared)}

tabel_vif <- data.frame(
  Variabel   = names(vif_values),
  VIF        = round(as.numeric(vif_values), 3),
  Keterangan = ifelse(vif_values < 10, "Bebas Multikolinieritas", "Indikasi Multikolinieritas"))

# C. EKSTRAKSI KOORDINAT SENTROID
geo_utm  <- st_transform(geo_data, 32749)
sentroid <- st_centroid(geo_utm)
coords   <- st_coordinates(sentroid)

geo_data$X_UTM <- coords[, 1]
geo_data$Y_UTM <- coords[, 2]

geo_wgs84 <- st_transform(sentroid, 4326)
coords_wgs84 <- st_coordinates(geo_wgs84)

tabel_sentroid <- data.frame(
  Kabupaten = geo_data$Kabupaten,
  Longitude = coords_wgs84[, 1],
  Latitude  = coords_wgs84[, 2],
  X_UTM     = coords[, 1],
  Y_UTM     = coords[, 2])

# D. SIMPAN SELURUH TABEL KE SATU FILE EXCEL
wb <- createWorkbook()
addWorksheet(wb, "Statistik Deskriptif")
writeData(wb, "Statistik Deskriptif", stat_deskriptif)

addWorksheet(wb, "VIF")
writeData(wb, "VIF", tabel_vif)

addWorksheet(wb, "Koordinat Sentroid")
writeData(wb, "Koordinat Sentroid", tabel_sentroid)

saveWorkbook(wb, file.path(folder_out_01, "01_Laporan_Deskriptif.xlsx"), overwrite = TRUE)

# E. CHOROPLETH MAP OF RESPONSE VARIABLE (Y)
# 1. Menghitung rate per 100.000 penduduk
geo_data <- geo_data %>%
  mutate(rate_dm = (Y / Jumlah_Penduduk) * 100000)

# 2. Menentukan poligon utama setiap kabupaten/kota
geo_projected <- st_transform(geo_data, 32749)

label_poly <- geo_projected %>%
  select(Kabupaten, nama, geometry) %>%
  st_cast("POLYGON") %>%
  mutate(luas = as.numeric(st_area(geometry))) %>%
  group_by(Kabupaten, nama) %>%
  slice_max(order_by = luas, n = 1, with_ties = FALSE) %>%
  ungroup()

# 3. Membuat titik label dan mengubahnya ke CRS peta
label_points <- label_poly %>%
  st_point_on_surface() %>%
  st_transform(st_crs(geo_data))

# 4. Membuat nama label
label_points <- label_points %>%
  mutate(
    nama_label = ifelse(
      grepl("^Kabupaten ", nama),
      sub("^Kabupaten ", "", nama),
      nama))

# 5. Mengambil koordinat label dari geometri
koordinat_label <- st_coordinates(label_points)
label_points <- label_points %>%
  mutate(lon = koordinat_label[, 1],
         lat = koordinat_label[, 2])

# 6. Interval kelas legenda (jumlah warna = jumlah kelas)
nilai_maks  <- max(geo_data$rate_dm, na.rm = TRUE)
batas_atas  <- ceiling(nilai_maks / 500) * 500
breaks_rate <- seq(0, batas_atas, by = 500)
n_kelas     <- length(breaks_rate) - 1

peta_y <- ggplot() +
  # 1. Peta Utama
  geom_sf(
    data = geo_data,
    aes(fill = rate_dm),
    color = "grey20", 
    linewidth = 0.3
  ) +
  
  # 2. Label teks langsung (tanpa titik)
  geom_text(
    data = label_points,
    aes(x = lon, y = lat, label = nama_label),
    color = "black",
    fontface = "bold",
    size = 3.2, # Sedikit diperbesar agar nama lebih terbaca
    check_overlap = TRUE
    # nudge_x dan hjust dihapus agar teks pas di tengah koordinat
  ) +
  
  # 3. Skala warna Viridis
  scale_fill_stepsn(
    colors = viridisLite::viridis(n_kelas),
    breaks = breaks_rate,
    limits = c(0, batas_atas),
    name = "Rate per 100,000",
    labels = scales::comma,
    na.value = "grey85",
    guide = guide_colorsteps(
      show.limits = TRUE,
      even.steps = TRUE,
      frame.colour = "black", 
      ticks.colour = "black"
    )
  ) +
  
  # 4. Kompas (North Arrow) yang lebih besar dan jelas
  annotation_north_arrow(
    location = "tr", # Kanan atas
    which_north = "true",
    style = north_arrow_fancy_orienteering( # Menggunakan gaya yang lebih tebal
      text_size = 12,
      line_width = 1.2
    ),
    height = grid::unit(1.8, "cm"), # Diperbesar dari 1.2cm ke 1.8cm
    width = grid::unit(1.8, "cm"),  # Diperbesar dari 0.8cm ke 1.8cm
    pad_x = unit(0.5, "cm"),        # Diberi jarak sedikit dari ujung frame
    pad_y = unit(0.5, "cm")
  ) +
  
  # 5. Skala jarak
  annotation_scale(
    location = "br", # Kanan bawah
    width_hint = 0.2,
    line_width = 1,
    text_cex = 0.8,
    style = "ticks" 
  ) +
  
  coord_sf(
    xlim = c(110.8, 116.1), # Membatasi garis bujur (kiri-kanan)
    ylim = c(-8.9, -6.3),   # Membatasi garis lintang (bawah-atas)
    expand = FALSE) +
  
  labs(x = NULL, y = NULL) +
  
  # 6. Tema 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F4F7F9", color = "black", linewidth = 0.8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.9), color = "grey70"),
    legend.margin = margin(5, 5, 5, 5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    
    axis.text = element_text(size = 8, color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

# 10. Menyimpan gambar
ggsave(
  filename = file.path(
    folder_out_01,
    "03_Choropleth_Map_Diabetes_Mellitus_Rate.png"
  ),
  plot = peta_y,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ============================================================
# TAHAP 2: POISSON vs NBR (+ BASELINE NB-OFFSET)
# ============================================================
folder_out_02 <- file.path(path_folder, "02_Poisson_NBR")
if (!dir.exists(folder_out_02)) dir.create(folder_out_02, recursive = TRUE)

# Ambil data frame numerik
df_model <- geo_data %>% st_drop_geometry()

# A. ESTIMASI MODEL REGRESI POISSON
model_poisson <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + offset(log(Jumlah_Penduduk)),
                     family = poisson(link = "log"),
                     data   = df_model)
sum_poisson <- summary(model_poisson)

tabel_poisson <- data.frame(
  Variabel    = rownames(sum_poisson$coefficients),
  Estimasi    = round(sum_poisson$coefficients[, 1], 5),
  Std_Error   = round(sum_poisson$coefficients[, 2], 5),
  z_value     = round(sum_poisson$coefficients[, 3], 4),
  p_value     = round(sum_poisson$coefficients[, 4], 5),
  Signifikansi= ifelse(sum_poisson$coefficients[, 4] < 0.05, "Signifikan", "Tidak Signifikan"))

# Pearson chi-square / degrees of freedom
pearson_chisq <- sum(residuals(model_poisson, type = "pearson")^2)
df_residual <- df.residual(model_poisson)
pearson_ratio <- pearson_chisq / df_residual

# B. ESTIMASI MODEL NEGATIVE BINOMIAL REGRESSION (NBR, NB2)
model_nbr <- glm.nb(Y ~ X1 + X2 + X3 + X4 + X5 + offset(log(Jumlah_Penduduk)),
                    data = df_model)
nbr_model <- model_nbr
sum_nbr   <- summary(model_nbr)

# B2. BASELINE NB-OFFSET (intercept-only + offset populasi)
# ln(mu_i) = ln(Jumlah_Penduduk_i) + b0  -> laju kejadian sama di semua wilayah
model_nb0 <- glm.nb(Y ~ 1 + offset(log(Jumlah_Penduduk)),
                    data = df_model)
sum_nb0   <- summary(model_nb0)

# Fungsi bantu AIC / AICc / BIC
hitung_ic <- function(m, nama) {
  ll <- logLik(m)
  k  <- attr(ll, "df")          # termasuk parameter theta
  n  <- nobs(m)
  aic <- AIC(m)
  data.frame(Model          = nama,
             Jumlah_Param   = k,
             Log_Likelihood = round(as.numeric(ll), 3),
             AIC            = round(aic, 3),
             AICc           = round(aic + (2 * k * (k + 1)) / (n - k - 1), 3),
             BIC            = round(BIC(m), 3))}

tabel_aicc <- rbind(
  hitung_ic(model_nb0, "Baseline NB-offset"),
  hitung_ic(model_nbr, "NBR (penuh)"))
tabel_aicc_nbr <- tabel_aicc  

tabel_nbr <- data.frame(
  Variabel     = rownames(sum_nbr$coefficients),
  Estimasi     = round(sum_nbr$coefficients[, 1], 5),
  Std_Error    = round(sum_nbr$coefficients[, 2], 5),
  z_value      = round(sum_nbr$coefficients[, 3], 4),
  p_value      = ifelse(sum_nbr$coefficients[, 4] < 0.001, "<0.001",
                        sprintf("%.5f", sum_nbr$coefficients[, 4])),
  IRR          = round(exp(sum_nbr$coefficients[, 1]), 5),
  Signifikansi = ifelse(sum_nbr$coefficients[, 4] < 0.05,
                        "Signifikan", "Tidak Signifikan"))

tabel_nb0 <- data.frame(
  Variabel  = rownames(sum_nb0$coefficients),
  Estimasi  = round(sum_nb0$coefficients[, 1], 5),
  Std_Error = round(sum_nb0$coefficients[, 2], 5),
  z_value   = round(sum_nb0$coefficients[, 3], 4),
  p_value   = sprintf("%.5f", sum_nb0$coefficients[, 4]),
  Laju_Dasar = round(exp(sum_nb0$coefficients[, 1]), 6)  # exp(b0) = laju kejadian umum
)

# C. UJI OVERDISPERSI (Poisson vs NBR) + KONTRIBUSI VARIABEL (Baseline vs NBR)
disp_poisson <- sum_poisson$deviance / sum_poisson$df.residual
lrt_test <- lrtest(model_poisson, model_nbr)
lrt_stat <- lrt_test$Chisq[2]
lrt_pval <- lrt_test$`Pr(>Chisq)`[2]

tabel_overdispersi <- data.frame(
  Indikator = c("Deviance Poisson",
                "Derajat Bebas (DF)",
                "Rasio Dispersi (Deviance/DF)",
                "Rasio Pearson Chi-Square/DF",
                "Parameter Ukuran NBR (Theta)",
                "LRT Statistic (Poisson vs NBR)",
                "p-value LRT",
                "Kesimpulan"),
  Nilai = c(round(sum_poisson$deviance, 3),
            sum_poisson$df.residual,
            round(disp_poisson, 3),
            round(pearson_ratio, 3),
            round(model_nbr$theta, 3),
            round(lrt_stat, 3),
            round(lrt_pval, 5),
            ifelse(disp_poisson > 1 & lrt_pval < 0.05,
                   "Terjadi Overdispersi (Model NBR Lebih Layak)",
                   "Tidak Terjadi Overdispersi (Model Poisson Layak)")))

# Kontribusi variabel penjelas: Baseline NB-offset vs NBR penuh
lrt_base <- lrtest(model_nb0, model_nbr)
mcfadden <- 1 - as.numeric(logLik(model_nbr)) / as.numeric(logLik(model_nb0))

tabel_kontribusi <- data.frame(
  Indikator = c("Log-Likelihood Baseline NB-offset",
                "Log-Likelihood NBR penuh",
                "LRT Statistic (Baseline vs NBR)",
                "Derajat Bebas LRT",
                "p-value LRT",
                "Pseudo R2 McFadden (vs Baseline)",
                "Kesimpulan"),
  Nilai = c(round(as.numeric(logLik(model_nb0)), 3),
            round(as.numeric(logLik(model_nbr)), 3),
            round(lrt_base$Chisq[2], 3),
            lrt_base$Df[2],
            round(lrt_base$`Pr(>Chisq)`[2], 5),
            round(mcfadden, 4),
            ifelse(lrt_base$`Pr(>Chisq)`[2] < 0.05,
                   "Variabel penjelas berkontribusi signifikan terhadap Baseline",
                   "Variabel penjelas tidak berkontribusi signifikan")))

# D. PERBANDINGAN KEBAIKAN MODEL (Poisson, Baseline NB-offset, NBR)
pred_poisson <- predict(model_poisson, type = "response")
pred_nb0     <- predict(model_nb0,     type = "response")
pred_nbr     <- predict(model_nbr,     type = "response")

rmse <- function(p) sqrt(mean((df_model$Y - p)^2))
mae  <- function(p) mean(abs(df_model$Y - p))

tabel_evaluasi <- data.frame(
  Kriteria_Evaluasi = c("AIC", "BIC", "Log-Likelihood", "Deviance", "RMSE", "MAE"),
  Poisson = c(round(AIC(model_poisson), 3),
              round(BIC(model_poisson), 3),
              round(as.numeric(logLik(model_poisson)), 3),
              round(deviance(model_poisson), 3),
              round(rmse(pred_poisson), 3),
              round(mae(pred_poisson), 3)),
  Baseline_NB_offset = c(round(AIC(model_nb0), 3),
                         round(BIC(model_nb0), 3),
                         round(as.numeric(logLik(model_nb0)), 3),
                         round(deviance(model_nb0), 3),
                         round(rmse(pred_nb0), 3),
                         round(mae(pred_nb0), 3)),
  Negative_Binomial = c(round(AIC(model_nbr), 3),
                        round(BIC(model_nbr), 3),
                        round(as.numeric(logLik(model_nbr)), 3),
                        round(deviance(model_nbr), 3),
                        round(rmse(pred_nbr), 3),
                        round(mae(pred_nbr), 3)))

# Mengambil estimasi parameter dispersi
theta <- model_nbr$theta

# Mengambil standard error parameter dispersi
SE_theta <- model_nbr$SE.theta

# Menampilkan hasil
cat("Theta:", theta, "\n")
cat("SE Theta:", SE_theta, "\n")

# E. DIAGNOSTIK PENGARUH: COOK'S DISTANCE MODEL NBR
# Menghitung Cook's distance
cook_nbr <- cooks.distance(model_nbr)

# Batas pemeriksaan awal 4/n
batas_cook <- 4 / nrow(df_model)

# Membuat tabel Cook's distance
tabel_cook <- data.frame(Kabupaten = df_model$kab_kota,Cooks_Distance = as.numeric(cook_nbr),
                         Batas_4_n = batas_cook,Melebihi_Batas = ifelse(cook_nbr > batas_cook, "Ya", "Tidak"))

# Mengurutkan berdasarkan Cook's distance terbesar
tabel_cook <- tabel_cook[order(tabel_cook$Cooks_Distance, decreasing = TRUE),]
print(head(tabel_cook, 10))

# Menampilkan hasil khusus Kota Surabaya
hasil_cook_surabaya <- tabel_cook[tabel_cook$Kabupaten == "Kota Surabaya", ]
print(hasil_cook_surabaya)

# F. ANALISIS SENSITIVITAS TRANSFORMASI LOG X4
# F1. Statistik deskriptif dan skewness X4 asli
statistik_x4 <- data.frame(Variabel = c("X4 Asli", "Log X4"),
                           Skewness = c(moments::skewness(df_model$X4),moments::skewness(log(df_model$X4))))

# F2. Membuat variabel log X4
df_model$X4_log <- log(df_model$X4)

# F3. Estimasi NBR dengan X4 asli dan log X4
model_nbr_asli <- model_nbr
model_nbr_log <- glm.nb(Y ~ X1 + X2 + X3 + X4_log + X5 +offset(log(Jumlah_Penduduk)),data = df_model)

# F4. Perbandingan AIC
tabel_aic_sensitivitas <- data.frame(
  Model = c("NBR X4 Asli", "NBR Log X4"),
  AIC = c(AIC(model_nbr_asli),AIC(model_nbr_log))
)

# F5. LOOCV kedua model
n <- nrow(df_model)

pred_asli <- numeric(n)
pred_log <- numeric(n)

for (i in 1:n) {
  
  data_train <- df_model[-i, ]
  data_test <- df_model[i, ]
  
  # NBR X4 asli
  fit_asli <- glm.nb( Y ~ X1 + X2 + X3 + X4 + X5 +offset(log(Jumlah_Penduduk)),data = data_train)
  pred_asli[i] <- predict(fit_asli,newdata = data_test,type = "response")
  
  # NBR log X4
  fit_log <- glm.nb(Y ~ X1 + X2 + X3 + X4_log + X5 +offset(log(Jumlah_Penduduk)),data = data_train)
  pred_log[i] <- predict(fit_log,newdata = data_test,type = "response")
}

# Fungsi menghitung metrik LOOCV
hitung_metrik <- function(y, pred) {
  c(R2 = 1 - sum((y - pred)^2) /sum((y - mean(y))^2),
    RMSE = sqrt(mean((y - pred)^2)),
    MAE = mean(abs(y - pred)))
}

tabel_loocv_sensitivitas <- as.data.frame(
  rbind(NBR_X4_Asli = hitung_metrik(df_model$Y, pred_asli),
        NBR_Log_X4 = hitung_metrik(df_model$Y, pred_log))
)

tabel_loocv_sensitivitas$Model <- rownames(tabel_loocv_sensitivitas)

rownames(tabel_loocv_sensitivitas) <- NULL

# F6. Cook's distance kedua model
cook_asli <- cooks.distance(model_nbr_asli)
cook_log <- cooks.distance(model_nbr_log)

batas_cook_sens <- 4 / nrow(df_model)

idx_surabaya <- which(df_model$kab_kota == "Kota Surabaya")

tabel_cook_sensitivitas <- data.frame(
  Model = c("NBR X4 Asli", "NBR Log X4"),
  Cook_Surabaya = c(cook_asli[idx_surabaya],cook_log[idx_surabaya]),
  Batas_4_n = batas_cook_sens)

tabel_cook_sensitivitas$Melebihi_Batas <- ifelse(tabel_cook_sensitivitas$Cook_Surabaya >
                                                   tabel_cook_sensitivitas$Batas_4_n,"Ya", "Tidak")

# F7. Cook's distance seluruh wilayah pada model log X4
tabel_cook_log <- data.frame(Kabupaten = df_model$kab_kota,Cooks_Distance = as.numeric(cook_log),
                             Batas_4_n = batas_cook_sens,Melebihi_Batas = ifelse(cook_log > batas_cook_sens,"Ya", "Tidak"))

tabel_cook_log <- tabel_cook_log[order(tabel_cook_log$Cooks_Distance,decreasing = TRUE),]

# G. SIMPAN SELURUH HASIL KE SATU FILE EXCEL MULTI-SHEET
wb_02 <- createWorkbook()

addWorksheet(wb_02, "Evaluasi Kebaikan Model")
writeData(wb_02, "Evaluasi Kebaikan Model", tabel_evaluasi)

addWorksheet(wb_02, "AIC_AICc_LogLik")
writeData(wb_02, "AIC_AICc_LogLik", tabel_aicc)

addWorksheet(wb_02, "Uji Overdispersi")
writeData(wb_02, "Uji Overdispersi", tabel_overdispersi)

addWorksheet(wb_02, "Kontribusi Variabel")
writeData(wb_02, "Kontribusi Variabel", tabel_kontribusi)

addWorksheet(wb_02, "Estimasi Poisson")
writeData(wb_02, "Estimasi Poisson", tabel_poisson)

addWorksheet(wb_02, "Estimasi Baseline NB")
writeData(wb_02, "Estimasi Baseline NB", tabel_nb0)

addWorksheet(wb_02, "Estimasi NBR")
writeData(wb_02, "Estimasi NBR", tabel_nbr)

addWorksheet(wb_02, "Cook Distance NBR")
writeData(wb_02, "Cook Distance NBR", tabel_cook)

addWorksheet(wb_02, "Sensitivitas X4")
writeData(wb_02, "Sensitivitas X4", statistik_x4)

addWorksheet(wb_02, "AIC Sensitivitas X4")
writeData(wb_02, "AIC Sensitivitas X4", tabel_aic_sensitivitas)

addWorksheet(wb_02, "LOOCV Sensitivitas X4")
writeData(wb_02, "LOOCV Sensitivitas X4",tabel_loocv_sensitivitas)

addWorksheet(wb_02, "Cook Sensitivitas X4")
writeData(wb_02, "Cook Sensitivitas X4",tabel_cook_sensitivitas)

addWorksheet(wb_02, "Cook Seluruh Wilayah Log")
writeData(wb_02, "Cook Seluruh Wilayah Log",tabel_cook_log)

saveWorkbook(wb_02, file.path(folder_out_02, "02_Laporan_Poisson_vs_NBR.xlsx"), overwrite = TRUE)

# ============================================================
# TAHAP 3: AUTOKORELASI SPASIAL (MORAN'S I, RESIDUAL PEARSON NBR)
# ============================================================
folder_out_03 <- file.path(path_folder, "03_Heterogenitas")

df_model$resid_nbr <- residuals(model_nbr, type = "pearson")

k_knn  <- 4   
nb_knn <- spdep::knn2nb(spdep::knearneigh(coords, k = k_knn))
w_knn  <- spdep::nb2listw(nb_knn, style = "W", zero.policy = TRUE)

moran_res <- spdep::moran.test(df_model$resid_nbr, w_knn, zero.policy = TRUE)

tabel_moran <- data.frame(
  Objek_Uji      = "Residual Pearson NBR",
  K_Tetangga     = k_knn,
  Indeks_Moran_I = round(moran_res$estimate[1], 4),
  E_I_Ekspektasi = round(moran_res$estimate[2], 4),
  z_score        = round(as.numeric(moran_res$statistic), 4),
  p_value        = round(moran_res$p.value, 5),
  Kesimpulan     = ifelse(moran_res$p.value < 0.05,
                          "Residual NBR memiliki autokorelasi spasial signifikan",
                          "Residual NBR tidak memiliki autokorelasi spasial signifikan"))
wb_03 <- createWorkbook()
addWorksheet(wb_03, "Morans_I_Residual_NBR")
writeData(wb_03, "Morans_I_Residual_NBR", tabel_moran)
saveWorkbook(wb_03, file.path(folder_out_03, "03_Hasil_Moran.xlsx"), overwrite = TRUE)

# ============================================================
# TAHAP 4: GWNBR (Adaptive Bisquare dan Fixed Gaussian)
# ============================================================
folder_out_04 <- file.path(path_folder, "04_GWNBR")
folder_peta_c <- file.path(folder_out_04, "Peta_Koefisien_GWNBR")

dir.create(folder_out_04, recursive = TRUE, showWarnings = FALSE)
dir.create(folder_peta_c, recursive = TRUE, showWarnings = FALSE)

# ---- B. PERSIAPAN DATA ----
X_mat <- as.matrix(model.matrix(~ X1 + X2 + X3 + X4 + X5, data = df_model))
mode(X_mat) <- "numeric"

Y_vec <- as.numeric(df_model$Y)
off_vec <- log(as.numeric(df_model$Jumlah_Penduduk))

n_vars <- ncol(X_mat)
n_obs <- nrow(df_model)

coef_names <- c("Intercept", "X1", "X2", "X3", "X4", "X5")

# Theta tetap menggunakan estimasi NBR global,
# sedangkan koefisien beta diestimasi secara lokal.
theta_global <- as.numeric(model_nbr$theta)
beta_global <- as.numeric(coef(model_nbr))

dist_matrix <- as.matrix(dist(coords))

# Pemeriksaan kesesuaian data
stopifnot(
  nrow(X_mat) == length(Y_vec),
  length(Y_vec) == length(off_vec),
  nrow(dist_matrix) == n_obs,
  ncol(dist_matrix) == n_obs,
  n_vars == length(coef_names),
  all(is.finite(X_mat)),
  all(is.finite(Y_vec)),
  all(is.finite(off_vec)),
  all(is.finite(dist_matrix)),
  all(Y_vec >= 0),
  all(off_vec > -Inf),
  is.finite(theta_global),
  theta_global > 0,
  length(beta_global) == n_vars
)

# ---- C. FUNGSI DASAR ----
# C1. Bobot Adaptive Bisquare untuk data lengkap
bobot_adaptif <- function(dists, k) {
  if (length(dists) < 2 ||
      any(!is.finite(dists)) ||
      any(dists < 0)) {
    stop("Matriks jarak tidak valid.")
  }
  if (length(k) != 1 || k != as.integer(k) ||
      k < 1 || k >= length(dists)) {
    stop("Nilai k tidak valid.")
  }
  h <- sort(dists)[k + 1]
  if (!is.finite(h) || h <= 0) {
    stop("Bandwidth adaptive tidak valid.")
  }
  w <- ifelse(dists <= h,(1 - (dists / h)^2)^2,0)
  as.numeric(w)
}

# C2. Bobot Fixed Gaussian
# Bandwidth h menggunakan satuan meter.
bobot_fixed_gaussian <- function(dists, h) {
  if (length(h) != 1 || !is.finite(h) || h <= 0) {
    stop("Bandwidth fixed tidak valid.")
  }
  
  if (any(!is.finite(dists)) || any(dists < 0)) {
    stop("Matriks jarak tidak valid.")
  }
  
  exp(-0.5 * (dists / h)^2)
}

# C3. Estimasi parameter lokal GWNBR
fit_gwnbr_local <- function(X, Y, offset_val, theta, W_i, beta_start,
                            max_iter = 100, tol = 1e-5) {
  
  p <- ncol(X)
  n <- nrow(X)
  
  if (length(Y) != n ||
      length(offset_val) != n ||
      length(W_i) != n ||
      length(beta_start) != p) {
    stop("Dimensi data tidak sesuai.")
  }
  
  if (!is.finite(theta) || theta <= 0 ||
      any(!is.finite(W_i)) ||
      any(W_i < 0)) {
    stop("Theta atau bobot tidak valid.")
  }
  
  beta <- as.numeric(beta_start)
  ridge <- diag(1e-6, p)
  
  # Komponen Fisher Scoring
  komponen <- function(b) {
    
    eta <- pmin(
      pmax(as.vector(X %*% b + offset_val), -20),
      20
    )
    
    mu <- exp(eta)
    var_y <- mu + mu^2 / theta
    w_diag <- as.numeric(W_i * mu^2 / var_y)
  
    list(w_diag = w_diag,score = t(X) %*% (W_i * (Y - mu) * mu / var_y),
      hessian = t(X) %*% (X * w_diag)
    )
  }
  
  konvergen <- FALSE
  for (iter in seq_len(max_iter)) {
    
    k <- komponen(beta)
    
    langkah <- tryCatch(
      as.vector(solve(k$hessian + ridge, k$score)),
      error = function(e) {as.vector(MASS::ginv(k$hessian + ridge) %*% k$score)
      }
    )
    
    beta_new <- beta + langkah
    selisih <- max(abs(beta_new - beta), na.rm = TRUE)
    
    if (!is.finite(selisih)) break
    
    beta <- beta_new
    
    if (selisih < tol) {
      konvergen <- TRUE
      break
    }
  }
  
  # Menghitung kembali komponen pada beta akhir
  k <- komponen(beta)
  inv_hessian <- tryCatch(
    solve(k$hessian + ridge),
    error = function(e) {
      MASS::ginv(k$hessian + ridge)
    }
  )
  
  inv_hessian <- matrix(
    as.numeric(inv_hessian),
    nrow = p,
    ncol = p
  )
  
  se <- sqrt(pmax(diag(inv_hessian), 1e-8))
  
  eta <- pmin(
    pmax(as.vector(X %*% beta + offset_val), -20),
    20
  )
  
  y_hat <- exp(eta)
  
  list(
    beta = beta,
    se = se,
    y_hat = y_hat,
    inv_hessian = inv_hessian,
    w_diag = k$w_diag,
    konvergen = konvergen
  )
}

# C4. Menghitung AICc GWNBR
hitung_ic_gwnbr <- function(W_fun, X, Y, offset_val,
                            theta, beta_start) {
  n <- nrow(X)
  
  gagal <- list(
    AIC = Inf,
    AICc = Inf,
    EDF = NA_real_,
    K_eff = NA_real_,
    LogLik = NA_real_
  )
  
  y_hat <- rep(NA_real_, n)
  S_diag <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    
    fit_i <- tryCatch(
      fit_gwnbr_local(
        X = X,
        Y = Y,
        offset_val = offset_val,
        theta = theta,
        W_i = W_fun(i),
        beta_start = beta_start
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit_i) || !isTRUE(fit_i$konvergen)) {
      return(gagal)
    }
    
    y_hat[i] <- fit_i$y_hat[i]
    
    x_i <- matrix(
      as.numeric(X[i, ]),
      nrow = 1
    )
    
    S_diag[i] <- as.numeric(
      x_i %*% fit_i$inv_hessian %*% t(x_i)
    ) * fit_i$w_diag[i]
  }
  
  if (any(!is.finite(y_hat)) ||
      any(!is.finite(S_diag))) {
    return(gagal)
  }
  
  # Effective Degrees of Freedom
  EDF <- sum(S_diag)
  
  # Theta dihitung sebagai satu parameter tambahan
  K_eff <- EDF + 1
  
  loglik <- sum(
    dnbinom(
      Y,
      size = theta,
      mu = y_hat,
      log = TRUE
    )
  )
  
  denom <- n - K_eff - 1
  
  if (!is.finite(loglik) || denom <= 0) {
    return(gagal)
  }
  
  AIC <- -2 * loglik + 2 * K_eff
  
  AICc <- AIC +
    (2 * K_eff * (K_eff + 1)) / denom
  
  list(
    AIC = AIC,
    AICc = AICc,
    EDF = EDF,
    K_eff = K_eff,
    LogLik = loglik
  )
}

# C5. Estimasi NBR untuk inisialisasi GWNBR
# Digunakan pada data training, termasuk setiap fold LOOCV.
estimasi_nbr_awal <- function(X, Y, offset_val) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  if (length(Y) != n || length(offset_val) != n) {
    stop("Dimensi data NBR tidak sesuai.")
  }
  
  data_nb <- data.frame(
    y = as.numeric(Y),
    off = as.numeric(offset_val)
  )
  
  if (p > 1) {
    for (j in 2:p) {
      data_nb[[paste0("x", j - 1)]] <- X[, j]
    }
  }
  
  prediktor <- paste0("x", seq_len(p - 1))
  
  formula_nb <- as.formula(
    paste(
      "y ~",
      paste(prediktor, collapse = " + "),
      "+ offset(off)"
    )
  )
  
  fit_nb <- suppressWarnings(
    MASS::glm.nb(
      formula_nb,
      data = data_nb,
      control = glm.control(maxit = 100)
    )
  )
  
  hasil_beta <- as.numeric(coef(fit_nb))
  hasil_theta <- as.numeric(fit_nb$theta)
  
  if (!is.finite(hasil_theta) ||
      hasil_theta <= 0 ||
      length(hasil_beta) != p ||
      any(!is.finite(hasil_beta))) {
    stop("Estimasi NBR untuk inisialisasi tidak valid.")
  }
  
  list(
    theta = hasil_theta,
    beta_start = hasil_beta
  )
}

# C6. Bobot Adaptive Bisquare untuk lokasi uji
bobot_adaptif_new <- function(dists, k) {
  
  if (length(dists) < 1 ||
      any(!is.finite(dists)) ||
      any(dists < 0)) {
    stop("Jarak lokasi uji tidak valid.")
  }
  
  if (length(k) != 1 ||
      k != as.integer(k) ||
      k < 1 ||
      k > length(dists)) {
    stop("Bandwidth adaptive tidak valid.")
  }
  
  h <- sort(dists)[k]
  
  if (!is.finite(h) || h <= 0) {
    stop("Bandwidth adaptive tidak valid.")
  }
  
  w <- ifelse(
    dists <= h,
    (1 - (dists / h)^2)^2,
    0
  )
  
  as.numeric(w)
}

# D. PEMILIHAN BANDWIDTH BERDASARKAN AICc
# ---- D1. Persiapan ----
min_k <- ncol(X_mat) + 3
k_range <- min_k:(n_obs - 1)

if (length(k_range) == 0) {
  stop("Rentang bandwidth Adaptive Bisquare tidak valid.")
}

jarak_unik <- dist_matrix[upper.tri(dist_matrix)]
jarak_unik <- jarak_unik[is.finite(jarak_unik) & jarak_unik > 0]

if (length(jarak_unik) == 0) {
  stop("Tidak ditemukan jarak antarlokasi yang valid.")
}

jarak_maks <- max(jarak_unik)


# ---- D2. Profil AICc Adaptive Bisquare ----
profil_aicc_adaptive <- data.frame(
  k = k_range,
  EDF = NA_real_,
  K_eff = NA_real_,
  LogLik = NA_real_,
  AIC = NA_real_,
  AICc = NA_real_
)

for (j in seq_along(k_range)) {
  
  k <- k_range[j]
  
  hs <- hitung_ic_gwnbr(
    function(i) bobot_adaptif(dist_matrix[i, ], k),
    X_mat, Y_vec, off_vec,
    theta_global, beta_global
  )
  
  profil_aicc_adaptive[j, 2:6] <- unlist(
    hs[c("EDF", "K_eff", "LogLik", "AIC", "AICc")]
  )
}

# ---- D3. Profil AICc Fixed Gaussian ----
# Kandidat bandwidth awal berdasarkan kuantil jarak
h_awal <- as.numeric(
  quantile(
    jarak_unik,
    probs = seq(0.25, 1, by = 0.05),
    na.rm = TRUE
  )
)

# Perluasan rentang hingga 32 kali jarak maksimum
h_lanjutan <- seq(
  jarak_maks * 1.05,
  jarak_maks * 32,
  length.out = 100
)

h_range <- sort(unique(c(h_awal, h_lanjutan)))

profil_aicc_gauss <- data.frame(
  Bandwidth_m = h_range,
  EDF = NA_real_,
  K_eff = NA_real_,
  LogLik = NA_real_,
  AIC = NA_real_,
  AICc = NA_real_
)

for (j in seq_along(h_range)) {
  
  h <- h_range[j]
  
  hs <- hitung_ic_gwnbr(
    function(i) bobot_fixed_gaussian(dist_matrix[i, ], h),
    X_mat, Y_vec, off_vec,
    theta_global, beta_global
  )
  
  profil_aicc_gauss[j, 2:6] <- unlist(
    hs[c("EDF", "K_eff", "LogLik", "AIC", "AICc")]
  )
}

# ---- D4. Pemilihan dan pemeriksaan posisi minimum ----
if (!any(is.finite(profil_aicc_adaptive$AICc)) ||
    !any(is.finite(profil_aicc_gauss$AICc))) {
  stop("Tidak ditemukan nilai AICc yang valid.")
}

idx_ad <- which.min(profil_aicc_adaptive$AICc)
idx_ga <- which.min(profil_aicc_gauss$AICc)

bw_opt <- profil_aicc_adaptive$k[idx_ad]
bw_gauss <- profil_aicc_gauss$Bandwidth_m[idx_ga]

status_ad <- if (idx_ad == 1) {
  "Batas bawah"
} else if (idx_ad == nrow(profil_aicc_adaptive)) {
  "Batas atas"
} else {
  "Minimum interior"
}

status_ga <- if (idx_ga == 1) {
  "Batas bawah"
} else if (idx_ga == nrow(profil_aicc_gauss)) {
  "Batas atas"
} else {
  "Minimum interior"
}

tabel_bandwidth <- data.frame(
  Kernel = c("Adaptive Bisquare", "Fixed Gaussian"),
  Bandwidth = c(bw_opt, bw_gauss),
  Satuan = c("Jumlah tetangga", "Meter"),
  Minimum_AICc = c(
    profil_aicc_adaptive$AICc[idx_ad],
    profil_aicc_gauss$AICc[idx_ga]
  ),
  Posisi_Minimum = c(status_ad, status_ga)
)

print(tabel_bandwidth)

if (status_ad != "Minimum interior") {
  warning(
    "Adaptive Bisquare belum memiliki minimum interior. ",
    "Periksa profil AICc dan interpretasikan hasil sebagai minimum pada batas rentang."
  )
}

if (status_ga != "Minimum interior") {
  warning(
    "Fixed Gaussian belum memiliki minimum interior. ",
    "Periksa profil AICc dan interpretasikan hasil sebagai minimum pada batas rentang."
  )
}

# ---- D5. Grafik Adaptive Bisquare ----
png(
  file.path(folder_out_04, "Profil_AICc_Adaptive_Bisquare.png"),
  width = 1800, height = 1200, res = 300
)

plot(
  profil_aicc_adaptive$k,
  profil_aicc_adaptive$AICc,
  type = "b",
  xlab = "Number of nearest neighbors (k)",
  ylab = "AICc",
  main = "Adaptive Bisquare Bandwidth Selection"
)

abline(v = bw_opt, lty = 2)

dev.off()

# ---- D6. Grafik Fixed Gaussian ----
png(
  file.path(folder_out_04, "Profil_AICc_Fixed_Gaussian.png"),
  width = 1800, height = 1200, res = 300
)

plot(
  profil_aicc_gauss$Bandwidth_m / 1000,
  profil_aicc_gauss$AICc,
  type = "l",
  xlab = "Bandwidth (km)",
  ylab = "AICc",
  main = "Fixed Gaussian Bandwidth Selection"
)

points(
  profil_aicc_gauss$Bandwidth_m[idx_ga] / 1000,
  profil_aicc_gauss$AICc[idx_ga],
  pch = 19
)

abline(v = bw_gauss / 1000, lty = 2)

dev.off()

# F. ESTIMASI PARAMETER LOKAL
beta_mat    <- matrix(NA_real_, nrow = n_obs, ncol = n_vars)
se_mat      <- matrix(NA_real_, nrow = n_obs, ncol = n_vars)
y_hat_gwnbr <- rep(NA_real_, n_obs)
S_mat       <- matrix(NA_real_, nrow = n_obs, ncol = n_obs)
konv_vec    <- logical(n_obs)
fit_simpan  <- vector("list", n_obs)

for (i in seq_len(n_obs)) {
  
  w_i <- bobot_adaptif(dist_matrix[i, ], bw_opt)
  
  fit_i <- tryCatch(
    fit_gwnbr_local(
      X_mat, Y_vec, off_vec,
      theta_global, w_i, beta_global
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit_i) || !fit_i$konvergen) next
  
  fit_simpan[[i]] <- fit_i
  beta_mat[i, ]   <- fit_i$beta
  se_mat[i, ]     <- fit_i$se
  y_hat_gwnbr[i]  <- fit_i$y_hat[i]
  
  # Baris ke-i matriks smoother S
  S_mat[i, ] <- as.numeric(
    X_mat[i, , drop = FALSE] %*%
      fit_i$inv_hessian %*%
      t(X_mat)
  ) * fit_i$w_diag
  
  konv_vec[i] <- TRUE
}

cat(
  "Lokasi tidak konvergen:",
  sum(!konv_vec), "dari", n_obs, "\n"
)

if (any(!konv_vec)) {
  stop("Ada lokasi yang tidak konvergen; periksa sebelum melanjutkan.")
}

if (any(!is.finite(beta_mat)) ||
    any(!is.finite(se_mat)) ||
    any(se_mat <= 0) ||
    any(!is.finite(y_hat_gwnbr)) ||
    any(!is.finite(S_mat))) {
  stop("Terdapat koefisien, SE, prediksi, atau elemen S yang tidak valid.")
}

colnames(beta_mat) <- coef_names
colnames(se_mat)   <- paste0(coef_names, "_SE")

v_eff_opt <- sum(diag(S_mat))
tr_STS <- sum(S_mat^2)
p_eff <- 2 * v_eff_opt - tr_STS

# Koreksi multiple testing da Silva dan Fotheringham
alpha_ds <- 0.05 / (p_eff / n_vars)

# G. TABEL KOEFISIEN LOKAL DAN UJI Z DUA SISI
df_04_lengkap <- df_model %>%
  dplyr::select(Kabupaten)

for (j in seq_along(coef_names)) {
  
  v <- coef_names[j]
  
  z_val <- beta_mat[, j] / se_mat[, j]
  p_val <- 2 * pnorm(-abs(z_val))
  
  df_04_lengkap[[paste0("Beta_", v)]] <- beta_mat[, j]
  df_04_lengkap[[paste0("SE_", v)]]   <- se_mat[, j]
  df_04_lengkap[[paste0("Z_", v)]]    <- z_val
  df_04_lengkap[[paste0("p_", v)]]    <- p_val
}

df_04_lengkap$Y_Aktual   <- Y_vec
df_04_lengkap$Y_Prediksi <- y_hat_gwnbr
df_04_lengkap$Residual   <- Y_vec - y_hat_gwnbr

# H. RINGKASAN DESKRIPTIF PARAMETER LOKAL
summary_list <- list()

for (v in coef_names) {
  
  vals <- df_04_lengkap[[paste0("Beta_", v)]]
  
  summary_list[[v]] <- data.frame(
    Variabel = v,
    Min = min(vals, na.rm = TRUE),
    Q1 = quantile(vals, 0.25, na.rm = TRUE),
    Median = median(vals, na.rm = TRUE),
    Mean = mean(vals, na.rm = TRUE),
    Q3 = quantile(vals, 0.75, na.rm = TRUE),
    Max = max(vals, na.rm = TRUE),
    Std_Dev = sd(vals, na.rm = TRUE)
  )
}

df_05_ringkas <- do.call(rbind, summary_list)
rownames(df_05_ringkas) <- NULL

# I. SIGNIFIKANSI DAN ARAH KOEFISIEN
sig_list <- list()
dir_list <- list()

for (v in coef_names) {
  
  p_col <- df_04_lengkap[[paste0("p_", v)]]
  b_col <- df_04_lengkap[[paste0("Beta_", v)]]
  
  n_05  <- sum(p_col < 0.05, na.rm = TRUE)
  n_adj <- sum(p_col < alpha_ds, na.rm = TRUE)
  
  sig_list[[v]] <- data.frame(
    Variabel = v,
    Signif_p005 = n_05,
    Persen_p005 = round(n_05 / n_obs * 100, 2),
    Signif_Terkoreksi = n_adj,
    Persen_Terkoreksi = round(n_adj / n_obs * 100, 2)
  )
  
  dir_list[[v]] <- data.frame(
    Variabel = v,
    Pengaruh_Positif = sum(b_col > 0, na.rm = TRUE),
    Pengaruh_Negatif = sum(b_col < 0, na.rm = TRUE)
  )
}

df_06_jumlah_sig <- do.call(rbind, sig_list)
df_07_arah_peng  <- do.call(rbind, dir_list)

# J. EVALUASI GWNBR IN-SAMPLE
SSE_gwnbr <- sum((Y_vec - y_hat_gwnbr)^2)
SST_gwnbr <- sum((Y_vec - mean(Y_vec))^2)

r2_gwnbr <- 1 - SSE_gwnbr / SST_gwnbr
rmse_gwnbr_fit <- sqrt(mean((Y_vec - y_hat_gwnbr)^2))
mae_gwnbr_fit  <- mean(abs(Y_vec - y_hat_gwnbr))

ll_gwnbr <- sum(
  dnbinom(
    Y_vec,
    size = theta_global,
    mu = y_hat_gwnbr,
    log = TRUE
  )
)

k_par_gwnbr <- v_eff_opt + 1

if (n_obs - k_par_gwnbr - 1 <= 0) {
  stop("AICc GWNBR tidak dapat dihitung karena penyebut tidak valid.")
}

aic_gwnbr <- -2 * ll_gwnbr + 2 * k_par_gwnbr

aicc_gwnbr <- aic_gwnbr +
  (2 * k_par_gwnbr * (k_par_gwnbr + 1)) /
  (n_obs - k_par_gwnbr - 1)

df_11_evaluasi <- data.frame(
  Indikator = c(
    "Bandwidth (k-NN)",
    "Effective Degrees of Freedom (v)",
    "Log-Likelihood",
    "AIC (theta dihitung)",
    "AICc (theta dihitung)",
    "R-Squared (in-sample)",
    "RMSE (in-sample)",
    "MAE (in-sample)"
  ),
  Nilai = c(
    bw_opt, v_eff_opt, ll_gwnbr,
    aic_gwnbr, aicc_gwnbr,
    r2_gwnbr, rmse_gwnbr_fit, mae_gwnbr_fit
  )
)
# K. KELOMPOK SIGNIFIKANSI PER WILAYAH
buat_kelompok <- function(ambang) {
  
  sapply(seq_len(n_obs), function(i) {
    
    p_i <- sapply(
      coef_names,
      function(v) df_04_lengkap[[paste0("p_", v)]][i]
    )
    
    paste(
      coef_names[which(p_i < ambang)],
      collapse = ", "
    )
  })
}

df_kelompok_sig <- data.frame(
  Kabupaten = df_04_lengkap$Kabupaten,
  Parameter_Signifikan_p005 = buat_kelompok(0.05),
  Parameter_Signifikan_Terkoreksi = buat_kelompok(alpha_ds)
)
# L. PERBANDINGAN AIC/AICc NBR DAN GWNBR
ll_nbr  <- as.numeric(logLik(model_nbr))
k_nbr   <- attr(logLik(model_nbr), "df")

aic_nbr <- -2 * ll_nbr + 2 * k_nbr

if (n_obs - k_nbr - 1 <= 0) {
  stop("AICc NBR tidak dapat dihitung karena penyebut tidak valid.")
}

aicc_nbr <- aic_nbr +
  (2 * k_nbr * (k_nbr + 1)) /
  (n_obs - k_nbr - 1)

tabel_aicc_gwnbr <- data.frame(
  Model = c(
    "NBR",
    "GWNBR Adaptive Bisquare",
    "GWNBR Fixed Gaussian"
  ),
  Log_Likelihood = round(
    c(
      ll_nbr,
      ll_gwnbr,
      profil_aicc_gauss$LogLik[idx_ga]
    ), 3
  ),
  Param_Efektif = round(
    c(
      k_nbr,
      k_par_gwnbr,
      profil_aicc_gauss$K_eff[idx_ga]
    ), 3
  ),
  AIC = round(
    c(
      aic_nbr,
      aic_gwnbr,
      profil_aicc_gauss$AIC[idx_ga]
    ), 3
  ),
  AICc = round(
    c(
      aicc_nbr,
      aicc_gwnbr,
      profil_aicc_gauss$AICc[idx_ga]
    ), 3
  )
)

print(tabel_aicc_gwnbr)

# ---- M. NESTED LOOCV GWNBR ----
# M1. Fungsi seleksi bandwidth pada data training
pilih_bandwidth_fold <- function(X_train, Y_train, off_train,
                                 D_train, theta_train, beta_train,
                                 kernel = c("adaptive", "gaussian")) {
  
  kernel <- match.arg(kernel)
  n_train <- nrow(X_train)
  
  if (nrow(D_train) != n_train ||
      ncol(D_train) != n_train) {
    stop("Dimensi matriks jarak training tidak sesuai.")
  }
  
  # --------------------------------
  # Adaptive Bisquare
  # --------------------------------
  if (kernel == "adaptive") {
    
    min_k_fold <- ncol(X_train) + 3
    k_range_fold <- min_k_fold:(n_train - 1)
    
    if (length(k_range_fold) == 0) {
      stop("Rentang bandwidth Adaptive Bisquare tidak valid.")
    }
    
    hasil_aicc <- rep(Inf, length(k_range_fold))
    
    for (j in seq_along(k_range_fold)) {
      
      k <- k_range_fold[j]
      
      hasil_ic <- tryCatch(
        hitung_ic_gwnbr(
          W_fun = function(i) {
            bobot_adaptif(D_train[i, ], k)
          },
          X = X_train,
          Y = Y_train,
          offset_val = off_train,
          theta = theta_train,
          beta_start = beta_train
        ),
        error = function(e) NULL
      )
      
      if (!is.null(hasil_ic) &&
          is.finite(hasil_ic$AICc)) {
        hasil_aicc[j] <- hasil_ic$AICc
      }
    }
    
    if (!any(is.finite(hasil_aicc))) {
      stop("Tidak ditemukan AICc Adaptive Bisquare yang valid.")
    }
    
    idx <- which.min(hasil_aicc)
    
    return(list(
      bandwidth = k_range_fold[idx],
      AICc = hasil_aicc[idx]
    ))
  }
  
  # --------------------------------
  # Fixed Gaussian
  # --------------------------------
  if (kernel == "gaussian") {
    
    jarak_unik_fold <- D_train[upper.tri(D_train)]
    jarak_unik_fold <- jarak_unik_fold[
      is.finite(jarak_unik_fold) & jarak_unik_fold > 0
    ]
    
    if (length(jarak_unik_fold) == 0) {
      stop("Jarak antarlokasi training tidak valid.")
    }
    
    jarak_maks_fold <- max(jarak_unik_fold)
    
    h_awal_fold <- as.numeric(
      quantile(
        jarak_unik_fold,
        probs = seq(0.25, 1, by = 0.05),
        na.rm = TRUE
      )
    )
    
    h_lanjutan_fold <- seq(
      jarak_maks_fold * 1.05,
      jarak_maks_fold * 32,
      length.out = 100
    )
    
    h_range_fold <- sort(
      unique(c(h_awal_fold, h_lanjutan_fold))
    )
    
    hasil_aicc <- rep(Inf, length(h_range_fold))
    
    for (j in seq_along(h_range_fold)) {
      
      h <- h_range_fold[j]
      
      hasil_ic <- tryCatch(
        hitung_ic_gwnbr(
          W_fun = function(i) {
            bobot_fixed_gaussian(D_train[i, ], h)
          },
          X = X_train,
          Y = Y_train,
          offset_val = off_train,
          theta = theta_train,
          beta_start = beta_train
        ),
        error = function(e) NULL
      )
      
      if (!is.null(hasil_ic) &&
          is.finite(hasil_ic$AICc)) {
        hasil_aicc[j] <- hasil_ic$AICc
      }
    }
    
    if (!any(is.finite(hasil_aicc))) {
      stop("Tidak ditemukan AICc Fixed Gaussian yang valid.")
    }
    
    idx <- which.min(hasil_aicc)
    
    return(list(
      bandwidth = h_range_fold[idx],
      AICc = hasil_aicc[idx]
    ))
  }
}


# M2. Fungsi Nested LOOCV untuk satu kernel
nested_loocv_gwnbr <- function(kernel = c("adaptive", "gaussian")) {
  
  kernel <- match.arg(kernel)
  
  y_pred <- rep(NA_real_, n_obs)
  bandwidth_fold <- rep(NA_real_, n_obs)
  aicc_fold <- rep(NA_real_, n_obs)
  theta_fold <- rep(NA_real_, n_obs)
  status_fold <- rep("Gagal", n_obs)
  
  for (i in seq_len(n_obs)) {
    
    cat(
      "\nKernel:", kernel,
      "| Fold:", i, "dari", n_obs, "\n"
    )
    
    # Data training dan testing
    idx_train <- setdiff(seq_len(n_obs), i)
    
    X_train <- X_mat[idx_train, , drop = FALSE]
    Y_train <- Y_vec[idx_train]
    off_train <- off_vec[idx_train]
    
    X_test <- X_mat[i, , drop = FALSE]
    off_test <- off_vec[i]
    
    D_train <- dist_matrix[
      idx_train, idx_train,
      drop = FALSE
    ]
    
    D_test <- as.numeric(
      dist_matrix[i, idx_train]
    )
    
    # Estimasi NBR hanya dari data training
    nbr_train <- tryCatch(
      estimasi_nbr_awal(
        X_train,
        Y_train,
        off_train
      ),
      error = function(e) NULL
    )
    
    if (is.null(nbr_train)) {
      status_fold[i] <- "Gagal estimasi NBR"
      next
    }
    
    theta_train <- nbr_train$theta
    beta_train <- nbr_train$beta_start
    
    theta_fold[i] <- theta_train
    
    # Seleksi bandwidth hanya dari data training
    bw_train <- tryCatch(
      pilih_bandwidth_fold(
        X_train = X_train,
        Y_train = Y_train,
        off_train = off_train,
        D_train = D_train,
        theta_train = theta_train,
        beta_train = beta_train,
        kernel = kernel
      ),
      error = function(e) NULL
    )
    
    if (is.null(bw_train)) {
      status_fold[i] <- "Gagal seleksi bandwidth"
      next
    }
    
    bandwidth_fold[i] <- bw_train$bandwidth
    aicc_fold[i] <- bw_train$AICc
    
    # Bobot lokasi testing terhadap data training
    if (kernel == "adaptive") {
      
      W_test <- tryCatch(
        bobot_adaptif_new(
          D_test,
          bw_train$bandwidth
        ),
        error = function(e) NULL
      )
      
    } else {
      
      W_test <- tryCatch(
        bobot_fixed_gaussian(
          D_test,
          bw_train$bandwidth
        ),
        error = function(e) NULL
      )
    }
    
    if (is.null(W_test)) {
      status_fold[i] <- "Gagal menghitung bobot"
      next
    }
    
    # Estimasi koefisien lokal pada lokasi testing
    fit_test <- tryCatch(
      fit_gwnbr_local(
        X = X_train,
        Y = Y_train,
        offset_val = off_train,
        theta = theta_train,
        W_i = W_test,
        beta_start = beta_train
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit_test) ||
        !isTRUE(fit_test$konvergen)) {
      status_fold[i] <- "Gagal konvergensi GWNBR"
      next
    }
    
    # Prediksi observasi yang dikeluarkan
    eta_test <- as.numeric(
      X_test %*% fit_test$beta + off_test
    )
    
    eta_test <- pmin(pmax(eta_test, -20), 20)
    
    y_pred[i] <- exp(eta_test)
    
    status_fold[i] <- "Berhasil"
  }
  
  list(
    prediksi = y_pred,
    bandwidth = bandwidth_fold,
    AICc = aicc_fold,
    theta = theta_fold,
    status = status_fold
  )
}


# M3. Menjalankan Nested LOOCV kedua kernel

hasil_nested_ad <- nested_loocv_gwnbr("adaptive")

hasil_nested_ga <- nested_loocv_gwnbr("gaussian")


# M4. Menghitung metrik evaluasi
metrik_nested <- function(nama, hasil) {
  
  valid <- is.finite(hasil$prediksi)
  n_valid <- sum(valid)
  
  if (n_valid < 2) {
    return(data.frame(
      Model = nama,
      N_Valid = n_valid,
      R2_LOOCV = NA_real_,
      RMSE_LOOCV = NA_real_,
      MAE_LOOCV = NA_real_
    ))
  }
  
  evaluasi <- hitung_evaluasi(
    Y_vec[valid],
    hasil$prediksi[valid]
  )
  
  data.frame(
    Model = nama,
    N_Valid = n_valid,
    R2_LOOCV = evaluasi$R2,
    RMSE_LOOCV = evaluasi$RMSE,
    MAE_LOOCV = evaluasi$MAE
  )
}

evaluasi_nested_gwnbr <- rbind(
  metrik_nested(
    "GWNBR Adaptive Bisquare (Nested LOOCV)",
    hasil_nested_ad
  ),
  metrik_nested(
    "GWNBR Fixed Gaussian (Nested LOOCV)",
    hasil_nested_ga
  )
)

print(evaluasi_nested_gwnbr)

# M5. Tabel bandwidth setiap fold
hasil_bandwidth_nested <- data.frame(
  Fold = seq_len(n_obs),
  Kabupaten_Diuji = df_model$Kabupaten,
  Bandwidth_Adaptive = hasil_nested_ad$bandwidth,
  AICc_Adaptive = hasil_nested_ad$AICc,
  Status_Adaptive = hasil_nested_ad$status,
  Bandwidth_Gaussian_m = hasil_nested_ga$bandwidth,
  AICc_Gaussian = hasil_nested_ga$AICc,
  Status_Gaussian = hasil_nested_ga$status
)

print(hasil_bandwidth_nested)

# M6. Tabel prediksi Nested LOOCV
hasil_prediksi_nested <- data.frame(
  Kabupaten = df_model$Kabupaten,
  Aktual = Y_vec,
  Prediksi_Adaptive = hasil_nested_ad$prediksi,
  Prediksi_Gaussian = hasil_nested_ga$prediksi,
  Theta_Adaptive = hasil_nested_ad$theta,
  Theta_Gaussian = hasil_nested_ga$theta
)

# 1. Bersihkan spasi liar dan samakan tipe data pada kolom kunci 'Kabupaten'
df_04_lengkap <- df_04_lengkap %>%mutate(Kabupaten = as.character(trimws(Kabupaten)))

geo_data <- geo_data %>%mutate(Kabupaten = as.character(trimws(Kabupaten)))

# 2. Lakukan gabungan data (left_join)
geo_data <- geo_data %>%left_join(df_04_lengkap, by = "Kabupaten")

# 3. Tentukan daftar nama variabel koefisien
variabel_x <- c("Beta_X1", "Beta_X2", "Beta_X3", "Beta_X4", "Beta_X5")

# 4. Validasi Keberadaan Kolom (Pencegah Error geom_sf)
missing_cols <- setdiff(variabel_x, colnames(geo_data))
if (length(missing_cols) > 0) {
  stop(paste("Gagal Join! Kolom berikut tidak ditemukan di geo_data:", 
             paste(missing_cols, collapse = ", "),
             "\nPeriksa apakah nama Kabupaten di geo_data dan df_04_lengkap sudah persis sama."))
}

# 5. Looping Pembuatan Peta
for (i in seq_along(variabel_x)) {
  
  kolom <- variabel_x[i]
  peta_koefisien <- ggplot() +
    # Layer 1: Peta Utama
    geom_sf(
      data = geo_data,
      aes(fill = .data[[kolom]]), 
      color = "grey20", 
      linewidth = 0.3
    ) +
    # Layer 2: Label Teks Nama Daerah (tanpa titik)
    geom_text(
      data = label_points,
      aes(x = lon, y = lat, label = nama_label),
      color = "black",
      fontface = "bold",
      size = 3.2,
      check_overlap = TRUE
    ) +
    # Layer 3: Skala Warna Divergen (Dikunci -0.02 s.d 0.02)
    scale_fill_gradient2(
      low = "#2166AC",            # Biru (Negatif)
      mid = "white",              # Putih (Nol)
      high = "#B2182B",           # Merah (Positif)
      midpoint = 0,
      limits = c(-0.02, 0.02),    # Dikunci -0.02 s.d 0.02
      oob = scales::squish,       # Nilai > 0.02 tetap merah pekat, < -0.02 tetap biru pekat
      name = "Local coefficient",
      guide = guide_colorbar(
        barheight = unit(4.5, "cm"),
        barwidth = unit(0.5, "cm"),
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    # Layer 4: Kompas (North Arrow) Kanan Atas
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering(
        text_size = 12,
        line_width = 1.2
      ),
      height = grid::unit(1.8, "cm"),
      width = grid::unit(1.8, "cm"),
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm")
    ) +
    # Layer 5: Skala Jarak Kiri Bawah (Dipindah ke Kiri Bawah agar Kanan Bawah fokus untuk legenda)
    annotation_scale(
      location = "bl", # Bottom Left
      width_hint = 0.2,
      line_width = 1,
      text_cex = 0.8,
      style = "ticks",
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm")
    ) +
    # Layer 6: Batas Koordinat Zoom-In
    coord_sf(
      xlim = c(110.8, 116.1),
      ylim = c(-8.9, -6.3),
      expand = FALSE
    ) +
    labs(x = NULL, y = NULL) +
    # Layer 7: Tema Peta
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#F4F7F9", color = "black", linewidth = 0.8),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "white", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      
      # LEGENDA DI DALAM PETA (Kanan Bawah / Sebelah Banyuwangi)
      legend.position = c(0.85, 0.25),
      legend.justification = c(0.5, 0.5),
      legend.background = element_rect(fill = alpha("white", 0.9), color = "grey70", linewidth = 0.5),
      legend.margin = margin(6, 6, 6, 6),
      legend.title = element_text(size = 9.5, face = "bold"),
      legend.text = element_text(size = 8.5),
      
      axis.text = element_text(size = 8, color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    )
  
  # Menyimpan gambar
  nama_file <- sprintf("04_Peta_Koefisien_GWNBR_%s.png", toupper(kolom))
  
  ggsave(filename = file.path(folder_peta_c, nama_file),plot = peta_koefisien,
         width = 12,height = 7,dpi = 300,bg = "white"
  )
}

# M7. Pemeriksaan fold yang gagal
cat("\nStatus Nested LOOCV Adaptive Bisquare:\n")
print(table(hasil_nested_ad$status))

cat("\nStatus Nested LOOCV Fixed Gaussian:\n")
print(table(hasil_nested_ga$status))

# ---- N. Tabel GWNBR untuk peta (dari hasil bagian F, tanpa estimasi ulang) ----
hasil_gwnbr <- data.frame(
  Kabupaten = df_model$Kabupaten,
  setNames(as.data.frame(beta_mat),                 paste0("Beta_", coef_names)),
  setNames(as.data.frame(unname(se_mat)),           paste0("SE_",   coef_names)),
  Prediksi  = y_hat_gwnbr)


# ---- P. UJI MONTE CARLO NON-STASIONERITAS KOEFISIEN GWNBR ----
set.seed(123)

B <- 999

nama_beta      <- colnames(X_mat)
nama_prediktor <- nama_beta[-1]

# ---- P1. Varians koefisien lokal aktual ----
beta_obs <- as.matrix(
  hasil_gwnbr[, paste0("Beta_", coef_names)]
)

colnames(beta_obs) <- nama_beta

if (any(!is.finite(beta_obs))) {
  stop("Koefisien lokal aktual mengandung nilai tidak valid.")
}

var_obs <- apply(beta_obs, 2, stats::var)


# ---- P2. Matriks bobot Adaptive Bisquare ----
W_opt <- do.call(
  rbind,
  lapply(
    seq_len(n_obs),
    function(i) bobot_adaptif(dist_matrix[i, ], bw_opt)
  )
)

if (any(!is.finite(W_opt)) || any(rowSums(W_opt) <= 0)) {
  stop("Matriks bobot Adaptive Bisquare tidak valid.")
}


# ---- P3. Fungsi estimasi varians koefisien lokal ----
hitung_var_lokal <- function(Y_input, theta_input, beta_start) {
  
  p <- ncol(X_mat)
  
  beta_sim <- matrix(
    NA_real_,
    nrow = n_obs,
    ncol = p
  )
  
  for (i in seq_len(n_obs)) {
    
    fit_i <- tryCatch(
      fit_gwnbr_local(
        X_mat,
        Y_input,
        off_vec,
        theta_input,
        W_opt[i, ],
        beta_start
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit_i) ||
        !isTRUE(fit_i$konvergen) ||
        length(fit_i$beta) != p ||
        any(!is.finite(fit_i$beta))) {
      return(NULL)
    }
    
    beta_sim[i, ] <- as.numeric(fit_i$beta)
  }
  
  apply(beta_sim, 2, stats::var)
}

# ---- P4. Persiapan simulasi di bawah H0 ----
data_sim <- df_model[, c("X1", "X2", "X3", "X4", "X5")]
data_sim$offset_sim <- off_vec

formula_sim <- Y ~ X1 + X2 + X3 + X4 + X5 +
  offset(offset_sim)

eta_h0 <- as.numeric(
  X_mat %*% beta_global + off_vec
)

mu_h0 <- exp(pmin(pmax(eta_h0, -20), 20))

if (any(!is.finite(mu_h0)) || any(mu_h0 <= 0)) {
  stop("Nilai mean simulasi di bawah H0 tidak valid.")
}

# ---- P5. Proses simulasi Monte Carlo ----
var_sim <- matrix(
  NA_real_,
  nrow = B,
  ncol = ncol(X_mat),
  dimnames = list(NULL, nama_beta)
)

gagal <- 0L

for (b in seq_len(B)) {
  
  Y_sim <- MASS::rnegbin(
    n = n_obs,
    mu = mu_h0,
    theta = theta_global
  )
  
  data_sim$Y <- Y_sim
  
  # Estimasi ulang NBR global pada setiap simulasi
  fit_nbr_sim <- tryCatch(
    suppressWarnings(
      MASS::glm.nb(
        formula_sim,
        data = data_sim,
        control = glm.control(maxit = 100)
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit_nbr_sim)) {
    gagal <- gagal + 1L
    next
  }
  
  beta_start_sim <- coef(fit_nbr_sim)
  theta_sim <- fit_nbr_sim$theta
  
  if (any(!is.finite(beta_start_sim)) ||
      !is.finite(theta_sim) ||
      theta_sim <= 0) {
    gagal <- gagal + 1L
    next
  }
  
  # Estimasi koefisien lokal GWNBR
  v_sim <- hitung_var_lokal(
    Y_sim,
    theta_sim,
    beta_start_sim
  )
  
  if (is.null(v_sim) || any(!is.finite(v_sim))) {
    gagal <- gagal + 1L
    next
  }
  
  var_sim[b, ] <- v_sim
  
  if (b %% 50 == 0) {
    cat(
      "Simulasi selesai:",
      b, "dari", B, "\n"
    )
  }
}

jumlah_valid <- colSums(is.finite(var_sim))

cat(
  "\nSimulasi gagal:",
  gagal, "dari", B, "\n"
)

cat(
  "Simulasi valid:",
  B - gagal, "dari", B, "\n"
)

if (B - gagal == 0) {
  stop("Seluruh simulasi gagal.")
}

# ---- P6. Menghitung p-value Monte Carlo ----
p_mc <- setNames(
  rep(NA_real_, ncol(X_mat)),
  nama_beta
)

for (j in seq_len(ncol(X_mat))) {
  
  v <- var_sim[, j]
  v <- v[is.finite(v)]
  
  if (length(v) > 0) {
    p_mc[j] <- (
      1 + sum(v >= var_obs[j])
    ) / (length(v) + 1)
  }
}

# ---- P7. Koreksi Benjamini-Hochberg ----
p_bh <- setNames(
  rep(NA_real_, ncol(X_mat)),
  nama_beta
)

p_bh[nama_prediktor] <- p.adjust(
  p_mc[nama_prediktor],
  method = "BH"
)

# ---- P8. Ringkasan hasil Monte Carlo ----
rata_var_sim <- apply(
  var_sim,
  2,
  function(x) {
    if (all(!is.finite(x))) {
      return(NA_real_)
    }
    mean(x, na.rm = TRUE)
  }
)

hasil_mc <- data.frame(
  Variabel = nama_beta,
  Varians_Observasi = as.numeric(var_obs),
  Rata2_Varians_Sim = as.numeric(rata_var_sim),
  Simulasi_Valid = as.numeric(jumlah_valid),
  P_Monte_Carlo = as.numeric(p_mc),
  P_BH = as.numeric(p_bh),
  Kesimpulan = ifelse(
    !is.na(p_bh) & p_bh < 0.05,
    "Variasi spasial signifikan",
    "Belum ada bukti variasi spasial"
  )
)

print(hasil_mc)

# ---- Q. Ekspor Tahap 4 ----
wb_04 <- createWorkbook()
hasil_sheet <- list(
  "Pemilihan Bandwidth" = tabel_bandwidth,
  "Profil AICc Adaptive" = profil_aicc_adaptive,
  "Profil AICc Gaussian" = profil_aicc_gauss,
  "Evaluasi In Sample" = df_11_evaluasi,
  "Evaluasi LOOCV bw Tetap" = evaluasi_nested_gwnbr,
  "Perbandingan AIC AICc" = tabel_aicc_gwnbr,
  "Koreksi Uji Ganda" = tabel_koreksi,
  "Ringkasan Koefisien" = df_05_ringkas,
  "Jumlah Signifikansi" = df_06_jumlah_sig,
  "Arah Koefisien" = df_07_arah_peng,
  "Tabel Koefisien Lokal" = df_04_lengkap,
  "Kelompok Signifikansi" = df_kelompok_sig,
  "Monte Carlo GWNBR" = hasil_mc
)

for (nm in names(hasil_sheet)) {
  addWorksheet(wb_04, nm)
  writeData(wb_04,sheet = nm,x = hasil_sheet[[nm]])}

saveWorkbook(wb_04,file.path(folder_out_04,"04_Laporan_Hasil_GWNBR.xlsx"),
  overwrite = TRUE)

# ============================================================
# TAHAP 5 & 6: XGBOOST NESTED LOOCV & SHAP
# ============================================================
set.seed(123)
folder_out_05 <- file.path(path_folder, "05_XGBoost")
folder_out_06 <- file.path(path_folder, "06_SHAP")

features   <- c("X1", "X2", "X3", "X4", "X5")
X_matrix   <- as.matrix(df_model[, features]); storage.mode(X_matrix) <- "double"
y_vector   <- as.numeric(df_model$Y)
offset_log <- log(as.numeric(df_model$Jumlah_Penduduk))
n_xgb      <- nrow(X_matrix)

stopifnot(n_xgb == length(y_vector), n_xgb == length(offset_log),
          all(is.finite(X_matrix)), all(is.finite(y_vector)),
          all(is.finite(offset_log)), all(y_vector >= 0))

grid_xgb <- expand.grid(eta = c(0.03, 0.05, 0.10),
                        max_depth = c(2, 3),
                        max_delta_step = c(0.3, 0.7, 1.0))
stopifnot(nrow(grid_xgb) == 18)

buat_params <- function(row) {
  list(booster = "gbtree", objective = "count:poisson", eval_metric = "rmse",
       eta = as.numeric(row$eta), max_depth = as.integer(row$max_depth),
       max_delta_step = as.numeric(row$max_delta_step),
       subsample = 0.70, colsample_bytree = 0.70,
       nthread = 1, seed = 123)   # seed xgboost agar subsampling dapat direproduksi
  }

# Inner LOOCV: setiap elemen folds_inner adalah indeks validasi
tune_internal <- function(idx_train) {
  dtrain_inner <- xgb.DMatrix(data = X_matrix[idx_train, , drop = FALSE],
                              label = y_vector[idx_train],
                              base_margin = offset_log[idx_train])
  folds_inner <- lapply(seq_along(idx_train), function(j) j)
  
  hasil <- vector("list", nrow(grid_xgb))
  for (g in seq_len(nrow(grid_xgb))) {
    cv_g <- xgb.cv(params = buat_params(grid_xgb[g, ]), data = dtrain_inner,
                   nrounds = 1000, folds = folds_inner, early_stopping_rounds = 20,
                   maximize = FALSE, prediction = FALSE, verbose = 0)
    log_cv   <- cv_g$evaluation_log
    idx_best <- which.min(log_cv$test_rmse_mean)
    hasil[[g]] <- data.frame(eta = grid_xgb$eta[g], max_depth = grid_xgb$max_depth[g],
                             max_delta_step = grid_xgb$max_delta_step[g],
                             subsample = 0.70, colsample_bytree = 0.70,
                             best_nrounds = idx_best,
                             inner_CV_RMSE = log_cv$test_rmse_mean[idx_best])}
  hasil <- do.call(rbind, hasil)
  hasil <- hasil[order(hasil$inner_CV_RMSE), ]
  rownames(hasil) <- NULL
  list(best = hasil[1, , drop = FALSE], tuning_table = hasil)}

# Outer LOOCV
pred_outer      <- rep(NA_real_, n_xgb)
parameter_outer <- vector("list", n_xgb)
tuning_outer    <- vector("list", n_xgb)

for (i in seq_len(n_xgb)) {
  cat("Outer fold", i, "dari", n_xgb, "\n")
  idx_train <- setdiff(seq_len(n_xgb), i)
  
  hasil_inner <- tune_internal(idx_train)       # tuning hanya pada training outer fold
  best_i <- hasil_inner$best
  tuning_outer[[i]] <- cbind(data.frame(fold = i), hasil_inner$tuning_table)
  
  nrounds_i <- as.integer(best_i$best_nrounds)
  dtrain_outer <- xgb.DMatrix(X_matrix[idx_train, , drop = FALSE],
                              label = y_vector[idx_train], base_margin = offset_log[idx_train])
  dtest_outer  <- xgb.DMatrix(X_matrix[i, , drop = FALSE], base_margin = offset_log[i])
  
  model_i <- xgb.train(params = buat_params(best_i), data = dtrain_outer,
                       nrounds = nrounds_i, verbose = 0)
  pred_outer[i] <- as.numeric(predict(model_i, dtest_outer))   # skala jumlah kasus
  
  parameter_outer[[i]] <- data.frame(fold = i, eta = best_i$eta, max_depth = best_i$max_depth,
                                     max_delta_step = best_i$max_delta_step,
                                     subsample = best_i$subsample,
                                     colsample_bytree = best_i$colsample_bytree,
                                     nrounds = nrounds_i, inner_CV_RMSE = best_i$inner_CV_RMSE)
}
stopifnot(all(is.finite(pred_outer)))

hasil_eval_nested     <- hitung_evaluasi(y_vector, pred_outer)
hasil_eval_nested <- data.frame(
  R2   = as.numeric(hasil_eval_nested["R2"]),
  RMSE = as.numeric(hasil_eval_nested["RMSE"]),
  MAE  = as.numeric(hasil_eval_nested["MAE"])
)
hasil_parameter_outer <- do.call(rbind, parameter_outer)
hasil_tuning_outer    <- do.call(rbind, tuning_outer)
df_01_pred_xgb <- data.frame(Kabupaten = df_model$Kabupaten, Y_Aktual = y_vector,
                             Y_Prediksi_Nested_LOOCV = pred_outer,
                             Residual = y_vector - pred_outer)

# Frekuensi kombinasi hyperparameter pada 38 outer folds
frekuensi_parameter <- hasil_parameter_outer %>%
  count(eta, max_depth, max_delta_step, name = "Frekuensi")

# Memasukkan kombinasi yang tidak pernah terpilih (frekuensi = 0)
tabel_frekuensi <- grid_xgb %>%
  left_join(
    frekuensi_parameter,
    by = c("eta", "max_depth", "max_delta_step")
  ) %>%
  mutate(
    Frekuensi = tidyr::replace_na(Frekuensi, 0L),
    Persentase = Frekuensi / n_xgb * 100
  ) %>%
  arrange(desc(Frekuensi), eta, max_depth, max_delta_step)

ringkasan_inner_cv <- data.frame(
  Mean_RMSE = mean(hasil_parameter_outer$inner_CV_RMSE),
  SD_RMSE = sd(hasil_parameter_outer$inner_CV_RMSE),
  Min_RMSE = min(hasil_parameter_outer$inner_CV_RMSE),
  Max_RMSE = max(hasil_parameter_outer$inner_CV_RMSE)
)

# Model final (seluruh data) khusus untuk interpretasi SHAP.
# Metrik performa tetap dari nested LOOCV.
hasil_final_tuning <- tune_internal(seq_len(n_xgb))
best_final     <- hasil_final_tuning$best
nrounds_final  <- as.integer(best_final$best_nrounds)
df_tuning_final <- hasil_final_tuning$tuning_table

set.seed(123)

# Ambil params lalu hapus elemen 'seed'
params_clean <- buat_params(best_final)
params_clean$seed <- NULL
params_clean$random_state <- NULL

xgb_final <- xgb.train(
  params = params_clean,
  data = xgb.DMatrix(X_matrix, label = y_vector, base_margin = offset_log),
  nrounds = nrounds_final,
  verbose = 0)

# ---- SHAP (kontribusi pada skala prediksi mentah / log, bukan efek kausal) ----
nama_variabel <- c(
  X1 = "Poor population (%)",
  X2 = "non-PBI BPJS Health participants (%)",
  X3 = "population aged 45-54 who smoke (%)",
  X4 = "Population density (1,000 persons/km2)",
  X5 = "community health centers (puskesmas)(Per 100,000 residents)")

# SHAP dengan base_margin yang sama seperti model training
dmat_shap <- xgb.DMatrix(data = X_matrix,base_margin = offset_log)
shap_contrib <- predict(xgb_final,newdata = dmat_shap,predcontrib = TRUE)

# Mengambil kontribusi kelima variabel (tanpa BIAS)
shap_mat <- shap_contrib[, features, drop = FALSE]

# Membentuk objek shapviz
shp <- shapviz::shapviz(shap_mat,X = as.data.frame(X_matrix))
df_importance <- data.frame(Variabel = features,Label = unname(nama_variabel[features]),
                            Mean_Abs_SHAP = colMeans(abs(shap_mat))) %>%arrange(desc(Mean_Abs_SHAP))
p_imp <- sv_importance(shp, kind = "bar") +scale_y_discrete(labels = nama_variabel) +
  theme_minimal(base_size = 11) +labs(title = "XGBoost Feature Importance (SHAP)",
                                      x = "Mean |SHAP value|",y = NULL) +
  theme(plot.title = element_text(face = "bold", size = 13),axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),panel.grid.major.y = element_blank())
ggsave(file.path(folder_out_06, "01_SHAP_Importance.png"),
       p_imp, width = 10, height = 7,units = "in", dpi = 300, bg = "white")

p_beeswarm <- sv_importance(shp, kind = "beeswarm") +scale_y_discrete(labels = nama_variabel) +
  theme_minimal(base_size = 12) +labs(title = "SHAP Summary (Beeswarm)",x = "SHAP value",y = NULL) +
  theme(axis.text = element_text(size = 10),plot.title = element_text(face = "bold"))
ggsave(file.path(folder_out_06, "02_SHAP_Beeswarm.png"),p_beeswarm, width = 12, height = 8,
       units = "in", dpi = 300, bg = "white")

# BOOTSTRAP SHAP STABILITY
set.seed(123)
B <- 500

# DMatrix untuk menghitung SHAP pada data referensi yang sama
dmat_reference <- xgb.DMatrix(
  data = X_matrix,
  base_margin = offset_log
)

hasil_shap_boot <- vector("list", B)

for (b in seq_len(B)) {
  
  cat("Bootstrap", b, "dari", B, "\n")
  
  # Bootstrap sample
  idx_boot <- sample(
    seq_len(n_xgb),
    size = n_xgb,
    replace = TRUE
  )
  
  dtrain_boot <- xgb.DMatrix(
    data = X_matrix[idx_boot, , drop = FALSE],
    label = y_vector[idx_boot],
    base_margin = offset_log[idx_boot]
  )
  
  # Seed berbeda untuk setiap refit
  params_boot <- params_clean
  params_boot$seed <- 1000 + b
  
  # Refit model dengan konfigurasi final yang sama
  model_boot <- xgb.train(
    params = params_boot,
    data = dtrain_boot,
    nrounds = nrounds_final,
    verbose = 0
  )
  
  # SHAP pada 38 observasi referensi yang sama
  shap_boot <- predict(model_boot,
    newdata = dmat_reference,
    predcontrib = TRUE
  )
  
  shap_boot <- shap_boot[, features, drop = FALSE]
  
  # Mean absolute SHAP setiap variabel
  importance_boot <- colMeans(abs(shap_boot))
  
  # Ranking (1 = paling penting)
  rank_boot <- rank(
    -importance_boot,
    ties.method = "average"
  )
  
  hasil_shap_boot[[b]] <- data.frame(
    Bootstrap = b,
    Variabel = features,
    Mean_Abs_SHAP = as.numeric(importance_boot),
    Rank = as.numeric(rank_boot)
  )
}

# Gabungkan seluruh hasil bootstrap
df_shap_boot <- do.call(rbind, hasil_shap_boot)

# Pastikan seluruh 500 bootstrap berhasil
stopifnot(length(unique(df_shap_boot$Bootstrap)) == B)
stopifnot(nrow(df_shap_boot) == B * length(features))

ringkasan_shap_boot <- df_shap_boot %>%
  group_by(Variabel) %>%
  summarise(
    Rata_Abs_SHAP = mean(Mean_Abs_SHAP, na.rm = TRUE),
    SD_Abs_SHAP = sd(Mean_Abs_SHAP, na.rm = TRUE),
    Mean_Rank = mean(Rank, na.rm = TRUE),
    SD_Rank = sd(Rank, na.rm = TRUE),
    Frekuensi_Rank_1 = sum(Rank == 1, na.rm = TRUE),
    Persentase_Rank_1 = mean(Rank == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  rename(Mean_Abs_SHAP = Rata_Abs_SHAP) %>%
  arrange(Mean_Rank)

openxlsx::write.xlsx(
  list(Prediksi_Nested_LOOCV = df_01_pred_xgb,
       Evaluasi_Nested_LOOCV = hasil_eval_nested,
       Parameter_Outer_Fold  = hasil_parameter_outer,
       Tuning_Inner_Fold     = hasil_tuning_outer,
       Tuning_Model_Final    = df_tuning_final,
       Feature_Importance    = df_importance,
       ringkasan_inner_cv    = ringkasan_inner_cv,
       tabel_frekuensi       = tabel_frekuensi,
       Bootstrap_SHAP        = df_shap_boot,
       Ringkasan_Stabilitas_SHAP = ringkasan_shap_boot),
  file.path(folder_out_05, "05_Laporan_Hasil_XGBoost.xlsx"), overwrite = TRUE)

# ============================================================
# TAHAP 7: LEAVE-SURABAYA-OUT REFIT DAN EVALUASI SENSITIVITAS
# ============================================================
folder_out_07 <- file.path(path_folder, "07_Leave_Surabaya_Out")
dir.create(folder_out_07, showWarnings = FALSE, recursive = TRUE)

# A. Mengeluarkan Kota Surabaya
idx_surabaya <- grep("Kota Surabaya",df_model$Kabupaten,ignore.case = TRUE)

if (length(idx_surabaya) != 1) {
  stop("Kota Surabaya tidak ditemukan atau terdeteksi lebih dari satu.")}

df_lso <- df_model[-idx_surabaya, , drop = FALSE]
coords_lso <- coords[-idx_surabaya, , drop = FALSE]

n_lso <- nrow(df_lso)

cat("Wilayah yang dikeluarkan:", df_model$Kabupaten[idx_surabaya], "\n")
cat("Jumlah wilayah setelah Surabaya dikeluarkan:", n_lso, "\n")

stopifnot(n_lso == 37,nrow(coords_lso) == n_lso)

# Matriks jarak baru untuk 37 wilayah
dist_lso <- as.matrix(dist_matrix[-idx_surabaya, -idx_surabaya])

stopifnot(all(dim(dist_lso) == c(n_lso, n_lso)),all(is.finite(dist_lso)))

# Respons dan offset
Y_lso <- as.numeric(df_lso$Y)
off_lso <- log(as.numeric(df_lso$Jumlah_Penduduk))

# Formula dan matriks prediktor GWNBR
X_lso <- as.matrix( model.matrix(~ X1 + X2 + X3 + X4 + X5, data = df_lso))
mode(X_lso) <- "numeric"

n_vars_lso <- ncol(X_lso)
coef_names_lso <- colnames(X_lso)

# Matriks prediktor XGBoost
features <- c("X1", "X2", "X3", "X4", "X5")

X_xgb_lso <- as.matrix(df_lso[, features])
storage.mode(X_xgb_lso) <- "double"

stopifnot(all(is.finite(Y_lso)),all(is.finite(off_lso)),all(is.finite(X_lso)),all(is.finite(X_xgb_lso)),all(Y_lso >= 0))

# Formula NBR
formula_nbr <- Y ~ X1 + X2 + X3 + X4 + X5 +offset(log(Jumlah_Penduduk))

# B. LOOCV BASELINE NB-OFFSET DAN NBR
pred_nb0_lso <- rep(NA_real_, n_lso)
pred_nbr_lso <- rep(NA_real_, n_lso)

for (i in seq_len(n_lso)) {
  idx_train <- setdiff(seq_len(n_lso), i)
  
  # Baseline NB-offset
  fit_nb0_i <- tryCatch(suppressWarnings(MASS::glm.nb(
    Y ~ 1 + offset(log(Jumlah_Penduduk)),
    data = df_lso[idx_train, , drop = FALSE])),
    error = function(e) NULL)
  
  if (!is.null(fit_nb0_i)) {
    pred_nb0_lso[i] <- as.numeric(predict(fit_nb0_i,newdata = df_lso[i, , drop = FALSE],
                                          type = "response"))}
  # NBR
  fit_nbr_i <- tryCatch(
    suppressWarnings(
      MASS::glm.nb(formula_nbr,data = df_lso[idx_train, , drop = FALSE])),
    error = function(e) {
      message("Fold ", i, " gagal: ", conditionMessage(e))
      return(NULL)})
  
  if (!is.null(fit_nbr_i)) {
    pred_nbr_lso[i] <- tryCatch(
      as.numeric(predict(fit_nbr_i,newdata = df_lso[i, , drop = FALSE],type = "response")),
      error = function(e) NA_real_)
  }
}
  if (any(!is.finite(pred_nb0_lso))) {
    stop("Terdapat prediksi LOOCV baseline NB-offset yang tidak valid.")}
  
  if (any(!is.finite(pred_nbr_lso))) {
    stop("Terdapat prediksi LOOCV NBR yang tidak valid.")}
  
# C. GWNBR ADAPTIVE BISQUARE
# C1. Estimasi NBR global pada 37 wilayah
model_nbr_lso <- MASS::glm.nb(formula_nbr,data = df_lso)

theta_lso <- as.numeric(model_nbr_lso$theta)
beta_lso_global <- as.numeric(coef(model_nbr_lso))

## C2. Pemilihan bandwidth GWNBR pada 37 wilayah

# Estimasi NBR awal pada seluruh data LSO
nbr_lso <- estimasi_nbr_awal(X = X_lso,Y = Y_lso,offset_val = off_lso)

theta_lso <- nbr_lso$theta
beta_start_lso <- nbr_lso$beta_start

# 1. ADAPTIVE BISQUARE
min_k_lso <- ncol(X_lso) + 3

# Maksimum k adalah n_lso - 1 karena jarak memuat
# jarak wilayah terhadap dirinya sendiri (0)
k_range_lso <- min_k_lso:(n_lso - 1)

hasil_bw_ad_lso <- lapply(k_range_lso, function(k) {
  tryCatch({
    hitung_ic_gwnbr(
      W_fun = function(i) {
        bobot_adaptif(dist_lso[i, ], k)
      },
      X = X_lso,
      Y = Y_lso,
      offset_val = off_lso,
      theta = theta_lso,
      beta_start = beta_start_lso
    )
  }, error = function(e) NULL)
})

aicc_ad_lso <- sapply(hasil_bw_ad_lso, function(x) {
  if (is.null(x)) return(Inf)
  x$AICc
})

idx_ad_lso <- which.min(aicc_ad_lso)

bw_lso <- k_range_lso[idx_ad_lso]
aicc_min_ad_lso <- aicc_ad_lso[idx_ad_lso]

# Tabel profil Adaptive Bisquare
profil_ad_lso <- data.frame(Kernel = "Adaptive Bisquare",Bandwidth = k_range_lso,AICc = aicc_ad_lso)
# 2. FIXED GAUSSIAN
# Mengambil jarak antarwilayah tanpa diagonal
jarak_unik_lso <- dist_lso[upper.tri(dist_lso)]
jarak_unik_lso <- sort(unique(as.numeric(jarak_unik_lso)))
jarak_maks_lso <- max(jarak_unik_lso, na.rm = TRUE)

# Kandidat bandwidth dari kuantil jarak dan perluasan
h_quantile_lso <- as.numeric(
  quantile(
    jarak_unik_lso,
    probs = seq(0.05, 0.95, length.out = 19),
    na.rm = TRUE
  )
)

h_extended_lso <- seq(jarak_maks_lso * 1.05,jarak_maks_lso * 32,length.out = 100)

h_candidates_lso <- sort(unique(c(h_quantile_lso,h_extended_lso)))

hasil_bw_ga_lso <- lapply(h_candidates_lso, function(h) {
  tryCatch({
    hitung_ic_gwnbr(
      W_fun = function(i) {
        bobot_fixed_gaussian(dist_lso[i, ], h)
      },
      X = X_lso,
      Y = Y_lso,
      offset_val = off_lso,
      theta = theta_lso,
      beta_start = beta_start_lso
    )
  }, error = function(e) NULL)
})

aicc_ga_lso <- sapply(hasil_bw_ga_lso, function(x) {
  if (is.null(x)) return(Inf)
  x$AICc
})

idx_ga_lso <- which.min(aicc_ga_lso)

bw_gauss_lso <- h_candidates_lso[idx_ga_lso]
aicc_min_ga_lso <- aicc_ga_lso[idx_ga_lso]

# Tabel profil Fixed Gaussian
profil_ga_lso <- data.frame(Kernel = "Fixed Gaussian",Bandwidth = h_candidates_lso,AICc = aicc_ga_lso)

# Gabungkan profil bandwidth
profil_bw_lso <- rbind(profil_ad_lso,profil_ga_lso)

# Ringkasan bandwidth terpilih
ringkasan_bw_lso <- data.frame(
  Kernel = c("Adaptive Bisquare", "Fixed Gaussian"),
  Bandwidth = c(bw_lso, bw_gauss_lso),
  Satuan = c("Jumlah tetangga", "Meter"),
  AICc = c(aicc_min_ad_lso, aicc_min_ga_lso)
)

## C3. Nested LOOCV GWNBR pada 37 wilayah
nested_loocv_gwnbr_lso <- function(X, Y, offset_val, dist_mat
) {
  n <- nrow(X)
  hasil_fold <- vector("list", n)
  
  for (i in seq_len(n)) {
    cat("Nested LOOCV GWNBR LSO - Fold", i, "dari", n, "\n")
    
    # 1. Membagi data training dan testing
    idx_train <- setdiff(seq_len(n), i)
    
    X_train <- X[idx_train, , drop = FALSE]
    Y_train <- Y[idx_train]
    off_train <- offset_val[idx_train]
    
    # Matriks jarak training saja
    dist_train <- dist_mat[idx_train, idx_train, drop = FALSE]
    
    # Jarak wilayah testing terhadap seluruh training
    dist_test_train <- dist_mat[i, idx_train]
    
    X_test <- X[i, , drop = FALSE]
    off_test <- offset_val[i]
    
    n_train <- nrow(X_train)
    
    # 2. Estimasi NBR pada data training
    nbr_train <- tryCatch(
      estimasi_nbr_awal(
        X = X_train,
        Y = Y_train,
        offset_val = off_train
      ),
      error = function(e) NULL
    )
    
    if (is.null(nbr_train)) {
      hasil_fold[[i]] <- data.frame(
        Fold = i,
        Pred_Adaptive = NA_real_,
        Pred_Gaussian = NA_real_,
        BW_Adaptive = NA_real_,
        BW_Gaussian = NA_real_,
        AICc_Adaptive = NA_real_,
        AICc_Gaussian = NA_real_
      )
      next
    }
    
    theta_train <- nbr_train$theta
    beta_start_train <- nbr_train$beta_start
    
    # 3. Pemilihan bandwidth Adaptive Bisquare
    #    hanya menggunakan data training
    min_k_fold <- ncol(X_train) + 3
    k_range_fold <- min_k_fold:(n_train - 1)
    
    hasil_ad_fold <- lapply(k_range_fold, function(k) {
      tryCatch({
        hitung_ic_gwnbr(
          W_fun = function(j) {
            bobot_adaptif(dist_train[j, ], k)
          },
          X = X_train,
          Y = Y_train,
          offset_val = off_train,
          theta = theta_train,
          beta_start = beta_start_train
        )
      }, error = function(e) NULL)
    })
    
    aicc_ad_fold <- sapply(hasil_ad_fold, function(x) {
      if (is.null(x)) return(Inf)
      x$AICc
    })
    
    if (all(!is.finite(aicc_ad_fold))) {
      hasil_fold[[i]] <- data.frame(
        Fold = i,
        Pred_Adaptive = NA_real_,
        Pred_Gaussian = NA_real_,
        BW_Adaptive = NA_real_,
        BW_Gaussian = NA_real_,
        AICc_Adaptive = NA_real_,
        AICc_Gaussian = NA_real_
      )
      next
    }
    
    idx_ad_fold <- which.min(aicc_ad_fold)
    bw_ad_fold <- k_range_fold[idx_ad_fold]
    aicc_ad_fold_min <- aicc_ad_fold[idx_ad_fold]
    
    # 4. Pemilihan bandwidth Fixed Gaussian
    #    hanya menggunakan data training
    jarak_train <- dist_train[upper.tri(dist_train)]
    jarak_train <- sort(unique(as.numeric(jarak_train)))
    jarak_maks_train <- max(jarak_train, na.rm = TRUE)
    
    h_quantile_fold <- as.numeric(
      quantile(
        jarak_train,
        probs = seq(0.05, 0.95, length.out = 19),
        na.rm = TRUE
      )
    )
    
    h_extended_fold <- seq(
      jarak_maks_train * 1.05,
      jarak_maks_train * 32,
      length.out = 100
    )
    
    h_candidates_fold <- sort(unique(c(
      h_quantile_fold,
      h_extended_fold
    )))
    
    hasil_ga_fold <- lapply(h_candidates_fold, function(h) {
      tryCatch({
        hitung_ic_gwnbr(
          W_fun = function(j) {
            bobot_fixed_gaussian(dist_train[j, ], h)
          },
          X = X_train,
          Y = Y_train,
          offset_val = off_train,
          theta = theta_train,
          beta_start = beta_start_train
        )
      }, error = function(e) NULL)
    })
    
    aicc_ga_fold <- sapply(hasil_ga_fold, function(x) {
      if (is.null(x)) return(Inf)
      x$AICc
    })
    
    if (all(!is.finite(aicc_ga_fold))) {
      hasil_fold[[i]] <- data.frame(
        Fold = i,
        Pred_Adaptive = NA_real_,
        Pred_Gaussian = NA_real_,
        BW_Adaptive = bw_ad_fold,
        BW_Gaussian = NA_real_,
        AICc_Adaptive = aicc_ad_fold_min,
        AICc_Gaussian = NA_real_
      )
      next
    }
    
    idx_ga_fold <- which.min(aicc_ga_fold)
    bw_ga_fold <- h_candidates_fold[idx_ga_fold]
    aicc_ga_fold_min <- aicc_ga_fold[idx_ga_fold]
    
    # 5. Bobot untuk wilayah testing
    #    testing tidak termasuk dalam data training
    W_ad_test <- bobot_adaptif_new(
      dist_test_train,
      bw_ad_fold
    )
    
    W_ga_test <- bobot_fixed_gaussian(
      dist_test_train,
      bw_ga_fold
    )
    
      # 6. Estimasi model lokal Adaptive Bisquare
    fit_ad_test <- tryCatch(
      fit_gwnbr_local(
        X = X_train,
        Y = Y_train,
        offset_val = off_train,
        theta = theta_train,
        W_i = W_ad_test,
        beta_start = beta_start_train
      ),
      error = function(e) NULL
    )
    
    pred_ad <- NA_real_
    
    if (!is.null(fit_ad_test)) {
      if (all(is.finite(fit_ad_test$beta))) {
        eta_ad <- sum(X_test * fit_ad_test$beta) + off_test
        pred_ad <- exp(pmin(pmax(eta_ad, -20), 20))
      }
    }
    
    # 7. Estimasi model lokal Fixed Gaussian
    fit_ga_test <- tryCatch(
      fit_gwnbr_local(
        X = X_train,
        Y = Y_train,
        offset_val = off_train,
        theta = theta_train,
        W_i = W_ga_test,
        beta_start = beta_start_train
      ),
      error = function(e) NULL
    )
    
    pred_ga <- NA_real_
    
    if (!is.null(fit_ga_test)) {
      if (all(is.finite(fit_ga_test$beta))) {
        eta_ga <- sum(X_test * fit_ga_test$beta) + off_test
        pred_ga <- exp(pmin(pmax(eta_ga, -20), 20))
      }
    }
    
    # 8. Menyimpan hasil fold
    hasil_fold[[i]] <- data.frame(
      Fold = i,
      Pred_Adaptive = as.numeric(pred_ad),
      Pred_Gaussian = as.numeric(pred_ga),
      BW_Adaptive = bw_ad_fold,
      BW_Gaussian = bw_ga_fold,
      AICc_Adaptive = aicc_ad_fold_min,
      AICc_Gaussian = aicc_ga_fold_min
    )
  }
  
  do.call(rbind, hasil_fold)
}

# Jalankan nested LOOCV
hasil_nested_lso <- nested_loocv_gwnbr_lso(
  X = X_lso,
  Y = Y_lso,
  offset_val = off_lso,
  dist_mat = dist_lso
)

# Tambahkan nilai aktual
hasil_nested_lso$Aktual <- as.numeric(Y_lso)

# Urutkan kolom
hasil_nested_lso <- hasil_nested_lso[, c(
  "Fold", "Aktual",
  "Pred_Adaptive", "Pred_Gaussian",
  "BW_Adaptive", "BW_Gaussian",
  "AICc_Adaptive", "AICc_Gaussian"
)]

# Persiapan objek XGBoost LSO
y_xgb_lso <- Y_lso
offset_xgb_lso <- off_lso
n_xgb_lso <- nrow(X_xgb_lso)

stopifnot(
  n_xgb_lso == 37,
  length(y_xgb_lso) == n_xgb_lso,
  length(offset_xgb_lso) == n_xgb_lso
)

# D1. Inner LOOCV untuk tuning
grid_xgb_lso <- expand.grid(eta = c(0.03, 0.05, 0.10),
                            max_depth = c(2, 3),
                            max_delta_step = c(0.3, 0.7, 1.0))
stopifnot(nrow(grid_xgb_lso) == 18)

buat_params_lso <- function(row) {
  list(
    booster = "gbtree",
    objective = "count:poisson",
    eval_metric = "rmse",
    eta = as.numeric(row$eta),
    max_depth = as.integer(row$max_depth),
    max_delta_step = as.numeric(row$max_delta_step),
    subsample = 0.70,
    colsample_bytree = 0.70,
    nthread = 1
  )
}

tune_internal_lso <- function(idx_train) {
  
  dtrain_inner <- xgb.DMatrix(data = X_xgb_lso[idx_train, , drop = FALSE],
                              label = y_xgb_lso[idx_train],base_margin = offset_xgb_lso[idx_train])
  
  folds_inner <- lapply(seq_along(idx_train),function(j) j)
  
  hasil <- vector("list", nrow(grid_xgb_lso))
  
  for (g in seq_len(nrow(grid_xgb_lso))) {
    
    cv_g <- xgb.cv(
      params = buat_params_lso(grid_xgb_lso[g, ]),
      data = dtrain_inner,
      nrounds = 1000,
      folds = folds_inner,
      early_stopping_rounds = 20,
      maximize = FALSE,
      prediction = FALSE,
      verbose = 0
    )
    
    log_cv <- cv_g$evaluation_log
    idx_best <- which.min(log_cv$test_rmse_mean)
    
    hasil[[g]] <- data.frame(
      eta = grid_xgb_lso$eta[g],
      max_depth = grid_xgb_lso$max_depth[g],
      max_delta_step = grid_xgb_lso$max_delta_step[g],
      subsample = 0.70,
      colsample_bytree = 0.70,
      best_nrounds = idx_best,
      inner_CV_RMSE = log_cv$test_rmse_mean[idx_best]
    )
  }
  
  hasil <- do.call(rbind, hasil)
  hasil <- hasil[order(hasil$inner_CV_RMSE), ]
  rownames(hasil) <- NULL
  
  list(best = hasil[1, , drop = FALSE],tuning_table = hasil)
}

# D2. Outer LOOCV
pred_xgb_lso <- rep(NA_real_, n_xgb_lso)
parameter_xgb_lso <- vector("list", n_xgb_lso)
tuning_xgb_lso <- vector("list", n_xgb_lso)
set.seed(123)
for (i in seq_len(n_xgb_lso)) {
  
  cat("XGBoost LSO outer fold", i, "dari", n_xgb_lso, "\n")
  
  idx_train <- setdiff(seq_len(n_xgb_lso), i)
  
  hasil_inner <- tune_internal_lso(idx_train)
  best_i <- hasil_inner$best
  
  tuning_xgb_lso[[i]] <- cbind(data.frame(fold = i),hasil_inner$tuning_table)
  
  nrounds_i <- as.integer(best_i$best_nrounds)
  
  dtrain_outer <- xgb.DMatrix(data = X_xgb_lso[idx_train, , drop = FALSE],
                              label = y_xgb_lso[idx_train],base_margin = offset_xgb_lso[idx_train])
  
  dtest_outer <- xgb.DMatrix(data = X_xgb_lso[i, , drop = FALSE],base_margin = offset_xgb_lso[i])
  
  model_i <- xgb.train(params = buat_params_lso(best_i),data = dtrain_outer,
                       nrounds = nrounds_i,verbose = 0)
  
  pred_xgb_lso[i] <- as.numeric(predict(model_i, dtest_outer))
  
  parameter_xgb_lso[[i]] <- data.frame(
    fold = i,
    eta = best_i$eta,
    max_depth = best_i$max_depth,
    max_delta_step = best_i$max_delta_step,
    subsample = best_i$subsample,
    colsample_bytree = best_i$colsample_bytree,
    nrounds = nrounds_i,
    inner_CV_RMSE = best_i$inner_CV_RMSE
  )
}

if (any(!is.finite(pred_xgb_lso))) {stop("Terdapat prediksi nested LOOCV XGBoost yang tidak valid.")}

## E. Evaluasi Nested LOOCV GWNBR LSO
hitung_evaluasi_lso <- function(actual, pred) {
  
  valid <- is.finite(actual) & is.finite(pred)
  actual <- actual[valid]
  pred <- pred[valid]
  n_valid <- length(actual)
  
  if (n_valid < 2) {
    return(data.frame(
      N_Valid = n_valid,
      R2_LOOCV = NA_real_,
      RMSE_LOOCV = NA_real_,
      MAE_LOOCV = NA_real_))
  }
  
  SSE <- sum((actual - pred)^2)
  SST <- sum((actual - mean(actual))^2)
  
  data.frame(
    N_Valid = n_valid,
    R2_LOOCV = 1 - SSE / SST,
    RMSE_LOOCV = sqrt(mean((actual - pred)^2)),
    MAE_LOOCV = mean(abs(actual - pred))
  )
}

evaluasi_ad_lso <- hitung_evaluasi_lso(hasil_nested_lso$Aktual,
                                       hasil_nested_lso$Pred_Adaptive)

evaluasi_ga_lso <- hitung_evaluasi_lso(hasil_nested_lso$Aktual,
                                       hasil_nested_lso$Pred_Gaussian)

hasil_evaluasi_gwnbr_lso <- rbind(
  cbind(Model = "GWNBR Adaptive Bisquare",evaluasi_ad_lso),
  cbind(Model = "GWNBR Fixed Gaussian",evaluasi_ga_lso)
)

# F. MODEL FINAL XGBOOST PADA 37 WILAYAH
hasil_tuning_final_lso <- tune_internal_lso(seq_len(n_xgb_lso))

best_final_lso <- hasil_tuning_final_lso$best
nrounds_final_lso <- as.integer(best_final_lso$best_nrounds)

dtrain_final_lso <- xgb.DMatrix(data = X_xgb_lso,label = y_xgb_lso,base_margin = offset_xgb_lso)

params_final_lso <- buat_params_lso(best_final_lso)
params_final_lso$seed <- NULL

set.seed(123)

xgb_final_lso <- xgb.train(params = params_final_lso,data = dtrain_final_lso,
                           nrounds = nrounds_final_lso,verbose = 0)

# Prediksi in-sample model final (bukan metrik LOOCV)
pred_xgb_final_lso <- as.numeric(predict(xgb_final_lso, dtrain_final_lso))

## G. Estimasi akhir GWNBR pada 37 wilayah
# 1. ADAPTIVE BISQUARE
hasil_gwnbr_lso_ad <- vector("list", n_lso)

for (i in seq_len(n_lso)) {
  W_i <- bobot_adaptif(dist_lso[i, ],bw_lso)
  hasil_gwnbr_lso_ad[[i]] <- fit_gwnbr_local(
    X = X_lso,
    Y = Y_lso,
    offset_val = off_lso,
    theta = theta_lso,
    W_i = W_i,
    beta_start = beta_start_lso
  )
}

beta_lokal_ad_lso <- do.call(rbind,
  lapply(hasil_gwnbr_lso_ad, function(x) x$beta))

# 2. FIXED GAUSSIAN
hasil_gwnbr_lso_ga <- vector("list", n_lso)

for (i in seq_len(n_lso)) {
  W_i <- bobot_fixed_gaussian(dist_lso[i, ],bw_gauss_lso)
  hasil_gwnbr_lso_ga[[i]] <- fit_gwnbr_local(
    X = X_lso,
    Y = Y_lso,
    offset_val = off_lso,
    theta = theta_lso,
    W_i = W_i,
    beta_start = beta_start_lso
  )
}

beta_lokal_ga_lso <- do.call(rbind,lapply(hasil_gwnbr_lso_ga, function(x) x$beta))

colnames(beta_lokal_ad_lso) <- colnames(X_lso)
colnames(beta_lokal_ga_lso) <- colnames(X_lso)
rownames(beta_lokal_ad_lso) <- df_lso$Kabupaten_Kota
rownames(beta_lokal_ga_lso) <- df_lso$Kabupaten_Kota

# H. REKAP PARAMETER MODEL GLOBAL
# Model Baseline NB-offset pada 37 wilayah (tanpa Surabaya)
model_nb0_no_sby <- MASS::glm.nb(Y ~ 1 + offset(log(Jumlah_Penduduk)),data = df_lso)

hasil_parameter_global_lso <- data.frame(
  Model = c("Baseline NB-offset", "NBR"),
  Theta = c(model_nb0_no_sby$theta,model_nbr_lso$theta),
  AIC = c(AIC(model_nb0_no_sby),AIC(model_nbr_lso))
)

# I. TABEL PREDIKSI NESTED LOOCV
hasil_prediksi_lso <- data.frame(
  Kabupaten = df_lso$Kabupaten,
  Y_Aktual = Y_lso,
  Baseline_NB_offset = pred_nb0_lso,
  NBR = pred_nbr_lso,
  GWNBR_Adaptive_Bisquare = hasil_nested_lso$Pred_Adaptive,
  GWNBR_Fixed_Gaussian = hasil_nested_lso$Pred_Gaussian,
  XGBoost = pred_xgb_lso
)

# TABEL 11. EVALUASI LOOCV LSO KELIMA MODEL
# Fungsi evaluasi LOOCV
hitung_evaluasi_lso <- function(actual, pred, populasi) {

  valid <- is.finite(actual) & 
           is.finite(pred) & 
           is.finite(populasi) & 
           populasi > 0

  actual <- actual[valid]
  pred <- pred[valid]
  populasi <- populasi[valid]

  n_valid <- length(actual)

  if (n_valid < 2) {
    return(data.frame(
      N = n_valid,
      R2 = NA_real_,
      RMSE = NA_real_,
      RMSE_per_100000 = NA_real_,
      MAE = NA_real_
    ))
  }

  SSE <- sum((actual - pred)^2)
  SST <- sum((actual - mean(actual))^2)

  data.frame(
    N = n_valid,
    R2 = 1 - SSE / SST,
    RMSE = sqrt(mean((actual - pred)^2)),
    RMSE_per_100000 = sqrt(mean(
      ((actual - pred) / populasi * 100000)^2
    )),
    MAE = mean(abs(actual - pred))
  )
}

# Prediksi kelima model
prediksi_lima_model_lso <- list(
  "Baseline NB-offset" = hasil_prediksi_lso$Baseline_NB_offset,
  "NBR" = hasil_prediksi_lso$NBR,
  "GWNBR Adaptive Bisquare" = hasil_prediksi_lso$GWNBR_Adaptive_Bisquare,
  "GWNBR Fixed Gaussian" = hasil_prediksi_lso$GWNBR_Fixed_Gaussian,
  "Poisson XGBoost" = hasil_prediksi_lso$XGBoost
)

# Mengambil populasi usia 15 tahun ke atas dari offset LSO
populasi_15_lso <- exp(off_lso)

# Memastikan jumlah dan urutan data
length(Y_lso)
length(populasi_15_lso)

# Menghitung evaluasi masing-masing model
tabel11_lso <- do.call(
  rbind, lapply(names(prediksi_lima_model_lso), function(nama_model) {
    
    hasil <- hitung_evaluasi_lso(
      actual = Y_lso,
      pred = prediksi_lima_model_lso[[nama_model]],
      populasi = populasi_15_lso
    )
    
    cbind(Model = nama_model, hasil)
  })
)

rownames(tabel11_lso) <- NULL

# Memastikan seluruh model memiliki 37 prediksi valid
stopifnot(all(tabel11_lso$N == 37))

tabel11_naskah <- tabel11_lso[, c("Model", "R2", "RMSE", "MAE", "RMSE_per_100000")]

# PERBANDINGAN PARAMETER SEBELUM DAN SESUDAH SURABAYA DIKELUARKAN
# 1. PERBANDINGAN PARAMETER NBR
parameter_nbr_38 <- coef(model_nbr)
parameter_nbr_37 <- coef(model_nbr_lso)

hasil_perbandingan_nbr <- data.frame(
  Parameter = names(parameter_nbr_38),
  NBR_38_Wilayah = as.numeric(parameter_nbr_38),
  NBR_37_Wilayah = as.numeric(parameter_nbr_37))

# Tambahkan parameter dispersi (Theta)
hasil_perbandingan_nbr <- rbind(
  hasil_perbandingan_nbr,
  data.frame(Parameter = "Theta",NBR_38_Wilayah = model_nbr$theta,NBR_37_Wilayah = model_nbr_lso$theta))

# Hitung perubahan parameter NBR
hasil_perbandingan_nbr$Perubahan <-
  hasil_perbandingan_nbr$NBR_37_Wilayah -
  hasil_perbandingan_nbr$NBR_38_Wilayah

# 2. PERBANDINGAN KOEFISIEN LOKAL GWNBR
hasil_38 <- hasil_gwnbr
hasil_37 <- data.frame(
  Kabupaten = df_lso$Kabupaten,
  Beta_Intercept = beta_lokal_ad_lso[, "(Intercept)"],
  Beta_X1 = beta_lokal_ad_lso[, "X1"],
  Beta_X2 = beta_lokal_ad_lso[, "X2"],
  Beta_X3 = beta_lokal_ad_lso[, "X3"],
  Beta_X4 = beta_lokal_ad_lso[, "X4"],
  Beta_X5 = beta_lokal_ad_lso[, "X5"]
)

# Samakan nama kolom intercept
names(hasil_37)[
  names(hasil_37) == "Beta_.Intercept."
] <- "Beta_Intercept"

# Ambil kolom kabupaten dan koefisien
koef_38 <- hasil_38[, c("Kabupaten", "Beta_Intercept","Beta_X1", "Beta_X2", "Beta_X3","Beta_X4", "Beta_X5")]
koef_37 <- hasil_37[, c("Kabupaten", "Beta_Intercept","Beta_X1", "Beta_X2", "Beta_X3","Beta_X4", "Beta_X5")]

# Cocokkan berdasarkan nama kabupaten/kota
perbandingan_gwnbr <- merge(koef_38,koef_37,by = "Kabupaten",suffixes = c("_38", "_37"))

# Pastikan terdapat 37 wilayah yang sama
stopifnot(nrow(perbandingan_gwnbr) == 37)

# Nama parameter koefisien
variabel_beta <- c("Beta_Intercept", "Beta_X1", "Beta_X2","Beta_X3", "Beta_X4", "Beta_X5")

# Hitung perubahan koefisien setiap wilayah
for (v in variabel_beta) {
  perbandingan_gwnbr[[paste0(v, "_Perubahan")]] <-
    perbandingan_gwnbr[[paste0(v, "_37")]] -
    perbandingan_gwnbr[[paste0(v, "_38")]]
}

# 3. RINGKASAN PERUBAHAN KOEFISIEN GWNBR
ringkasan_gwnbr <- data.frame(
  Variabel = variabel_beta,
  
  Rata_rata_38 = sapply(variabel_beta, function(v) {
    mean(perbandingan_gwnbr[[paste0(v, "_38")]],na.rm = TRUE)}),
  
  Rata_rata_37 = sapply(variabel_beta, function(v) {
    mean(perbandingan_gwnbr[[paste0(v, "_37")]],na.rm = TRUE)}),
  
  Rata_rata_Perubahan = sapply(variabel_beta, function(v) {
    mean(perbandingan_gwnbr[[paste0(v, "_Perubahan")]],na.rm = TRUE)}),
  
  Jumlah_Berubah_Tanda = sapply(variabel_beta, function(v) {
    a <- perbandingan_gwnbr[[paste0(v, "_38")]]
    b <- perbandingan_gwnbr[[paste0(v, "_37")]]
    
    sum(a * b < 0, na.rm = TRUE)})
)

# TABEL 12. PERBANDINGAN PARAMETER NBR DAN GWNBR
tabel12_lso <- data.frame(
  Variabel = c("Intercept", "X1", "X2", "X3", "X4", "X5", "Theta"),
  NBR_38 = c(unname(parameter_nbr_38),model_nbr$theta),
  NBR_37 = c(unname(parameter_nbr_37),model_nbr_lso$theta),
  GWNBR_mean_38 = c(ringkasan_gwnbr$Rata_rata_38,NA_real_),
  GWNBR_mean_37 = c(ringkasan_gwnbr$Rata_rata_37,NA_real_))

# Format empat angka desimal
tabel12_naskah <- tabel12_lso
kolom_numerik <- c("NBR_38", "NBR_37","GWNBR_mean_38", "GWNBR_mean_37")

for (kolom in kolom_numerik) {
  tabel12_naskah[[kolom]] <- ifelse(
    is.na(tabel12_lso[[kolom]]),"–",
    formatC(tabel12_lso[[kolom]],format = "f",digits = 4)
  )
}

# 4. IDENTIFIKASI WILAYAH YANG BERUBAH TANDA KOEFISIEN
# Perubahan tanda koefisien X3
perubahan_tanda_X3 <- subset(perbandingan_gwnbr,Beta_X3_38 * Beta_X3_37 < 0,
  select = c(Kabupaten,Beta_X3_38,Beta_X3_37,Beta_X3_Perubahan))

# Perubahan tanda koefisien X5
perubahan_tanda_X5 <- subset(perbandingan_gwnbr,Beta_X5_38 * Beta_X5_37 < 0,
  select = c(Kabupaten,Beta_X5_38,Beta_X5_37,Beta_X5_Perubahan))


# 5. SIMPAN SELURUH HASIL KE EXCEL
openxlsx::write.xlsx(
  list(
    Tabel_11_Evaluasi_LOOCV = tabel11_naskah,
    Tabel_12_Perbandingan_Parameter = tabel12_naskah,
    Ringkasan_Bandwidth = ringkasan_bw_lso,
    Profil_Bandwidth = profil_bw_lso,
    Hasil_Nested_LOOCV_GWNBR = hasil_nested_lso,
    Evaluasi_LOOCV_GWNBR = hasil_evaluasi_gwnbr_lso,
    Koefisien_Adaptive = as.data.frame(beta_lokal_ad_lso),
    Koefisien_Fixed_Gaussian = as.data.frame(beta_lokal_ga_lso),
    Prediksi_LOOCV = hasil_prediksi_lso,
    Parameter_Global = hasil_parameter_global_lso,
    Parameter_Nested_XGB = do.call(rbind, parameter_xgb_lso),
    Tuning_Nested_XGB = do.call(rbind, tuning_xgb_lso),
    Tuning_Final_XGB = hasil_tuning_final_lso$tuning_table,
    Perbandingan_Parameter_NBR = hasil_perbandingan_nbr,
    Perbandingan_Koefisien_GWNBR = perbandingan_gwnbr,
    Ringkasan_Perubahan_GWNBR = ringkasan_gwnbr,
    Perubahan_Tanda_X3 = perubahan_tanda_X3,
    Perubahan_Tanda_X5 = perubahan_tanda_X5
  ),
  
  file.path(folder_out_07,"07_Seluruh_Hasil_LOSO_37_Wilayah.xlsx"),
  overwrite = TRUE
)

# ============================================================
# TAHAP 8: PERBANDINGAN MODEL
# ============================================================
folder_out_08 <- file.path(path_folder, "08_Perbandingan")
dir.create(folder_out_08, showWarnings = FALSE, recursive = TRUE)

# 1. PERSIAPAN DAN PENYELARASAN HASIL PREDIKSI
n_loo <- nrow(df_model)

y_aktual <- as.numeric(df_model$Y)
pop <- as.numeric(df_model$Jumlah_Penduduk)
nama_wilayah <- as.character(df_model$Kabupaten)

# Pastikan seluruh prediksi berasal dari 38 wilayah yang sama
stopifnot(length(pred_nb0_loo) == n_loo)
stopifnot(length(y_pred_nbr_loo) == n_loo)

# Nested LOOCV GWNBR
idx_gwnbr <- match(nama_wilayah,hasil_prediksi_nested$Kabupaten)

# Nested LOOCV XGBoost
idx_xgb <- match(nama_wilayah,df_01_pred_xgb$Kabupaten)

stopifnot(!anyNA(idx_gwnbr))
stopifnot(!anyNA(idx_xgb))

# Ambil prediksi GWNBR kedua kernel
pred_adaptive <- hasil_prediksi_nested$Prediksi_Adaptive[idx_gwnbr]
pred_gaussian <- hasil_prediksi_nested$Prediksi_Gaussian[idx_gwnbr]

# Ambil prediksi XGBoost
pred_xgb <- df_01_pred_xgb$Y_Prediksi_Nested_LOOCV[idx_xgb]

# Periksa jumlah prediksi valid
jumlah_valid <- c(
  Baseline_NB = sum(is.finite(pred_nb0_loo)),
  NBR = sum(is.finite(y_pred_nbr_loo)),
  GWNBR_Adaptive = sum(is.finite(pred_adaptive)),
  GWNBR_Gaussian = sum(is.finite(pred_gaussian)),
  XGBoost = sum(is.finite(pred_xgb))
)

# 2. MENYELARASKAN SELURUH PREDIKSI
pred_list <- list(
  "Baseline NB-offset" = as.numeric(pred_nb0_loo),
  "NBR" = as.numeric(y_pred_nbr_loo),
  "GWNBR Adaptive Bisquare" = as.numeric(pred_adaptive),
  "GWNBR Fixed Gaussian" = as.numeric(pred_gaussian),
  "XGBoost" = as.numeric(pred_xgb)
)

# Tabel aktual dan prediksi
df_01_match <- data.frame(Kabupaten = nama_wilayah,
                          Y_Aktual = y_aktual,
                          as.data.frame(pred_list, check.names = FALSE))

# 3. EVALUASI KINERJA MODEL
rate_actual <- y_aktual / pop * 100000

tabel9 <- do.call(
  rbind,
  lapply(names(pred_list), function(nm) {
    
    p <- pred_list[[nm]]
    
    valid <- is.finite(y_aktual) &
      is.finite(p) &
      is.finite(pop) &
      pop > 0
    
    actual <- y_aktual[valid]
    pred <- p[valid]
    rate_act <- rate_actual[valid]
    rate_pred <- pred / pop[valid] * 100000
    
    e <- hitung_evaluasi(actual, pred)
    
    data.frame(
      Model = nm,
      N_Valid = length(actual),
      R2 = round(e$R2, 4),
      RMSE = round(e$RMSE, 2),
      MAE = round(e$MAE, 2),
      RMSE_rate_100k = round(sqrt(mean((rate_act - rate_pred)^2)), 2),
      MAE_rate_100k = round(mean(abs(rate_act - rate_pred)), 2)
    )
  })
)

# 4. KESALAHAN ABSOLUT SETIAP MODEL
errors <- data.frame(
  Wilayah = nama_wilayah,
  Y_Aktual = y_aktual,
  Baseline_NB_offset = abs(y_aktual - pred_list[["Baseline NB-offset"]]),
  NBR = abs(y_aktual - pred_list[["NBR"]]),
  GWNBR_Adaptive = abs(y_aktual - pred_list[["GWNBR Adaptive Bisquare"]]),
  GWNBR_Gaussian = abs(y_aktual - pred_list[["GWNBR Fixed Gaussian"]]),
  XGBoost = abs(y_aktual - pred_list[["XGBoost"]])
)

# 5. UJI WILCOXON BERPASANGAN
kolom_model <- c("Baseline_NB_offset","NBR","GWNBR_Adaptive",
                 "GWNBR_Gaussian","XGBoost")

pairs <- combn(kolom_model, 2, simplify = FALSE)

hasil_wilcoxon <- do.call(
  rbind,
  lapply(pairs, function(p) {
    
    valid <- is.finite(errors[[p[1]]]) &
      is.finite(errors[[p[2]]])
    
    test <- wilcox.test(
      errors[[p[1]]][valid],
      errors[[p[2]]][valid],
      paired = TRUE,
      exact = FALSE
    )
    
    data.frame(
      Model_1 = p[1],
      Model_2 = p[2],
      N_Pasangan = sum(valid),
      W = unname(test$statistic),
      p_value = test$p.value
    )
  })
)

# Koreksi pengujian berganda
hasil_wilcoxon$p_adjusted <- p.adjust(hasil_wilcoxon$p_value,
                                      method = "holm")

# 6. TABEL AIC, AICc, DAN LOG-LIKELIHOOD
# Ambil Baseline NB-offset dan NBR dari Tahap 2
tabel_nbr_pilih <- tabel_aicc %>%
  dplyr::filter(Model %in% c("Baseline NB-offset", "NBR (penuh)")) %>%
  dplyr::transmute(Model,Param_Efektif = Jumlah_Param,Log_Likelihood,AIC,AICc)

# Ambil GWNBR Adaptive Bisquare dari Tahap 4
tabel_gwnbr_pilih <- tabel_aicc_gwnbr %>%
  dplyr::filter(
    Model %in% c("GWNBR Adaptive Bisquare","GWNBR Fixed Gaussian")) %>%
  dplyr::select(Model,Param_Efektif,Log_Likelihood,AIC,AICc)

tabel_aicc_gabungan <- dplyr::bind_rows(tabel_nbr_pilih,tabel_gwnbr_pilih)

# 7. EKSPOR SELURUH HASIL
list_sheets <- list(
  "Tabel Perbandingan" = tabel9,
  "Match Prediksi" = df_01_match,
  "Kesalahan Absolut" = errors,
  "Hasil Wilcoxon" = hasil_wilcoxon,
  "AIC AICc" = tabel_aicc_gabungan
)

writexl::write_xlsx(list_sheets,file.path(folder_out_08,"08_Laporan_Perbandingan_5_Model.xlsx"))

