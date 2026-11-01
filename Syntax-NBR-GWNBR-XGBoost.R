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
  "07_Perbandingan",
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

# 7. Membuat peta choropleth
peta_y <- ggplot() +
  geom_sf(
    data = geo_data,
    aes(fill = rate_dm),
    color = "grey25",
    linewidth = 0.45) +
  
  geom_text(
    data = label_points,
    aes(x = lon, y = lat, label = nama_label),
    color = "black",
    fontface = "bold",
    size = 2.0,
    check_overlap = TRUE) +
  
  scale_fill_stepsn(
    colors = viridisLite::viridis(n_kelas),
    breaks = breaks_rate,
    limits = c(0, batas_atas),
    name = "Rate per 100,000",
    labels = scales::comma,
    na.value = "grey85",
    guide = guide_colorsteps(show.limits = TRUE, even.steps = TRUE)) +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = grid::unit(0.9, "cm"),
    width = grid::unit(0.9, "cm")) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7,
    line_width = 0.6,
    unit_category = "metric") +
  
  labs(x = "Longitude", y = "Latitude") +
  
  # Peta memenuhi panel (tanpa ruang kosong di tepi)
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    aspect.ratio = 5/7,    # tinggi : lebar panel peta = 5 : 7
    legend.position = "right",
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.major = element_line(color = "grey90", linetype = "dashed", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
# 10. Menyimpan gambar
ggsave(
  filename = file.path(folder_out_01, "03_Choropleth_Map_Diabetes_Mellitus_Rate.png"),
  plot = peta_y,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white")

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
                          
# E. SIMPAN SELURUH HASIL KE SATU FILE EXCEL MULTI-SHEET
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
# TAHAP 4: GWNBR (adaptive bisquare utama, fixed Gaussian alternatif)
# ============================================================
folder_out_04 <- file.path(path_folder, "04_GWNBR")
folder_peta_c <- file.path(folder_out_04, "Peta_Koefisien_GWNBR")
folder_peta_s <- file.path(folder_out_04, "Peta_Signifikansi_GWNBR")

# ---- B. Persiapan data ----
X_mat <- as.matrix(model.matrix(~ X1 + X2 + X3 + X4 + X5, data = df_model))
mode(X_mat) <- "numeric"
Y_vec   <- as.numeric(df_model$Y)
off_vec <- log(as.numeric(df_model$Jumlah_Penduduk))
n_vars  <- ncol(X_mat)
coef_names <- c("Intercept", "X1", "X2", "X3", "X4", "X5")

# Theta ditahan tetap pada estimasi NBR global; hanya beta yang lokal
theta_global <- as.numeric(model_nbr$theta)
beta_global  <- as.numeric(coef(model_nbr))

stopifnot(nrow(X_mat) == length(Y_vec), length(Y_vec) == length(off_vec),
          nrow(dist_matrix) == n_obs, all(is.finite(Y_vec)),
          all(is.finite(off_vec)), all(is.finite(dist_matrix)))
cat("Theta global NBR:", theta_global, "| n:", n_obs, "| koefisien lokal:", n_vars, "\n")

# ---- C. Fungsi dasar ----
# C1. Bobot adaptive bisquare (k = jumlah tetangga selain lokasi itu sendiri)
bobot_adaptif <- function(dists, k) {
  n <- length(dists)
  if (k < 1 || k > n) stop("Nilai k tidak valid.")
  h <- if (k == n) max(dists) else sort(dists)[k + 1]
  if (!is.finite(h) || h <= 0) stop("Bandwidth adaptive tidak valid.")
  ifelse(dists <= h, (1 - (dists / h)^2)^2, 0)}

# C2. Bobot adaptive untuk LOOCV (observasi i dikeluarkan)
bobot_adaptif_cv <- function(dists, i, k) {
  d_train <- dists[-i]
  n_train <- length(d_train)
  if (k < 1 || k > n_train) stop("Bandwidth LOOCV tidak valid.")
  h <- if (k == n_train) max(d_train) else sort(d_train)[k]
  if (!is.finite(h) || h <= 0) stop("Bandwidth LOOCV tidak valid.")
  w <- ifelse(dists <= h, (1 - (dists / h)^2)^2, 0)
  w[i] <- 0
  w}

# C3. Bobot fixed Gaussian (h dalam meter)
bobot_fixed_gaussian <- function(dists, h) {
  if (!is.finite(h) || h <= 0) stop("Bandwidth fixed tidak valid.")
  exp(-0.5 * (dists / h)^2)}

# C4. Estimasi lokal GWNBR (Fisher scoring, link log, theta tetap)
fit_gwnbr_local <- function(X, Y, offset_val, theta, W_i, beta_start,
                            max_iter = 100, tol = 1e-5) {
  p     <- ncol(X)
  ridge <- diag(1e-6, p)
  beta  <- as.numeric(beta_start)
  
  komponen <- function(b) {
    eta    <- pmin(pmax(as.vector(X %*% b + offset_val), -20), 20)
    mu     <- exp(eta)
    var_y  <- mu + mu^2 / theta
    w_diag <- as.numeric(W_i * mu^2 / var_y)
    list(w_diag  = w_diag,
         score   = t(X) %*% (W_i * (Y - mu) * mu / var_y),
         hessian = t(X) %*% (X * w_diag))}
  
  konvergen <- FALSE
  for (iter in seq_len(max_iter)) {
    k <- komponen(beta)
    langkah <- tryCatch(
      as.vector(solve(k$hessian + ridge, k$score)),
      error = function(e) as.vector(MASS::ginv(k$hessian + ridge) %*% k$score))
    beta_new <- beta + langkah
    selisih  <- max(abs(beta_new - beta), na.rm = TRUE)
    if (!is.finite(selisih)) break
    beta <- beta_new
    if (selisih < tol) { konvergen <- TRUE; break }}
  
  k <- komponen(beta)
  inv_hessian <- tryCatch(solve(k$hessian + ridge),
                          error = function(e) MASS::ginv(k$hessian + ridge))
  inv_hessian <- matrix(as.numeric(inv_hessian), nrow = p, ncol = p)
  se    <- sqrt(pmax(diag(inv_hessian), 1e-8))
  y_hat <- exp(pmin(pmax(as.vector(X %*% beta + offset_val), -20), 20))
  
  list(beta = beta, se = se, y_hat = y_hat, inv_hessian = inv_hessian,
       w_diag = k$w_diag, konvergen = konvergen)}

# C5. Elemen diagonal matriks hat S_ii
hitung_S_ii <- function(fit, i) {
  x_i <- matrix(as.numeric(X_mat[i, ]), nrow = 1)
  as.numeric(x_i %*% fit$inv_hessian %*% t(x_i)) * as.numeric(fit$w_diag[i])}

# ---- D. Pemilihan bandwidth dengan AICc (tiap kernel) ----
hitung_ic_gwnbr <- function(W_fun, X, Y, offset_val, theta, beta_start) {
  n <- nrow(X)
  gagal <- list(AIC = Inf, AICc = Inf, EDF = NA_real_, K_eff = NA_real_, LogLik = NA_real_)
  y_hat  <- rep(NA_real_, n)
  S_diag <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    fit_i <- tryCatch(
      fit_gwnbr_local(X, Y, offset_val, theta, W_fun(i), beta_start),
      error = function(e) NULL)
    if (is.null(fit_i) || !fit_i$konvergen) return(gagal)
    y_hat[i] <- fit_i$y_hat[i]
    x_i <- matrix(as.numeric(X[i, ]), nrow = 1)
    S_diag[i] <- as.numeric(x_i %*% fit_i$inv_hessian %*% t(x_i)) * fit_i$w_diag[i]
  }
  if (any(!is.finite(y_hat)) || any(!is.finite(S_diag))) return(gagal)
  
  EDF    <- sum(S_diag)
  K_eff  <- EDF + 1
  loglik <- sum(dnbinom(Y, size = theta, mu = y_hat, log = TRUE))
  denom  <- n - K_eff - 1
  if (!is.finite(loglik) || denom <= 0) return(gagal)
  
  AIC  <- -2 * loglik + 2 * K_eff
  AICc <- AIC + (2 * K_eff * (K_eff + 1)) / denom
  list(AIC = AIC, AICc = AICc, EDF = EDF, K_eff = K_eff, LogLik = loglik)}

# D2. Profil AICc adaptive bisquare
min_k   <- n_vars + 3
k_range <- min_k:n_obs

profil_aicc_adaptive <- data.frame(k = k_range, EDF = NA_real_, K_eff = NA_real_,
                                   LogLik = NA_real_, AIC = NA_real_, AICc = NA_real_)
for (j in seq_along(k_range)) {
  k <- k_range[j]
  hs <- hitung_ic_gwnbr(function(i) bobot_adaptif(dist_matrix[i, ], k),
                        X_mat, Y_vec, off_vec, theta_global, beta_global)
  profil_aicc_adaptive[j, c("EDF", "K_eff", "LogLik", "AIC", "AICc")] <-
    c(hs$EDF, hs$K_eff, hs$LogLik, hs$AIC, hs$AICc)}

# D3. Profil AICc fixed Gaussian (h dalam meter)
jarak_unik <- dist_matrix[upper.tri(dist_matrix)]
h_range <- unique(as.numeric(quantile(jarak_unik, probs = seq(0.25, 1, by = 0.05), na.rm = TRUE)))

profil_aicc_gauss <- data.frame(Bandwidth_m = h_range, EDF = NA_real_, K_eff = NA_real_,
                                LogLik = NA_real_, AIC = NA_real_, AICc = NA_real_)
for (j in seq_along(h_range)) {
  h <- h_range[j]
  hs <- hitung_ic_gwnbr(function(i) bobot_fixed_gaussian(dist_matrix[i, ], h),
                        X_mat, Y_vec, off_vec, theta_global, beta_global)
  profil_aicc_gauss[j, c("EDF", "K_eff", "LogLik", "AIC", "AICc")] <-
    c(hs$EDF, hs$K_eff, hs$LogLik, hs$AIC, hs$AICc)}

# D4. Bandwidth optimal = AICc minimum tiap kernel
stopifnot(any(is.finite(profil_aicc_adaptive$AICc)), any(is.finite(profil_aicc_gauss$AICc)))
idx_ad <- which.min(profil_aicc_adaptive$AICc)
idx_ga <- which.min(profil_aicc_gauss$AICc)
bw_opt   <- profil_aicc_adaptive$k[idx_ad]          # adaptive bisquare (k tetangga)
bw_gauss <- profil_aicc_gauss$Bandwidth_m[idx_ga]   # fixed Gaussian (meter)

batas_atas_ad <- max(k_range)
batas_atas_ga <- max(h_range)
di_batas_ad <- bw_opt == batas_atas_ad
di_batas_ga <- isTRUE(all.equal(bw_gauss, batas_atas_ga))
if (di_batas_ad) warning("Bandwidth adaptive bisquare berada di batas atas rentang pencarian.")
if (di_batas_ga) warning("Bandwidth fixed Gaussian berada di batas atas rentang pencarian.")

tabel_bandwidth <- data.frame(
  Kernel        = c("Adaptive Bisquare", "Fixed Gaussian"),
  Kriteria      = "AICc",
  Bandwidth     = c(bw_opt, bw_gauss),
  Satuan        = c("jumlah tetangga (k)", "meter"),
  Minimum_AICc  = c(profil_aicc_adaptive$AICc[idx_ad], profil_aicc_gauss$AICc[idx_ga]),
  Batas_Atas    = c(batas_atas_ad, batas_atas_ga),
  Di_Batas_Atas = c(di_batas_ad, di_batas_ga))

png(file.path(folder_out_04, "Profil_AICc_Adaptive_Bisquare.png"), width = 1800, height = 1200, res = 300)
plot(profil_aicc_adaptive$k, profil_aicc_adaptive$AICc, type = "b",
     xlab = "Number of nearest neighbors (k)", ylab = "AICc",
     main = "Adaptive Bisquare Bandwidth Selection")
abline(v = bw_opt, lty = 2)
dev.off()

png(file.path(folder_out_04, "Profil_AICc_Fixed_Gaussian.png"), width = 1800, height = 1200, res = 300)
plot(profil_aicc_gauss$Bandwidth_m / 1000, profil_aicc_gauss$AICc, type = "b",
     xlab = "Bandwidth (km)", ylab = "AICc", main = "Fixed Gaussian Bandwidth Selection")
abline(v = bw_gauss / 1000, lty = 2)
dev.off()

# ---- F. Estimasi parameter lokal pada bandwidth terpilih (adaptive bisquare) ----
beta_mat    <- matrix(NA_real_, nrow = n_obs, ncol = n_vars)
se_mat      <- matrix(NA_real_, nrow = n_obs, ncol = n_vars)
y_hat_gwnbr <- rep(NA_real_, n_obs)
tr_S_opt    <- rep(NA_real_, n_obs)
konv_vec    <- logical(n_obs)
fit_simpan  <- vector("list", n_obs)

for (i in seq_len(n_obs)) {
  w_i   <- bobot_adaptif(dist_matrix[i, ], bw_opt)
  fit_i <- tryCatch(
    fit_gwnbr_local(X_mat, Y_vec, off_vec, theta_global, w_i, beta_global),
    error = function(e) NULL)
  if (is.null(fit_i) || !fit_i$konvergen) next
  fit_simpan[[i]] <- fit_i
  beta_mat[i, ]   <- fit_i$beta
  se_mat[i, ]     <- fit_i$se
  y_hat_gwnbr[i]  <- fit_i$y_hat[i]
  tr_S_opt[i]     <- hitung_S_ii(fit_i, i)
  konv_vec[i]     <- TRUE
}
cat("Lokasi tidak konvergen:", sum(!konv_vec), "dari", n_obs, "\n")
if (any(!konv_vec)) stop("Ada lokasi yang tidak konvergen; periksa sebelum melanjutkan.")

colnames(beta_mat) <- coef_names
colnames(se_mat)   <- paste0(coef_names, "_SE")
v_eff_opt <- sum(tr_S_opt)

# ---- G. Tabel koefisien lokal (uji Z dua sisi) ----
df_04_lengkap <- df_model %>% dplyr::select(Kabupaten)
for (j in seq_along(coef_names)) {
  v     <- coef_names[j]
  z_val <- beta_mat[, j] / se_mat[, j]
  df_04_lengkap[[paste0("Beta_", v)]] <- beta_mat[, j]
  df_04_lengkap[[paste0("SE_", v)]]   <- se_mat[, j]
  df_04_lengkap[[paste0("Z_", v)]]    <- z_val
  df_04_lengkap[[paste0("p_", v)]]    <- 2 * pnorm(-abs(z_val))
}
df_04_lengkap$Y_Aktual   <- Y_vec
df_04_lengkap$Y_Prediksi <- y_hat_gwnbr
df_04_lengkap$Residual   <- Y_vec - y_hat_gwnbr

# ---- G2. Koreksi pengujian ganda (aproksimasi Byrne) ----
S_hat <- matrix(0, nrow = n_obs, ncol = n_obs)
for (i in seq_len(n_obs)) {
  fit_i <- fit_simpan[[i]]
  x_i   <- matrix(as.numeric(X_mat[i, ]), nrow = 1)
  S_hat[i, ] <- as.numeric(x_i %*% fit_i$inv_hessian %*% t(X_mat)) * fit_i$w_diag
}
stopifnot(all(dim(S_hat) == c(n_obs, n_obs)), all(is.finite(S_hat)))

tr_S         <- sum(diag(S_hat))
tr_StS       <- sum(S_hat^2)
p_eff_byrne  <- 2 * tr_S - tr_StS
m_tests      <- n_obs * n_vars
alpha_family <- 0.05
if (!is.finite(p_eff_byrne) || p_eff_byrne <= 0 || p_eff_byrne > m_tests)
  stop("Effective number of parameters tidak valid; periksa S_hat.")
alpha_byrne <- alpha_family / (1 + p_eff_byrne - p_eff_byrne / m_tests)

tabel_koreksi <- data.frame(
  Indikator = c("tr(S)", "tr(S'S)", "Effective parameters (2tr(S) - tr(S'S))",
                "Jumlah pengujian (n x p)", "Alpha terkoreksi"),
  Nilai = c(tr_S, tr_StS, p_eff_byrne, m_tests, alpha_byrne))

# ---- H. Ringkasan deskriptif parameter lokal ----
summary_list <- list()
for (v in coef_names) {
  vals <- df_04_lengkap[[paste0("Beta_", v)]]
  summary_list[[v]] <- data.frame(
    Variabel = v, Min = min(vals, na.rm = TRUE),
    Q1 = quantile(vals, 0.25, na.rm = TRUE), Median = median(vals, na.rm = TRUE),
    Mean = mean(vals, na.rm = TRUE), Q3 = quantile(vals, 0.75, na.rm = TRUE),
    Max = max(vals, na.rm = TRUE), Std_Dev = sd(vals, na.rm = TRUE))
}
df_05_ringkas <- do.call(rbind, summary_list)
rownames(df_05_ringkas) <- NULL

# ---- I. Signifikansi (alpha 0.05 dan terkoreksi) & arah koefisien ----
sig_list <- list(); dir_list <- list()
for (v in coef_names) {
  p_col <- df_04_lengkap[[paste0("p_", v)]]
  b_col <- df_04_lengkap[[paste0("Beta_", v)]]
  n_05  <- sum(p_col < 0.05, na.rm = TRUE)
  n_adj <- sum(p_col < alpha_byrne, na.rm = TRUE)
  sig_list[[v]] <- data.frame(Variabel = v, Signif_p005 = n_05,
                              Persen_p005 = round(n_05 / n_obs * 100, 2),
                              Signif_Terkoreksi = n_adj,
                              Persen_Terkoreksi = round(n_adj / n_obs * 100, 2))
  dir_list[[v]] <- data.frame(Variabel = v,
                              Pengaruh_Positif = sum(b_col > 0, na.rm = TRUE),
                              Pengaruh_Negatif = sum(b_col < 0, na.rm = TRUE))
}
df_06_jumlah_sig <- do.call(rbind, sig_list)
df_07_arah_peng  <- do.call(rbind, dir_list)

# ---- J. Evaluasi GWNBR in-sample (theta dihitung sebagai parameter pada semua model) ----
SSE_gwnbr <- sum((Y_vec - y_hat_gwnbr)^2)
SST_gwnbr <- sum((Y_vec - mean(Y_vec))^2)
r2_gwnbr       <- 1 - SSE_gwnbr / SST_gwnbr
rmse_gwnbr_fit <- sqrt(mean((Y_vec - y_hat_gwnbr)^2))
mae_gwnbr_fit  <- mean(abs(Y_vec - y_hat_gwnbr))

ll_gwnbr    <- sum(dnbinom(Y_vec, size = theta_global, mu = y_hat_gwnbr, log = TRUE))
k_par_gwnbr <- v_eff_opt + 1
aic_gwnbr   <- -2 * ll_gwnbr + 2 * k_par_gwnbr
aicc_gwnbr  <- aic_gwnbr + (2 * k_par_gwnbr * (k_par_gwnbr + 1)) / (n_obs - k_par_gwnbr - 1)

df_11_evaluasi <- data.frame(
  Indikator = c("Bandwidth (k-NN)", "Effective Degrees of Freedom (v)", "Log-Likelihood",
                "AIC (theta dihitung)", "AICc (theta dihitung)",
                "R-Squared (in-sample)", "RMSE (in-sample)", "MAE (in-sample)"),
  Nilai = c(bw_opt, v_eff_opt, ll_gwnbr, aic_gwnbr, aicc_gwnbr,
            r2_gwnbr, rmse_gwnbr_fit, mae_gwnbr_fit))

# ---- K. Kelompok signifikansi per wilayah ----
buat_kelompok <- function(ambang) {
  sapply(seq_len(n_obs), function(i) {
    p_i <- sapply(coef_names, function(v) df_04_lengkap[[paste0("p_", v)]][i])
    paste(coef_names[which(p_i < ambang)], collapse = ", ")
  })
}
df_kelompok_sig <- data.frame(
  Kabupaten = df_04_lengkap$Kabupaten,
  Parameter_Signifikan_p005       = buat_kelompok(0.05),
  Parameter_Signifikan_Terkoreksi = buat_kelompok(alpha_byrne))

# ---- L. Perbandingan AIC/AICc: NBR vs GWNBR (kedua kernel) ----
ll_nbr   <- as.numeric(logLik(model_nbr))
k_nbr    <- attr(logLik(model_nbr), "df")
aic_nbr  <- -2 * ll_nbr + 2 * k_nbr
aicc_nbr <- aic_nbr + (2 * k_nbr * (k_nbr + 1)) / (n_obs - k_nbr - 1)

tabel_aicc_gwnbr <- data.frame(
  Model          = c("NBR", "GWNBR Adaptive Bisquare", "GWNBR Fixed Gaussian"),
  Log_Likelihood = round(c(ll_nbr, ll_gwnbr, profil_aicc_gauss$LogLik[idx_ga]), 3),
  Param_Efektif  = round(c(k_nbr, k_par_gwnbr, profil_aicc_gauss$K_eff[idx_ga]), 3),
  AIC            = round(c(aic_nbr, aic_gwnbr, profil_aicc_gauss$AIC[idx_ga]), 3),
  AICc           = round(c(aicc_nbr, aicc_gwnbr, profil_aicc_gauss$AICc[idx_ga]), 3))

# ---- M. LOOCV GWNBR dengan bandwidth tetap dari data penuh (analisis tambahan) ----
cv_global <- lapply(seq_len(n_obs), function(i) {
  fit <- tryCatch(suppressWarnings(MASS::glm.nb(form_nbr, data = df_model[-i, ])),
                  error = function(e) NULL)
  if (is.null(fit) || !is.finite(fit$theta) || fit$theta <= 0 || any(!is.finite(coef(fit)))) {
    list(theta = theta_global, beta = beta_global, ok = FALSE)
  } else {
    list(theta = fit$theta, beta = as.numeric(coef(fit)), ok = TRUE)
  }
})
cat("Fold dengan re-estimasi NBR gagal:", sum(!sapply(cv_global, `[[`, "ok")), "\n")

loocv_gwnbr_bw_tetap <- function(W_cv_fun) {
  y_pred <- rep(NA_real_, n_obs)
  for (i in seq_len(n_obs)) {
    fit_i <- tryCatch(
      fit_gwnbr_local(X_mat, Y_vec, off_vec, theta = cv_global[[i]]$theta,
                      W_i = W_cv_fun(i), beta_start = cv_global[[i]]$beta),
      error = function(e) NULL)
    if (is.null(fit_i) || !fit_i$konvergen) next
    eta_i <- sum(X_mat[i, ] * fit_i$beta) + off_vec[i]
    y_pred[i] <- exp(pmin(pmax(eta_i, -20), 20))
  }
  y_pred
}

metrik_loocv <- function(nama, y_pred) {
  if (!all(is.finite(y_pred))) {
    warning("Sebagian prediksi LOOCV ", nama, " tidak valid.")
    return(data.frame(Model = nama, N_Valid = sum(is.finite(y_pred)),
                      R2_LOOCV = NA_real_, RMSE_LOOCV = NA_real_, MAE_LOOCV = NA_real_))
  }
  e <- hitung_evaluasi(Y_vec, y_pred)
  data.frame(Model = nama, N_Valid = length(y_pred),
             R2_LOOCV = e$R2, RMSE_LOOCV = e$RMSE, MAE_LOOCV = e$MAE)}

y_pred_ad <- loocv_gwnbr_bw_tetap(function(i) bobot_adaptif_cv(dist_matrix[i, ], i, bw_opt))
y_pred_ga <- loocv_gwnbr_bw_tetap(function(i) {
  w <- bobot_fixed_gaussian(dist_matrix[i, ], bw_gauss); w[i] <- 0; w
})
y_pred_gwnbr_loocv <- y_pred_ad
evaluasi_loocv_gwnbr <- rbind(
  metrik_loocv("GWNBR Adaptive Bisquare (bw tetap)", y_pred_ad),
  metrik_loocv("GWNBR Fixed Gaussian (bw tetap)",    y_pred_ga))

# ---- N. Tabel GWNBR untuk peta (dari hasil bagian F, tanpa estimasi ulang) ----
hasil_gwnbr <- data.frame(
  Kabupaten = df_model$Kabupaten,
  setNames(as.data.frame(beta_mat),                 paste0("Beta_", coef_names)),
  setNames(as.data.frame(unname(se_mat)),           paste0("SE_",   coef_names)),
  Prediksi  = y_hat_gwnbr)

# ---- O. Peta ----
ukuran_label <- 2.0
elemen_peta <- list(
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = grid::unit(0.9, "cm"), width = grid::unit(0.9, "cm")),
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7,
                   line_width = 0.6, unit_category = "metric"),
  labs(x = "Longitude", y = "Latitude"),
  coord_sf(expand = FALSE))

tema_peta <- theme_minimal() +
  theme(aspect.ratio = 5/7, legend.position = "right",
        axis.title = element_text(size = 10), axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "grey90", linetype = "dashed", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

buat_label_points <- function(data) {
  st_transform(data, 32749) %>%
    dplyr::select(Kabupaten, nama, geometry) %>%
    st_cast("POLYGON") %>%
    mutate(luas = as.numeric(st_area(geometry))) %>%
    group_by(Kabupaten, nama) %>%
    slice_max(order_by = luas, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(nama_label = ifelse(grepl("^Kabupaten ", nama), sub("^Kabupaten ", "", nama), nama)) %>%
    filter(!nama %in% kota_tanpa_label) %>%
    st_point_on_surface() %>%
    st_transform(st_crs(data))}

peta_gwnbr <- geo_data %>%
  left_join(hasil_gwnbr %>% dplyr::select(Kabupaten, Beta_X1, Beta_X2, Beta_X3, Beta_X4, Beta_X5),
            by = "Kabupaten")

buat_peta_koef <- function(data, variabel, nama_file) {
  label_points <- buat_label_points(data)
  batas <- max(abs(range(data[[variabel]], na.rm = TRUE)))
  p <- ggplot(data) +
    geom_sf(aes(fill = .data[[variabel]]), color = "grey25", linewidth = 0.45) +
    geom_sf_text(data = label_points, aes(label = nama_label, geometry = geometry),
                 stat = "sf_coordinates", color = "black", fontface = "bold",
                 size = ukuran_label, check_overlap = TRUE) +
    scale_fill_gradient2(name = "Local coefficient", low = "#2166AC", mid = "white",
                         high = "#B2182B", midpoint = 0, limits = c(-batas, batas),
                         na.value = "grey85",
                         guide = guide_colorbar(barheight = grid::unit(6, "cm"),
                                                barwidth = grid::unit(0.5, "cm"))) +
    elemen_peta + tema_peta
  ggsave(file.path(folder_peta_c, nama_file), plot = p, width = 12, height = 7, dpi = 300, bg = "white")
  p}
for (v in 1:5) buat_peta_koef(peta_gwnbr, paste0("Beta_X", v), paste0("Peta_Koefisien_X", v, ".png"))

rentang_koef <- sapply(paste0("Beta_X", 1:5), function(v) range(peta_gwnbr[[v]], na.rm = TRUE))
rownames(rentang_koef) <- c("Min", "Max")
print(round(t(rentang_koef), 4))

buat_peta_signif <- function(no_var, nama_file) {
  beta <- paste0("Beta_X", no_var); se <- paste0("SE_X", no_var)
  hasil_signif <- hasil_gwnbr %>%
    mutate(Z = .data[[beta]] / .data[[se]], P = 2 * pnorm(-abs(Z)),
           Signif = ifelse(P < 0.05, "Significant", "Not Significant")) %>%
    dplyr::select(Kabupaten, Signif)
  peta_signif <- geo_data %>% left_join(hasil_signif, by = "Kabupaten")
  cat("\nX", no_var, ":\n", sep = ""); print(table(peta_signif$Signif))
  label_points <- buat_label_points(peta_signif)
  p <- ggplot(peta_signif) +
    geom_sf(aes(fill = Signif), color = "grey25", linewidth = 0.45) +
    geom_sf_text(data = label_points, aes(label = nama_label, geometry = geometry),
                 stat = "sf_coordinates", color = "black", fontface = "bold",
                 size = ukuran_label, check_overlap = TRUE) +
    scale_fill_manual(values = c("Significant" = "red", "Not Significant" = "grey85"),
                      breaks = c("Significant", "Not Significant"), name = "Local significance") +
    elemen_peta + tema_peta
  ggsave(file.path(folder_peta_s, nama_file), plot = p, width = 12, height = 7, dpi = 300, bg = "white")
  p}
peta_signifikansi_X2 <- buat_peta_signif(2, "Peta_Signifikansi_X2_GWNBR.png")
peta_signifikansi_X4 <- buat_peta_signif(4, "Peta_Signifikansi_X4_GWNBR.png")

# ---- P. UJI MONTE CARLO NON-STASIONERITAS KOEFISIEN GWNBR ----
set.seed(123)
B <- 999

nama_beta      <- colnames(X_mat)
nama_prediktor <- nama_beta[-1]

beta_obs <- as.matrix(hasil_gwnbr[, paste0("Beta_", coef_names)])
colnames(beta_obs) <- nama_beta
if (any(!is.finite(beta_obs))) stop("Koefisien lokal aktual mengandung nilai tidak valid.")
var_obs <- apply(beta_obs, 2, stats::var)

# Matriks bobot dihitung sekali (bw_opt tetap)
W_opt <- do.call(rbind, lapply(seq_len(n_obs), function(i) bobot_adaptif(dist_matrix[i, ], bw_opt)))

hitung_var_lokal <- function(Y_input, theta_input, beta_start) {
  p <- ncol(X_mat)
  beta_sim <- matrix(NA_real_, nrow = n_obs, ncol = p)
  for (i in seq_len(n_obs)) {
    fit_i <- tryCatch(
      fit_gwnbr_local(X_mat, Y_input, off_vec, theta_input, W_opt[i, ], beta_start),
      error = function(e) NULL)
    if (is.null(fit_i) || !isTRUE(fit_i$konvergen) ||
        length(fit_i$beta) != p || any(!is.finite(fit_i$beta))) return(NULL)
    beta_sim[i, ] <- as.numeric(fit_i$beta)
  }
  apply(beta_sim, 2, stats::var)}

data_sim <- df_model[, c("X1", "X2", "X3", "X4", "X5")]
data_sim$offset_sim <- off_vec
formula_sim <- Y ~ X1 + X2 + X3 + X4 + X5 + offset(offset_sim)

mu_h0   <- as.numeric(exp(X_mat %*% beta_global + off_vec))
var_sim <- matrix(NA_real_, nrow = B, ncol = ncol(X_mat), dimnames = list(NULL, nama_beta))
gagal   <- 0L

for (b in seq_len(B)) {
  Y_sim <- MASS::rnegbin(n = n_obs, mu = mu_h0, theta = theta_global)
  data_sim$Y <- Y_sim
  
  fit_nbr_sim <- tryCatch(
    suppressWarnings(MASS::glm.nb(formula_sim, data = data_sim, control = glm.control(maxit = 100))),
    error = function(e) NULL)
  if (is.null(fit_nbr_sim)) { gagal <- gagal + 1L; next }
  
  beta_start_sim <- coef(fit_nbr_sim)
  theta_sim      <- fit_nbr_sim$theta
  if (any(!is.finite(beta_start_sim)) || !is.finite(theta_sim) || theta_sim <= 0) {
    gagal <- gagal + 1L; next
  }
  
  v_sim <- hitung_var_lokal(Y_sim, theta_sim, beta_start_sim)
  if (is.null(v_sim) || any(!is.finite(v_sim))) { gagal <- gagal + 1L; next }
  
  var_sim[b, ] <- v_sim
  if (b %% 50 == 0) cat("Simulasi selesai:", b, "dari", B, "\n")}
cat("Simulasi gagal:", gagal, "dari", B, "\n")

p_mc <- setNames(rep(NA_real_, ncol(X_mat)), nama_beta)
for (j in seq_len(ncol(X_mat))) {
  v <- var_sim[, j]; v <- v[is.finite(v)]
  if (length(v) > 0) p_mc[j] <- (1 + sum(v >= var_obs[j])) / (length(v) + 1)}
p_bh <- setNames(rep(NA_real_, ncol(X_mat)), nama_beta)
p_bh[nama_prediktor] <- p.adjust(p_mc[nama_prediktor], method = "BH")

hasil_mc <- data.frame(
  Variabel          = nama_beta,
  Varians_Observasi = as.numeric(var_obs),
  Rata2_Varians_Sim = colMeans(var_sim, na.rm = TRUE),
  Simulasi_Valid    = colSums(is.finite(var_sim)),
  P_Monte_Carlo     = as.numeric(p_mc),
  P_BH              = as.numeric(p_bh),
  Kesimpulan        = ifelse(!is.na(p_bh) & p_bh < 0.05,
                             "Variasi spasial signifikan", "Belum ada bukti variasi spasial"))

# ---- Q. Ekspor Tahap 4 ----
wb_04 <- createWorkbook()
hasil_sheet <- list(
  "Pemilihan Bandwidth"   = tabel_bandwidth,
  "Profil AICc Adaptive"  = profil_aicc_adaptive,
  "Profil AICc Gaussian"  = profil_aicc_gauss,
  "Evaluasi Lengkap"      = df_11_evaluasi,
  "Evaluasi LOOCV bw tetap" = evaluasi_loocv_gwnbr,
  "AIC AICc Perbandingan" = tabel_aicc_gwnbr,
  "Koreksi Uji Ganda"     = tabel_koreksi,
  "Ringkasan Koefisien"   = df_05_ringkas,
  "Jumlah Signifikansi"   = df_06_jumlah_sig,
  "Arah Koefisien"        = df_07_arah_peng,
  "Tabel per Daerah"      = df_04_lengkap,
  "Kelompok Signifikansi" = df_kelompok_sig,
  "Monte Carlo GWNBR"     = hasil_mc)
for (nm in names(hasil_sheet)) {
  addWorksheet(wb_04, nm); writeData(wb_04, sheet = nm, x = hasil_sheet[[nm]])
}
saveWorkbook(wb_04, file.path(folder_out_04, "04_Laporan_Hasil_GWNBR.xlsx"), overwrite = TRUE)

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

grid_xgb <- expand.grid(eta = c(0.03, 0.05, 0.10), max_depth = c(2, 3), min_child_weight = c(1, 3))
stopifnot(nrow(grid_xgb) == 12)

buat_params <- function(row) {
  list(booster = "gbtree", objective = "count:poisson", eval_metric = "rmse",
       eta = as.numeric(row$eta), max_depth = as.integer(row$max_depth),
       min_child_weight = as.numeric(row$min_child_weight),
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
                             min_child_weight = grid_xgb$min_child_weight[g],
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
                                     min_child_weight = best_i$min_child_weight,
                                     subsample = best_i$subsample,
                                     colsample_bytree = best_i$colsample_bytree,
                                     nrounds = nrounds_i, inner_CV_RMSE = best_i$inner_CV_RMSE)
}
stopifnot(all(is.finite(pred_outer)))

hasil_eval_nested     <- hitung_evaluasi(y_vector, pred_outer)
hasil_parameter_outer <- do.call(rbind, parameter_outer)
hasil_tuning_outer    <- do.call(rbind, tuning_outer)
df_01_pred_xgb <- data.frame(Kabupaten = df_model$Kabupaten, Y_Aktual = y_vector,
                             Y_Prediksi_Nested_LOOCV = pred_outer,
                             Residual = y_vector - pred_outer)

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
shp <- shapviz(xgb_final, X_pred = X_matrix)

nama_variabel <- c(
  X1 = "Poor population (%)",
  X2 = "non-PBI BPJS Health participants (%)",
  X3 = "population aged 45-54 who smoke (%)",
  X4 = "Population density (1,000 persons/km2)",
  X5 = "community health centers (puskesmas)(Per 100,000 residents)")

shap_mat <- shapviz::get_shap_values(shp)
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

openxlsx::write.xlsx(
  list(Prediksi_Nested_LOOCV = df_01_pred_xgb,
       Evaluasi_Nested_LOOCV = hasil_eval_nested,
       Parameter_Outer_Fold  = hasil_parameter_outer,
       Tuning_Inner_Fold     = hasil_tuning_outer,
       Tuning_Model_Final    = df_tuning_final,
       Feature_Importance    = df_importance),
  file.path(folder_out_05, "05_Laporan_Hasil_XGBoost.xlsx"), overwrite = TRUE)

# ============================================================
# TAHAP 7: PERBANDINGAN MODEL
# ============================================================
folder_out_07 <- file.path(path_folder, "07_Perbandingan")
dir.create(folder_out_07, showWarnings = FALSE, recursive = TRUE)

n_loo <- nrow(df_model)
y_aktual <- as.numeric(df_model$Y)
pop <- as.numeric(df_model$Jumlah_Penduduk)

# 1. LOOCV BASELINE NB-OFFSET & NBR
pred_nb0_loo <- numeric(n_loo)
y_pred_nbr_loo <- numeric(n_loo)

for (i in seq_len(n_loo)) {
  # Baseline NB
  fit_nb0 <- MASS::glm.nb(Y ~ 1 + offset(log(Jumlah_Penduduk)), data = df_model[-i, ])
  pred_nb0_loo[i] <- predict(fit_nb0, newdata = df_model[i, ], type = "response")
  
  # Global NBR
  fit_nbr <- MASS::glm.nb(formula_nbr, data = df_model[-i, ])
  y_pred_nbr_loo[i] <- predict(fit_nbr, newdata = df_model[i, ], type = "response")}

# 2. LOOCV GWNBR (Menggunakan fungsi helper agar loop bersih)
y_pred_gwnbr_loo <- sapply(seq_len(n_loo), function(i) cv_gwnbr_predict_one(i, df_model, coords))

# 3. PENYELARASAN PREDIKSI & METRIK EVALUASI
pred_list <- list(
  "Baseline NB-offset" = pred_nb0_loo,
  "NBR"                = y_pred_nbr_loo,
  "GWNBR"              = y_pred_ad,
  "XGBoost"            = as.numeric(pred_outer))

rate_actual <- y_aktual / pop * 100000

tabel9 <- do.call(rbind, lapply(names(pred_list), function(nm) {
  p <- pred_list[[nm]]
  e <- hitung_evaluasi(y_aktual, p)
  r <- p / pop * 100000
  data.frame(
    Model = nm, 
    R2 = round(e$R2, 4), 
    RMSE = round(e$RMSE, 2), 
    MAE = round(e$MAE, 2),
    RMSE_rate_100k = round(sqrt(mean((rate_actual - r)^2)), 2),
    MAE_rate_100k  = round(mean(abs(rate_actual - r)), 2))}))

# 4. KEEPOUT SENSITIVITAS (SURABAYA)
idx_surabaya <- grep("Kota Surabaya", df_model$Kabupaten, ignore.case = TRUE)
keep <- setdiff(seq_len(n_loo), idx_surabaya)

sensitivity_results <- do.call(rbind, lapply(names(pred_list), function(nm) {
  p <- pred_list[[nm]]
  data.frame(
    Model = c(nm, nm),
    Evaluasi = c("Semua wilayah", "Tanpa Surabaya"),
    RMSE = c(round(sqrt(mean((y_aktual - p)^2)), 2), 
             round(sqrt(mean((y_aktual[keep] - p[keep])^2)), 2)),
    MAE = c(round(mean(abs(y_aktual - p)), 2), 
            round(mean(abs(y_aktual[keep] - p[keep])), 2)))}))

# 5. AKOMODASI TABEL RESIDUAL & AIC/AICc
df_01_match <- data.frame(Kabupaten = df_model$Kabupaten, Y_Aktual = y_aktual, as.data.frame(pred_list))

ic_nb0 <- hitung_ic(model_nb0, "Baseline NB-offset")
ic_nbr <- hitung_ic(model_nbr, "NBR")
df_06_aicc <- tibble(
  Model = c("Baseline NB-offset", "NBR", "GWNBR"),
  Param_Efektif = round(c(ic_nb0$Jumlah_Param, ic_nbr$Jumlah_Param, k_par_gwnbr), 2),
  Log_Likelihood = round(c(ic_nb0$Log_Likelihood, ic_nbr$Log_Likelihood, ll_gwnbr), 3),
  AIC = round(c(ic_nb0$AIC, ic_nbr$AIC, aic_gwnbr), 3),
  AICc = round(c(ic_nb0$AICc, ic_nbr$AICc, aicc_gwnbr), 3))

# 6. PLOTTING & EXPORT AUTOMATION
for (m in c("R2", "RMSE", "MAE")) {
  p <- ggplot(tabel9, aes(x = factor(Model, levels = names(pred_list)), y = .data[[m]])) +
    geom_col(width = 0.5, fill = "steelblue") +
    geom_text(aes(label = .data[[m]]), vjust = -0.5, fontface = "bold") +
    theme_minimal() + 
    labs(title = paste("Kinerja Model:", m), x = "Model", y = m)
  
  ggsave(file.path(folder_out_07, paste0("Perbandingan_", m, ".png")), p, width = 7, height = 4.5, dpi = 300, bg = "white")
  }

# Ekspor Excel
list_sheets <- list("Tabel Perbandingan" = tabel9, "Match Prediksi" = df_01_match, 
                    "AIC AICc" = df_06_aicc, "Sensitivitas Surabaya" = sensitivity_results)
writexl::write_xlsx(list_sheets, file.path(folder_out_07, "07_Laporan_Perbandingan_4_Model.xlsx"))
