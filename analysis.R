# =======================================================
# LANGKAH 1: SETUP DAN MUAT DATA
# =======================================================

# 1.1 Muat library
library(dplyr)
library(readr)

# 1.2 Muat SEMUA dataset di awal
print("Memuat data...")
df_laps <- read_delim("D:/RaceStrategySimulator/data/ver_2/23_AnalysisEnduranceWithSections_Race 2_Anonymized.CSV", 
                      delim = ";")
df_weather <- read_delim("D:/RaceStrategySimulator/data/ver_2/26_Weather_Race 2_Anonymized.CSV", 
                         delim = ";")
df_baseline <- read_delim("D:/RaceStrategySimulator/data/ver_2/99_Best 10 Laps By Driver_Race 2_Anonymized.CSV", 
                          delim = ";")

# 1.3 Bersihkan nama kolom
names(df_laps) <- trimws(names(df_laps))
names(df_weather) <- trimws(names(df_weather))
names(df_baseline) <- trimws(names(df_baseline))

print("Data berhasil dimuat dan nama kolom dibersihkan.")


# =======================================================
# LANGKAH 1.5: FUNGSI KONVERSI WAKTU (BARU!)
# =======================================================
# Fungsi ini akan mengubah format "MM:SS.mmm" atau "SS.mmm" menjadi total detik

convert_to_seconds <- function(time_strings) {
  sapply(time_strings, function(time_string) {
    # Ganti koma desimal jadi titik (untuk jaga-jaga)
    time_string <- gsub(",", ".", time_string)
    
    # Cek apakah ada ':'
    if (grepl(":", time_string)) {
      parts <- as.numeric(strsplit(time_string, ":")[[1]])
      
      if (length(parts) == 3) { 
        # Format MM:SS:mmm (e.g., 03:18:00 -> 3*60 + 18 + 0/1000)
        total_seconds <- (parts[1] * 60) + parts[2] + (parts[3] / 1000)
      } else if (length(parts) == 2) { 
        # Format MM:SS.mmm (e.g., 1:09.145 -> 1*60 + 9.145)
        total_seconds <- (parts[1] * 60) + parts[2]
      } else {
        total_seconds <- NA # Format aneh
      }
    } else { 
      # Format SS.mmm (e.g., 46.382)
      total_seconds <- as.numeric(time_string)
    }
    return(total_seconds)
  })
}

print("Fungsi konversi waktu (convert_to_seconds) berhasil dibuat.")

# =======================================================
# LANGKAH 2: BUAT VARIABEL STINT
# =======================================================
# (Ini tetap sama, tidak ada perubahan)
print("Membuat variabel stint...")

df_laps_sorted <- df_laps %>%
  arrange(NUMBER, LAP_NUMBER)

df_stints <- df_laps_sorted %>%
  group_by(NUMBER) %>%
  mutate(
    is_pit_lap = ifelse(is.na(PIT_TIME), 0, ifelse(PIT_TIME > 0, 1, 0)),
    new_stint_flag = lag(is_pit_lap, default = 0),
    stint_id = cumsum(new_stint_flag) + 1
  ) %>%
  ungroup() %>%
  group_by(NUMBER, stint_id) %>% 
  mutate(
    laps_on_tire = row_number()
  ) %>%
  ungroup()

print("Variabel stint berhasil dibuat.")

# =======================================================
# LANGKAH 3: PERSIAPAN SEBELUM MODEL
# =======================================================
# (Ini tetap sama, tidak ada perubahan)

print("Contoh hasil stint untuk Pembalap #13:")
df_stints %>%
  filter(NUMBER == 13) %>%
  select(NUMBER, LAP_NUMBER, PIT_TIME, stint_id, laps_on_tire) %>%
  head(n = 20) %>%
  print()

avg_air_temp <- mean(df_weather$AIR_TEMP, na.rm = TRUE)
print(paste("Suhu udara rata-rata:", round(avg_air_temp, 2), "°C"))

# =======================================================
# LANGKAH 4: FILTER, KONVERSI, DAN MODEL (CARA AMAN)
# =======================================================
print("Memulai pemfilteran data untuk model...")

# 4.1 Filter data mentah untuk model (Filter "GF" Anda sudah benar)
df_model_filtered <- df_stints %>%
  filter(
    trimws(FLAG_AT_FL) == "GF", 
    is_pit_lap == 0, 
    laps_on_tire > 1 
  )

print(paste(nrow(df_model_filtered), "lap data 'GF' bersih ditemukan."))

# 4.2 Lakukan konversi HANYA JIKA ada data
if (nrow(df_model_filtered) > 0) {
  
  print("Mengonversi data waktu (LAP_TIME, S1, S2) ke angka detik...")
  
  # --- INI BAGIAN YANG DIPERBARUI ---
  df_model_converted <- df_model_filtered %>%
    mutate(
      # Gunakan fungsi baru kita, bukan as.numeric()
      LAP_TIME = convert_to_seconds(LAP_TIME),
      S1 = convert_to_seconds(S1),
      S2 = convert_to_seconds(S2),
      laps_on_tire = as.numeric(laps_on_tire) 
    ) %>%
    # Hapus baris yang gagal dikonversi (menjadi NA)
    filter(!is.na(LAP_TIME) & !is.na(S1) & !is.na(S2) & !is.na(laps_on_tire))
  
  print(paste(nrow(df_model_converted), "lap data tersisa setelah konversi & pembersihan NA."))
  
  # 4.3 Jalankan model HANYA JIKA masih ada data
  if (nrow(df_model_converted) > 10) {
    
    print("Data cukup, membangun model regresi...")
    
    model_degradation <- lm(LAP_TIME ~ laps_on_tire + S1 + S2, data = df_model_converted)
    
    # 4.4 Tampilkan hasil
    print("--- HASIL MODEL REGRESI (BERHASIL) ---")
    summary(model_degradation)
    
  } else {
    print("ERROR: Data tidak cukup untuk membuat model setelah konversi (kurang dari 10 baris).")
  }
  
} else {
  print("ERROR: Tidak ada data 'GF' yang ditemukan. Model tidak bisa dibuat.")
}
# =======================================================
# LANGKAH 5: HITUNG BIAYA PIT STOP
# =======================================================

# Kita perlu mengonversi PIT_TIME juga, karena formatnya mungkin sama
# Kita gunakan fungsi 'convert_to_seconds' yang sudah kita buat

# Pertama, filter untuk mendapatkan HANYA lap di mana pit terjadi
pit_laps <- df_stints %>%
  filter(is_pit_lap == 1) %>%
  mutate(
    # Konversi teks waktu pit menjadi detik
    pit_time_seconds = convert_to_seconds(PIT_TIME)
  ) %>%
  # Hapus jika ada NA (gagal konversi)
  filter(!is.na(pit_time_seconds))

# Sekarang, hitung rata-ratanya
pit_stop_delta <- mean(pit_laps$pit_time_seconds, na.rm = TRUE)

print(paste("Jumlah pit stop yang dianalisis:", nrow(pit_laps)))
print(paste("WAKTU PIT STOP RATA-RATA (pit_stop_delta):", round(pit_stop_delta, 2), "detik"))





