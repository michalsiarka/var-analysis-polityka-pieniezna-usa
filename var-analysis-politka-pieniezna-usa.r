#Michał Siarka, Konrad Bodziony, Albert Dańko
#Analiza odpowiedzi makroekonomicznej na nieoczekiwane zmiany w polityce pieniężnej Stanów Zjednoczonych

###### pakiety ######

if(!require(tseries)){
  install.packages('tseries')
  library(tseries)
}

if(!require(plotrix)){
  install.packages('plotrix')
  library(plotrix)
}

if(!require(zoo)){
  install.packages('zoo')
  library(zoo)
}

if(!require(vars)){
  install.packages('vars')
  library(vars)
}


getwd() 

#####  Import i przygotowanie danych #####

# Wczytanie plików
raw_fed <- read.csv("FEDFUNDS.csv", stringsAsFactors = FALSE)
raw_cpi <- read.csv("CPIAUCSL.csv", stringsAsFactors = FALSE)
raw_ind <- read.csv("INDPRO.csv", stringsAsFactors = FALSE)
raw_wti <- read.csv("WTISPLC.csv", stringsAsFactors = FALSE)

# Ujednolicenie nazw kolumn
colnames(raw_fed) <- c("date", "FEDFUNDS")
colnames(raw_cpi) <- c("date", "CPIAUCSL")
colnames(raw_ind) <- c("date", "INDPRO")
colnames(raw_wti) <- c("date", "WTISPLC")

# Łączenie danych (Merge)
dane_temp <- merge(raw_fed, raw_cpi, by = "date")
dane_temp <- merge(dane_temp, raw_ind, by = "date")
dane_full <- merge(dane_temp, raw_wti, by = "date")

# Konwersja daty i sortowanie
dane_full$date <- as.Date(dane_full$date)
dane_full <- dane_full[order(dane_full$date), ]

# Usunięcie braków danych
dane <- na.omit(dane_full)

# Zapisujemy daty (będą potrzebne do osi czasu)
daty_wekt <- dane$date

#####  Przeksztalcanie zmiennych (OBLICZENIE INFLACJI NA STARCIE) #####

# Logarytmujemy zmienne "poziomowe"
ln_indpro <- log(dane$INDPRO)
ln_wti    <- log(dane$WTISPLC)
ln_cpi    <- log(dane$CPIAUCSL) # Potrzebne tylko do obliczenia inflacji

# Obliczamy INFLACJĘ (Różnica logarytmów CPI)
# diff() zwraca wektor krótszy o 1 element
inflacja <- diff(ln_cpi)

# Dostosowujemy pozostałe zmiennych
ln_indpro_adj <- ln_indpro[-1]
ln_wti_adj    <- ln_wti[-1]
fedfunds_adj  <- dane$FEDFUNDS[-1]
daty_adj      <- daty_wekt[-1]

# Tworzymy ramkę danych do modelu
dane_model <- data.frame(
  LN_INDPRO = ln_indpro_adj,
  INFLATION = inflacja,       # <-- Tutaj wchodzi już gotowa inflacja
  FEDFUNDS  = fedfunds_adj,
  LN_WTISPLC = ln_wti_adj
)

# Przypisanie dat jako nazw wierszy
row.names(dane_model) <- daty_adj


##### wczytanie danych i wstepna analiza #####

head(dane_model)
tail(dane_model)
n <- ncol(dane_model)
tytuly <- names(dane_model)
T_obs <- nrow(dane_model)

# Zapis danych w postaci szeregu czasowego (ts)
# Frequency = 12 (miesięczne)
start_date <- as.Date(row.names(dane_model)[1])
start_year <- as.numeric(format(start_date, "%Y"))
start_month <- as.numeric(format(start_date, "%m"))

tsdane = ts(dane_model, start = c(start_year, start_month), frequency = 12)


#####  Wykresy i wstępna analiza poziomów #####

# Wykresy przebiegu 
par(mfrow=c(1,1))
par(mfrow=c(2,2)) 
for (i in 1:n){
  plot.ts(tsdane[,i], main=tytuly[i], ylab="", col="blue")
}
dev.print(pdf, "dane_poziomy_inflacja.pdf") 

# Wykresy przebiegu z trendem liniowym
par(mfrow=c(2,2))
for (i in 1:n){
  plot.ts(dane_model[,i], lwd = 2, ylab = tytuly[i], main=paste("Trend:", tytuly[i]))
  lines(predict(lm(dane_model[,i]~seq(1,nrow(dane_model),1))), col='darkgreen', lwd = 1)
}
dev.print(pdf, "wykres_trendy_inflacja.pdf")

# Analiza ACF i PACF dla POZIOMÓW

lag_max_val <- 36 

# Zapis do PDF
pdf(file = "analiza_poziomy_inflacja.pdf", width = 11, height = 8)
par(mfrow=c(4,3), mar=c(2,2,2,1)) 
for (i in 1:n){
  plot.ts(tsdane[,i], main=tytuly[i], ylab="") 
  acf(dane_model[,i], lag.max=lag_max_val, main=paste("ACF:", tytuly[i]), ylab="") 
  pacf(dane_model[,i], lag.max=lag_max_val, main=paste("PACF:", tytuly[i]), ylab="") 
}
dev.off() 


par(mfrow=c(4,3), mar=c(2,2,2,1))
for (i in 1:n){
  plot.ts(tsdane[,i], main=tytuly[i], ylab="")
  acf(dane_model[,i], lag.max=lag_max_val, main=paste("ACF:", tytuly[i]), ylab="")
  pacf(dane_model[,i], lag.max=lag_max_val, main=paste("PACF:", tytuly[i]), ylab="")
}


######  Przyrosty #######


# podzbiór danych do różnicowania 
zmienne_do_roznicowania <- tsdane[, c("LN_INDPRO", "FEDFUNDS", "LN_WTISPLC")]

# Obliczamy pierwsze różnice
ts_diff_only <- diff(zmienne_do_roznicowania)

# Aktualizujemy zmienne pomocnicze do pętli 
tytuly_diff <- colnames(ts_diff_only)
n_diff <- ncol(ts_diff_only)

# Konwersja do macierzy dla funkcji acf/pacf
dd_diff <- as.matrix(ts_diff_only)


#####  Wykresy i wstępna analiza przyrostów (BEZ INFLACJI) #####

# Same wykresy przebiegu (3 zmienne)
par(mfrow=c(2,2)) # Układ 2x2 (jedno pole zostanie puste)
for (i in 1:n_diff) {
  plot.ts(ts_diff_only[,i], main=paste("D.", tytuly_diff[i]), ylab="", col="red")
}
dev.print(pdf, "dane_przyrosty_bez_inflacji.pdf")


# Pełna analiza: Wykres + ACF + PACF
# Zapis do PDF
pdf(file = "analiza_przyrosty_bez_inflacji.pdf", width = 11, height = 8)

# Ustawiamy układ: 3 wiersze (zmienne), 3 kolumny (Wykres, ACF, PACF)
par(mfrow=c(3,3), mar=c(2,2,2,1)) 

for (i in 1:n_diff) {
  # A. Wykres przebiegu różnicy
  plot.ts(ts_diff_only[,i], main=paste("D.", tytuly_diff[i]), ylab="", col="red")
  
  # B. ACF dla różnicy
  acf(dd_diff[,i], lag.max=lag_max_val, main=paste("ACF: D.", tytuly_diff[i]), ylab="")
  
  # C. PACF dla różnicy
  pacf(dd_diff[,i], lag.max=lag_max_val, main=paste("PACF: D.", tytuly_diff[i]), ylab="")
}
dev.off()


par(mfrow=c(3,3), mar=c(2,2,2,1))
for (i in 1:n_diff) {
  plot.ts(ts_diff_only[,i], main=paste("D.", tytuly_diff[i]), col="red")
  acf(dd_diff[,i], lag.max=lag_max_val, main=paste("ACF: D.", tytuly_diff[i]))
  pacf(dd_diff[,i], lag.max=lag_max_val, main=paste("PACF: D.", tytuly_diff[i]))
}

par(mfrow=c(1,1))

##### 3. Budowa Modelu VAR (Zadanie Domowe nr 3) #####

# Wyznaczenie liczby opóźnień (Kryteria Informacyjne)

# opóźnienia do 1.5 roku (18 miesięcy)
# type = "const" (zakładamy stałą w modelu)

#kryteria <- VARselect(tsdane, lag.max = 18, type = "const") poprawka niby na dole
#Zgodnie z uwagą o INDPRO, model na poziomach powinien uwzględniać stałą i trend (type = "both")
kryteria <- VARselect(tsdane, lag.max = 18, type = "both") # Uwzględniamy trend

cat("\n[Punkt 1] Wyniki kryteriów informacyjnych:\n")
print(kryteria$selection)

# Wybór rzędu p (zazwyczaj SC/BIC dla modeli makroekonomicznych)
p_AIC <- kryteria$selection["AIC(n)"]
p_SC  <- kryteria$selection["SC(n)"]

cat("\nSugerowane rzędy opóźnień:\n")
cat("AIC (Akaike): p =", p_AIC, "\n")
cat("SC (Schwarz/BIC): p =", p_SC, "\n")

# Wybieramy p = 2 (zgodnie z SC i wcześniejszą analizą)
p_wybrany <- 2 


# Estymacja modelu VAR na POZIOMACH

# Estymacja modelu metodą najmniejszych kwadratów
# type = "const"
#var_model_poziomy <- VAR(tsdane, p = p_wybrany, type = "const") poprawka na dole
var_model_poziomy <- VAR(tsdane, p = p_wybrany, type = "both")

cat("\n[Punkt 2] Oszacowano model VAR na poziomach (p =", p_wybrany, ")\n")


# Analiza stabilności (Metoda graficzna z zajęć)
cat("\n[Punkt 3] Analiza stabilności (Pierwiastki macierzy towarzyszącej)\n")

# Obliczenie wartości pierwiastków (bez modułów do wykresu, z modułami do oceny)
pierwiastki_poz <- roots(var_model_poziomy, modulus = FALSE)
moduly_poz      <- roots(var_model_poziomy, modulus = TRUE)
max_modul       <- max(moduly_poz)

cat("Największy moduł pierwiastka (Poziomy):", max_modul, "\n")

# Wykres stabilności (Ręczny - styl laboratoryjny)
par(mfrow=c(1,1))
par(mar = c(4, 4, 4, 4))

# Rysujemy punkty na płaszczyźnie zespolonej
plot(Re(pierwiastki_poz), Im(pierwiastki_poz), 
     xlim=c(-1.1, 1.1), ylim=c(-1.1, 1.1), asp=1, 
     xlab="Real", ylab="Imaginary", main="Stabilność VAR (Poziomy)", pch=19, col="red")

# Rysujemy okręgi: jednostkowy (czarny) i pomocniczy 0.9 (niebieski)
plotrix::draw.circle(0, 0, 1, border="black") 
plotrix::draw.circle(0, 0, 0.9, lty = 2, border = 'blue')
abline(h=0, v=0, lty=3, col="gray")


# PUNKT 4: Warunkowa estymacja VAR dla przyrostów

if (max_modul > 0.99) {
  
  cat("\n[Punkt 4] WNIOSEK: Model na granicy niestabilności (moduł > 0.99).\n")
  cat("-> Przechodzę do estymacji modelu dla przyrostów (Punkt 4).\n")
  
  
  # macierz przyrostów
  data_diff_matrix <- cbind(
    diff(tsdane[, "LN_INDPRO"]),
    tsdane[-1, "INFLATION"],      # Inflacja bez zmian (tylko dopasowanie czasu)
    diff(tsdane[, "FEDFUNDS"]),
    diff(tsdane[, "LN_WTISPLC"])
  )
  
  colnames(data_diff_matrix) <- c("D.LN_INDPRO", "INFLATION", "D.FEDFUNDS", "D.LN_WTISPLC")
  
  # Konwersja na obiekt ts (zachowanie dat i częstotliwości)
  # start przesuwamy o 1 miesiąc względem oryginału
  ts_diff_final <- ts(data_diff_matrix, start = time(tsdane)[2], frequency = 12)
  
  
  p_prime <- max(1, p_wybrany - 1)
  
  type_prime <- "const" # <--- TUTAJ ZMIEŃ Z "none" NA "const"
  
  # Estymacja VAR na przyrostach
  var_model_przyrosty <- VAR(ts_diff_final, p = p_prime, type = type_prime)
  
  
  # Ocena stabilności procesu (Metoda graficzna)
  pierwiastki_diff <- roots(var_model_przyrosty, modulus = FALSE)
  moduly_diff      <- roots(var_model_przyrosty, modulus = TRUE)
  max_modul_diff   <- max(moduly_diff)
  
  cat("Największy moduł pierwiastka (Przyrosty):", max_modul_diff, "\n")
  
  # Wykres dla przyrostów
  plot(Re(pierwiastki_diff), Im(pierwiastki_diff), 
       xlim=c(-1.1, 1.1), ylim=c(-1.1, 1.1), asp=1, 
       xlab="Real", ylab="Imaginary", main="Stabilność VAR (Przyrosty)", pch=19, col="red")
  
  plotrix::draw.circle(0, 0, 1, border="black") 
  plotrix::draw.circle(0, 0, 0.9, lty = 2, border = 'blue')
  abline(h=0, v=0, lty=3, col="gray")
  
  # Podsumowanie końcowe
  if (max_modul_diff < 1) {
    cat("WNIOSEK KOŃCOWY: Model VAR dla przyrostów jest STABILNY.\n")
  } else {
    cat("WNIOSEK KOŃCOWY: Model VAR dla przyrostów nadal niestabilny.\n")
  }
  
} else {
  cat("\n[Punkt 4] WNIOSEK: Model na poziomach jest wyraźnie STABILNY (moduł < 0.99).\n")
  cat("Estymacja modelu dla przyrostów nie jest wymagana.\n")
}



# OBSERWACJE NIETYPOWE 

# Parametr rzędu opóźnień
rzad <- 1 

# --- RUNDA 1: Diagnoza modelu wyjściowego ---

# 1. Wyciągamy reszty z modelu podstawowego (bez dummies)
E1 <- residuals(var_model_przyrosty)
E1_standard <- scale(E1)

# 2. Wykres reszt standaryzowanych
par(mfrow = c(ncol(E1_standard), 1), mar = c(2, 4, 2, 1))
for (i in 1:ncol(E1_standard)) {
  plot.ts(E1_standard[,i], main = paste("Runda 1 - Reszty:", colnames(E1_standard)[i]))
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

cat("Czy w modelu wyjściowym są outliery? ", any(abs(E1_standard) > 3.3), "\n")

# 3. Wyświetlenie numerów obserwacji przekraczających próg 3.3
cat("\n--- WYNIKI RUNDY 1 ---\n")
# Zbieramy wszystkie indeksy do wektora, żeby od razu użyć ich do macierzy
indeksy_runda1 <- c()

for (i in 1:ncol(E1_standard)) {
  ind <- which(abs(E1_standard[,i]) > 3.3) + rzad
  if(length(ind) > 0) {
    cat(paste("Zmienna:", colnames(E1_standard)[i], "- Obs:", paste(ind, collapse=", "), "\n"))
    indeksy_runda1 <- c(indeksy_runda1, ind)
  }
}

# Usuwamy duplikaty i sortujemy (to ważne dla porządku w macierzy)
indeksy_runda1 <- sort(unique(indeksy_runda1))


# 4. Tworzymy macierz zer
# Wymiary muszą być takie jak danych wejściowych (ts_diff_final)
mat1 <- matrix(0, nrow = nrow(ts_diff_final), ncol = length(indeksy_runda1))

# 5. Wstawiamy jedynki (odpowiednik mat[96,1]=1 z zajęć)
for (i in 1:length(indeksy_runda1)) {
  # Wstawiamy 1 dokładnie w wierszu odpowiadającym indeksowi szoku
  mat1[indeksy_runda1[i], i] <- 1
}

# Nadajemy nazwy kolumnom (np. d283, d421)
colnames(mat1) <- paste0("d", indeksy_runda1)

# 6. Szacujemy PIERWSZY model poprawiony (var_model_1)
var_model_1 <- VAR(ts_diff_final, p = rzad, type = "const", exogen = mat1)



# RUNDA 2

# --- RUNDA 2: Diagnoza modelu po pierwszej korekcie (var_model_1) ---

# 1. Wyciągamy reszty z modelu var_model_1 (oszacowanego w Rundzie 1)
E2 <- residuals(var_model_1)
E2_standard <- scale(E2)

# 2. Wykres reszt standaryzowanych
par(mfrow = c(ncol(E2_standard), 1), mar = c(2, 4, 2, 1))
for (i in 1:ncol(E2_standard)) {
  plot.ts(E2_standard[,i], main = paste("Runda 2 - Reszty:", colnames(E2_standard)[i]))
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

# Zadajemy pytanie kontrolne
cat("Czy w modelu po 1. korekcie wciąż są outliery? ", any(abs(E2_standard) > 3.3), "\n")

# 3. Wyświetlenie numerów nowych obserwacji nietypowych
cat("\n--- WYNIKI RUNDY 2 (Nowe outliery) ---\n")
indeksy_runda2 <- c()

for (i in 1:ncol(E2_standard)) {
  # Szukamy przekroczeń w resztach modelu 1
  ind <- which(abs(E2_standard[,i]) > 3.3) + rzad
  
  if(length(ind) > 0) {
    cat(paste("Zmienna:", colnames(E2_standard)[i], "- Obs:", paste(ind, collapse=", "), "\n"))
    indeksy_runda2 <- c(indeksy_runda2, ind)
  }
}

# Usuwamy ewentualne duplikaty w ramach tej rundy i sortujemy
indeksy_runda2 <- sort(unique(indeksy_runda2))

# budowa kolejnej macierzy i łączenie

if(length(indeksy_runda2) > 0) {
  
  # 4. Tworzymy macierz zer TYLKO dla nowych punktów
  mat2 <- matrix(0, nrow = nrow(ts_diff_final), ncol = length(indeksy_runda2))
  
  # 5. Wstawiamy jedynki dla nowych winowajców
  for (i in 1:length(indeksy_runda2)) {
    mat2[indeksy_runda2[i], i] <- 1
  }
  
  # Nadajemy nazwy kolumnom (żeby odróżnić je od tych z rundy 1)
  colnames(mat2) <- paste0("d", indeksy_runda2)
  
  # 6. ŁĄCZENIE MACIERZY (Metoda z zajęć: cbind)
  # Sklejamy starą mat1 z nową mat2
  mat_combined <- cbind(mat1, mat2)
  
  # 7. Szacujemy DRUGI model poprawiony (var_model_2)
  # Uwaga: jako exogen podajemy połączoną macierz 'mat_combined'
  var_model_2 <- VAR(ts_diff_final, p = rzad, type = "const", exogen = mat_combined)
  
  cat("\nMacierz mat2 utworzona. Połączono z mat1. Model var_model_2 oszacowany.\n")
  
} else {
  cat("\nBrak nowych outlierów. Model var_model_1 jest ostateczny.\n")
}

# --- RUNDA 3: Diagnoza modelu po drugiej korekcie (var_model_2) ---

# 1. Wyciągamy reszty z modelu var_model_2
E3 <- residuals(var_model_2)
E3_standard <- scale(E3)

# 2. Wykres reszt standaryzowanych
par(mfrow = c(ncol(E3_standard), 1), mar = c(2, 4, 2, 1))
for (i in 1:ncol(E3_standard)) {
  plot.ts(E3_standard[,i], main = paste("Runda 3 - Reszty:", colnames(E3_standard)[i]))
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

# Zadajemy pytanie kontrolne
cat("Czy w modelu po 2. korekcie wciąż są outliery? ", any(abs(E3_standard) > 3.3), "\n")

# 3. Wyświetlenie numerów NOWYCH obserwacji nietypowych
cat("\n--- WYNIKI RUNDY 3 (Nowe outliery) ---\n")
indeksy_runda3 <- c()

for (i in 1:ncol(E3_standard)) {
  # Szukamy przekroczeń w resztach modelu 2
  ind <- which(abs(E3_standard[,i]) > 3.3) + rzad
  
  if(length(ind) > 0) {
    cat(paste("Zmienna:", colnames(E3_standard)[i], "- Obs:", paste(ind, collapse=", "), "\n"))
    indeksy_runda3 <- c(indeksy_runda3, ind)
  }
}


indeksy_runda3 <- sort(unique(indeksy_runda3))

# budowa kolejnej macierzy i łączenie

if(length(indeksy_runda3) > 0) {
  
  mat3 <- matrix(0, nrow = nrow(ts_diff_final), ncol = length(indeksy_runda3))
  
  for (i in 1:length(indeksy_runda3)) {
    mat3[indeksy_runda3[i], i] <- 1
  }
  
  # Nadajemy nazwy kolumnom
  colnames(mat3) <- paste0("d", indeksy_runda3)
  
  # Dołączamy mat3 do poprzednio połączonej macierzy mat_combined
  mat_combined_final <- cbind(mat_combined, mat3)
  
  var_model_3 <- VAR(ts_diff_final, p = rzad, type = "const", exogen = mat_combined_final)
  
  cat("\nMacierz mat3 utworzona. Połączono z poprzednimi. Model var_model_3 oszacowany.\n")
  
} else {
  cat("\nBrak nowych przekroczeń progu 3.3. Model var_model_2 można uznać za finalny.\n")
}

# --- RUNDA 4: Diagnoza modelu po trzeciej korekcie (var_model_3) ---

# 1. Wyciągamy reszty z modelu var_model_3
E4 <- residuals(var_model_3)
E4_standard <- scale(E4)

# 2. Wykres reszt standaryzowanych
par(mfrow = c(ncol(E4_standard), 1), mar = c(2, 4, 2, 1))
for (i in 1:ncol(E4_standard)) {
  plot.ts(E4_standard[,i], main = paste("Runda 4 - Reszty:", colnames(E4_standard)[i]))
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

cat("Czy w modelu po 3. korekcie wciąż są outliery? ", any(abs(E4_standard) > 3.3), "\n")

# 3. Wyświetlenie numerów NOWYCH obserwacji nietypowych
cat("\n--- WYNIKI RUNDY 4 (Nowe outliery) ---\n")
indeksy_runda4 <- c()

for (i in 1:ncol(E4_standard)) {
  # Szukamy przekroczeń w resztach modelu 3
  ind <- which(abs(E4_standard[,i]) > 3.3) + rzad
  
  if(length(ind) > 0) {
    cat(paste("Zmienna:", colnames(E4_standard)[i], "- Obs:", paste(ind, collapse=", "), "\n"))
    indeksy_runda4 <- c(indeksy_runda4, ind)
  }
}

indeksy_runda4 <- sort(unique(indeksy_runda4))

# budowa kolejnej macierzy i łączenie

if(length(indeksy_runda4) > 0) {
  
  mat4 <- matrix(0, nrow = nrow(ts_diff_final), ncol = length(indeksy_runda4))
  
  for (i in 1:length(indeksy_runda4)) {
    mat4[indeksy_runda4[i], i] <- 1
  }
  
  colnames(mat4) <- paste0("d", indeksy_runda4)
  
  mat_combined_r4 <- cbind(mat_combined_final, mat4)
  
  var_model_4 <- VAR(ts_diff_final, p = rzad, type = "const", exogen = mat_combined_r4)
  
  cat("\nMacierz mat4 utworzona. Połączono z poprzednimi. Model var_model_4 oszacowany.\n")
  
} else {
  cat("\nBrak nowych przekroczeń progu 3.3. Model var_model_3 można uznać za finalny.\n")
}

# --- RUNDA 5: Diagnoza modelu po czwartej korekcie (var_model_4) ---

# 1. Wyciągamy reszty z modelu var_model_4
E5 <- residuals(var_model_4)
E5_standard <- scale(E5)

# 2. Wykres reszt standaryzowanych
par(mfrow = c(ncol(E5_standard), 1), mar = c(2, 4, 2, 1))
for (i in 1:ncol(E5_standard)) {
  plot.ts(E5_standard[,i], main = paste("Runda 5 - Reszty:", colnames(E5_standard)[i]))
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

cat("Czy w modelu po 4. korekcie wciąż są outliery? ", any(abs(E5_standard) > 3.3), "\n")

# 3. Wyświetlenie numerów NOWYCH obserwacji nietypowych
cat("\n--- WYNIKI RUNDY 5 (Nowe outliery) ---\n")
indeksy_runda5 <- c()

for (i in 1:ncol(E5_standard)) {
  # Szukamy przekroczeń w resztach modelu 4
  ind <- which(abs(E5_standard[,i]) > 3.3) + rzad
  
  if(length(ind) > 0) {
    cat(paste("Zmienna:", colnames(E5_standard)[i], "- Obs:", paste(ind, collapse=", "), "\n"))
    indeksy_runda5 <- c(indeksy_runda5, ind)
  }
}

indeksy_runda5 <- sort(unique(indeksy_runda5))

# budowa kolejnej macierzy i łączenie

if(length(indeksy_runda5) > 0) {
  
  mat5 <- matrix(0, nrow = nrow(ts_diff_final), ncol = length(indeksy_runda5))
  
  for (i in 1:length(indeksy_runda5)) {
    mat5[indeksy_runda5[i], i] <- 1
  }
  
  colnames(mat5) <- paste0("d", indeksy_runda5)
  
  mat_combined_r5 <- cbind(mat_combined_r4, mat5)
  
  var_model_5 <- VAR(ts_diff_final, p = rzad, type = "const", exogen = mat_combined_r5)
  
  cat("\nMacierz mat5 utworzona. Połączono z poprzednimi. Model var_model_5 oszacowany.\n")
  
} else {
  cat("\nBrak nowych przekroczeń progu 3.3. Model var_model_4 można uznać za finalny.\n")
}

# --- RUNDA 6: Diagnoza modelu po piątej korekcie (var_model_5) ---

# 1. Wyciągamy reszty z modelu var_model_5
E6 <- residuals(var_model_5)
E6_standard <- scale(E6)

# 2. Wykres reszt standaryzowanych
par(mfrow = c(ncol(E6_standard), 1), mar = c(2, 4, 2, 1))
for (i in 1:ncol(E6_standard)) {
  plot.ts(E6_standard[,i], main = paste("Runda 6 - Reszty:", colnames(E6_standard)[i]))
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

cat("Czy w modelu po 5. korekcie wciąż są outliery? ", any(abs(E6_standard) > 3.3), "\n")

# 3. Wyświetlenie numerów NOWYCH obserwacji nietypowych
cat("\n--- WYNIKI RUNDY 6 (Nowe outliery) ---\n")
indeksy_runda6 <- c()

for (i in 1:ncol(E6_standard)) {
  # Szukamy przekroczeń w resztach modelu 5
  ind <- which(abs(E6_standard[,i]) > 3.3) + rzad
  
  if(length(ind) > 0) {
    cat(paste("Zmienna:", colnames(E6_standard)[i], "- Obs:", paste(ind, collapse=", "), "\n"))
    indeksy_runda6 <- c(indeksy_runda6, ind)
  }
}

indeksy_runda6 <- sort(unique(indeksy_runda6))

# budowa kolejnej macierzy i łączenie

if(length(indeksy_runda6) > 0) {
  
  mat6 <- matrix(0, nrow = nrow(ts_diff_final), ncol = length(indeksy_runda6))
  
  for (i in 1:length(indeksy_runda6)) {
    mat6[indeksy_runda6[i], i] <- 1
  }
  
  colnames(mat6) <- paste0("d", indeksy_runda6)
  
  mat_combined_r6 <- cbind(mat_combined_r5, mat6)
  
  var_model_6 <- VAR(ts_diff_final, p = rzad, type = "const", exogen = mat_combined_r6)
  
  cat("\nMacierz mat6 utworzona. Połączono z poprzednimi. Model var_model_6 oszacowany.\n")
  
} else {
  cat("\nBrak nowych przekroczeń progu 3.3. Model var_model_5 można uznać za finalny.\n")
}

# --- RUNDA 7: Diagnoza modelu po szóstej korekcie (var_model_6) ---

# 1. Wyciągamy reszty z modelu var_model_6
E7 <- residuals(var_model_6)
E7_standard <- scale(E7)

# 2. Wykres reszt standaryzowanych
par(mfrow = c(ncol(E7_standard), 1), mar = c(2, 4, 2, 1))
for (i in 1:ncol(E7_standard)) {
  plot.ts(E7_standard[,i], main = paste("Runda 7 - Reszty:", colnames(E7_standard)[i]))
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

cat("Czy w modelu po 6. korekcie wciąż są outliery? ", any(abs(E7_standard) > 3.3), "\n")


var_model_final <- var_model_6

# --- SEKCJA GENEROWANIA WYKRESÓW Z DATAMI ---

# 1. Definiujemy start na marzec 1985 (1985, 3)
start_daty <- c(1985, 3)

# 2. Tworzymy obiekt szeregu czasowego dla reszt standaryzowanych
reszty_standaryzowane_ts <- ts(scale(residuals(var_model_final)), 
                               start = start_daty, 
                               frequency = 12)

# 3. Generujemy wykresy (to są te do Rundy 7 / Finalne)
par(mfrow = c(4, 1), mar = c(3, 4, 2, 1))
for (i in 1:4) {
  plot(reszty_standaryzowane_ts[,i], 
       main = paste("Finalne reszty z osia czasu:", colnames(reszty_standaryzowane_ts)[i]), 
       xlab = "Rok", 
       ylab = "Std. Residuals",
       col = "black")
  abline(h = c(-3.3, 3.3), col = "red", lty = 2)
}

# 4. Sprawdzenie konkretnych dat dla Twoich wyników (opcjonalnie do konsoli)
library(zoo)
os_czasu <- as.yearmon(time(reszty_standaryzowane_ts))
cat("Data dla obs 283:", format(os_czasu[283], "%B %Y"), "\n")
cat("Data dla obs 421:", format(os_czasu[421], "%B %Y"), "\n")

# --- TESTY DIAGNOSTYCZNE (wykonujemy dla modelu końcowego) ---

# ----------------------------------------------------------
# 1. Testowanie normalności zaburzeń losowych
# ----------------------------------------------------------

# Wersja wielowymiarowa + test dla każdego równania osobno
test_norm_final <- normality.test(var_model_final, multivariate.only = FALSE)
print(test_norm_final)

# Wykresy: histogramy reszt, empiryczna gęstość oraz wykresy Q-Q
plot(test_norm_final)


# ----------------------------------------------------------
# 2. Testy autokorelacji zaburzeń losowych
# ----------------------------------------------------------

# Test łączny (Portmanteau) - sekwencja dla różnych h
cat("\n--- Test Portmanteau (adjusted) ---\n")
for (h in seq(from = 10, to = floor(var_model_final$obs/4)-1, by = 10)) {
  print(paste("Opóźnienie h =", h))
  print(serial.test(var_model_final, lags.pt = h, type = "PT.adjusted"))
}

# Sekwencja testów Breuscha i Godfreya (BG) oraz Edgertona i Shukura (ES)
# Sprawdzamy dla rzędów od 1 do 5
for (h in 1:5) {
  print(paste("Sprawdzenie autokorelacji rzędu h =", h))
  print(serial.test(var_model_final, lags.bg = h, type = "BG"))
  print(serial.test(var_model_final, lags.bg = h, type = "ES"))
}

# ----------------------------------------------------------
# 3. Testowanie homoskedastyczności (Efekt ARCH)
# ----------------------------------------------------------

# Sekwencja testów stałości warunkowej wariancji
# Sprawdzamy każde równanie osobno + wersja wielowymiarowa (sekwencja 1-5)
for (i in 1:5) {
  print(paste("Test ARCH rzędu =", i))
  print(arch.test(var_model_final, lags.single = i, lags.multi = i, multivariate.only = FALSE))
}


# ----------------------------------------------------------
# 4. Testowanie stałości parametrów (OLS-CUSUM)
# ----------------------------------------------------------

# Ustawienie marginesów wykresu
par(mar = rep(2, 4))
par(mfrow = c(1,1))

# Generowanie procesu CUSUM na resztach OLS
plot(stability(var_model_final, type = "OLS-CUSUM"))

# Opcjonalny zapis wykresu do pliku (zgodnie z zajęciami)
# dev.print(pdf, "VAR_final_stalosc_parametrow.pdf")

# Weryfikacja wizualna reszt (ACF/PACF)
par(mar = c(2,2,2,2))
plot(var_model_final)

###########################################################################
#Zadanie 5

##### Pakiety do VEC i kointegracji #####

if(!require(urca)){
  install.packages('urca')
  library(urca)
}


# PRZYGOTOWANIE DANYCH DO VEC


# Musimy dopasować wymiary zmiennych zero-jedynkowych (dummies) do danych na poziomach.
# mat_combined_r6 (z pętli outlierów) jest krótsza o 1 obs. (przez różnicowanie).
# Dodajemy wiersz zer na początku (dla pierwszej obserwacji).

dumm_vec_final <- rbind(rep(0, ncol(mat_combined_r6)), mat_combined_r6)
colnames(dumm_vec_final) <- colnames(mat_combined_r6)

# Sprawdzenie wymiarów (powinny być równe T_obs)
cat("Liczba wierszy danych:", nrow(tsdane), "\n")
cat("Liczba wierszy dummies:", nrow(dumm_vec_final), "\n")



# 1. TEST LICZBY RELACJI KOINTEGRUJĄCYCH (Johansen)


# A. Test największej wartości własnej (Eigenvalue test)
test_eigen <- ca.jo(tsdane,
                    type = "eigen",
                    ecdet = "none",      # Brak trendu w relacji kointegrującej (zgodnie z Twoim wzorem)
                    K = p_wybrany,       # Liczba opóźnień w poziomach (u nas 2)
                    spec = "transitory",
                    dumvar = dumm_vec_final) # Włączamy zidentyfikowane outliery

cat("\n--- Wyniki testu Eigenvalue ---\n")
summary(test_eigen)

# B. Test śladu (Trace test)
test_trace <- ca.jo(tsdane,
                    type = "trace",
                    ecdet = "none",
                    K = p_wybrany,
                    spec = "transitory",
                    dumvar = dumm_vec_final)

cat("\n--- Wyniki testu Trace ---\n")
summary(test_trace)




# 2. SZACOWANIE MODELU VEC (dla r = 1)

# Używamy obiektu testu (np. test_eigen) do estymacji parametrów.
r_wybrane <- 1

vec_estymacja <- cajorls(test_eigen, r = r_wybrane)

cat("\n--- Ocena istotności współczynników dostosowań (alpha) ---\n")
# To nam mówi, które zmienne reagują na odchylenia od równowagi długookresowej
print(vec_estymacja$rlm)
summary(vec_estymacja$rlm)


# Opcjonalnie: Zapis wektora kointegrującego (beta)
beta_vec <- vec_estymacja$beta
cat("\nWektor kointegrujący (Beta):\n")
print(beta_vec)
# write.csv(beta_vec, "beta_wektor.csv")



# 3. TRANSFORMACJA DO MODELU VAR



VAR_from_VEC <- vec2var(test_eigen, r = r_wybrane)
VAR_from_VEC

cat("\nModel został przekształcony do postacji VAR (VAR_from_VEC).\n")
# print(VAR_from_VEC) # Można odkomentować, żeby zobaczyć współczynniki



# 4. PROGNOZOWANIE (Punktowe i Przedziałowe)

horyzont <- 36 # Prognoza na 36 miesięcy

# Musimy przygotować macierz dummies na okres prognozy.
dumm_future <- matrix(0, nrow = horyzont, ncol = ncol(dumm_vec_final))
colnames(dumm_future) <- colnames(dumm_vec_final)

# Wykonanie prognozy
# ci = 0.95 oznacza przedział ufności 95%
prognoza_vec <- predict(VAR_from_VEC, 
                        n.ahead = horyzont, 
                        ci = 0.95, 
                        dumvar = dumm_future) 

# Wyświetlenie wykresów prognozy dla każdej zmiennej
plot(prognoza_vec)

# Wykres wachlarzowy (fanchart)
fanchart(prognoza_vec)


# 1. Zapis wykresu prognozy standardowej (linie + przedziały)
pdf("prognoza_vec_standard.pdf", width = 12, height = 8)
plot(prognoza_vec)
dev.off()

# 2. Zapis wykresu wachlarzowego (fanchart)
pdf("prognoza_vec_fanchart.pdf", width = 12, height = 8)
fanchart(prognoza_vec)
dev.off()


#ZAPIS

library(vars)

png("prognozy_fanchart_wysoki.png", width = 14, height = 16, units = "in", res = 300)

# Marginesy zostawiamy bez zmian, są odpowiednie dla tych opisów
par(mar = c(4, 4, 4, 2))

# Generowanie wykresu z polskimi opisami (tak jak poprzednio)
fanchart(prognoza_vec, 
         main = c("Prognoza wachlarzowa: Produkcja przemysłowa", 
                  "Prognoza wachlarzowa: Inflacja", 
                  "Prognoza wachlarzowa: Stopy procentowe FED", 
                  "Prognoza wachlarzowa: Cena ropy WTI"),
         xlab = "Rok", 
         ylab = "Poziom",
         
         # --- SEKCJA POWIĘKSZANIA CZCIONEK ---
         cex.main = 2.0,  # Tytuły 2x większe niż standardowo
         cex.lab = 3.0,   # Etykiety osi (Rok, Poziom) 1.8x większe
         cex.axis = 3.0   # Liczby na osiach 1.5x większe
         )
# Zamknięcie urządzenia graficznego
dev.off()



# ZADANIE 7: Analiza strukturalna (IRF, FEVD) i przyczynowość Grangera



# 1. USTALENIE KOLEJNOŚCI ZMIENNYCH (Cholesky)


# Używamy obiektu 'tsdane' (Twoje dane projektowe), a nie 'dane_skrocone' (z zajęć)
dane_ordered <- tsdane[, c("LN_WTISPLC", "LN_INDPRO", "INFLATION", "FEDFUNDS")]
tytuly_ord <- colnames(dane_ordered)

cat("\n[Zadanie 7.1] Nowa kolejność zmiennych do analizy IRF:\n")
print(tytuly_ord)


# 2. PONOWNE SZACOWANIE MODELU (Dla nowej kolejności)


# Estymacja VEC dla uporządkowanych danych
# Używamy Twojej macierzy zmiennych sztucznych: dummVEC (tę utworzyłeś w poprzednim kroku)
estymacja_ord <- ca.jo(dane_ordered,
                       type = "eigen",
                       ecdet = "none",
                       K = p_wybrany, # Używamy p_wybrany (u Ciebie to było 2)
                       spec = "transitory",
                       dumvar = dumm_vec_final) # Używamy Twojej macierzy dummies

# Transformacja do VAR (dla r = 1, jak ustalono wcześniej)
VAR_final_ord <- vec2var(estymacja_ord, r = 1)


# 3. FUNKCJE REAKCJI NA IMPULS (IRF)


# Ustawienia wykresów
options("scipen" = 100, "digits" = 5)
par(mfrow = c(1,1))
par(mar = c(4,4,2,2))

# A. Reakcja gospodarki na szok stóp procentowych (Polityka Monetarna)
plot(irf(VAR_final_ord, 
         impulse = "FEDFUNDS", 
         response = c("LN_INDPRO", "INFLATION"), 
         n.ahead = 36,  
         boot = TRUE, 
         ci = 0.90,     
         cumulative = FALSE),
     main = "Reakcja Produkcji i Inflacji na szok Stóp Procentowych")

# B. Reakcja gospodarki na szok cen ropy (Szok Podażowy)
plot(irf(VAR_final_ord, 
         impulse = "LN_WTISPLC", 
         response = c("INFLATION", "LN_INDPRO"), 
         n.ahead = 36, 
         boot = TRUE, 
         ci = 0.90),
     main = "Reakcja Inflacji i Produkcji na szok Cen Ropy")


# 4. DEKOMPOZYCJA WARIANCJI BŁĘDÓW PROGNOZ (FEVD)


FEVD_res <- fevd(VAR_final_ord, n.ahead = 60) 

# Wyświetlenie wyników numerycznych 
cat("\n[Zadanie 7.4] Dekompozycja wariancji dla Inflacji (ostatnie okresy):\n")
print(tail(FEVD_res$INFLATION))

cat("\n[Zadanie 7.4] Dekompozycja wariancji dla Produkcji (ostatnie okresy):\n")
print(tail(FEVD_res$LN_INDPRO))

# Wykres dekompozycji (Zapis do PDF)
pdf("fevd_dekompozycja.pdf", width = 10, height = 7)
plot(FEVD_res, col = c("#4477AA", "#DDCC77", "#117733", "#CC6677")) 
dev.off()

# Podgląd w RStudio
plot(FEVD_res, col = c("#4477AA", "#DDCC77", "#117733", "#CC6677"))


# 5. TEST PRZYCZYNOWOŚCI GRANGERA


cat("\n[Zadanie 7.5] Testy przyczynowości Grangera (Model na przyrostach)\n")



# A. Czy Sfera Monetarna (Stopy + Inflacja) przyczynuje się do zmian w Sferze Realnej?
granger_monetary_to_real <- causality(var_model_final, 
                                      cause = c("INFLATION", "D.FEDFUNDS"))
print(granger_monetary_to_real$Granger)

# B. Czy Sfera Realna (Produkcja + Ropa) przyczynuje się do zmian w Sferze Monetarnej?
granger_real_to_monetary <- causality(var_model_final, 
                                      cause = c("D.LN_INDPRO", "D.LN_WTISPLC"))
print(granger_real_to_monetary$Granger)

# C. Test szczegółowy: Czy Stopy (D.FEDFUNDS) wpływają na Produkcję?
granger_fed_to_ind <- causality(var_model_final, cause = "D.FEDFUNDS")
cat("\nTest: Czy D.FEDFUNDS -> Wszystkie inne zmienne?\n")
print(granger_fed_to_ind$Granger)



# ZAPIS WYKRESÓW DO PLIKÓW PNG (

# 1. Zapis IRF: Szok Stóp Procentowych (Polityka Monetarna)
png("IRF_szok_stop_procentowych.png", width = 10, height = 6, units = "in", res = 300)
par(mar = c(4,4,2,2))
plot(irf(VAR_final_ord, 
         impulse = "FEDFUNDS", 
         response = c("LN_INDPRO", "INFLATION"), 
         n.ahead = 36, 
         boot = TRUE, 
         ci = 0.90, 
         cumulative = FALSE),
     main = "Reakcja Produkcji i Inflacji na szok Stóp Procentowych")
dev.off()

# 2. Zapis IRF: Szok Cen Ropy (Szok Podażowy)
png("IRF_szok_cen_ropy.png", width = 10, height = 6, units = "in", res = 300)
par(mar = c(4,4,2,2))
plot(irf(VAR_final_ord, 
         impulse = "LN_WTISPLC", 
         response = c("INFLATION", "LN_INDPRO"), 
         n.ahead = 36, 
         boot = TRUE, 
         ci = 0.90, 
         cumulative = FALSE),
     main = "Reakcja Inflacji i Produkcji na szok Cen Ropy")
dev.off()

# 3. Zapis FEVD: Dekompozycja Wariancji
png("FEVD_dekompozycja.png", width = 12, height = 8, units = "in", res = 300)
par(mar = c(2,2,2,2))
# Używamy 4 kolorów, bo masz 4 zmienne w modelu
plot(FEVD_res, col = c("#4477AA", "#DDCC77", "#117733", "#CC6677")) 
dev.off()

cat("\nWykresy zostały zapisane jako pliki PNG w katalogu roboczym.\n")
