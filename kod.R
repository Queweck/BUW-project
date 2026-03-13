install.packages("jsonlite")
install.packages("ggplot2")
install.packages("MASS")
install.packages("dplyr")
install.packages("performance")
install.packages("see")
install.packages("insight")
install.packages("dynlm")
install.packages("prophet")

update.packages(ask = FALSE)

install.packages("ARDL")
install.packages("zoo")
install.packages("viridis")
library(viridis)
library(ARDL)
library(zoo)
library(dynlm)
library(insight)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(lubridate)
library(corrplot)
library(lmtest)
library(MASS)
library(performance)
library(see)
library(DHARMa)
library(prophet)

library(skedastic)
library(stargazer)
library(rstudioapi)
library(strucchange)
library(car)
library(corrplot)
library(ggplot2)
library(lmtest)
library(sandwich)
library(readstata13)
library(dplyr)
library(tseries)
library(flextable)
library(modelsummary)
################################################################################
#data buw

data <- fromJSON("buw_people.json")

data$datetime <- as.POSIXct(
  paste(data$date, data$hour),
  format = "%Y-%m-%d %H:%M"
)

ggplot(data, aes(x = datetime, y = people)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Liczba ludzi w czasie",
    x = "Czas",
    y = "Liczba ludzi"
  ) +
  theme_minimal()

data_max <- data %>%
  group_by(date) %>%
  summarise(max_ludzie = max(people, na.rm = TRUE),
            sesja_1=max(sesja),
            days_to_sesja_1=max(days_to_sesja))

ggplot(data_max, aes(x = as.Date(date), y = max_ludzie)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Maksymalna ilość studentów w BUWie w danym dniu",
    x = "Data",
    y = "Max liczba studentów"
  ) +
  theme_minimal()

ggplot(data_max, aes(x = as.Date(date), y = max_ludzie)) +
  geom_rect(data = data_max %>% filter(sesja_1 == 1),
            aes(xmin = as.Date(date) - 0.5, xmax = as.Date(date) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.3) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "#d7191c", se = TRUE, span = 0.05) + # se=TRUE pokaże przedział ufności
  labs(
    title = "Ewolucja obłożenia BUW w czasie",
    subtitle = "Linia czerwona: wygładzony trend (LOESS)",
    x = "Data",
    y = "Maksymalna liczba osób"
  ) +
  theme_light() +
  theme(plot.title = element_text(face = "bold", size = 14))

data_max_0<-data_max%>%
  filter(max_ludzie==0)

ggplot(data_max, aes(x = as.Date(date), y = max_ludzie)) +
  # Dodajemy prostokąty dla okresów sesji (tło)
  geom_rect(data = data_max %>% filter(sesja_1 == 1),
            aes(xmin = as.Date(date) - 0.5, xmax = as.Date(date) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.3) +
  # Główna linia
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(title = "Obłożenie BUW: Podświetlone okresy sesji", 
       x = "Data", y = "Max osób") +
  theme_minimal()

data<-data%>%
  filter(date!="2023-10-01",
         date!="2025-12-25",
         date!="2025-12-26",
         date!="2025-12-27",
         date!="2025-12-28")

################################################################################
#dane pogodowe


weather_head <- read.csv("Dane pogodowe/s_t_nagłówek.csv",header=FALSE)
weather_2023<-read.csv("Dane pogodowe/s_t_375_2023.csv",header=FALSE)
weather_2024<-read.csv("Dane pogodowe/s_t_375_2024.csv",header=FALSE)
weather_2025<-read.csv("Dane pogodowe/s_t_375_2025.csv",header=FALSE)
weather_2026<-read.csv("Dane pogodowe/s_t_01_2026.csv",header=FALSE)

weather_2026<-weather_2026%>%
  filter(V2=="WARSZAWA")

weather<-bind_rows(weather_2023,weather_2024,weather_2025,weather_2026)

colnames(weather)=weather_head

data_w <- weather %>%
  select("ROK","MC","DZ","GG","WID","NOG","FWR","TEMP","TWLW","WLGW","WO6G","ROPT","PKSN","HSS")

data_w <- weather[, c("ROK","MC","DZ","GG","WID","NOG","FWR","TEMP","TWLW","WLGW","WO6G","ROPT","PKSN","HSS")]

colnames(data_w)<-c("year","month","day","hour","view","cloud","wind","temp","ice",
                    "wet","rain","rain_type","snow","snowfall")  

colSums(is.na(data_w))

data_w$datetime <- as.POSIXct(
  paste(data_w$year,
        sprintf("%02d", data_w$month),
        sprintf("%02d", data_w$day),
        sprintf("%02d", data_w$hour),
        sep = "-"),  # tymczasowo łączymy z "-"
  format = "%Y-%m-%d-%H"
)

# Możemy ustawić minutę i sekundę na 00
data_w$datetime <- format(data_w$datetime, "%Y-%m-%d %H:00:00")
data_w$datetime <- as.POSIXct(data_w$datetime, format = "%Y-%m-%d %H:%M:%S")

data_w <- data_w %>%
  mutate(
    rain = ifelse(hour %in% c(0,6,12,18) & is.na(rain), 0, rain)
  )

# data_w <- data_w %>%
#   arrange(day, hour) %>%  # upewniamy się, że dane są posortowane po dniu i godzinie
#   group_by(day) %>%
#   mutate(
#     rain_new = ifelse(hour %in% c(0,6,12,18) | !is.na(rain), rain, NA)
#   ) %>%
#   fill(rain_new, .direction = "down") %>%  # wypełniamy wartości w dół
#   ungroup()

data_w <- data_w %>%
  arrange(datetime) %>%  # upewniamy się, że dane są posortowane po dniu i godzinie
  mutate(
    rain_new = ifelse(hour %in% c(0,6,12,18), rain, NA)
  ) %>%
  fill(rain_new, .direction = "down") # wypełniamy wartości w dół

data_w[is.na(data_w)]<-0

data_w<-data_w[,c("datetime","year","month","day","hour","view","cloud","wind","temp",
         "wet","rain")]

################################################################################
#połączenie pogody i buw

data <- data %>%
  left_join(data_w, by = "datetime")

class(data$datetime)
class(data_w$datetime)

################################################################################
#dane kalendarzowe

data_c<-read_excel("Dane kalendarzowe/kalendarz.xlsx")

data_c[is.na(data_c)]<-0

data_c$date <- as.Date(data_c$Date)
data$date <- as.Date(data$date)

data <- data %>%
  left_join(data_c, by = "date")

################################################################################
#dane uneployment
data_u<-read_excel("unemployment.xlsx")
data_u$date <- as.Date(data_u$Date)

data <- data %>%
  left_join(data_u, by = "date")

data <- data %>%
  arrange(datetime) %>%  # upewniamy się, że dane są posortowane po dniu i godzinie
  mutate(
    unemp = ifelse(hour %in% c(0,6,12,18), unemp, NA)
  ) %>%
  fill(unemp, .direction = "down") # wypełniamy wartości w dół

################################################################################
#wyczyszczenie danych

data<-data[,c("date","datetime","day.x","month.x","year.x","hour.y","week_day","stud_year","semester","work_day","holidays",
         "BUW_24_7","tragic_events","days_from_tragic_event","special_events",
         "days_from_special_event","sesja","sesja_poprawkowa","break",
         "days_to_sesja","days_from_sesja_to_sesja_po","days_in_sesja","days_in_sesja_pop",
         "people",
         "temp","rain","cloud","wind","view","wet","unemployment")]

colnames(data)<-c("date","datetime","day","month","year","hour","week_day","stud_year","semester","work_day","holidays",
                  "BUW_24_7","tragic_events","days_from_tragic_event","special_events",
                  "days_from_special_event","sesja","sesja_poprawkowa","break_",
                  "days_to_sesja","days_from_sesja_to_sesja_po","days_in_sesja","days_in_sesja_pop",
                  "people",
                  "temp","rain","cloud","wind","view","wet","unemp")


data <- data %>%
  filter(BUW_24_7 == 1 | (BUW_24_7 == 0 & hour >= 7))

data <- data %>%
  arrange(datetime) %>%     # sortujemy dane
  group_by(day) %>%          # działamy osobno dla każdego dnia
  mutate(
    people_1 = ifelse(hour - lag(hour) == 1,
                         lag(people),
                         0),
    people_2 = ifelse(hour - lag(hour,n=2) == 2,
                      lag(people_1),
                      0),
    people_3 = ifelse(hour - lag(hour,n=3) == 3,
                      lag(people_2),
                      0),
    people_4 = ifelse(hour - lag(hour,n=4) == 4,
                      lag(people_3),
                      0)
  ) %>%
  ungroup()

data<-data[,c("date","datetime","day","month","year","hour","week_day","stud_year","semester","work_day","holidays",
              "BUW_24_7","tragic_events","days_from_tragic_event","special_events",
              "days_from_special_event","sesja","sesja_poprawkowa","break_",
              "days_to_sesja","days_from_sesja_to_sesja_po","days_in_sesja","days_in_sesja_pop",
              "people",
              "temp","rain","cloud","wind","view","wet")]

data<-data%>%
  filter(!is.na(day))

colSums(is.na(data))

data[is.na(data)]<-0

library(tidyr)

data <- data %>%
  arrange(datetime) %>%  # upewniamy się, że dane są posortowane po dniu i godzinie
  mutate(
    rain_new = ifelse(hour %in% c(0,6,12,18), rain, NA)
  ) %>%
  fill(rain_new, .direction = "down") # wypełniamy wartości w dół

data[is.na(data)]<-0

data <- data %>%
  mutate(
    hour2 = hour^2,
    hour3 = hour^3,
    temp2 = temp^2
  )

data <- data %>%
  arrange(datetime) %>%     # sortujemy dane
  group_by(day) %>%          # działamy osobno dla każdego dnia
  mutate(
    rain_2 = ifelse(hour - lag(hour) == 1,
                      lag(rain),
                      0),
    rain_4 = ifelse(hour - lag(hour,4) == 4,
                  lag(rain,4),
                  0),
    temp_4 = ifelse(hour - lag(hour,4) == 4,
                  lag(temp,4),
                  0)
  ) %>%
  ungroup()

data <- data %>%
  group_by(month, hour) %>%
  mutate(
    srednia_temp = mean(temp, na.rm = TRUE),
    # To jest Twoja zmienna var_temp
    var_temp = temp - srednia_temp 
  ) %>%
  ungroup() # Zawsze odgrupowuj po operacjach grupowych!
################################################################################
#modele
data$rain_exp <- 0.99^(data$rain_new)



model1<-lm(people~hour+hour2+hour3+I(var_temp^2)+wind+I(sesja*hour)+sesja_poprawkowa
             +week_day+break_+days_to_sesja+I(days_to_sesja^2)+days_in_sesja+semester+stud_year
           +holidays+tragic_events
           +people_1+people_4,data)

summary(model1)

durbinWatsonTest(model1)

dw_test <- dwtest(model1) 
print(dw_test)

Dbp_test <- bptest(model1)
print(bp_test)

vif_values <- vif(model1)
print(vif_values)

model_odp<-coeftest(model1, vcov = vcovHC(model1, type = "HC3"))

var_names <- c(
  "hour"="godzina",
  "hour2"="godzina^2",
  "hour3"="godzina^3",
  "I(var_temp^2)"="odchylenie_temp^2",
  "wind"="wiatr",
  "I(sesja*hour)"="sesja*godzina",
  "sesja_poprawkowa"="sesja_poprawkowa",
  "week_dayWtorek"="wtorek",
  "week_dayŚroda"="środa",
  "week_dayCzwartek"="czwartek",
  "week_dayPiątek"="piątek",
  "week_daySobota"="sobota",
  "week_dayNiedziela"="niedziela",
  "break_"="wakacje",
  "days_to_sesja"="dni_do_sesji",
  "I(days_to_sesja^2)"="dni_do_sesji^2",
  "days_in_sesja"="dzień_sesji",
  "semester"="semestr (1 lub 2)",
  "stud_year2024/2025"="rok ak. 2024/2025",
  "stud_year2025/2026"="rok ak. 2025/2026",
  "holidays"="święta",
  "tragic_events"="tragiczne_wydarzenie",
  "people_1"="ilość_ludzi_godzinę_wcześniej",
  "people_4"="ilość_ludzi_4_godziny_wcześniej"
)

ft_2<-modelsummary(
  list("Model podstawowy"=model1,"Model z błędami odpornymi"=model_odp),
  coef_map = var_names,
  stars = TRUE,
  statistic = "std.error",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "flextable"
)
ft_2 <- ft_2 %>%
  fontsize(size = 9) %>% 
  padding(padding.top = 1, padding.bottom = 1) %>%
  line_spacing(space = 0.9)%>%
  autofit()
ft_2

plot(model1)

resettest(model1, power = 2:3, type = "fitted")

model<-lm(people~hour+I(hour^2)+I(hour^3)+rain+temp+I(temp^2)+wind
          +sesja+days_to_sesja+sesja_poprawkowa+break_
          +week_day+holidays+tragic_events
          +stud_year+days_in_sesja+days_from_sesja_to_sesja_po
          +people_1+people_2+people_3,data)
summary(model)

wynik_auto <- auto_ardl(people ~ hour+hour2+hour3, 
                        data = data, 
                        max_order = c(24,24,8,8)) # Przykładowe limity opóźnień

# Wyświetlenie najlepszego modelu
summary(wynik_auto$best_model)

model2<-lm(people~hour+I(hour^2)+I(hour^3)+rain+cloud+view+temp+I(temp^2)+wind
          +sesja+days_to_sesja+sesja_poprawkowa+break_,data)
summary(model2)

#RESER
resettest(model, power = 2:3, type = "fitted")

#współliniowość
vif(model)

#heteroskedastyczność
bptest(model)
white(model)

################################################################################


modelnowy <- glm.nb(people~hour+I(hour^2)+I(hour^3)+rain_exp+cloud+view+temp+I(temp^2)+wind
                    +sesja+days_to_sesja+sesja_poprawkowa+break_
                    +week_day+holidays+tragic_events+special_events
                    +semester+stud_year,
                data = data)

summary(modelnowy)

check_overdispersion(modelnowy)

ks.test(modelnowy)

check_model(modelnowy, check = "pp_check", iterations = 50,residual_type="normal")

res <- simulateResiduals(modelnowy)
plot(res)
testUniformity(res)

library(forecast)

data$hour2<-data$hour^2
data$hour3<-data$hour^3

# Definiujemy macierz zmiennych zewnętrznych (regresorów)
xreg_matrix <- as.matrix(data[, c("temp", "rain_exp", "days_to_sesja","hour",
                                  "view","sesja","days_in_sesja","sesja_poprawkowa","break_",
                                  "work_day","semester","tragic_events",
                                  "hour2","hour3")])

# Modelowanie - auto.arima znajdzie najlepszą strukturę błędów
model_reg_arima <- auto.arima(data$people, xreg = xreg_matrix)

summary(model_reg_arima)

# Sprawdzenie założeń ekonometrycznych (czy błędy są białym szumem)
checkresiduals(model_reg_arima)

dwtest(model_reg_arima)

#######################################################

vars<- data[,c("people","hour","rain","cloud","view","temp","wind","days_to_sesja")]      
cor_mat <- cor(vars, use = "complete.obs", method = "spearman")

corrplot(cor_mat,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.8)

ggplot(data, aes(x = hour, y = people, color = days_to_sesja)) +
  # Punkty z przezroczystością
  geom_point(alpha = 0.3, size = 1.5) +
  # Linia trendu (wielomian 3 stopnia)
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2) + I(x^3),
              se = TRUE,              # Dodaję przedział ufności dla profesjonalnego wyglądu
              linewidth = 1.2,
              color = "blue",         # Kontrastuje z zielonymi punktami
              fill = "lightblue") +   # Kolor tła przedziału ufności
  # Gradient kolorów od ciemnozielonego (sesja blisko) do jasnozielonego (daleko)
  scale_color_gradient(low = "darkgreen", high = "lightgreen", 
                       name = "Dni do sesji") +
  labs(
    title = "Dobowy rozkład frekwencji w BUW",
    subtitle = "Wpływ odległości od sesji na zagęszczenie studentów",
    x = "Godzina dnia",
    y = "Liczba osób w bibliotece"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )


ggplot(data, aes(x = hour, y = people, color = days_to_sesja)) +
  # Punkty z przezroczystością
  geom_point(alpha = 0.3, size = 1.5) +
  # Linia trendu (wielomian 3 stopnia)
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2) + I(x^3),
              se = TRUE,              # Dodaję przedział ufności dla profesjonalnego wyglądu
              linewidth = 1.2,
              color = "blue",         # Kontrastuje z zielonymi punktami
              fill = "lightblue") +   # Kolor tła przedziału ufności
  # Gradient kolorów od ciemnozielonego (sesja blisko) do jasnozielonego (daleko)
  scale_color_gradient(low = "darkgreen", high = "lightgreen", 
                       name = "Dni do sesji") +
  labs(
    title = "Dobowy rozkład frekwencji w BUW",
    subtitle = "Wpływ odległości od sesji na zagęszczenie studentów",
    x = "Godzina dnia",
    y = "Liczba osób w bibliotece"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggplot(data, aes(x = rain_new, y = people, color = days_to_sesja)) +
  # Punkty z przezroczystością
  geom_point(alpha = 0.3, size = 1.5) +
  # Linia trendu (wielomian 3 stopnia)
  geom_smooth(method = "lm",
              formula = y ~ x+x^2,
              se = TRUE,              # Dodaję przedział ufności dla profesjonalnego wyglądu
              linewidth = 1.2,
              color = "blue",         # Kontrastuje z zielonymi punktami
              fill = "lightblue") +   # Kolor tła przedziału ufności
  # Gradient kolorów od ciemnozielonego (sesja blisko) do jasnozielonego (daleko)
  scale_color_gradient(low = "darkgreen", high = "lightgreen", 
                       name = "Dni do sesji") +
  labs(
    title ="Rozkład frekwencji w BUW w zależności od deszczu",
    subtitle = "Wpływ deszczu na zagęszczenie studentów",
    x = "Wielkość deszczu w ciągu ostatnich 6h w mm",
    y = "Liczba osób w bibliotece"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggplot(data, aes(x = var_temp, y = people, color = days_to_sesja)) +
  # Punkty z przezroczystością
  geom_point(alpha = 0.3, size = 1.5) +
  # Linia trendu (wielomian 3 stopnia)
  geom_smooth(method = "lm",
              formula = y ~ x+I(x^2),
              se = TRUE,              # Dodaję przedział ufności dla profesjonalnego wyglądu
              linewidth = 1.2,
              color = "blue",         # Kontrastuje z zielonymi punktami
              fill = "lightblue") +   # Kolor tła przedziału ufności
  # Gradient kolorów od ciemnozielonego (sesja blisko) do jasnozielonego (daleko)
  scale_color_gradient(low = "darkgreen", high = "lightgreen", 
                       name = "Dni do sesji") +
  labs(
    title ="Rozkład frekwencji w BUW w zależności od odchyłu temperatury od średniej",
    subtitle = "Wpływ temperatury na zagęszczenie studentów",
    x = "Odchył temperatury od średniej w miesiącu",
    y = "Liczba osób w bibliotece"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )



data$week_day <- factor(data$week_day, 
                        levels = c("Poniedziałek", "Wtorek", "Środa", "Czwartek", 
                                   "Piątek", "Sobota", "Niedziela"))

ggplot(data, aes(x = hour, y = people, color = days_to_sesja)) +
  # Punkty (przezroczystość 0.2, żeby przy 16k obserwacji nie tworzyły plam)
  geom_point(alpha = 0.2, size = 1) +
  # Linia trendu dla każdego dnia osobno
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2) + I(x^3),
              se = FALSE, 
              color = "blue", linewidth = 1) +
  # Podział na dni tygodnia
  facet_wrap(~week_day, ncol = 4) +
  # Gradient kolorów (od sesji do czasu wolnego)
  scale_color_gradient(low = "darkgreen", high = "lightgreen", 
                       name = "Dni do sesji") +
  labs(
    title = "Dobowy rozkład frekwencji w BUW z podziałem na dni tygodnia",
    x = "Godzina",
    y = "Liczba osób"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10))


ggplot(data, aes(x = days_to_sesja, y = people)) +
  # 1. Punkty z dużą przezroczystością (minimalizm)
  geom_point(alpha = 0.1, color = "seagreen", size = 1) +
  
  # 2. Wyraźna linia trendu kwadratowego
  geom_smooth(method = "lm", 
              formula = y ~ x + I(x^2), 
              color = "darkgreen", 
              linewidth = 1.5, 
              se = TRUE, 
              fill = "lightgreen", 
              alpha = 0.3) +
  
  # 3. Minimalistyczny motyw
  labs(
    title = "Zależność frekwencji od sesji",
    x = "Dni do sesji",
    y = "Liczba studentów"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(), # usunięcie zbędnych linii pomocniczych
    plot.title = element_text(face = "bold", color = "darkgreen")
  )
