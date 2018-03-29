library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
data=read.csv("D:/R/eddypro.csv", skip=1, na=c("","NA","-9999","-9999.0"),comment=c("["))
##Удаляем первую строку таблицы
data = data[-1,]
data
## Просмотрим все переменные
glimpse(data)
## Убираем ненужную переменную roll
data = select(data, -(roll))
## Оставляем только весенние месяцы
filter(data, DOY>90 & DOY<150)
## Преобразуем в факторы переменные типа char, которые содержат повторяющиеся значения:
data = data %>% mutate_if(is.character, factor)
## Заменяем ненужные символы для упрощения работы с данными
library(stringr)
names(data) =  str_replace_all(names(data), "[!]","_emph_")
names(data) = names(data) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(data)
## Оставляем только численные данные 
data_numeric = data[,sapply(data,is.numeric) ]
## Создаём обучающую и тестовую выборки
row_numbers = 1:length(data$date)
teach = sample(row_numbers, floor(length(data$date)*.7))
test = row_numbers[-teach]
teaching_data_unq = spring.data[teach,]
testing_data_unq = spring.data[test,]
#Корелляционный анализ
cor_td = cor(data_numeric)
cor_td
## Избавляемся от всех строк, где есть хоть одно значение NA
cor_td = cor(drop_na(data_numeric))
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .2] %>% na.exclude
vars
##  Собираем все переменные из вектора с именнами переменных в одну формулу
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
##Собственно линейная модель
my.model = lm(formula, data=data)
my.model
summary(my.model)
anova(my.model)
my.model1 = lm(h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                             rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                             h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                             air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                             un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + v_var + 
                             w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + flowrate)^2,
               data = data)
my.model1
summary(my.model1)
anova(my.model1)
my.model2 = lm(h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                             rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                             h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                             air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                             un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + v_var + 
                             w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + flowrate)^2 -
                 Tau:rand_err_Tau - Tau:H - Tau:rand_err_H - Tau:LE - Tau:qc_LE - Tau:rand_err_LE -
                 Tau:co2_flux - h2o_flux:Tau - Tau:qc_h2o_flux - Tau:rand_err_h2o_flux - 
                 Tau:h2o_time_lag - Tau:sonic_temperature - Tau:air_density - Tau:air_molar_volume -
                 Tau:es - Tau:RH - Tau:VPD - Tau:u. - Tau:TKE - Tau:T. - Tau:un_Tau - Tau:un_H -
                 Tau:un_LE - Tau:un_co2_flux - Tau:un_h2o_flux - Tau:u_var - Tau:v_var -
                 Tau:w_var - Tau:h2o_var - Tau:w.ts_cov - Tau:w.co2_cov - Tau:w.h2o_cov - 
                 Tau:flowrate - rand_err_Tau:H - rand_err_Tau:rand_err_H - rand_err_Tau:LE -
                 rand_err_Tau:qc_LE - rand_err_Tau:rand_err_LE - rand_err_Tau:co2_flux - 
                 h2o_flux:rand_err_Tau - rand_err_Tau:qc_h2o_flux - rand_err_Tau:h2o_time_lag - 
                 rand_err_Tau:sonic_temperature - rand_err_Tau:air_temperature - 
                 rand_err_Tau:air_density - rand_err_Tau:air_molar_volume - rand_err_Tau:es - 
                 rand_err_Tau:RH - rand_err_Tau:VPD - rand_err_Tau:u. - rand_err_Tau:TKE -
                 rand_err_Tau:T. - rand_err_Tau:un_Tau - rand_err_Tau:un_H - rand_err_Tau:un_LE -
                 rand_err_Tau:un_co2_flux - rand_err_Tau:un_h2o_flux - rand_err_Tau:u_var -
                 rand_err_Tau:v_var - rand_err_Tau:w_var - rand_err_Tau:h2o_var - 
                 rand_err_Tau:w.ts_cov - rand_err_Tau:w.co2_cov - rand_err_Tau:w.h2o_cov - 
                 rand_err_Tau:flowrate - H:rand_err_H - H:LE - H:qc_LE - H:rand_err_LE - 
                 H:co2_flux - h2o_flux:H - H:qc_h2o_flux - H:rand_err_h2o_flux - H:h2o_time_lag -
                 H:sonic_temperature - H:air_density - H:air_molar_volume - H:es - H:RH - H:VPD -
                 H:u. - H:TKE - H:T. - H:un_Tau - H:un_H - H:un_LE - H:un_co2_flux - 
                 H:un_h2o_flux - H:u_var - H:v_var - H:w_var - H:h2o_var - H:w.ts_cov -
                 H:w.co2_cov - H:w.h2o_cov - H:flowrate - rand_err_H:LE - rand_err_H:qc_LE - 
                 rand_err_H:co2_flux - h2o_flux:rand_err_H - rand_err_H:qc_h2o_flux -
                 rand_err_H:h2o_time_lag-rand_err_H:sonic_temperature-rand_err_H:air_temperature-
                 rand_err_H:u.-rand_err_H:T.-rand_err_H:un_Tau - rand_err_H:un_H-
                 rand_err_H:un_LE -rand_err_H:un_co2_flux-rand_err_H:un_h2o_flux-
                 rand_err_H:w_var -rand_err_H:h2o_var-rand_err_H:w.ts_cov-
                 rand_err_H:w.co2_cov-rand_err_H:flowrate-LE:qc_LE-LE:rand_err_LE-
                 LE:co2_flux-h2o_flux:LE-LE:qc_h2o_flux-LE:rand_err_h2o_flux-
                 LE:h2o_time_lag-LE:sonic_temperature-LE:air_temperature-LE:air_density-
                 LE:air_molar_volume-LE:es-LE:RH-LE:VPD -LE:u.-LE:TKE-LE:T.-
                 LE:un_Tau-LE:un_H -LE:un_LE-LE:un_co2_flux-LE:un_h2o_flux-
                 LE:u_var-LE:v_var-LE:w_var-LE:h2o_var-LE:w.ts_cov-LE:w.co2_cov-
                 LE:w.h2o_cov-LE:flowrate-qc_LE:rand_err_LE-qc_LE:co2_flux-
                 h2o_flux:qc_LE-qc_LE:qc_h2o_flux-qc_LE:rand_err_h2o_flux -
                 qc_LE:h2o_time_lag-qc_LE:sonic_temperature-qc_LE:air_temperature-
                 qc_LE:air_density -qc_LE:air_molar_volume-qc_LE:es-
                 qc_LE:RH-qc_LE:VPD-qc_LE:u.-qc_LE:TKE-qc_LE:un_Tau-qc_LE:un_H-
                 qc_LE:un_LE-qc_LE:un_co2_flux-qc_LE:un_h2o_flux-qc_LE:u_var-
                 qc_LE:v_var-qc_LE:w_var-qc_LE:h2o_var-qc_LE:w.ts_cov-
                 qc_LE:w.co2_cov-qc_LE:w.h2o_cov-qc_LE:flowrate-rand_err_LE:co2_flux-
                 rand_err_LE:h2o_time_lag-rand_err_LE:sonic_temperature-
                 rand_err_LE:u.-rand_err_LE:TKE-rand_err_LE:T.-rand_err_LE:un_Tau -
                 rand_err_LE:un_H-rand_err_LE:un_LE-rand_err_LE:un_co2_flux-
                 rand_err_LE:un_h2o_flux -rand_err_LE:u_var-rand_err_LE:v_var-
                 rand_err_LE:w_var-rand_err_LE:h2o_var-rand_err_LE:w.ts_cov-
                 rand_err_LE:w.co2_cov-rand_err_LE:w.h2o_cov-rand_err_LE:flowrate-
                 h2o_flux:co2_flux-co2_flux:qc_h2o_flux-co2_flux:rand_err_h2o_flux-
                 co2_flux:h2o_time_lag-co2_flux:sonic_temperature-
                 co2_flux:air_temperature-co2_flux:air_density-co2_flux:air_molar_volume-
                 co2_flux:es-co2_flux:RH-co2_flux:VPD-co2_flux:u.-co2_flux:TKE-
                 co2_flux:T.-co2_flux:un_Tau-co2_flux:un_H-co2_flux:un_LE-
                 co2_flux:un_co2_flux-co2_flux:un_h2o_flux-co2_flux:u_var-
                 co2_flux:v_var-co2_flux:w_var-co2_flux:h2o_var-co2_flux:w.ts_cov-
                 co2_flux:w.co2_cov-co2_flux:w.h2o_cov-co2_flux:flowrate-
                 h2o_flux:qc_h2o_flux-h2o_flux:rand_err_h2o_flux-h2o_flux:h2o_time_lag-
                 h2o_flux:sonic_temperature-h2o_flux:air_density-h2o_flux:air_molar_volume-
                 h2o_flux:es-h2o_flux:RH-h2o_flux:VPD-h2o_flux:u.-h2o_flux:TKE-
                 h2o_flux:T.-h2o_flux:un_Tau-h2o_flux:un_H-h2o_flux:un_LE-
                 h2o_flux:un_co2_flux-h2o_flux:un_h2o_flux-h2o_flux:u_var-
                 h2o_flux:v_var-h2o_flux:w_var-h2o_flux:h2o_var-
                 h2o_flux:w.ts_cov-h2o_flux:w.co2_cov-h2o_flux:w.h2o_cov-h2o_flux:flowrate-
                 qc_h2o_flux:rand_err_h2o_flux-qc_h2o_flux:h2o_time_lag-
                 qc_h2o_flux:sonic_temperature-qc_h2o_flux:air_temperature-
                 qc_h2o_flux:air_density-qc_h2o_flux:air_molar_volume-
                 qc_h2o_flux:es-qc_h2o_flux:RH-qc_h2o_flux:VPD-qc_h2o_flux:u.-
                 qc_h2o_flux:TKE-qc_h2o_flux:T.-qc_h2o_flux:un_Tau-qc_h2o_flux:un_H-
                 qc_h2o_flux:un_LE-qc_h2o_flux:un_co2_flux-qc_h2o_flux:un_h2o_flux-
                 qc_h2o_flux:u_var-qc_h2o_flux:v_var-qc_h2o_flux:w_var-
                 qc_h2o_flux:h2o_var-qc_h2o_flux:w.ts_cov-qc_h2o_flux:w.co2_cov-
                 qc_h2o_flux:w.h2o_cov-qc_h2o_flux:flowrate -rand_err_h2o_flux:h2o_time_lag-
                 rand_err_h2o_flux:sonic_temperature-rand_err_h2o_flux:air_temperature-
                 rand_err_h2o_flux:u.-rand_err_h2o_flux:TKE-rand_err_h2o_flux:T.-
                 rand_err_h2o_flux:un_Tau-rand_err_h2o_flux:un_H-
                 rand_err_h2o_flux:un_LE-rand_err_h2o_flux:un_co2_flux-
                 rand_err_h2o_flux:un_h2o_flux-rand_err_h2o_flux:u_var -
                 rand_err_h2o_flux:v_var-rand_err_h2o_flux:w_var-rand_err_h2o_flux:h2o_var-
                 rand_err_h2o_flux:w.ts_cov-rand_err_h2o_flux:w.co2_cov-
                 rand_err_h2o_flux:w.h2o_cov-rand_err_h2o_flux:flowrate -
                 h2o_time_lag:sonic_temperature-h2o_time_lag:air_temperature-
                 h2o_time_lag:air_density-h2o_time_lag:air_molar_volume-
                 h2o_time_lag:es-h2o_time_lag:RH-h2o_time_lag:VPD-h2o_time_lag:u.-
                 h2o_time_lag:TKE-h2o_time_lag:T.-h2o_time_lag:un_Tau-
                 h2o_time_lag:un_H -h2o_time_lag:un_LE-h2o_time_lag:un_co2_flux-
                 h2o_time_lag:un_h2o_flux-h2o_time_lag:u_var-h2o_time_lag:v_var-
                 h2o_time_lag:w_var-h2o_time_lag:h2o_var-h2o_time_lag:w.ts_cov -
                 h2o_time_lag:w.co2_cov-h2o_time_lag:w.h2o_cov-h2o_time_lag:flowrate-
                 sonic_temperature:air_temperature-sonic_temperature:air_density-
                 sonic_temperature:air_molar_volume-sonic_temperature:es-
                 sonic_temperature:RH-sonic_temperature:VPD-sonic_temperature:u.-
                 sonic_temperature:TKE-sonic_temperature:T.-sonic_temperature:un_Tau-
                 sonic_temperature:un_H-sonic_temperature:un_LE-sonic_temperature:un_co2_flux-
                 sonic_temperature:un_h2o_flux-sonic_temperature:u_var-
                 sonic_temperature:v_var-sonic_temperature:w_var-sonic_temperature:h2o_var-
                 sonic_temperature:w.ts_cov-sonic_temperature:w.co2_cov-sonic_temperature:w.h2o_cov-
                 sonic_temperature:flowrate-air_temperature:air_density-
                 air_temperature:air_molar_volume-air_temperature:es-air_temperature:RH-
                 air_temperature:VPD-air_temperature:u.-air_temperature:TKE-
                 air_temperature:T.-air_temperature:un_Tau-air_temperature:un_H -
                 air_temperature:un_LE-air_temperature:un_co2_flux-air_temperature:un_h2o_flux-
                 air_temperature:u_var-air_temperature:v_var-air_temperature:w_var-
                 air_temperature:h2o_var-air_temperature:w.ts_cov-air_temperature:w.co2_cov-
                 air_temperature:w.h2o_cov-air_temperature:flowrate-air_density:air_molar_volume -
                 air_density:es-air_density:RH-air_density:VPD-air_density:u.-
                 air_density:T.-air_density:un_Tau-air_density:un_H-air_density:un_LE-
                 air_density:un_co2_flux-air_density:un_h2o_flux-air_density:w_var-
                 air_density:h2o_var -air_density:w.co2_cov -air_density:w.h2o_cov-
                 air_density:flowrate-air_molar_volume:es-air_molar_volume:RH-
                 air_molar_volume:VPD-air_molar_volume:u.-air_molar_volume:T.-
                 air_molar_volume:un_Tau -air_molar_volume:un_H-air_molar_volume:un_LE-
                 air_molar_volume:un_co2_flux-air_molar_volume:un_h2o_flux-
                 air_molar_volume:w_var-air_molar_volume:h2o_var-
                 air_molar_volume:w.ts_cov-air_molar_volume:w.co2_cov-
                 air_molar_volume:w.h2o_cov-air_molar_volume:flowrate-es:RH-es:VPD-
                 es:u.-es:T.-es:un_Tau-es:un_H-es:un_LE-es:un_co2_flux-es:un_h2o_flux-
                 es:w_var-es:h2o_var-es:w.ts_cov-es:w.co2_cov-es:w.h2o_cov-es:flowrate-
                 RH:VPD-RH:u.-RH:T.-RH:un_Tau-RH:un_H-RH:un_LE-RH:un_co2_flux-RH:un_h2o_flux-
                 RH:w_var-RH:h2o_var-RH:w.co2_cov-RH:w.h2o_cov-RH:flowrate-VPD:u.-VPD:T.-
                 VPD:un_Tau-VPD:un_H-VPD:un_LE-VPD:un_co2_flux-VPD:un_h2o_flux-VPD:w_var-
                 VPD:h2o_var-VPD:w.ts_cov-VPD:w.co2_cov-VPD:w.h2o_cov-VPD:flowrate-
                 u.:TKE-u.:T.-u.:un_Tau-u.:un_H-u.:un_LE-u.:un_co2_flux-u.:un_h2o_flux-
                 u.:flowrate-TKE:un_Tau-TKE:un_H-TKE:un_LE-TKE:un_co2_flux-TKE:un_h2o_flux-
                 TKE:u_var-TKE:v_var-TKE:w_var-TKE:h2o_var-TKE:w.co2_cov-TKE:w.h2o_cov-
                 TKE:flowrate-T.:un_Tau-T.:un_H-T.:un_LE-T.:un_co2_flux-T.:un_h2o_flux-
                 T.:u_var-T.:v_var-T.:w_var-T.:w_var-T.:h2o_var-T.:w.ts_cov-T.:w.co2_cov-
                 T.:w.h2o_cov-T.:flowrate-un_Tau:un_H-un_Tau:un_LE-un_Tau:un_co2_flux-
                 un_Tau:un_h2o_flux-un_Tau:u_var-un_Tau:v_var-un_Tau:w_var-un_Tau:h2o_var-
                 un_Tau:w.ts_cov-un_Tau:w.co2_cov-un_Tau:w.h2o_cov-un_Tau:flowrate-
                 un_H:un_LE-un_H:un_co2_flux-un_H:un_h2o_flux-un_H:u_var-un_H:v_var-
                 un_H:w_var-un_H:h2o_var-un_H:w.ts_cov-un_H:w.co2_cov-un_H:w.h2o_cov-
                 un_H:flowrate-un_LE:un_co2_flux-un_LE:un_h2o_flux-un_LE:u_var-un_LE:v_var-
                 un_LE:w_var-un_LE:h2o_var-un_LE:w.ts_cov-un_LE:w.co2_cov-un_LE:w.h2o_cov-
                 un_LE:flowrate-un_co2_flux:un_h2o_flux-un_co2_flux:u_var-un_co2_flux:v_var-
                 un_co2_flux:w_var-un_co2_flux:h2o_var-un_co2_flux:w.ts_cov-un_co2_flux:w.co2_cov-
                 un_co2_flux:w.h2o_cov-un_co2_flux:flowrate-un_h2o_flux:u_var-un_h2o_flux:v_var-
                 un_h2o_flux:w_var-un_h2o_flux:h2o_var-un_h2o_flux:w.ts_cov-un_h2o_flux:w.co2_cov-
                 un_h2o_flux:w.h2o_cov-un_h2o_flux:flowrate-u_var:v_var-u_var:w_var-
                 u_var:h2o_var-u_var:w.co2_cov-u_var:w.h2o_cov-u_var:flowrate-v_var:w_var-
                 v_var:h2o_var-v_var:w.co2_cov-v_var:w.h2o_cov-v_var:flowrate-w_var:h2o_var-
                 w_var:w.ts_cov-w_var:w.co2_cov-w_var:w.h2o_cov-w_var:flowrate-w.ts_cov:w.co2_cov-
                 w.ts_cov:w.h2o_cov-w.ts_cov:flowrate-w.co2_cov:w.h2o_cov-w.co2_cov:flowrate-
                 w.h2o_cov:flowrate,data = data)
                 
                           
summary(my.model2)
anova(my.model2)
my.model3 = lm(h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                             rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                             h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                             air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                             un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + v_var + 
                             w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + flowrate)^2 -
                 Tau:rand_err_Tau - Tau:H - Tau:rand_err_H - Tau:LE - Tau:qc_LE - Tau:rand_err_LE -
                 Tau:co2_flux - h2o_flux:Tau - Tau:qc_h2o_flux - Tau:rand_err_h2o_flux - 
                 Tau:h2o_time_lag - Tau:sonic_temperature - Tau:air_density - Tau:air_molar_volume -
                 Tau:es - Tau:RH - Tau:VPD - Tau:u. - Tau:TKE - Tau:T. - Tau:un_Tau - Tau:un_H -
                 Tau:un_LE - Tau:un_co2_flux - Tau:un_h2o_flux - Tau:u_var - Tau:v_var -
                 Tau:w_var - Tau:h2o_var - Tau:w.ts_cov - Tau:w.co2_cov - Tau:w.h2o_cov - 
                 Tau:flowrate - rand_err_Tau:H - rand_err_Tau:rand_err_H - rand_err_Tau:LE -
                 rand_err_Tau:qc_LE - rand_err_Tau:rand_err_LE - rand_err_Tau:co2_flux - 
                 h2o_flux:rand_err_Tau - rand_err_Tau:qc_h2o_flux - rand_err_Tau:h2o_time_lag - 
                 rand_err_Tau:sonic_temperature - rand_err_Tau:air_temperature - 
                 rand_err_Tau:air_density - rand_err_Tau:air_molar_volume - rand_err_Tau:es - 
                 rand_err_Tau:RH - rand_err_Tau:VPD - rand_err_Tau:u. - rand_err_Tau:TKE -
                 rand_err_Tau:T. - rand_err_Tau:un_Tau - rand_err_Tau:un_H - rand_err_Tau:un_LE -
                 rand_err_Tau:un_co2_flux - rand_err_Tau:un_h2o_flux - rand_err_Tau:u_var -
                 rand_err_Tau:v_var - rand_err_Tau:w_var - rand_err_Tau:h2o_var - 
                 rand_err_Tau:w.ts_cov - rand_err_Tau:w.co2_cov - rand_err_Tau:w.h2o_cov - 
                 rand_err_Tau:flowrate - H:rand_err_H - H:LE - H:qc_LE - H:rand_err_LE - 
                 H:co2_flux - h2o_flux:H - H:qc_h2o_flux - H:rand_err_h2o_flux - H:h2o_time_lag -
                 H:sonic_temperature - H:air_density - H:air_molar_volume - H:es - H:RH - H:VPD -
                 H:u. - H:TKE - H:T. - H:un_Tau - H:un_H - H:un_LE - H:un_co2_flux - 
                 H:un_h2o_flux - H:u_var - H:v_var - H:w_var - H:h2o_var - H:w.ts_cov -
                 H:w.co2_cov - H:w.h2o_cov - H:flowrate - rand_err_H:LE - rand_err_H:qc_LE - 
                 rand_err_H:co2_flux - h2o_flux:rand_err_H - rand_err_H:qc_h2o_flux -
                 rand_err_H:h2o_time_lag-rand_err_H:sonic_temperature-rand_err_H:air_temperature-
                 rand_err_H:u.-rand_err_H:T.-rand_err_H:un_Tau - rand_err_H:un_H-
                 rand_err_H:un_LE -rand_err_H:un_co2_flux-rand_err_H:un_h2o_flux-
                 rand_err_H:w_var -rand_err_H:h2o_var-rand_err_H:w.ts_cov-
                 rand_err_H:w.co2_cov-rand_err_H:flowrate-LE:qc_LE-LE:rand_err_LE-
                 LE:co2_flux-h2o_flux:LE-LE:qc_h2o_flux-LE:rand_err_h2o_flux-
                 LE:h2o_time_lag-LE:sonic_temperature-LE:air_temperature-LE:air_density-
                 LE:air_molar_volume-LE:es-LE:RH-LE:VPD -LE:u.-LE:TKE-LE:T.-
                 LE:un_Tau-LE:un_H -LE:un_LE-LE:un_co2_flux-LE:un_h2o_flux-
                 LE:u_var-LE:v_var-LE:w_var-LE:h2o_var-LE:w.ts_cov-LE:w.co2_cov-
                 LE:w.h2o_cov-LE:flowrate-qc_LE:rand_err_LE-qc_LE:co2_flux-
                 h2o_flux:qc_LE-qc_LE:qc_h2o_flux-qc_LE:rand_err_h2o_flux -
                 qc_LE:h2o_time_lag-qc_LE:sonic_temperature-qc_LE:air_temperature-
                 qc_LE:air_density -qc_LE:air_molar_volume-qc_LE:es-
                 qc_LE:RH-qc_LE:VPD-qc_LE:u.-qc_LE:TKE-qc_LE:un_Tau-qc_LE:un_H-
                 qc_LE:un_LE-qc_LE:un_co2_flux-qc_LE:un_h2o_flux-qc_LE:u_var-
                 qc_LE:v_var-qc_LE:w_var-qc_LE:h2o_var-qc_LE:w.ts_cov-
                 qc_LE:w.co2_cov-qc_LE:w.h2o_cov-qc_LE:flowrate-rand_err_LE:co2_flux-
                 rand_err_LE:h2o_time_lag-rand_err_LE:sonic_temperature-
                 rand_err_LE:u.-rand_err_LE:TKE-rand_err_LE:T.-rand_err_LE:un_Tau -
                 rand_err_LE:un_H-rand_err_LE:un_LE-rand_err_LE:un_co2_flux-
                 rand_err_LE:un_h2o_flux -rand_err_LE:u_var-rand_err_LE:v_var-
                 rand_err_LE:w_var-rand_err_LE:h2o_var-rand_err_LE:w.ts_cov-
                 rand_err_LE:w.co2_cov-rand_err_LE:w.h2o_cov-rand_err_LE:flowrate-
                 h2o_flux:co2_flux-co2_flux:qc_h2o_flux-co2_flux:rand_err_h2o_flux-
                 co2_flux:h2o_time_lag-co2_flux:sonic_temperature-
                 co2_flux:air_temperature-co2_flux:air_density-co2_flux:air_molar_volume-
                 co2_flux:es-co2_flux:RH-co2_flux:VPD-co2_flux:u.-co2_flux:TKE-
                 co2_flux:T.-co2_flux:un_Tau-co2_flux:un_H-co2_flux:un_LE-
                 co2_flux:un_co2_flux-co2_flux:un_h2o_flux-co2_flux:u_var-
                 co2_flux:v_var-co2_flux:w_var-co2_flux:h2o_var-co2_flux:w.ts_cov-
                 co2_flux:w.co2_cov-co2_flux:w.h2o_cov-co2_flux:flowrate-
                 h2o_flux:qc_h2o_flux-h2o_flux:rand_err_h2o_flux-h2o_flux:h2o_time_lag-
                 h2o_flux:sonic_temperature-h2o_flux:air_density-h2o_flux:air_molar_volume-
                 h2o_flux:es-h2o_flux:RH-h2o_flux:VPD-h2o_flux:u.-h2o_flux:TKE-
                 h2o_flux:T.-h2o_flux:un_Tau-h2o_flux:un_H-h2o_flux:un_LE-
                 h2o_flux:un_co2_flux-h2o_flux:un_h2o_flux-h2o_flux:u_var-
                 h2o_flux:v_var-h2o_flux:w_var-h2o_flux:h2o_var-
                 h2o_flux:w.ts_cov-h2o_flux:w.co2_cov-h2o_flux:w.h2o_cov-h2o_flux:flowrate-
                 qc_h2o_flux:rand_err_h2o_flux-qc_h2o_flux:h2o_time_lag-
                 qc_h2o_flux:sonic_temperature-qc_h2o_flux:air_temperature-
                 qc_h2o_flux:air_density-qc_h2o_flux:air_molar_volume-
                 qc_h2o_flux:es-qc_h2o_flux:RH-qc_h2o_flux:VPD-qc_h2o_flux:u.-
                 qc_h2o_flux:TKE-qc_h2o_flux:T.-qc_h2o_flux:un_Tau-qc_h2o_flux:un_H-
                 qc_h2o_flux:un_LE-qc_h2o_flux:un_co2_flux-qc_h2o_flux:un_h2o_flux-
                 qc_h2o_flux:u_var-qc_h2o_flux:v_var-qc_h2o_flux:w_var-
                 qc_h2o_flux:h2o_var-qc_h2o_flux:w.ts_cov-qc_h2o_flux:w.co2_cov-
                 qc_h2o_flux:w.h2o_cov-qc_h2o_flux:flowrate -rand_err_h2o_flux:h2o_time_lag-
                 rand_err_h2o_flux:sonic_temperature-rand_err_h2o_flux:air_temperature-
                 rand_err_h2o_flux:u.-rand_err_h2o_flux:TKE-rand_err_h2o_flux:T.-
                 rand_err_h2o_flux:un_Tau-rand_err_h2o_flux:un_H-
                 rand_err_h2o_flux:un_LE-rand_err_h2o_flux:un_co2_flux-
                 rand_err_h2o_flux:un_h2o_flux-rand_err_h2o_flux:u_var -
                 rand_err_h2o_flux:v_var-rand_err_h2o_flux:w_var-rand_err_h2o_flux:h2o_var-
                 rand_err_h2o_flux:w.ts_cov-rand_err_h2o_flux:w.co2_cov-
                 rand_err_h2o_flux:w.h2o_cov-rand_err_h2o_flux:flowrate -
                 h2o_time_lag:sonic_temperature-h2o_time_lag:air_temperature-
                 h2o_time_lag:air_density-h2o_time_lag:air_molar_volume-
                 h2o_time_lag:es-h2o_time_lag:RH-h2o_time_lag:VPD-h2o_time_lag:u.-
                 h2o_time_lag:TKE-h2o_time_lag:T.-h2o_time_lag:un_Tau-
                 h2o_time_lag:un_H -h2o_time_lag:un_LE-h2o_time_lag:un_co2_flux-
                 h2o_time_lag:un_h2o_flux-h2o_time_lag:u_var-h2o_time_lag:v_var-
                 h2o_time_lag:w_var-h2o_time_lag:h2o_var-h2o_time_lag:w.ts_cov -
                 h2o_time_lag:w.co2_cov-h2o_time_lag:w.h2o_cov-h2o_time_lag:flowrate-
                 sonic_temperature:air_temperature-sonic_temperature:air_density-
                 sonic_temperature:air_molar_volume-sonic_temperature:es-
                 sonic_temperature:RH-sonic_temperature:VPD-sonic_temperature:u.-
                 sonic_temperature:TKE-sonic_temperature:T.-sonic_temperature:un_Tau-
                 sonic_temperature:un_H-sonic_temperature:un_LE-sonic_temperature:un_co2_flux-
                 sonic_temperature:un_h2o_flux-sonic_temperature:u_var-
                 sonic_temperature:v_var-sonic_temperature:w_var-sonic_temperature:h2o_var-
                 sonic_temperature:w.ts_cov-sonic_temperature:w.co2_cov-sonic_temperature:w.h2o_cov-
                 sonic_temperature:flowrate-air_temperature:air_density-
                 air_temperature:air_molar_volume-air_temperature:es-air_temperature:RH-
                 air_temperature:VPD-air_temperature:u.-air_temperature:TKE-
                 air_temperature:T.-air_temperature:un_Tau-air_temperature:un_H -
                 air_temperature:un_LE-air_temperature:un_co2_flux-air_temperature:un_h2o_flux-
                 air_temperature:u_var-air_temperature:v_var-air_temperature:w_var-
                 air_temperature:h2o_var-air_temperature:w.ts_cov-air_temperature:w.co2_cov-
                 air_temperature:w.h2o_cov-air_temperature:flowrate-air_density:air_molar_volume -
                 air_density:es-air_density:RH-air_density:VPD-air_density:u.-
                 air_density:T.-air_density:un_Tau-air_density:un_H-air_density:un_LE-
                 air_density:un_co2_flux-air_density:un_h2o_flux-air_density:w_var-
                 air_density:h2o_var -air_density:w.co2_cov -air_density:w.h2o_cov-
                 air_density:flowrate-air_molar_volume:es-air_molar_volume:RH-
                 air_molar_volume:VPD-air_molar_volume:u.-air_molar_volume:T.-
                 air_molar_volume:un_Tau -air_molar_volume:un_H-air_molar_volume:un_LE-
                 air_molar_volume:un_co2_flux-air_molar_volume:un_h2o_flux-
                 air_molar_volume:w_var-air_molar_volume:h2o_var-
                 air_molar_volume:w.ts_cov-air_molar_volume:w.co2_cov-
                 air_molar_volume:w.h2o_cov-air_molar_volume:flowrate-es:RH-es:VPD-
                 es:u.-es:T.-es:un_Tau-es:un_H-es:un_LE-es:un_co2_flux-es:un_h2o_flux-
                 es:w_var-es:h2o_var-es:w.ts_cov-es:w.co2_cov-es:w.h2o_cov-es:flowrate-
                 RH:VPD-RH:u.-RH:T.-RH:un_Tau-RH:un_H-RH:un_LE-RH:un_co2_flux-RH:un_h2o_flux-
                 RH:w_var-RH:h2o_var-RH:w.co2_cov-RH:w.h2o_cov-RH:flowrate-VPD:u.-VPD:T.-
                 VPD:un_Tau-VPD:un_H-VPD:un_LE-VPD:un_co2_flux-VPD:un_h2o_flux-VPD:w_var-
                 VPD:h2o_var-VPD:w.ts_cov-VPD:w.co2_cov-VPD:w.h2o_cov-VPD:flowrate-
                 u.:TKE-u.:T.-u.:un_Tau-u.:un_H-u.:un_LE-u.:un_co2_flux-u.:un_h2o_flux-
                 u.:flowrate-TKE:un_Tau-TKE:un_H-TKE:un_LE-TKE:un_co2_flux-TKE:un_h2o_flux-
                 TKE:u_var-TKE:v_var-TKE:w_var-TKE:h2o_var-TKE:w.co2_cov-TKE:w.h2o_cov-
                 TKE:flowrate-T.:un_Tau-T.:un_H-T.:un_LE-T.:un_co2_flux-T.:un_h2o_flux-
                 T.:u_var-T.:v_var-T.:w_var-T.:w_var-T.:h2o_var-T.:w.ts_cov-T.:w.co2_cov-
                 T.:w.h2o_cov-T.:flowrate-un_Tau:un_H-un_Tau:un_LE-un_Tau:un_co2_flux-
                 un_Tau:un_h2o_flux-un_Tau:u_var-un_Tau:v_var-un_Tau:w_var-un_Tau:h2o_var-
                 un_Tau:w.ts_cov-un_Tau:w.co2_cov-un_Tau:w.h2o_cov-un_Tau:flowrate-
                 un_H:un_LE-un_H:un_co2_flux-un_H:un_h2o_flux-un_H:u_var-un_H:v_var-
                 un_H:w_var-un_H:h2o_var-un_H:w.ts_cov-un_H:w.co2_cov-un_H:w.h2o_cov-
                 un_H:flowrate-un_LE:un_co2_flux-un_LE:un_h2o_flux-un_LE:u_var-un_LE:v_var-
                 un_LE:w_var-un_LE:h2o_var-un_LE:w.ts_cov-un_LE:w.co2_cov-un_LE:w.h2o_cov-
                 un_LE:flowrate-un_co2_flux:un_h2o_flux-un_co2_flux:u_var-un_co2_flux:v_var-
                 un_co2_flux:w_var-un_co2_flux:h2o_var-un_co2_flux:w.ts_cov-un_co2_flux:w.co2_cov-
                 un_co2_flux:w.h2o_cov-un_co2_flux:flowrate-un_h2o_flux:u_var-un_h2o_flux:v_var-
                 un_h2o_flux:w_var-un_h2o_flux:h2o_var-un_h2o_flux:w.ts_cov-un_h2o_flux:w.co2_cov-
                 un_h2o_flux:w.h2o_cov-un_h2o_flux:flowrate-u_var:v_var-u_var:w_var-
                 u_var:h2o_var-u_var:w.co2_cov-u_var:w.h2o_cov-u_var:flowrate-v_var:w_var-
                 v_var:h2o_var-v_var:w.co2_cov-v_var:w.h2o_cov-v_var:flowrate-w_var:h2o_var-
                 w_var:w.ts_cov-w_var:w.co2_cov-w_var:w.h2o_cov-w_var:flowrate-w.ts_cov:w.co2_cov-
                 w.ts_cov:w.h2o_cov-w.ts_cov:flowrate-w.co2_cov:w.h2o_cov-w.co2_cov:flowrate-
                 w.h2o_cov:flowrate-
                 rand_err_h2o_flux:air_density-rand_err_h2o_flux:air_molar_volume-
                 rand_err_h2o_flux:es-rand_err_h2o_flux:RH-air_density:TKE-
                 air_density:u_var-air_density:v_var-air_molar_volume:TKE-
                 air_molar_volume:u_var-air_molar_volume:v_var-es:TKE-es:u_var-
                 RH:v_var-VPD:TKE-VPD:u_var-VPD:v_var-u.:u_var-u.:v_var-u.:w_var-
                 u.:h2o_var-u.:w.ts_cov-u.:w.co2_cov-u.:w.h2o_cov-TKE:T.-TKE:w.ts_cov-
                 u_var:w.ts_cov-v_var:w.ts_cov-h2o_var:w.ts_cov-h2o_var:w.co2_cov-
                 h2o_var:w.h2o_cov-h2o_var:flowrate,data = data)
summary(my.model3)
anova(my.model3)
my.model4 = lm(h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                             rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                             h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                             air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                             un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + v_var + 
                             w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + flowrate)^2 -
                 Tau:rand_err_Tau - Tau:H - Tau:rand_err_H - Tau:LE - Tau:qc_LE - Tau:rand_err_LE -
                 Tau:co2_flux - h2o_flux:Tau - Tau:qc_h2o_flux - Tau:rand_err_h2o_flux - 
                 Tau:h2o_time_lag - Tau:sonic_temperature - Tau:air_density - Tau:air_molar_volume -
                 Tau:es - Tau:RH - Tau:VPD - Tau:u. - Tau:TKE - Tau:T. - Tau:un_Tau - Tau:un_H -
                 Tau:un_LE - Tau:un_co2_flux - Tau:un_h2o_flux - Tau:u_var - Tau:v_var -
                 Tau:w_var - Tau:h2o_var - Tau:w.ts_cov - Tau:w.co2_cov - Tau:w.h2o_cov - 
                 Tau:flowrate - rand_err_Tau:H - rand_err_Tau:rand_err_H - rand_err_Tau:LE -
                 rand_err_Tau:qc_LE - rand_err_Tau:rand_err_LE - rand_err_Tau:co2_flux - 
                 h2o_flux:rand_err_Tau - rand_err_Tau:qc_h2o_flux - rand_err_Tau:h2o_time_lag - 
                 rand_err_Tau:sonic_temperature - rand_err_Tau:air_temperature - 
                 rand_err_Tau:air_density - rand_err_Tau:air_molar_volume - rand_err_Tau:es - 
                 rand_err_Tau:RH - rand_err_Tau:VPD - rand_err_Tau:u. - rand_err_Tau:TKE -
                 rand_err_Tau:T. - rand_err_Tau:un_Tau - rand_err_Tau:un_H - rand_err_Tau:un_LE -
                 rand_err_Tau:un_co2_flux - rand_err_Tau:un_h2o_flux - rand_err_Tau:u_var -
                 rand_err_Tau:v_var - rand_err_Tau:w_var - rand_err_Tau:h2o_var - 
                 rand_err_Tau:w.ts_cov - rand_err_Tau:w.co2_cov - rand_err_Tau:w.h2o_cov - 
                 rand_err_Tau:flowrate - H:rand_err_H - H:LE - H:qc_LE - H:rand_err_LE - 
                 H:co2_flux - h2o_flux:H - H:qc_h2o_flux - H:rand_err_h2o_flux - H:h2o_time_lag -
                 H:sonic_temperature - H:air_density - H:air_molar_volume - H:es - H:RH - H:VPD -
                 H:u. - H:TKE - H:T. - H:un_Tau - H:un_H - H:un_LE - H:un_co2_flux - 
                 H:un_h2o_flux - H:u_var - H:v_var - H:w_var - H:h2o_var - H:w.ts_cov -
                 H:w.co2_cov - H:w.h2o_cov - H:flowrate - rand_err_H:LE - rand_err_H:qc_LE - 
                 rand_err_H:co2_flux - h2o_flux:rand_err_H - rand_err_H:qc_h2o_flux -
                 rand_err_H:h2o_time_lag-rand_err_H:sonic_temperature-rand_err_H:air_temperature-
                 rand_err_H:u.-rand_err_H:T.-rand_err_H:un_Tau - rand_err_H:un_H-
                 rand_err_H:un_LE -rand_err_H:un_co2_flux-rand_err_H:un_h2o_flux-
                 rand_err_H:w_var -rand_err_H:h2o_var-rand_err_H:w.ts_cov-
                 rand_err_H:w.co2_cov-rand_err_H:flowrate-LE:qc_LE-LE:rand_err_LE-
                 LE:co2_flux-h2o_flux:LE-LE:qc_h2o_flux-LE:rand_err_h2o_flux-
                 LE:h2o_time_lag-LE:sonic_temperature-LE:air_temperature-LE:air_density-
                 LE:air_molar_volume-LE:es-LE:RH-LE:VPD -LE:u.-LE:TKE-LE:T.-
                 LE:un_Tau-LE:un_H -LE:un_LE-LE:un_co2_flux-LE:un_h2o_flux-
                 LE:u_var-LE:v_var-LE:w_var-LE:h2o_var-LE:w.ts_cov-LE:w.co2_cov-
                 LE:w.h2o_cov-LE:flowrate-qc_LE:rand_err_LE-qc_LE:co2_flux-
                 h2o_flux:qc_LE-qc_LE:qc_h2o_flux-qc_LE:rand_err_h2o_flux -
                 qc_LE:h2o_time_lag-qc_LE:sonic_temperature-qc_LE:air_temperature-
                 qc_LE:air_density -qc_LE:air_molar_volume-qc_LE:es-
                 qc_LE:RH-qc_LE:VPD-qc_LE:u.-qc_LE:TKE-qc_LE:un_Tau-qc_LE:un_H-
                 qc_LE:un_LE-qc_LE:un_co2_flux-qc_LE:un_h2o_flux-qc_LE:u_var-
                 qc_LE:v_var-qc_LE:w_var-qc_LE:h2o_var-qc_LE:w.ts_cov-
                 qc_LE:w.co2_cov-qc_LE:w.h2o_cov-qc_LE:flowrate-rand_err_LE:co2_flux-
                 rand_err_LE:h2o_time_lag-rand_err_LE:sonic_temperature-
                 rand_err_LE:u.-rand_err_LE:TKE-rand_err_LE:T.-rand_err_LE:un_Tau -
                 rand_err_LE:un_H-rand_err_LE:un_LE-rand_err_LE:un_co2_flux-
                 rand_err_LE:un_h2o_flux -rand_err_LE:u_var-rand_err_LE:v_var-
                 rand_err_LE:w_var-rand_err_LE:h2o_var-rand_err_LE:w.ts_cov-
                 rand_err_LE:w.co2_cov-rand_err_LE:w.h2o_cov-rand_err_LE:flowrate-
                 h2o_flux:co2_flux-co2_flux:qc_h2o_flux-co2_flux:rand_err_h2o_flux-
                 co2_flux:h2o_time_lag-co2_flux:sonic_temperature-
                 co2_flux:air_temperature-co2_flux:air_density-co2_flux:air_molar_volume-
                 co2_flux:es-co2_flux:RH-co2_flux:VPD-co2_flux:u.-co2_flux:TKE-
                 co2_flux:T.-co2_flux:un_Tau-co2_flux:un_H-co2_flux:un_LE-
                 co2_flux:un_co2_flux-co2_flux:un_h2o_flux-co2_flux:u_var-
                 co2_flux:v_var-co2_flux:w_var-co2_flux:h2o_var-co2_flux:w.ts_cov-
                 co2_flux:w.co2_cov-co2_flux:w.h2o_cov-co2_flux:flowrate-
                 h2o_flux:qc_h2o_flux-h2o_flux:rand_err_h2o_flux-h2o_flux:h2o_time_lag-
                 h2o_flux:sonic_temperature-h2o_flux:air_density-h2o_flux:air_molar_volume-
                 h2o_flux:es-h2o_flux:RH-h2o_flux:VPD-h2o_flux:u.-h2o_flux:TKE-
                 h2o_flux:T.-h2o_flux:un_Tau-h2o_flux:un_H-h2o_flux:un_LE-
                 h2o_flux:un_co2_flux-h2o_flux:un_h2o_flux-h2o_flux:u_var-
                 h2o_flux:v_var-h2o_flux:w_var-h2o_flux:h2o_var-
                 h2o_flux:w.ts_cov-h2o_flux:w.co2_cov-h2o_flux:w.h2o_cov-h2o_flux:flowrate-
                 qc_h2o_flux:rand_err_h2o_flux-qc_h2o_flux:h2o_time_lag-
                 qc_h2o_flux:sonic_temperature-qc_h2o_flux:air_temperature-
                 qc_h2o_flux:air_density-qc_h2o_flux:air_molar_volume-
                 qc_h2o_flux:es-qc_h2o_flux:RH-qc_h2o_flux:VPD-qc_h2o_flux:u.-
                 qc_h2o_flux:TKE-qc_h2o_flux:T.-qc_h2o_flux:un_Tau-qc_h2o_flux:un_H-
                 qc_h2o_flux:un_LE-qc_h2o_flux:un_co2_flux-qc_h2o_flux:un_h2o_flux-
                 qc_h2o_flux:u_var-qc_h2o_flux:v_var-qc_h2o_flux:w_var-
                 qc_h2o_flux:h2o_var-qc_h2o_flux:w.ts_cov-qc_h2o_flux:w.co2_cov-
                 qc_h2o_flux:w.h2o_cov-qc_h2o_flux:flowrate -rand_err_h2o_flux:h2o_time_lag-
                 rand_err_h2o_flux:sonic_temperature-rand_err_h2o_flux:air_temperature-
                 rand_err_h2o_flux:u.-rand_err_h2o_flux:TKE-rand_err_h2o_flux:T.-
                 rand_err_h2o_flux:un_Tau-rand_err_h2o_flux:un_H-
                 rand_err_h2o_flux:un_LE-rand_err_h2o_flux:un_co2_flux-
                 rand_err_h2o_flux:un_h2o_flux-rand_err_h2o_flux:u_var -
                 rand_err_h2o_flux:v_var-rand_err_h2o_flux:w_var-rand_err_h2o_flux:h2o_var-
                 rand_err_h2o_flux:w.ts_cov-rand_err_h2o_flux:w.co2_cov-
                 rand_err_h2o_flux:w.h2o_cov-rand_err_h2o_flux:flowrate -
                 h2o_time_lag:sonic_temperature-h2o_time_lag:air_temperature-
                 h2o_time_lag:air_density-h2o_time_lag:air_molar_volume-
                 h2o_time_lag:es-h2o_time_lag:RH-h2o_time_lag:VPD-h2o_time_lag:u.-
                 h2o_time_lag:TKE-h2o_time_lag:T.-h2o_time_lag:un_Tau-
                 h2o_time_lag:un_H -h2o_time_lag:un_LE-h2o_time_lag:un_co2_flux-
                 h2o_time_lag:un_h2o_flux-h2o_time_lag:u_var-h2o_time_lag:v_var-
                 h2o_time_lag:w_var-h2o_time_lag:h2o_var-h2o_time_lag:w.ts_cov -
                 h2o_time_lag:w.co2_cov-h2o_time_lag:w.h2o_cov-h2o_time_lag:flowrate-
                 sonic_temperature:air_temperature-sonic_temperature:air_density-
                 sonic_temperature:air_molar_volume-sonic_temperature:es-
                 sonic_temperature:RH-sonic_temperature:VPD-sonic_temperature:u.-
                 sonic_temperature:TKE-sonic_temperature:T.-sonic_temperature:un_Tau-
                 sonic_temperature:un_H-sonic_temperature:un_LE-sonic_temperature:un_co2_flux-
                 sonic_temperature:un_h2o_flux-sonic_temperature:u_var-
                 sonic_temperature:v_var-sonic_temperature:w_var-sonic_temperature:h2o_var-
                 sonic_temperature:w.ts_cov-sonic_temperature:w.co2_cov-sonic_temperature:w.h2o_cov-
                 sonic_temperature:flowrate-air_temperature:air_density-
                 air_temperature:air_molar_volume-air_temperature:es-air_temperature:RH-
                 air_temperature:VPD-air_temperature:u.-air_temperature:TKE-
                 air_temperature:T.-air_temperature:un_Tau-air_temperature:un_H -
                 air_temperature:un_LE-air_temperature:un_co2_flux-air_temperature:un_h2o_flux-
                 air_temperature:u_var-air_temperature:v_var-air_temperature:w_var-
                 air_temperature:h2o_var-air_temperature:w.ts_cov-air_temperature:w.co2_cov-
                 air_temperature:w.h2o_cov-air_temperature:flowrate-air_density:air_molar_volume -
                 air_density:es-air_density:RH-air_density:VPD-air_density:u.-
                 air_density:T.-air_density:un_Tau-air_density:un_H-air_density:un_LE-
                 air_density:un_co2_flux-air_density:un_h2o_flux-air_density:w_var-
                 air_density:h2o_var -air_density:w.co2_cov -air_density:w.h2o_cov-
                 air_density:flowrate-air_molar_volume:es-air_molar_volume:RH-
                 air_molar_volume:VPD-air_molar_volume:u.-air_molar_volume:T.-
                 air_molar_volume:un_Tau -air_molar_volume:un_H-air_molar_volume:un_LE-
                 air_molar_volume:un_co2_flux-air_molar_volume:un_h2o_flux-
                 air_molar_volume:w_var-air_molar_volume:h2o_var-
                 air_molar_volume:w.ts_cov-air_molar_volume:w.co2_cov-
                 air_molar_volume:w.h2o_cov-air_molar_volume:flowrate-es:RH-es:VPD-
                 es:u.-es:T.-es:un_Tau-es:un_H-es:un_LE-es:un_co2_flux-es:un_h2o_flux-
                 es:w_var-es:h2o_var-es:w.ts_cov-es:w.co2_cov-es:w.h2o_cov-es:flowrate-
                 RH:VPD-RH:u.-RH:T.-RH:un_Tau-RH:un_H-RH:un_LE-RH:un_co2_flux-RH:un_h2o_flux-
                 RH:w_var-RH:h2o_var-RH:w.co2_cov-RH:w.h2o_cov-RH:flowrate-VPD:u.-VPD:T.-
                 VPD:un_Tau-VPD:un_H-VPD:un_LE-VPD:un_co2_flux-VPD:un_h2o_flux-VPD:w_var-
                 VPD:h2o_var-VPD:w.ts_cov-VPD:w.co2_cov-VPD:w.h2o_cov-VPD:flowrate-
                 u.:TKE-u.:T.-u.:un_Tau-u.:un_H-u.:un_LE-u.:un_co2_flux-u.:un_h2o_flux-
                 u.:flowrate-TKE:un_Tau-TKE:un_H-TKE:un_LE-TKE:un_co2_flux-TKE:un_h2o_flux-
                 TKE:u_var-TKE:v_var-TKE:w_var-TKE:h2o_var-TKE:w.co2_cov-TKE:w.h2o_cov-
                 TKE:flowrate-T.:un_Tau-T.:un_H-T.:un_LE-T.:un_co2_flux-T.:un_h2o_flux-
                 T.:u_var-T.:v_var-T.:w_var-T.:w_var-T.:h2o_var-T.:w.ts_cov-T.:w.co2_cov-
                 T.:w.h2o_cov-T.:flowrate-un_Tau:un_H-un_Tau:un_LE-un_Tau:un_co2_flux-
                 un_Tau:un_h2o_flux-un_Tau:u_var-un_Tau:v_var-un_Tau:w_var-un_Tau:h2o_var-
                 un_Tau:w.ts_cov-un_Tau:w.co2_cov-un_Tau:w.h2o_cov-un_Tau:flowrate-
                 un_H:un_LE-un_H:un_co2_flux-un_H:un_h2o_flux-un_H:u_var-un_H:v_var-
                 un_H:w_var-un_H:h2o_var-un_H:w.ts_cov-un_H:w.co2_cov-un_H:w.h2o_cov-
                 un_H:flowrate-un_LE:un_co2_flux-un_LE:un_h2o_flux-un_LE:u_var-un_LE:v_var-
                 un_LE:w_var-un_LE:h2o_var-un_LE:w.ts_cov-un_LE:w.co2_cov-un_LE:w.h2o_cov-
                 un_LE:flowrate-un_co2_flux:un_h2o_flux-un_co2_flux:u_var-un_co2_flux:v_var-
                 un_co2_flux:w_var-un_co2_flux:h2o_var-un_co2_flux:w.ts_cov-un_co2_flux:w.co2_cov-
                 un_co2_flux:w.h2o_cov-un_co2_flux:flowrate-un_h2o_flux:u_var-un_h2o_flux:v_var-
                 un_h2o_flux:w_var-un_h2o_flux:h2o_var-un_h2o_flux:w.ts_cov-un_h2o_flux:w.co2_cov-
                 un_h2o_flux:w.h2o_cov-un_h2o_flux:flowrate-u_var:v_var-u_var:w_var-
                 u_var:h2o_var-u_var:w.co2_cov-u_var:w.h2o_cov-u_var:flowrate-v_var:w_var-
                 v_var:h2o_var-v_var:w.co2_cov-v_var:w.h2o_cov-v_var:flowrate-w_var:h2o_var-
                 w_var:w.ts_cov-w_var:w.co2_cov-w_var:w.h2o_cov-w_var:flowrate-w.ts_cov:w.co2_cov-
                 w.ts_cov:w.h2o_cov-w.ts_cov:flowrate-w.co2_cov:w.h2o_cov-w.co2_cov:flowrate-
                 w.h2o_cov:flowrate-
                 rand_err_h2o_flux:air_density-rand_err_h2o_flux:air_molar_volume-
                 rand_err_h2o_flux:es-rand_err_h2o_flux:RH-air_density:TKE-
                 air_density:u_var-air_density:v_var-air_molar_volume:TKE-
                 air_molar_volume:u_var-air_molar_volume:v_var-es:TKE-es:u_var-
                 RH:v_var-VPD:TKE-VPD:u_var-VPD:v_var-u.:u_var-u.:v_var-u.:w_var-
                 u.:h2o_var-u.:w.ts_cov-u.:w.co2_cov-u.:w.h2o_cov-TKE:T.-TKE:w.ts_cov-
                 u_var:w.ts_cov-v_var:w.ts_cov-h2o_var:w.ts_cov-h2o_var:w.co2_cov-
                 h2o_var:w.h2o_cov-h2o_var:flowrate-
                 rand_err_h2o_flux:VPD-es:v_var-RH:TKE-RH:u_var,data = data)
my.model4
summary(my.model4)
anova(my.model4)
coef(my.model4) 
resid(my.model4) 
confint(my.model4) 
plot(my.model4)
