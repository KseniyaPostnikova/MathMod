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
my.model1 = lm(h2o_flux ~ (DOY + Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                 rand_err_LE + co2_flux + rand_err_h2o_flux + 
                 h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                 air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                 un_H + un_LE + un_co2_flux + un_h2o_flux + w.co2_cov + w.h2o_cov + flowrate)^2,
               data = data)
my.model1
summary(my.model1)
anova(my.model1)
my.model2 = lm(h2o_flux ~ (DOY + Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                             rand_err_LE + co2_flux + rand_err_h2o_flux + 
                             h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                             air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                             un_H + un_LE + un_co2_flux + un_h2o_flux + w.co2_cov + w.h2o_cov + 
                             flowrate)^2 -DOY:Tau - DOY:rand_err_Tau - DOY:H - DOY:rand_err_H - 
                 DOY:qc_LE - DOY:co2_flux - DOY:sonic_temperature - DOY:air_temperature -
                 DOY:air_density - DOY:air_molar_volume - DOY:es - DOY:RH - DOY:VPD - DOY:u. -
                 DOY:un_H - DOY:un_LE - DOY:un_co2_flux - DOY:w.co2_cov - DOY:flowrate -
                 Tau:rand_err_H - Tau:LE - Tau:air_temperature - Tau:RH - Tau:VPD - Tau:u. -
                 Tau:TKE - Tau:un_Tau - rand_err_Tau:H - rand_err_Tau:rand_err_H - rand_err_Tau:qc_LE - 
                 rand_err_Tau:h2o_time_lag - rand_err_Tau:sonic_temperature - rand_err_Tau:air_temperature -
                 rand_err_Tau:RH - rand_err_Tau:VPD - rand_err_Tau:u. - rand_err_Tau:T. - rand_err_Tau:un_H -
                 rand_err_Tau:un_co2_flux - rand_err_Tau:w.co2_cov - rand_err_Tau:w.h2o_cov -
                 rand_err_Tau:flowrate - H:rand_err_H - H:h2o_time_lag - H:sonic_temperature -
                 H:air_temperature - H:air_density - H:air_molar_volume - H:VPD - H:u. - H:TKE -
                 H:un_H - H:un_co2_flux  - H:w.co2_cov - H:flowrate - rand_err_H:LE - rand_err_H:qc_LE - 
                 rand_err_H:rand_err_LE - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_time_lag -
                 rand_err_H:sonic_temperature - rand_err_H:air_temperature - rand_err_H:air_density - 
                 rand_err_H:air_molar_volume - rand_err_H:es - rand_err_H:RH - rand_err_H:VPD -
                 rand_err_H:u. - rand_err_H:TKE - rand_err_H:T. - rand_err_H:un_Tau - rand_err_H:un_H -
                 rand_err_H:un_h2o_flux - rand_err_H:w.co2_cov - rand_err_H:w.h2o_cov - rand_err_H:flowrate -
                 LE:qc_LE - LE:co2_flux - LE:TKE - LE:un_Tau - LE:w.h2o_cov - LE:flowrate -
                 qc_LE:rand_err_LE - qc_LE:co2_flux - qc_LE:rand_err_h2o_flux - qc_LE:h2o_time_lag -
                 qc_LE:sonic_temperature - qc_LE:air_temperature - qc_LE:air_density - qc_LE:air_molar_volume -
                 qc_LE:RH - qc_LE:VPD - qc_LE:u. - qc_LE:TKE - qc_LE:T. - qc_LE:un_Tau - qc_LE:un_LE -
                 qc_LE:un_co2_flux - qc_LE:un_h2o_flux  - qc_LE:w.co2_cov - qc_LE:flowrate - 
                 rand_err_LE:rand_err_h2o_flux - rand_err_LE:h2o_time_lag - rand_err_LE:sonic_temperature -
                 rand_err_LE:air_temperature - rand_err_LE:air_molar_volume - rand_err_LE:es -
                 rand_err_LE:RH - rand_err_LE:VPD - rand_err_LE:TKE - rand_err_LE:un_LE - 
                 rand_err_LE:un_co2_flux - rand_err_LE:un_h2o_flux - rand_err_LE:w.co2_cov - 
                 rand_err_LE:flowrate - co2_flux:h2o_time_lag - co2_flux:sonic_temperature - 
                 co2_flux:air_temperature - co2_flux:air_density - co2_flux:air_molar_volume - 
                 co2_flux:RH - co2_flux:u. - co2_flux:un_LE - co2_flux:un_co2_flux - co2_flux:w.co2_cov -
                 rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:sonic_temperature -
                 rand_err_h2o_flux:air_temperature - rand_err_h2o_flux:air_molar_volume -
                 rand_err_h2o_flux:es - rand_err_h2o_flux:RH - rand_err_h2o_flux:VPD - rand_err_h2o_flux:TKE -
                 rand_err_h2o_flux:un_LE - rand_err_h2o_flux:un_co2_flux - rand_err_h2o_flux:un_h2o_flux -
                 rand_err_h2o_flux:w.co2_cov - rand_err_h2o_flux:flowrate - h2o_time_lag:sonic_temperature -
                 h2o_time_lag:air_temperature - h2o_time_lag:air_density - h2o_time_lag:air_molar_volume -
                 h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:u. - h2o_time_lag:TKE - h2o_time_lag:T. - 
                 h2o_time_lag:un_H - h2o_time_lag:un_LE - h2o_time_lag:un_h2o_flux  - 
                 h2o_time_lag:w.h2o_cov - h2o_time_lag:flowrate - sonic_temperature:es - 
                 sonic_temperature:u. - sonic_temperature:TKE - sonic_temperature:T. -
                 sonic_temperature:un_H - sonic_temperature:un_LE - sonic_temperature:un_h2o_flux -
                 sonic_temperature:flowrate - air_temperature:air_molar_volume - air_temperature:es - 
                 air_temperature:VPD - air_temperature:u. - air_temperature:TKE - 
                 air_temperature:T. - air_temperature:un_Tau - air_temperature:un_H - 
                 air_temperature:un_h2o_flux - air_temperature:flowrate - air_density:air_molar_volume -
                 air_density:es - air_density:VPD - air_density:u. - air_density:TKE - 
                 air_density:T. - air_density:un_H - air_density:un_co2_flux - air_density:w.co2_cov -
                 air_density:w.h2o_cov - air_molar_volume:es - air_molar_volume:VPD -
                 air_molar_volume:u. - air_molar_volume:TKE - air_molar_volume:T. - 
                 air_molar_volume:un_H - air_molar_volume:un_co2_flux - air_molar_volume:un_h2o_flux -
                 air_molar_volume:w.co2_cov - es:RH - es:u. - es:TKE - es:T. - es:w.h2o_cov -
                 es:flowrate - RH:u. - RH:TKE - RH:un_Tau - RH:w.h2o_cov - RH:flowrate -
                 VPD:u. - VPD:TKE - VPD:un_Tau - VPD:un_H - VPD:w.h2o_cov - u.:TKE -
                 u.:T. - u.:un_Tau - u.:w.co2_cov - TKE:T. - TKE:un_Tau - TKE:un_H -
                 TKE:flowrate - T.:un_co2_flux - T.:w.co2_cov - T.:w.h2o_cov - T.:flowrate -
                 un_H:un_co2_flux - un_H:w.co2_cov - un_H:flowrate - un_LE:flowrate - 
                 un_h2o_flux:w.co2_cov - un_h2o_flux:flowrate - w.h2o_cov:flowrate,data = data)
                 
                           
summary(my.model2)
anova(my.model2)
my.model3 = lm(h2o_flux ~ (DOY + Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                             rand_err_LE + co2_flux + rand_err_h2o_flux + 
                             h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                             air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                             un_H + un_LE + un_co2_flux + un_h2o_flux + w.co2_cov + w.h2o_cov + 
                             flowrate)^2 -DOY:Tau - DOY:rand_err_Tau - DOY:H - DOY:rand_err_H - 
                 DOY:qc_LE - DOY:co2_flux - DOY:sonic_temperature - DOY:air_temperature -
                 DOY:air_density - DOY:air_molar_volume - DOY:es - DOY:RH - DOY:VPD - DOY:u. -
                 DOY:un_H - DOY:un_LE - DOY:un_co2_flux - DOY:w.co2_cov - DOY:flowrate -
                 Tau:rand_err_H - Tau:LE - Tau:air_temperature - Tau:RH - Tau:VPD - Tau:u. -
                 Tau:TKE - Tau:un_Tau - rand_err_Tau:H - rand_err_Tau:rand_err_H - rand_err_Tau:qc_LE - 
                 rand_err_Tau:h2o_time_lag - rand_err_Tau:sonic_temperature - rand_err_Tau:air_temperature -
                 rand_err_Tau:RH - rand_err_Tau:VPD - rand_err_Tau:u. - rand_err_Tau:T. - rand_err_Tau:un_H -
                 rand_err_Tau:un_co2_flux - rand_err_Tau:w.co2_cov - rand_err_Tau:w.h2o_cov -
                 rand_err_Tau:flowrate - H:rand_err_H - H:h2o_time_lag - H:sonic_temperature -
                 H:air_temperature - H:air_density - H:air_molar_volume - H:VPD - H:u. - H:TKE -
                 H:un_H - H:un_co2_flux  - H:w.co2_cov - H:flowrate - rand_err_H:LE - rand_err_H:qc_LE - 
                 rand_err_H:rand_err_LE - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_time_lag -
                 rand_err_H:sonic_temperature - rand_err_H:air_temperature - rand_err_H:air_density - 
                 rand_err_H:air_molar_volume - rand_err_H:es - rand_err_H:RH - rand_err_H:VPD -
                 rand_err_H:u. - rand_err_H:TKE - rand_err_H:T. - rand_err_H:un_Tau - rand_err_H:un_H -
                 rand_err_H:un_h2o_flux - rand_err_H:w.co2_cov - rand_err_H:w.h2o_cov - rand_err_H:flowrate -
                 LE:qc_LE - LE:co2_flux - LE:TKE - LE:un_Tau - LE:w.h2o_cov - LE:flowrate -
                 qc_LE:rand_err_LE - qc_LE:co2_flux - qc_LE:rand_err_h2o_flux - qc_LE:h2o_time_lag -
                 qc_LE:sonic_temperature - qc_LE:air_temperature - qc_LE:air_density - qc_LE:air_molar_volume -
                 qc_LE:RH - qc_LE:VPD - qc_LE:u. - qc_LE:TKE - qc_LE:T. - qc_LE:un_Tau - qc_LE:un_LE -
                 qc_LE:un_co2_flux - qc_LE:un_h2o_flux  - qc_LE:w.co2_cov - qc_LE:flowrate - 
                 rand_err_LE:rand_err_h2o_flux - rand_err_LE:h2o_time_lag - rand_err_LE:sonic_temperature -
                 rand_err_LE:air_temperature - rand_err_LE:air_molar_volume - rand_err_LE:es -
                 rand_err_LE:RH - rand_err_LE:VPD - rand_err_LE:TKE - rand_err_LE:un_LE - 
                 rand_err_LE:un_co2_flux - rand_err_LE:un_h2o_flux - rand_err_LE:w.co2_cov - 
                 rand_err_LE:flowrate - co2_flux:h2o_time_lag - co2_flux:sonic_temperature - 
                 co2_flux:air_temperature - co2_flux:air_density - co2_flux:air_molar_volume - 
                 co2_flux:RH - co2_flux:u. - co2_flux:un_LE - co2_flux:un_co2_flux - co2_flux:w.co2_cov -
                 rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:sonic_temperature -
                 rand_err_h2o_flux:air_temperature - rand_err_h2o_flux:air_molar_volume -
                 rand_err_h2o_flux:es - rand_err_h2o_flux:RH - rand_err_h2o_flux:VPD - rand_err_h2o_flux:TKE -
                 rand_err_h2o_flux:un_LE - rand_err_h2o_flux:un_co2_flux - rand_err_h2o_flux:un_h2o_flux -
                 rand_err_h2o_flux:w.co2_cov - rand_err_h2o_flux:flowrate - h2o_time_lag:sonic_temperature -
                 h2o_time_lag:air_temperature - h2o_time_lag:air_density - h2o_time_lag:air_molar_volume -
                 h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:u. - h2o_time_lag:TKE - h2o_time_lag:T. - 
                 h2o_time_lag:un_H - h2o_time_lag:un_LE - h2o_time_lag:un_h2o_flux  - 
                 h2o_time_lag:w.h2o_cov - h2o_time_lag:flowrate - sonic_temperature:es - 
                 sonic_temperature:u. - sonic_temperature:TKE - sonic_temperature:T. -
                 sonic_temperature:un_H - sonic_temperature:un_LE - sonic_temperature:un_h2o_flux -
                 sonic_temperature:flowrate - air_temperature:air_molar_volume - air_temperature:es - 
                 air_temperature:VPD - air_temperature:u. - air_temperature:TKE - 
                 air_temperature:T. - air_temperature:un_Tau - air_temperature:un_H - 
                 air_temperature:un_h2o_flux - air_temperature:flowrate - air_density:air_molar_volume -
                 air_density:es - air_density:VPD - air_density:u. - air_density:TKE - 
                 air_density:T. - air_density:un_H - air_density:un_co2_flux - air_density:w.co2_cov -
                 air_density:w.h2o_cov - air_molar_volume:es - air_molar_volume:VPD -
                 air_molar_volume:u. - air_molar_volume:TKE - air_molar_volume:T. - 
                 air_molar_volume:un_H - air_molar_volume:un_co2_flux - air_molar_volume:un_h2o_flux -
                 air_molar_volume:w.co2_cov - es:RH - es:u. - es:TKE - es:T. - es:w.h2o_cov -
                 es:flowrate - RH:u. - RH:TKE - RH:un_Tau - RH:w.h2o_cov - RH:flowrate -
                 VPD:u. - VPD:TKE - VPD:un_Tau - VPD:un_H - VPD:w.h2o_cov - u.:TKE -
                 u.:T. - u.:un_Tau - u.:w.co2_cov - TKE:T. - TKE:un_Tau - TKE:un_H -
                 TKE:flowrate - T.:un_co2_flux - T.:w.co2_cov - T.:w.h2o_cov - T.:flowrate -
                 un_H:un_co2_flux - un_H:w.co2_cov - un_H:flowrate - un_LE:flowrate - 
                 un_h2o_flux:w.co2_cov - un_h2o_flux:flowrate - w.h2o_cov:flowrate -
                 DOY:LE - DOY:h2o_time_lag - DOY:TKE - DOY:un_Tau - DOY:un_h2o_flux - 
                 DOY:w.h2o_cov - Tau:qc_LE - Tau:T. - Tau:un_LE - Tau:un_h2o_flux -
                 rand_err_Tau:rand_err_LE - rand_err_Tau:co2_flux - rand_err_Tau:rand_err_h2o_flux -
                 rand_err_Tau:air_density - rand_err_Tau:air_molar_volume - rand_err_Tau:es -
                 rand_err_Tau:TKE - rand_err_Tau:un_LE - rand_err_Tau:un_h2o_flux -
                 H:qc_LE - H:T. - H:w.h2o_cov - LE:h2o_time_lag - qc_LE:es -
                 qc_LE:un_H - qc_LE:w.h2o_cov - rand_err_LE:co2_flux - rand_err_LE:u. -
                 co2_flux:VPD - rand_err_h2o_flux:u. - h2o_time_lag:RH -
                 h2o_time_lag:un_co2_flux - h2o_time_lag:w.co2_cov - sonic_temperature:air_temperature -
                 air_density:un_h2o_flux - air_density:un_LE - es:VPD - es:un_co2_flux -
                 es:w.co2_cov - RH:T. - VPD:T. - VPD:un_co2_flux - VPD:w.co2_cov -
                 u.:un_LE - u.:un_h2o_flux - u.:w.h2o_cov - u.:flowrate - TKE:un_co2_flux -
                 TKE:un_h2o_flux - TKE:w.co2_cov - T.:un_Tau - T.:un_H - un_Tau:un_LE - 
                 un_Tau:un_h2o_flux - un_LE:un_h2o_flux - w.co2_cov:flowrate -
                 RH:VPD - RH:un_co2_flux - RH:w.co2_cov - u.:un_H - 
                 u.:flowrate - TKE:un_LE - TKE:un_LE - TKE:w.h2o_cov -
                 un_Tau:un_H - un_Tau:un_LE - un_H:w.h2o_cov,data = data)
summary(my.model3)
anova(my.model3)
my.model4 = lm(h2o_flux ~ (DOY + Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                             rand_err_LE + co2_flux + rand_err_h2o_flux + 
                             h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                             air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                             un_H + un_LE + un_co2_flux + un_h2o_flux + w.co2_cov + w.h2o_cov + 
                             flowrate)^2 -DOY:Tau - DOY:rand_err_Tau - DOY:H - DOY:rand_err_H - 
                 DOY:qc_LE - DOY:co2_flux - DOY:sonic_temperature - DOY:air_temperature -
                 DOY:air_density - DOY:air_molar_volume - DOY:es - DOY:RH - DOY:VPD - DOY:u. -
                 DOY:un_H - DOY:un_LE - DOY:un_co2_flux - DOY:w.co2_cov - DOY:flowrate -
                 Tau:rand_err_H - Tau:LE - Tau:air_temperature - Tau:RH - Tau:VPD - Tau:u. -
                 Tau:TKE - Tau:un_Tau - rand_err_Tau:H - rand_err_Tau:rand_err_H - rand_err_Tau:qc_LE - 
                 rand_err_Tau:h2o_time_lag - rand_err_Tau:sonic_temperature - rand_err_Tau:air_temperature -
                 rand_err_Tau:RH - rand_err_Tau:VPD - rand_err_Tau:u. - rand_err_Tau:T. - rand_err_Tau:un_H -
                 rand_err_Tau:un_co2_flux - rand_err_Tau:w.co2_cov - rand_err_Tau:w.h2o_cov -
                 rand_err_Tau:flowrate - H:rand_err_H - H:h2o_time_lag - H:sonic_temperature -
                 H:air_temperature - H:air_density - H:air_molar_volume - H:VPD - H:u. - H:TKE -
                 H:un_H - H:un_co2_flux  - H:w.co2_cov - H:flowrate - rand_err_H:LE - rand_err_H:qc_LE - 
                 rand_err_H:rand_err_LE - rand_err_H:rand_err_h2o_flux - rand_err_H:h2o_time_lag -
                 rand_err_H:sonic_temperature - rand_err_H:air_temperature - rand_err_H:air_density - 
                 rand_err_H:air_molar_volume - rand_err_H:es - rand_err_H:RH - rand_err_H:VPD -
                 rand_err_H:u. - rand_err_H:TKE - rand_err_H:T. - rand_err_H:un_Tau - rand_err_H:un_H -
                 rand_err_H:un_h2o_flux - rand_err_H:w.co2_cov - rand_err_H:w.h2o_cov - rand_err_H:flowrate -
                 LE:qc_LE - LE:co2_flux - LE:TKE - LE:un_Tau - LE:w.h2o_cov - LE:flowrate -
                 qc_LE:rand_err_LE - qc_LE:co2_flux - qc_LE:rand_err_h2o_flux - qc_LE:h2o_time_lag -
                 qc_LE:sonic_temperature - qc_LE:air_temperature - qc_LE:air_density - qc_LE:air_molar_volume -
                 qc_LE:RH - qc_LE:VPD - qc_LE:u. - qc_LE:TKE - qc_LE:T. - qc_LE:un_Tau - qc_LE:un_LE -
                 qc_LE:un_co2_flux - qc_LE:un_h2o_flux  - qc_LE:w.co2_cov - qc_LE:flowrate - 
                 rand_err_LE:rand_err_h2o_flux - rand_err_LE:h2o_time_lag - rand_err_LE:sonic_temperature -
                 rand_err_LE:air_temperature - rand_err_LE:air_molar_volume - rand_err_LE:es -
                 rand_err_LE:RH - rand_err_LE:VPD - rand_err_LE:TKE - rand_err_LE:un_LE - 
                 rand_err_LE:un_co2_flux - rand_err_LE:un_h2o_flux - rand_err_LE:w.co2_cov - 
                 rand_err_LE:flowrate - co2_flux:h2o_time_lag - co2_flux:sonic_temperature - 
                 co2_flux:air_temperature - co2_flux:air_density - co2_flux:air_molar_volume - 
                 co2_flux:RH - co2_flux:u. - co2_flux:un_LE - co2_flux:un_co2_flux - co2_flux:w.co2_cov -
                 rand_err_h2o_flux:h2o_time_lag - rand_err_h2o_flux:sonic_temperature -
                 rand_err_h2o_flux:air_temperature - rand_err_h2o_flux:air_molar_volume -
                 rand_err_h2o_flux:es - rand_err_h2o_flux:RH - rand_err_h2o_flux:VPD - rand_err_h2o_flux:TKE -
                 rand_err_h2o_flux:un_LE - rand_err_h2o_flux:un_co2_flux - rand_err_h2o_flux:un_h2o_flux -
                 rand_err_h2o_flux:w.co2_cov - rand_err_h2o_flux:flowrate - h2o_time_lag:sonic_temperature -
                 h2o_time_lag:air_temperature - h2o_time_lag:air_density - h2o_time_lag:air_molar_volume -
                 h2o_time_lag:es - h2o_time_lag:VPD - h2o_time_lag:u. - h2o_time_lag:TKE - h2o_time_lag:T. - 
                 h2o_time_lag:un_H - h2o_time_lag:un_LE - h2o_time_lag:un_h2o_flux  - 
                 h2o_time_lag:w.h2o_cov - h2o_time_lag:flowrate - sonic_temperature:es - 
                 sonic_temperature:u. - sonic_temperature:TKE - sonic_temperature:T. -
                 sonic_temperature:un_H - sonic_temperature:un_LE - sonic_temperature:un_h2o_flux -
                 sonic_temperature:flowrate - air_temperature:air_molar_volume - air_temperature:es - 
                 air_temperature:VPD - air_temperature:u. - air_temperature:TKE - 
                 air_temperature:T. - air_temperature:un_Tau - air_temperature:un_H - 
                 air_temperature:un_h2o_flux - air_temperature:flowrate - air_density:air_molar_volume -
                 air_density:es - air_density:VPD - air_density:u. - air_density:TKE - 
                 air_density:T. - air_density:un_H - air_density:un_co2_flux - air_density:w.co2_cov -
                 air_density:w.h2o_cov - air_molar_volume:es - air_molar_volume:VPD -
                 air_molar_volume:u. - air_molar_volume:TKE - air_molar_volume:T. - 
                 air_molar_volume:un_H - air_molar_volume:un_co2_flux - air_molar_volume:un_h2o_flux -
                 air_molar_volume:w.co2_cov - es:RH - es:u. - es:TKE - es:T. - es:w.h2o_cov -
                 es:flowrate - RH:u. - RH:TKE - RH:un_Tau - RH:w.h2o_cov - RH:flowrate -
                 VPD:u. - VPD:TKE - VPD:un_Tau - VPD:un_H - VPD:w.h2o_cov - u.:TKE -
                 u.:T. - u.:un_Tau - u.:w.co2_cov - TKE:T. - TKE:un_Tau - TKE:un_H -
                 TKE:flowrate - T.:un_co2_flux - T.:w.co2_cov - T.:w.h2o_cov - T.:flowrate -
                 un_H:un_co2_flux - un_H:w.co2_cov - un_H:flowrate - un_LE:flowrate - 
                 un_h2o_flux:w.co2_cov - un_h2o_flux:flowrate - w.h2o_cov:flowrate -
                 DOY:LE - DOY:h2o_time_lag - DOY:TKE - DOY:un_Tau - DOY:un_h2o_flux - 
                 DOY:w.h2o_cov - Tau:qc_LE - Tau:T. - Tau:un_LE - Tau:un_h2o_flux -
                 rand_err_Tau:rand_err_LE - rand_err_Tau:co2_flux - rand_err_Tau:rand_err_h2o_flux -
                 rand_err_Tau:air_density - rand_err_Tau:air_molar_volume - rand_err_Tau:es -
                 rand_err_Tau:TKE - rand_err_Tau:un_LE - rand_err_Tau:un_h2o_flux -
                 H:qc_LE - H:T. - H:w.h2o_cov - LE:h2o_time_lag - qc_LE:es -
                 qc_LE:un_H - qc_LE:w.h2o_cov - rand_err_LE:co2_flux - rand_err_LE:u. -
                 co2_flux:VPD - rand_err_h2o_flux:u. - h2o_time_lag:RH -
                 h2o_time_lag:un_co2_flux - h2o_time_lag:w.co2_cov - sonic_temperature:air_temperature -
                 air_density:un_h2o_flux - air_density:un_LE - es:VPD - es:un_co2_flux -
                 es:w.co2_cov - RH:T. - VPD:T. - VPD:un_co2_flux - VPD:w.co2_cov -
                 u.:un_LE - u.:un_h2o_flux - u.:w.h2o_cov - u.:flowrate - TKE:un_co2_flux -
                 TKE:un_h2o_flux - TKE:w.co2_cov - T.:un_Tau - T.:un_H - un_Tau:un_LE - 
                 un_Tau:un_h2o_flux - un_LE:un_h2o_flux - w.co2_cov:flowrate -
                 RH:VPD - RH:un_co2_flux - RH:w.co2_cov - u.:un_H - 
                 u.:flowrate - TKE:un_LE - TKE:un_LE - TKE:w.h2o_cov -
                 un_Tau:un_H - un_Tau:un_LE - un_H:w.h2o_cov -
                 Tau:un_H - rand_err_Tau:LE - H:co2_flux - LE:sonic_temperature -
                 LE:u. - co2_flux:rand_err_h2o_flux - co2_flux:TKE - co2_flux:T. -
                 co2_flux:un_H - air_temperature:air_density - u.:un_co2_flux -
                 w.co2_cov:w.h2o_cov -
                 sonic_temperature:RH - VPD:un_LE - un_Tau:flowrate - un_LE:w.h2o_cov -
                 un_co2_flux:un_h2o_flux - w.co2_cov:w.h2o_cov,data = data)
summary(my.model4)
anova(my.model4)
coef(my.model4) 
resid(my.model4) 
confint(my.model4) 

plot(my.model4)
