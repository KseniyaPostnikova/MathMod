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
## Оставляем только весенние месяцы
ar = arrange(data, DOY)
spring.data = filter(ar, DOY %in% ar$DOY[(ar$DOY>59) & (ar$DOY<151)])
## Создаём обучающую и тестовую выборки
row_numbers = 1:length(spring.data$date)
teach = sample(row_numbers, floor(length(spring.data$date)*.7))
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
my.model = lm(h2o_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + v_var + 
                w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + flowrate,
              data = testing_data_unq)
summary(my.model)
anova(my.model)
my.model2 = lm(h2o_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                 rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                 h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                 air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                 un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + v_var + 
                 w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + flowrate,
               data = testing_data_unq)
summary(my.model2)
anova(my.model2)
my.model3 = lm(h2o_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                 rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                 h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                 air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + 
                 un_H + un_LE + un_co2_flux + un_h2o_flux + u_var + v_var + 
                 w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + flowrate,
               data = testing_data_unq)
summary(my.model3)
anova(my.model3)
