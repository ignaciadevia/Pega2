rm(list = ls())
library(haven)
library(dplyr)
library(tidyverse)
library(readxl)
library(sjlabelled)
library(labelled)
options(scipen = 999)
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")
load("base_pt1.RData")

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario")
d2021 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2021_BBDD_BicentenarioUC.sav")
d2022 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/bicentenario_2022_base.sav")
d2023 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/02-Bicentenario 2023 - Base encuesta final consolidada.sav")
d2024 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/241108 - Bicentenario 2024 - Base encuesta final consolidada.sav")

table(data_filtrada$iden_pol2, data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  rename(creencias_piedras = creencia_piedas)

#Op migrantes

data_filtrada$op_inmigrantes1 <- case_when(
  data_filtrada$op_inmigrantes1  == 1 ~ 5,
  data_filtrada$op_inmigrantes1  == 2 ~ 4,
  data_filtrada$op_inmigrantes1  == 3 ~ 3,
  data_filtrada$op_inmigrantes1  == 4 ~ 2,
  data_filtrada$op_inmigrantes1  == 5 ~ 1,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

data_filtrada$op_inmigrantes3 <- case_when(
  data_filtrada$op_inmigrantes3  == 1 ~ 5,
  data_filtrada$op_inmigrantes3  == 2 ~ 4,
  data_filtrada$op_inmigrantes3  == 3 ~ 3,
  data_filtrada$op_inmigrantes3  == 4 ~ 2,
  data_filtrada$op_inmigrantes3  == 5 ~ 1,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

val_labels(data_filtrada$op_inmigrantes1) <- val_labels(data_filtrada$op_inmigrantes5)
val_labels(data_filtrada$op_inmigrantes3) <- val_labels(data_filtrada$op_inmigrantes5)
data_filtrada$op_inmigrantes1 <- coalesce(data_filtrada$op_inmigrantes1, data_filtrada$op_inmigrantes5)
data_filtrada$op_inmigrantes3 <- coalesce(data_filtrada$op_inmigrantes3, data_filtrada$op_inmigrantes6)
data_filtrada$op_inmigrantes1 <- set_label(data_filtrada$op_inmigrantes1, "Los inmigrantes limitan las posibilidades de encontrar trabajo de los chilenos")
data_filtrada$op_inmigrantes3 <- set_label(data_filtrada$op_inmigrantes3, "Los inmigrantes con su situación legal al día, deberían tener los mismos dere")
table(data_filtrada$op_inmigrantes3, data_filtrada$ano)

#Moral econ 1, 3, 5 y 6
data_filtrada$moral_econ1 <- case_when(
  data_filtrada$moral_econ1  == 1 ~ 1,
  data_filtrada$moral_econ1  == 2 ~ 1,
  data_filtrada$moral_econ1  == 3 ~ 2,
  data_filtrada$moral_econ1  == 4 ~ 2,
  data_filtrada$moral_econ1  == 5 ~ 3,
  data_filtrada$moral_econ1  == 6 ~ 3,
  data_filtrada$moral_econ1  == 7 ~ 4,
  data_filtrada$moral_econ1  == 8 ~ 4,
  data_filtrada$moral_econ1  == 9 ~ 5,
  data_filtrada$moral_econ1  == 10 ~ 5,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

data_filtrada$moral_econ3 <- case_when(
  data_filtrada$moral_econ3  == 1 ~ 1,
  data_filtrada$moral_econ3  == 2 ~ 1,
  data_filtrada$moral_econ3  == 3 ~ 2,
  data_filtrada$moral_econ3  == 4 ~ 2,
  data_filtrada$moral_econ3  == 5 ~ 3,
  data_filtrada$moral_econ3  == 6 ~ 3,
  data_filtrada$moral_econ3  == 7 ~ 4,
  data_filtrada$moral_econ3  == 8 ~ 4,
  data_filtrada$moral_econ3  == 9 ~ 5,
  data_filtrada$moral_econ3  == 10 ~ 5,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

data_filtrada$moral_econ5 <- case_when(
  data_filtrada$moral_econ5  == 1 ~ 1,
  data_filtrada$moral_econ5  == 2 ~ 1,
  data_filtrada$moral_econ5  == 3 ~ 2,
  data_filtrada$moral_econ5  == 4 ~ 2,
  data_filtrada$moral_econ5  == 5 ~ 3,
  data_filtrada$moral_econ5  == 6 ~ 3,
  data_filtrada$moral_econ5  == 7 ~ 4,
  data_filtrada$moral_econ5  == 8 ~ 4,
  data_filtrada$moral_econ5  == 9 ~ 5,
  data_filtrada$moral_econ5  == 10 ~ 5,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

data_filtrada$moral_econ6 <- case_when(
  data_filtrada$moral_econ6  == 1 ~ 1,
  data_filtrada$moral_econ6  == 2 ~ 1,
  data_filtrada$moral_econ6  == 3 ~ 2,
  data_filtrada$moral_econ6  == 4 ~ 2,
  data_filtrada$moral_econ6  == 5 ~ 3,
  data_filtrada$moral_econ6  == 6 ~ 3,
  data_filtrada$moral_econ6  == 7 ~ 4,
  data_filtrada$moral_econ6  == 8 ~ 4,
  data_filtrada$moral_econ6  == 9 ~ 5,
  data_filtrada$moral_econ6  == 10 ~ 5,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

#Op trabajo mujer 2
data_filtrada$op_trabajo_mujer2 <- case_when(
  data_filtrada$op_trabajo_mujer2  == 1 ~ 5,
  data_filtrada$op_trabajo_mujer2  == 2 ~ 4,
  data_filtrada$op_trabajo_mujer2  == 3 ~ 3,
  data_filtrada$op_trabajo_mujer2  == 4 ~ 2,
  data_filtrada$op_trabajo_mujer2  == 5 ~ 1,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

#Iden pol
data_filtrada$iden_pol <- case_when(
  data_filtrada$iden_pol  == 1 ~ 1,
  data_filtrada$iden_pol  == 2 ~ 1,
  data_filtrada$iden_pol  == 3 ~ 2,
  data_filtrada$iden_pol  == 4 ~ 2,
  data_filtrada$iden_pol  == 5 ~ 3,
  data_filtrada$iden_pol  == 6 ~ 3,
  data_filtrada$iden_pol  == 7 ~ 4,
  data_filtrada$iden_pol  == 8 ~ 4,
  data_filtrada$iden_pol  == 9 ~ 5,
  data_filtrada$iden_pol  == 10 ~ 5,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

#Religion
data_filtrada <- data_filtrada %>%
  mutate(
    religion_encuestado = case_when(
      religion_encuestado == 5 ~ 6,
      religion_encuestado == 6 ~ 5,
      TRUE ~ religion_encuestado # Mantener el valor original si no se cumple ninguna condición
    ),
    religion_pareja = case_when(
      religion_pareja == 5 ~ 6,
      religion_pareja == 6 ~ 5,
      TRUE ~ religion_pareja # Mantener el valor original si no se cumple ninguna condición
    ),
    religion_madre = case_when(
      religion_madre == 5 ~ 6,
      religion_madre == 6 ~ 5,
      TRUE ~ religion_madre # Mantener el valor original si no se cumple ninguna condición
    ),
    religion_padre = case_when(
      religion_padre == 5 ~ 6,
      religion_padre == 6 ~ 5,
      TRUE ~ religion_padre # Mantener el valor original si no se cumple ninguna condición
    ),
    religion_hijo = case_when(
      religion_hijo == 5 ~ 6,
      religion_hijo == 6 ~ 5,
      TRUE ~ religion_hijo # Mantener el valor original si no se cumple ninguna condición
    )
  )

#Frec rezar 2
data_filtrada <- data_filtrada %>%
  mutate(
    frecuencia_rezar2 = case_when(
      frecuencia_rezar2 == 6 ~ 9,
      TRUE ~ frecuencia_rezar2 # Mantener el valor original si no se cumple ninguna condición
    ))

# Recodificar 2021
d2021 <- d2021 %>%
  mutate(
    S03_1 = case_when(
      S03_1 == 1 ~ 5,
      S03_1 == 2 ~ 4,
      S03_1 == 3 ~ 3,
      S03_1 == 4 ~ 2,
      S03_1 == 5 ~ 1,
      TRUE ~ S03_1 # Mantener el valor original si no se cumple ninguna condición
    ),
    S03_2 = case_when(
      S03_2 == 1 ~ 5,
      S03_2 == 2 ~ 4,
      S03_2 == 3 ~ 3,
      S03_2 == 4 ~ 2,
      S03_2 == 5 ~ 1,
      TRUE ~ S03_2
    ),
    S03_3 = case_when(
      S03_3 == 1 ~ 5,
      S03_3 == 2 ~ 4,
      S03_3 == 3 ~ 3,
      S03_3 == 4 ~ 2,
      S03_3 == 5 ~ 1,
      TRUE ~ S03_3
    ),
    S03_4 = case_when(
      S03_4 == 1 ~ 5,
      S03_4 == 2 ~ 4,
      S03_4 == 3 ~ 3,
      S03_4 == 4 ~ 2,
      S03_4 == 5 ~ 1,
      TRUE ~ S03_4
    ),
    S03_5 = case_when(
      S03_5 == 1 ~ 5,
      S03_5 == 2 ~ 4,
      S03_5 == 3 ~ 3,
      S03_5 == 4 ~ 2,
      S03_5 == 5 ~ 1,
      TRUE ~ S03_5
    ),
    S03_6 = case_when(
      S03_6 == 1 ~ 5,
      S03_6 == 2 ~ 4,
      S03_6 == 3 ~ 3,
      S03_6 == 4 ~ 2,
      S03_6 == 5 ~ 1,
      TRUE ~ S03_6
    ),
    S03_7 = case_when(
      S03_7 == 1 ~ 5,
      S03_7 == 2 ~ 4,
      S03_7 == 3 ~ 3,
      S03_7 == 4 ~ 2,
      S03_7 == 5 ~ 1,
      TRUE ~ S03_7
    ),
    S08_1 = case_when(
      S08_1 == 1 ~ 5,
      S08_1 == 2 ~ 4,
      S08_1 == 3 ~ 3,
      S08_1 == 4 ~ 2,
      S08_1 == 5 ~ 1,
      TRUE ~ S08_1
    ),
    S08_2 = case_when(
      S08_2 == 1 ~ 5,
      S08_2 == 2 ~ 4,
      S08_2 == 3 ~ 3,
      S08_2 == 4 ~ 2,
      S08_2 == 5 ~ 1,
      TRUE ~ S08_2
    ),
    S08_3 = case_when(
      S08_3 == 1 ~ 5,
      S08_3 == 2 ~ 4,
      S08_3 == 3 ~ 3,
      S08_3 == 4 ~ 2,
      S08_3 == 5 ~ 1,
      TRUE ~ S08_3
    ),
    S08_4 = case_when(
      S08_4 == 1 ~ 5,
      S08_4 == 2 ~ 4,
      S08_4 == 3 ~ 3,
      S08_4 == 4 ~ 2,
      S08_4 == 5 ~ 1,
      TRUE ~ S08_4
    ),
    S08_5 = case_when(
      S08_5 == 1 ~ 5,
      S08_5 == 2 ~ 4,
      S08_5 == 3 ~ 3,
      S08_5 == 4 ~ 2,
      S08_5 == 5 ~ 1,
      TRUE ~ S08_5
    ),
    I_1_P28 = case_when(
      I_1_P28 == 1 ~ 5,
      I_1_P28 == 2 ~ 4,
      I_1_P28 == 3 ~ 3,
      I_1_P28 == 4 ~ 2,
      I_1_P28 == 5 ~ 1,
      TRUE ~ I_1_P28
    ),
    I_2_P28 = case_when(
      I_2_P28 == 1 ~ 5,
      I_2_P28 == 2 ~ 4,
      I_2_P28 == 3 ~ 3,
      I_2_P28 == 4 ~ 2,
      I_2_P28 == 5 ~ 1,
      TRUE ~ I_2_P28
    ),
    I_3_P28 = case_when(
      I_3_P28 == 1 ~ 5,
      I_3_P28 == 2 ~ 4,
      I_3_P28 == 3 ~ 3,
      I_3_P28 == 4 ~ 2,
      I_3_P28 == 5 ~ 1,
      TRUE ~ I_3_P28
    ),
    I_4_P28 = case_when(
      I_4_P28 == 1 ~ 5,
      I_4_P28 == 2 ~ 4,
      I_4_P28 == 3 ~ 3,
      I_4_P28 == 4 ~ 2,
      I_4_P28 == 5 ~ 1,
      TRUE ~ I_4_P28
    ),
    R04 = case_when(
      R04 == 6 ~ 9,
      R04 == 7 ~ 9,
      TRUE ~ R04
    ),
    CP01 = case_when(
      CP01 == 1 ~ 1,
      CP01 == 2 ~ 1,
      CP01 == 3 ~ 1,
      CP01 == 4 ~ 1,
      CP01 == 5 ~ 1,
      CP01 == 6 ~ 2,
      CP01 == 7 ~ 2,
      CP01 == 8 ~ 4,
      CP01 == 9 ~ 4,
      CP01 == 10 ~ 4,
      CP01 == 11 ~ 4,
      CP01 == 12 ~ 5,
      CP01 == 13 ~ 6,
      CP01 == 14 ~ 6,
      CP01 == 15 ~ 7,
      CP01 == 16 ~ 8,
      CP01 == 17 ~ 8,
      CP01 == 18 ~ 9,
    ),
    CP02 = case_when(
      CP02 == 1 ~ 1,
      CP02 == 2 ~ 1,
      CP02 == 3 ~ 1,
      CP02 == 4 ~ 1,
      CP02 == 5 ~ 1,
      CP02 == 6 ~ 2,
      CP02 == 7 ~ 2,
      CP02 == 8 ~ 4,
      CP02 == 9 ~ 4,
      CP02 == 10 ~ 4,
      CP02 == 11 ~ 4,
      CP02 == 12 ~ 5,
      CP02 == 13 ~ 6,
      CP02 == 14 ~ 6,
      CP02 == 15 ~ 7,
      CP02 == 16 ~ 8,
      CP02 == 17 ~ 8,
      CP02 == 18 ~ 9,
    ),
    CP03 = case_when(
      CP03 == 1 ~ 1,
      CP03 == 2 ~ 1,
      CP03 == 3 ~ 1,
      CP03 == 4 ~ 1,
      CP03 == 5 ~ 1,
      CP03 == 6 ~ 2,
      CP03 == 7 ~ 2,
      CP03 == 8 ~ 4,
      CP03 == 9 ~ 4,
      CP03 == 10 ~ 4,
      CP03 == 11 ~ 4,
      CP03 == 12 ~ 5,
      CP03 == 13 ~ 6,
      CP03 == 14 ~ 6,
      CP03 == 15 ~ 7,
      CP03 == 16 ~ 8,
      CP03 == 17 ~ 8,
      CP03 == 18 ~ 9,
    ),
    CP05 = case_when(
      CP05 == 1 ~ 1,
      CP05 == 2 ~ 2,
      CP05 == 3 ~ 3,
      CP05 == 4 ~ 4,
      CP05 == 5 ~ 4,
      CP05 == 6 ~ 4,
      CP05 == 7 ~ 5,
      CP05 == 8 ~ 6,
      CP05 == 9 ~ 9,
    )
  )

#Recodificar 2022

d2022 <- d2022 %>%
  mutate(
    i_5_p110 = case_when(
      i_5_p110 == 1 ~ 5,
      i_5_p110 == 2 ~ 4,
      i_5_p110 == 3 ~ 3,
      i_5_p110 == 4 ~ 2,
      i_5_p110 == 5 ~ 1,
      TRUE ~ i_5_p110 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_4_p110 = case_when(
      i_4_p110 == 1 ~ 5,
      i_4_p110 == 2 ~ 4,
      i_4_p110 == 3 ~ 3,
      i_4_p110 == 4 ~ 2,
      i_4_p110 == 5 ~ 1,
      TRUE ~ i_4_p110 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_3_p110 = case_when(
      i_3_p110 == 1 ~ 5,
      i_3_p110 == 2 ~ 4,
      i_3_p110 == 3 ~ 3,
      i_3_p110 == 4 ~ 2,
      i_3_p110 == 5 ~ 1,
      TRUE ~ i_3_p110 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_2_p110 = case_when(
      i_2_p110 == 1 ~ 5,
      i_2_p110 == 2 ~ 4,
      i_2_p110 == 3 ~ 3,
      i_2_p110 == 4 ~ 2,
      i_2_p110 == 5 ~ 1,
      TRUE ~ i_2_p110 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_1_p110 = case_when(
      i_1_p110 == 1 ~ 5,
      i_1_p110 == 2 ~ 4,
      i_1_p110 == 3 ~ 3,
      i_1_p110 == 4 ~ 2,
      i_1_p110 == 5 ~ 1,
      TRUE ~ i_1_p110 # Mantener el valor original si no se cumple ninguna condición
    ),
    p706 = case_when(
      p706 == 1 ~ 1,
      p706 == 2 ~ 3,
      p706 == 3 ~ 4,
      p706 == 4 ~ 5,
      p706 == 5 ~ 9,
      p706 == 6 ~ 9,
      TRUE ~ p706 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_1_cp01 = case_when(
      i_1_cp01 == 1 ~ 1,
      i_1_cp01 == 2 ~ 1,
      i_1_cp01 == 3 ~ 1,
      i_1_cp01 == 4 ~ 1,
      i_1_cp01 == 5 ~ 1,
      i_1_cp01 == 6 ~ 2,
      i_1_cp01 == 7 ~ 2,
      i_1_cp01 == 8 ~ 4,
      i_1_cp01 == 9 ~ 4,
      i_1_cp01 == 10 ~ 4,
      i_1_cp01 == 11 ~ 4,
      i_1_cp01 == 12 ~ 5,
      i_1_cp01 == 13 ~ 6,
      i_1_cp01 == 14 ~ 6,
      i_1_cp01 == 15 ~ 7,
      i_1_cp01 == 16 ~ 8,
      i_1_cp01 == 17 ~ 8,
      i_1_cp01 == 18 ~ 9,
    ),
    i_2_cp01 = case_when(
      i_2_cp01 == 1 ~ 1,
      i_2_cp01 == 2 ~ 1,
      i_2_cp01 == 3 ~ 1,
      i_2_cp01 == 4 ~ 1,
      i_2_cp01 == 5 ~ 1,
      i_2_cp01 == 6 ~ 2,
      i_2_cp01 == 7 ~ 2,
      i_2_cp01 == 8 ~ 4,
      i_2_cp01 == 9 ~ 4,
      i_2_cp01 == 10 ~ 4,
      i_2_cp01 == 11 ~ 4,
      i_2_cp01 == 12 ~ 5,
      i_2_cp01 == 13 ~ 6,
      i_2_cp01 == 14 ~ 6,
      i_2_cp01 == 15 ~ 7,
      i_2_cp01 == 16 ~ 8,
      i_2_cp01 == 17 ~ 8,
      i_2_cp01 == 18 ~ 9,
    ),
    i_3_cp01 = case_when(
      i_3_cp01 == 1 ~ 1,
      i_3_cp01 == 2 ~ 1,
      i_3_cp01 == 3 ~ 1,
      i_3_cp01 == 4 ~ 1,
      i_3_cp01 == 5 ~ 1,
      i_3_cp01 == 6 ~ 2,
      i_3_cp01 == 7 ~ 2,
      i_3_cp01 == 8 ~ 4,
      i_3_cp01 == 9 ~ 4,
      i_3_cp01 == 10 ~ 4,
      i_3_cp01 == 11 ~ 4,
      i_3_cp01 == 12 ~ 5,
      i_3_cp01 == 13 ~ 6,
      i_3_cp01 == 14 ~ 6,
      i_3_cp01 == 15 ~ 7,
      i_3_cp01 == 16 ~ 8,
      i_3_cp01 == 17 ~ 8,
      i_3_cp01 == 18 ~ 9,
    ),
    cp05 = case_when(
      cp05 == 1 ~ 1,
      cp05 == 2 ~ 2,
      cp05 == 3 ~ 3,
      cp05 == 4 ~ 4,
      cp05 == 5 ~ 4,
      cp05 == 6 ~ 4,
      cp05 == 7 ~ 5,
      cp05 == 8 ~ 6,
      cp05 == 9 ~ 9,
    )
  )

#Recodificar 2023
d2023 <- d2023 %>%
  mutate(
    r51 = case_when(
      r51 == 1 ~ 1,
      r51 == 2 ~ 3,
      r51 == 3 ~ 4,
      r51 == 4 ~ 5,
      r51 == 8 ~ 9,
      r51 == 9 ~ 9,
      TRUE ~ r51 # Mantener el valor original si no se cumple ninguna condición
    ),
    r49 = case_when(
      r49 == 1 ~ 5,
      r49 == 2 ~ 4,
      r49 == 3 ~ 3,
      r49 == 4 ~ 2,
      r49 == 5 ~ 1,
      TRUE ~ r49 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_1_cp04_bis = case_when(
      i_1_cp04_bis == 1 ~ 1,
      i_1_cp04_bis == 2 ~ 1,
      i_1_cp04_bis == 3 ~ 1,
      i_1_cp04_bis == 4 ~ 1,
      i_1_cp04_bis == 5 ~ 1,
      i_1_cp04_bis == 6 ~ 2,
      i_1_cp04_bis == 7 ~ 2,
      i_1_cp04_bis == 8 ~ 4,
      i_1_cp04_bis == 9 ~ 4,
      i_1_cp04_bis == 10 ~ 4,
      i_1_cp04_bis == 11 ~ 4,
      i_1_cp04_bis == 12 ~ 5,
      i_1_cp04_bis == 13 ~ 6,
      i_1_cp04_bis == 14 ~ 6,
      i_1_cp04_bis == 15 ~ 7,
      i_1_cp04_bis == 16 ~ 8,
      i_1_cp04_bis == 17 ~ 8,
      i_1_cp04_bis == 18 ~ 9,
    ),
    i_2_cp04_bis = case_when(
      i_2_cp04_bis == 1 ~ 1,
      i_2_cp04_bis == 2 ~ 1,
      i_2_cp04_bis == 3 ~ 1,
      i_2_cp04_bis == 4 ~ 1,
      i_2_cp04_bis == 5 ~ 1,
      i_2_cp04_bis == 6 ~ 2,
      i_2_cp04_bis == 7 ~ 2,
      i_2_cp04_bis == 8 ~ 4,
      i_2_cp04_bis == 9 ~ 4,
      i_2_cp04_bis == 10 ~ 4,
      i_2_cp04_bis == 11 ~ 4,
      i_2_cp04_bis == 12 ~ 5,
      i_2_cp04_bis == 13 ~ 6,
      i_2_cp04_bis == 14 ~ 6,
      i_2_cp04_bis == 15 ~ 7,
      i_2_cp04_bis == 16 ~ 8,
      i_2_cp04_bis == 17 ~ 8,
      i_2_cp04_bis == 18 ~ 9,
    ),
    i_3_cp04_bis = case_when(
      i_3_cp04_bis == 1 ~ 1,
      i_3_cp04_bis == 2 ~ 1,
      i_3_cp04_bis == 3 ~ 1,
      i_3_cp04_bis == 4 ~ 1,
      i_3_cp04_bis == 5 ~ 1,
      i_3_cp04_bis == 6 ~ 2,
      i_3_cp04_bis == 7 ~ 2,
      i_3_cp04_bis == 8 ~ 4,
      i_3_cp04_bis == 9 ~ 4,
      i_3_cp04_bis == 10 ~ 4,
      i_3_cp04_bis == 11 ~ 4,
      i_3_cp04_bis == 12 ~ 5,
      i_3_cp04_bis == 13 ~ 6,
      i_3_cp04_bis == 14 ~ 6,
      i_3_cp04_bis == 15 ~ 7,
      i_3_cp04_bis == 16 ~ 8,
      i_3_cp04_bis == 17 ~ 8,
      i_3_cp04_bis == 18 ~ 9,
    ),
    cp05 = case_when(
      cp05 == 1 ~ 1,
      cp05 == 2 ~ 2,
      cp05 == 3 ~ 3,
      cp05 == 4 ~ 4,
      cp05 == 5 ~ 4,
      cp05 == 6 ~ 4,
      cp05 == 7 ~ 5,
      cp05 == 8 ~ 6,
      cp05 == 9 ~ 9,
    )
  )

#Recodificar 2024
d2024 <- d2024 %>%
  mutate(
    r48 = case_when(
      r48 == 1 ~ 1,
      r48 == 2 ~ 3,
      r48 == 3 ~ 4,
      r48 == 4 ~ 5,
      r48 == 8 ~ 9,
      r48 == 9 ~ 9,
      TRUE ~ r48 # Mantener el valor original si no se cumple ninguna condición
    ),
    r46 = case_when(
      r46 == 1 ~ 5,
      r46 == 2 ~ 4,
      r46 == 3 ~ 3,
      r46 == 4 ~ 2,
      r46 == 5 ~ 1,
      TRUE ~ r46 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_6_s8 = case_when(
      i_6_s8 == 1 ~ 5,
      i_6_s8 == 2 ~ 4,
      i_6_s8 == 3 ~ 3,
      i_6_s8 == 4 ~ 2,
      i_6_s8 == 5 ~ 1,
      TRUE ~ i_6_s8 # Mantener el valor original si no se cumple ninguna condición
    ),
    r46 = case_when(
      r46 == 1 ~ 5,
      r46 == 2 ~ 4,
      r46 == 3 ~ 3,
      r46 == 4 ~ 2,
      r46 == 5 ~ 1,
      TRUE ~ r46 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_1_r51 = case_when(
      i_1_r51 == 1 ~ 5,
      i_1_r51 == 2 ~ 4,
      i_1_r51 == 3 ~ 3,
      i_1_r51 == 4 ~ 2,
      i_1_r51 == 5 ~ 1,
      TRUE ~ i_1_r51 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_2_r51 = case_when(
      i_2_r51 == 1 ~ 5,
      i_2_r51 == 2 ~ 4,
      i_2_r51 == 3 ~ 3,
      i_2_r51 == 4 ~ 2,
      i_2_r51 == 5 ~ 1,
      TRUE ~ i_2_r51 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_3_r51 = case_when(
      i_3_r51 == 1 ~ 5,
      i_3_r51 == 2 ~ 4,
      i_3_r51 == 3 ~ 3,
      i_3_r51 == 4 ~ 2,
      i_3_r51 == 5 ~ 1,
      TRUE ~ i_3_r51 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_4_r51 = case_when(
      i_4_r51 == 1 ~ 5,
      i_4_r51 == 2 ~ 4,
      i_4_r51 == 3 ~ 3,
      i_4_r51 == 4 ~ 2,
      i_4_r51 == 5 ~ 1,
      TRUE ~ i_4_r51 # Mantener el valor original si no se cumple ninguna condición
    ),
    r53 = case_when(
      r53 == 1 ~ 5,
      r53 == 2 ~ 4,
      r53 == 3 ~ 3,
      r53 == 4 ~ 2,
      r53 == 5 ~ 1,
      TRUE ~ r53 # Mantener el valor original si no se cumple ninguna condición
    ),
    i_1_cp04_bis = case_when(
      i_1_cp04_bis == 1 ~ 1,
      i_1_cp04_bis == 2 ~ 1,
      i_1_cp04_bis == 3 ~ 1,
      i_1_cp04_bis == 4 ~ 1,
      i_1_cp04_bis == 5 ~ 1,
      i_1_cp04_bis == 6 ~ 2,
      i_1_cp04_bis == 7 ~ 2,
      i_1_cp04_bis == 8 ~ 4,
      i_1_cp04_bis == 9 ~ 4,
      i_1_cp04_bis == 10 ~ 4,
      i_1_cp04_bis == 11 ~ 4,
      i_1_cp04_bis == 12 ~ 5,
      i_1_cp04_bis == 13 ~ 6,
      i_1_cp04_bis == 14 ~ 6,
      i_1_cp04_bis == 15 ~ 7,
      i_1_cp04_bis == 16 ~ 8,
      i_1_cp04_bis == 17 ~ 8,
      i_1_cp04_bis == 18 ~ 9,
    ),
    i_2_cp04_bis = case_when(
      i_2_cp04_bis == 1 ~ 1,
      i_2_cp04_bis == 2 ~ 1,
      i_2_cp04_bis == 3 ~ 1,
      i_2_cp04_bis == 4 ~ 1,
      i_2_cp04_bis == 5 ~ 1,
      i_2_cp04_bis == 6 ~ 2,
      i_2_cp04_bis == 7 ~ 2,
      i_2_cp04_bis == 8 ~ 4,
      i_2_cp04_bis == 9 ~ 4,
      i_2_cp04_bis == 10 ~ 4,
      i_2_cp04_bis == 11 ~ 4,
      i_2_cp04_bis == 12 ~ 5,
      i_2_cp04_bis == 13 ~ 6,
      i_2_cp04_bis == 14 ~ 6,
      i_2_cp04_bis == 15 ~ 7,
      i_2_cp04_bis == 16 ~ 8,
      i_2_cp04_bis == 17 ~ 8,
      i_2_cp04_bis == 18 ~ 9,
    ),
    i_3_cp04_bis = case_when(
      i_3_cp04_bis == 1 ~ 1,
      i_3_cp04_bis == 2 ~ 1,
      i_3_cp04_bis == 3 ~ 1,
      i_3_cp04_bis == 4 ~ 1,
      i_3_cp04_bis == 5 ~ 1,
      i_3_cp04_bis == 6 ~ 2,
      i_3_cp04_bis == 7 ~ 2,
      i_3_cp04_bis == 8 ~ 4,
      i_3_cp04_bis == 9 ~ 4,
      i_3_cp04_bis == 10 ~ 4,
      i_3_cp04_bis == 11 ~ 4,
      i_3_cp04_bis == 12 ~ 5,
      i_3_cp04_bis == 13 ~ 6,
      i_3_cp04_bis == 14 ~ 6,
      i_3_cp04_bis == 15 ~ 7,
      i_3_cp04_bis == 16 ~ 8,
      i_3_cp04_bis == 17 ~ 8,
      i_3_cp04_bis == 18 ~ 9,
    ),
    cp08 = case_when(
      cp08 == 1 ~ 1,
      cp08 == 2 ~ 2,
      cp08 == 3 ~ 3,
      cp08 == 4 ~ 4,
      cp08 == 5 ~ 4,
      cp08 == 6 ~ 4,
      cp08 == 7 ~ 5,
      cp08 == 8 ~ 6,
      cp08 == 9 ~ 9,
    ),
    cp07 = case_when(
      cp07 == 1 ~ 1,
      cp07 == 4 ~ 2,
      cp07 == 2 ~ 3,
      cp07 == 3 ~ 3,
      cp07 == 4 ~ 6,
    )
  )

# Para d2021
d2021 <- d2021 %>%
  rename(
    prob_ascenso1 = S03_1,
    prob_ascenso2 = S03_2,
    prob_ascenso3 = S03_3,
    prob_ascenso4 = S03_4,
    prob_ascenso5 = S03_5,
    prob_ascenso6 = S03_6,
    prob_ascenso7_2021 = S03_7,
    conf_inst2 = S07_1,
    conf_inst3 = S07_2,
    conf_inst9 = S07_3,
    conf_inst7 = S07_4,
    conf_inst6 = S07_5,
    conf_inst5 = S07_6,
    conf_inst1 = S07_7,
    conf_inst17_2021 = S07_8,
    op_inmigrantes1 = S14_1,
    op_inmigrantes3 = S14_2,
    moral_econ1 = S04_1,
    moral_econ5 = S04_2,
    moral_econ6 = S04_3,
    moral_econ3 = S04_4,
    expectativa_mov = P12,
    mov_inter1 = S08_1,
    mov_inter2 = S08_2,
    mov_inter3 = S08_3,
    mov_inter4 = S08_4,
    mov_inter5 = S08_5,
    op_violencia1 = P18_BIS,
    justificacion_violencia1 = I_1_P19,
    justificacion_violencia2 = I_2_P19,
    justificacion_violencia3 = I_3_P19,
    justificacion_violencia4 = I_4_P19,
    justificacion_violencia5 = I_5_P19,
    justificacion_violencia7 = I_1_P20,
    justificacion_violencia8 = I_2_P20,
    justificacion_violencia9 = I_3_P20,
    pp_indispensables = P26,
    op_chile2 = I_2_P28,
    op_chile4 = I_1_P28,
    op_chile6 = I_3_P28,
    op_chile5 = I_4_P28, 
    op_matr5 = F01_1,
    op_matr1 = F01_2,
    op_matr6 = F01_3,
    op_matr11 = F01_4,
    op_matr7 = F01_5,
    op_trabajo_mujer2 = F04_1,
    op_trabajo_mujer14 = F04_3,
    op_trabajo_mujer16 = F04_4,
    op_trabajo_mujer15 = F04_5,
    op_aborto = F02,
    religion_encuestado = R01_1,
    religion_pareja = R01_2,
    religion_madre = R01_4,
    religion_padre = R01_5,
    creencia_dios = R03,
    define_como = R04,
    frecuencia_rezar2 = R05,
    conf_inst4 = R07_3,
    confianza_obispos = R07_2,
    confianza_sacerdotes = R07_4,
    creencia_personas_justas = R10,
    creencia_fe_siniglesia = R11,
    seguir_enseñanzas = R14,
    consejo_sacerdote = R15,
    abusos_pasado = R18,
    abusos_comunes_iglesia = R19,
    afectados_abusos_sacerdotes = P65,
    educ = CP01,
    educ_madre = CP02,
    educ_padre = CP03,
    sexo = SEXO,
    ponderador = POND,
    tramoedad = cvar2,
    gse = cvar11,
    zona = zona,
    region = REGION,
    edad = EDAD,
    comuna = COMUNA,
    tipocomuna = tipo_comuna_bicen,
    estado_civil = CP05,
    id_comuna = folio_mapa,
    n_personas_hogar = PERS_HOG,
    actividad_ppal3 = CP07,
    actividad_ppal4 = CP06
  )

# Para d2022
d2022 <- d2022 %>%
  rename(
    prob_ascenso1 = i_1_p103,
    prob_ascenso2 = i_2_p103,
    prob_ascenso3 = i_3_p103,
    prob_ascenso4 = i_4_p103,
    prob_ascenso5 = i_5_p103,
    prob_ascenso6 = i_6_p103,
    prob_ascenso7_2022 = i_7_p103,
    conf_inst2 = i_1_p104,
    conf_inst3 = i_2_p104,
    conf_inst9 = i_3_p104,
    conf_inst7 = i_4_p104,
    conf_inst6 = i_5_p104,
    conf_inst5 = i_6_p104,
    conf_inst1 = i_7_p104,
    conf_inst17_2022 = i_8_p104,
    conf_inst11 = i_9_p104,
    op_inmigrantes1 = i_1_p105,
    op_inmigrantes3 = i_2_p105,
    moral_econ1 = p108a,
    moral_econ5 = p108b,
    moral_econ6 = p108c,
    moral_econ3 = p108d,
    expectativa_mov = p109,
    mov_inter1 = i_5_p110,
    mov_inter2 = i_1_p110,
    mov_inter3 = i_4_p110,
    mov_inter4 = i_2_p110,
    mov_inter5 = i_3_p110,
    op_violencia1 = p201,
    justificacion_violencia1 = i_1_p202,
    justificacion_violencia2 = i_2_p202,
    justificacion_violencia3 = i_3_p202,
    justificacion_violencia4 = i_4_p202,
    justificacion_violencia5 = i_5_p202,
    justificacion_violencia7 = i_1_p203,
    justificacion_violencia8 = i_2_p203,
    justificacion_violencia9 = i_3_p203,
    op_matr5 = i_1_p501,
    op_matr1 = i_2_p501,
    op_matr6 = i_3_p501,
    op_matr11 = i_4_p501,
    op_matr7 = i_5_p501,
    op_trabajo_mujer2 = i_1_p502,
    op_trabajo_mujer14 = i_3_p502,
    op_trabajo_mujer16 = i_4_p502,
    op_trabajo_mujer15 = i_5_p502,
    op_aborto = p503,
    religion_encuestado = i_1_p702,
    religion_pareja = i_2_p702,
    religion_madre = i_3_p702,
    religion_padre = i_4_p702,
    religion_hijo = i_5_p702,
    creencia_dios = p705,
    define_como = p706,
    frecuencia_rezar2 = p704,
    frecuencia_misa_enc = i_1_p703,
    frecuencia_misa_pareja = i_2_p703,
    frecuencia_misa_madre = i_3_p703,
    frecuencia_misa_padre = i_4_p703, 
    frecuencia_misa_hijo = i_5_p703,
    culto_virgen1 = i_1_p707,
    culto_virgen2 = i_2_p707,
    culto_virgen6 = i_3_p707, 
    encomendar_familiares = i_4_p707,
    encomendarse_santo = i_5_p707,
    creencia_reencarnacion = i_1_p708,
    creencia_energia = i_2_p708,
    creencia_astrologia = i_3_p708,
    creencia_yoga = i_4_p708,
    creencia_milagros = i_5_p708,
    creencia_karma = i_6_p708,
    creencia_milagros_virgen = i_7_p708,
    creencias_piedras = i_8_p708,
    creencia_jesucristo_hijo = i_9_p708,
    creencia_animitas = i_10_p708,
    creencia_resureccion = i_11_p708,
    creencia_meditacion = i_12_p708,
    creencia_naturaleza = i_13_p708,
    creencia_finmundo = i_14_p708,
    educ = i_1_cp01,
    educ_madre = i_2_cp01,
    educ_padre = i_3_cp01,
    sexo = sexo,
    ponderador = pond,
    tramoedad = cvar2,
    nse = nse_3,
    zona = zona,
    region = region,
    edad = edad,
    tipocomuna = tipo_comuna_bicen,
    estado_civil = cp05,
    id_comuna = folio_mapa,
    gse = cvar11,
    n_personas_hogar = cp13,
    iden_pol = p803,
    actividad_ppal3 = cp07,
    actividad_ppal4 = cp06
  )

# Para d2023
d2023 <- d2023 %>%
  rename(
    prob_ascenso1 = i_1_s3,
    prob_ascenso2 = i_2_s3,
    prob_ascenso3 = i_3_s3,
    prob_ascenso4 = i_4_s3,
    prob_ascenso5 = i_5_s3,
    prob_ascenso6 = i_6_s3,
    prob_ascenso7_2023 = i_7_s3,
    conf_inst2 = i_1_s4,
    conf_inst3 = i_2_s4,
    conf_inst9 = i_3_s4,
    conf_inst7 = i_4_s4,
    conf_inst6 = i_5_s4,
    conf_inst5 = i_6_s4,
    conf_inst1 = i_7_s4,
    conf_inst17_2023 = i_8_s4,
    conf_inst11 = i_9_s4,
    op_inmigrantes1 = i_1_s14,
    op_inmigrantes3 = i_2_s14,
    moral_econ1 = s6_1,
    moral_econ5 = s6_2,
    moral_econ6 = s6_3,
    moral_econ3 = s6_4,
    op_violencia = v29,
    namigos = namigos,
    nvecinos = s9,
    justificacion_violencia1 = i_1_v30,
    justificacion_violencia2 = i_2_v30,
    justificacion_violencia3 = i_3_v30,
    justificacion_violencia4 = i_4_v30,
    justificacion_violencia5 = i_5_v30,
    justificacion_violencia7 = i_1_v31,
    justificacion_violencia8 = i_2_v31,
    justificacion_violencia9 = i_3_v31,
    confianza1 = s11,
    op_matr5 = i_1_f38,
    op_matr1 = i_2_f38,
    op_matr6 = i_3_f38,
    op_matr11 = i_4_f38,
    op_matr7 = i_5_f38,
    op_aborto = f39,
    religion_encuestado = i_1_r47,
    religion_pareja = i_2_r47,
    religion_madre = i_3_r47,
    religion_padre = i_4_r47,
    religion_hijo = i_5_r47,
    religion_14años = i_6_r47,
    creencia_dios = r50,
    define_como = r51,
    frecuencia_rezar2 = r49,
    frecuencia_misa_enc = i_1_r48,
    frecuencia_misa_pareja = i_2_r48,
    frecuencia_misa_madre = i_3_r48,
    frecuencia_misa_padre = i_4_r48, 
    creencia_cat2 = i_1_r52,
    creencia_maldeojo = i_2_r52,
    creencia_infierno = i_3_r52,
    educ = i_1_cp04_bis,
    educ_madre = i_2_cp04_bis,
    educ_padre = i_3_cp04_bis,
    sexo = sexo,
    ponderador = POND,
    tramoedad = cvar2,
    nse = nse_3,
    region = region,
    edad = edad,
    tipocomuna = tipo_comuna_bicen,
    estado_civil = cp05,
    id_comuna = folio_mapa,
    gse = cvar11,
    n_personas_hogar = cp13,
    iden_pol = cp03
  )


# Para d2024
d2024 <- d2024 %>%
  rename(
    prob_ascenso1 = i_1_s3,
    prob_ascenso2 = i_2_s3,
    prob_ascenso3 = i_3_s3,
    prob_ascenso4 = i_4_s3,
    prob_ascenso5 = i_5_s3,
    prob_ascenso6 = i_6_s3,
    prob_ascenso7_2024 = i_7_s3,
    conf_inst2 = i_1_s4,
    conf_inst3 = i_2_s4,
    conf_inst9 = i_3_s4,
    conf_inst7 = i_4_s4,
    conf_inst6 = i_5_s4,
    conf_inst5 = i_6_s4,
    conf_inst1 = i_7_s4,
    conf_inst17_2024 = i_8_s4,
    conf_inst11 = i_9_s4,
    op_inmigrantes1 = i_1_s11,
    op_inmigrantes2 = i_2_s11,
    moral_econ1 = s5_1,
    moral_econ5 = s5_2,
    moral_econ6 = s5_3,
    moral_econ3 = s5_4,
    op_violencia1 = s6,
    op_matr5 = i_1_f34,
    op_matr1 = i_2_f34,
    op_matr6 = i_3_f34,
    op_matr11 = i_4_f34,
    op_matr7 = i_5_f34,
    op_aborto = f35,
    nhijos = f37,
    ideal_hijos = f39,
    religion_encuestado = r44_a,
    religion_pareja = i_1_r44,
    religion_madre = i_2_r44,
    religion_padre = i_3_r44,
    religion_hijo = i_4_r44,
    religion_14años = i_5_r44,
    creencia_dios = r47,
    define_como = r48,
    frecuencia_rezar2 = r46,
    op_religion2 = i_1_r51,
    creencia_fe_siniglesia = r50,
    frecuencia_misa_enc = i_1_r45,
    frecuencia_misa_pareja = i_2_r45,
    frecuencia_misa_madre = i_3_r45,
    frecuencia_misa_padre = i_4_r45, 
    op_religion10 = i_2_r51,
    op_religion4 = i_3_r51,
    op_religion12 = i_4_r51,
    conoce_cura = r52,
    confianza_parroquia = r53,
    educ = i_1_cp04_bis,
    educ_madre = i_2_cp04_bis,
    educ_padre = i_3_cp04_bis,
    sexo = sexo,
    ponderador = POND,
    tramoedad = cvar2,
    nse = nse_3,
    region = region,
    comuna = comuna,
    zona = zona,
    edad = edad,
    tipocomuna = tipo_comuna_bicen,
    estado_civil = cp08,
    id_comuna = folio_mapa,
    gse = cvar11,
    n_personas_hogar = cp14,
    iden_pol = cp03,
    actividad_ppal3 = cp07
  )

#2021

d2021 <- d2021 %>%
  mutate(folio_unico = seq(28275, 30276, by = 1))

d2021 <- d2021 %>%
  mutate(folio = 1:n())

d2021 <- d2021 %>%
  mutate(ano = 2021)

d2021 <- d2021 %>%
  select(folio_unico, ano, folio, prob_ascenso1,
         prob_ascenso2,
         prob_ascenso3,
         prob_ascenso4,
         prob_ascenso5,
         prob_ascenso6,
         prob_ascenso7_2021,
         conf_inst2,
         conf_inst3,
         conf_inst9,
         conf_inst7,
         conf_inst6,
         conf_inst5,
         conf_inst1,
         conf_inst17_2021,
         op_inmigrantes1,
         op_inmigrantes3,
         moral_econ1,
         moral_econ5,
         moral_econ6,
         moral_econ3,
         expectativa_mov,
         mov_inter1,
         mov_inter2,
         mov_inter3,
         mov_inter4,
         mov_inter5,
         op_violencia1,
         justificacion_violencia1,
         justificacion_violencia2,
         justificacion_violencia3,
         justificacion_violencia4,
         justificacion_violencia5,
         justificacion_violencia7,
         justificacion_violencia8,
         justificacion_violencia9,
         pp_indispensables,
         op_chile2,
         op_chile4,
         op_chile6,
         op_chile5,
         op_matr5,
         op_matr1,
         op_matr6,
         op_matr11,
         op_matr7,
         op_trabajo_mujer2,
         op_trabajo_mujer14,
         op_trabajo_mujer16,
         op_trabajo_mujer15,
         op_aborto,
         religion_encuestado,
         religion_pareja,
         religion_madre,
         religion_padre,
         creencia_dios,
         define_como,
         frecuencia_rezar2,
         conf_inst4,
         confianza_obispos,
         confianza_sacerdotes,
         creencia_personas_justas,
         creencia_fe_siniglesia,
         seguir_enseñanzas,
         consejo_sacerdote,
         abusos_pasado,
         abusos_comunes_iglesia,
         afectados_abusos_sacerdotes,
         educ,
         educ_madre,
         educ_padre,
         sexo,
         ponderador,
         tramoedad,
         gse,
         zona,
         region,
         edad,
         comuna,
         tipocomuna,
         estado_civil,
         id_comuna,
         n_personas_hogar,
         actividad_ppal3,
         actividad_ppal4)

nuevas_filas <- data.frame(
  folio_unico = seq(28275, 30276),  # Generar folio_unico para 2021 (2002 casos)
  ano = rep(2021, 2002),
  folio = seq(1, 2002))

data_filtrada <- bind_rows(data_filtrada, nuevas_filas)

data_filtrada <- data_filtrada %>%
  left_join(d2021, by = c("folio_unico", "ano", "folio"))
table(data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    prob_ascenso1 = coalesce(prob_ascenso1.x, prob_ascenso1.y),
    prob_ascenso2 = coalesce(prob_ascenso2.x, prob_ascenso2.y),
    prob_ascenso3 = coalesce(prob_ascenso3.x, prob_ascenso3.y),
    prob_ascenso4 = coalesce(prob_ascenso4.x, prob_ascenso4.y),
    prob_ascenso5 = coalesce(prob_ascenso5.x, prob_ascenso5.y),
    prob_ascenso6 = coalesce(prob_ascenso6.x, prob_ascenso6.y),
    conf_inst2 = coalesce(conf_inst2.x, conf_inst2.y),
    conf_inst3 = coalesce(conf_inst3.x, conf_inst3.y),
    conf_inst9 = coalesce(conf_inst9.x, conf_inst9.y),
    conf_inst7 = coalesce(conf_inst7.x, conf_inst7.y),
    conf_inst6 = coalesce(conf_inst6.x, conf_inst6.y),
    conf_inst5 = coalesce(conf_inst5.x, conf_inst5.y),
    conf_inst1 = coalesce(conf_inst1.x, conf_inst1.y),
    op_inmigrantes1 = coalesce(op_inmigrantes1.x, op_inmigrantes1.y),
    op_inmigrantes3 = coalesce(op_inmigrantes3.x, op_inmigrantes3.y),
    moral_econ1 = coalesce(moral_econ1.x, moral_econ1.y),
    moral_econ5 = coalesce(moral_econ5.x, moral_econ5.y),
    moral_econ6 = coalesce(moral_econ6.x, moral_econ6.y),
    moral_econ3 = coalesce(moral_econ3.x, moral_econ3.y),
    expectativa_mov = coalesce(expectativa_mov.x, expectativa_mov.y),
    mov_inter1 = coalesce(mov_inter1.x, mov_inter1.y),
    mov_inter2 = coalesce(mov_inter2.x, mov_inter2.y),
    mov_inter3 = coalesce(mov_inter3.x, mov_inter3.y),
    mov_inter4 = coalesce(mov_inter4.x, mov_inter4.y),
    mov_inter5 = coalesce(mov_inter5.x, mov_inter5.y),
    op_violencia1 = coalesce(op_violencia1.x, op_violencia1.y),
    justificacion_violencia1 = coalesce(justificacion_violencia1.x, justificacion_violencia1.y),
    justificacion_violencia2 = coalesce(justificacion_violencia2.x, justificacion_violencia2.y),
    justificacion_violencia3 = coalesce(justificacion_violencia3.x, justificacion_violencia3.y),
    justificacion_violencia4 = coalesce(justificacion_violencia4.x, justificacion_violencia4.y),
    justificacion_violencia5 = coalesce(justificacion_violencia5.x, justificacion_violencia5.y),
    justificacion_violencia7 = coalesce(justificacion_violencia7.x, justificacion_violencia7.y),
    justificacion_violencia8 = coalesce(justificacion_violencia8.x, justificacion_violencia8.y),
    justificacion_violencia9 = coalesce(justificacion_violencia9.x, justificacion_violencia9.y),
    pp_indispensables = coalesce(pp_indispensables.x, pp_indispensables.y),
    op_chile2 = coalesce(op_chile2.x, op_chile2.y),
    op_chile4 = coalesce(op_chile4.x, op_chile4.y),
    op_chile6 = coalesce(op_chile6.x, op_chile6.y),
    op_chile5 = coalesce(op_chile5.x, op_chile5.y),
    op_matr5 = coalesce(op_matr5.x, op_matr5.y),
    op_matr1 = coalesce(op_matr1.x, op_matr1.y),
    op_matr6 = coalesce(op_matr6.x, op_matr6.y),
    op_matr11 = coalesce(op_matr11.x, op_matr11.y),
    op_matr7 = coalesce(op_matr7.x, op_matr7.y),
    op_trabajo_mujer2 = coalesce(op_trabajo_mujer2.x, op_trabajo_mujer2.y),
    op_trabajo_mujer14 = coalesce(op_trabajo_mujer14.x, op_trabajo_mujer14.y),
    op_trabajo_mujer16 = coalesce(op_trabajo_mujer16.x, op_trabajo_mujer16.y),
    op_trabajo_mujer15 = coalesce(op_trabajo_mujer15.x, op_trabajo_mujer15.y),
    op_aborto = coalesce(op_aborto.x, op_aborto.y),
    religion_encuestado = coalesce(religion_encuestado.x, religion_encuestado.y),
    religion_pareja = coalesce(religion_pareja.x, religion_pareja.y),
    religion_madre = coalesce(religion_madre.x, religion_madre.y),
    religion_padre = coalesce(religion_padre.x, religion_padre.y),
    creencia_dios = coalesce(creencia_dios.x, creencia_dios.y),
    define_como = coalesce(define_como.x, define_como.y),
    frecuencia_rezar2 = coalesce(frecuencia_rezar2.x, frecuencia_rezar2.y),
    conf_inst4 = coalesce(conf_inst4.x, conf_inst4.y),
    confianza_obispos = coalesce(confianza_obispos.x, confianza_obispos.y),
    confianza_sacerdotes = coalesce(confianza_sacerdotes.x, confianza_sacerdotes.y),
    creencia_personas_justas = coalesce(creencia_personas_justas.x, creencia_personas_justas.y),
    creencia_fe_siniglesia = coalesce(creencia_fe_siniglesia.x, creencia_fe_siniglesia.y),
    seguir_enseñanzas = coalesce(seguir_enseñanzas.x, seguir_enseñanzas.y),
    consejo_sacerdote = coalesce(consejo_sacerdote.x, consejo_sacerdote.y),
    abusos_comunes_iglesia = coalesce(abusos_comunes_iglesia.x, abusos_comunes_iglesia.y),
    afectados_abusos_sacerdotes = coalesce(afectados_abusos_sacerdotes.x, afectados_abusos_sacerdotes.y),
    educ = coalesce(educ.x, educ.y),
    educ_madre = coalesce(educ_madre.x, educ_madre.y),
    educ_padre = coalesce(educ_padre.x, educ_padre.y),
    sexo = coalesce(sexo.x, sexo.y),
    ponderador = coalesce(ponderador.x, ponderador.y),
    tramoedad = coalesce(tramoedad.x, tramoedad.y),
    gse = coalesce(gse.x, gse.y),
    zona = coalesce(zona.x, zona.y),
    region = coalesce(region.x, region.y),
    edad = coalesce(edad.x, edad.y),
    comuna = coalesce(comuna.x, comuna.y),
    tipocomuna = coalesce(tipocomuna.x, tipocomuna.y),
    estado_civil = coalesce(estado_civil.x, estado_civil.y),
    id_comuna = coalesce(id_comuna.x, id_comuna.y),
    n_personas_hogar = coalesce(n_personas_hogar.x, n_personas_hogar.y),
    actividad_ppal3 = coalesce(actividad_ppal3.x, actividad_ppal3.y),
    actividad_ppal4 = coalesce(actividad_ppal4.x, actividad_ppal4.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))  # Elimina las columnas con sufijos

table(data_filtrada$prob_ascenso4, data_filtrada$ano)

#2022

d2022 <- d2022 %>%
  mutate(folio_unico = seq(30276, 32181, by = 1))

d2022 <- d2022 %>%
  mutate(folio = 1:n())

d2022 <- d2022 %>%
  mutate(ano = 2022)

d2022 <- d2022 %>%
  select(
    folio_unico, ano, folio,
    prob_ascenso1, prob_ascenso2, prob_ascenso3, prob_ascenso4, prob_ascenso5, prob_ascenso6, prob_ascenso7_2022,
    conf_inst2, conf_inst3, conf_inst9, conf_inst7, conf_inst6, conf_inst5, conf_inst1, conf_inst17_2022, conf_inst11,
    op_inmigrantes1, op_inmigrantes3,
    moral_econ1, moral_econ5, moral_econ6, moral_econ3,
    expectativa_mov,
    mov_inter1, mov_inter2, mov_inter3, mov_inter4, mov_inter5,
    op_violencia1,
    justificacion_violencia1, justificacion_violencia2, justificacion_violencia3, justificacion_violencia4, justificacion_violencia5,
    justificacion_violencia7, justificacion_violencia8, justificacion_violencia9,
    op_matr5, op_matr1, op_matr6, op_matr11, op_matr7,
    op_trabajo_mujer2, op_trabajo_mujer14, op_trabajo_mujer16, op_trabajo_mujer15,
    op_aborto,
    religion_encuestado, religion_pareja, religion_madre, religion_padre, religion_hijo,
    creencia_dios, define_como, frecuencia_rezar2,
    frecuencia_misa_enc, frecuencia_misa_pareja, frecuencia_misa_madre, frecuencia_misa_padre, frecuencia_misa_hijo,
    culto_virgen1, culto_virgen2, culto_virgen6, encomendar_familiares, encomendarse_santo,
    creencia_reencarnacion, creencia_energia, creencia_astrologia, creencia_yoga, creencia_milagros, creencia_karma,
    creencia_milagros_virgen, creencias_piedras, creencia_jesucristo_hijo, creencia_animitas, creencia_resureccion,
    creencia_meditacion, creencia_naturaleza, creencia_finmundo,
    educ, educ_madre, educ_padre,
    sexo, ponderador, tramoedad, nse, zona, region, edad, tipocomuna, estado_civil, id_comuna, gse, n_personas_hogar,
    iden_pol, actividad_ppal3, actividad_ppal4
  )

nuevas_filas <- data.frame(
  folio_unico = seq(30276, 32181),  # Generar folio_unico para 2021 (2002 casos)
  ano = rep(2022, 1906),
  folio = seq(1, 1906))

data_filtrada <- bind_rows(data_filtrada, nuevas_filas)

data_filtrada <- data_filtrada %>%
  left_join(d2022, by = c("folio_unico", "ano", "folio"))
table(data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    prob_ascenso1 = coalesce(prob_ascenso1.x, prob_ascenso1.y),
    prob_ascenso2 = coalesce(prob_ascenso2.x, prob_ascenso2.y),
    prob_ascenso3 = coalesce(prob_ascenso3.x, prob_ascenso3.y),
    prob_ascenso4 = coalesce(prob_ascenso4.x, prob_ascenso4.y),
    prob_ascenso5 = coalesce(prob_ascenso5.x, prob_ascenso5.y),
    prob_ascenso6 = coalesce(prob_ascenso6.x, prob_ascenso6.y),
    conf_inst2 = coalesce(conf_inst2.x, conf_inst2.y),
    conf_inst3 = coalesce(conf_inst3.x, conf_inst3.y),
    conf_inst9 = coalesce(conf_inst9.x, conf_inst9.y),
    conf_inst7 = coalesce(conf_inst7.x, conf_inst7.y),
    conf_inst6 = coalesce(conf_inst6.x, conf_inst6.y),
    conf_inst5 = coalesce(conf_inst5.x, conf_inst5.y),
    conf_inst1 = coalesce(conf_inst1.x, conf_inst1.y),
    conf_inst11 = coalesce(conf_inst11.x, conf_inst11.y),
    op_inmigrantes1 = coalesce(op_inmigrantes1.x, op_inmigrantes1.y),
    op_inmigrantes3 = coalesce(op_inmigrantes3.x, op_inmigrantes3.y),
    moral_econ1 = coalesce(moral_econ1.x, moral_econ1.y),
    moral_econ5 = coalesce(moral_econ5.x, moral_econ5.y),
    moral_econ6 = coalesce(moral_econ6.x, moral_econ6.y),
    moral_econ3 = coalesce(moral_econ3.x, moral_econ3.y),
    expectativa_mov = coalesce(expectativa_mov.x, expectativa_mov.y),
    mov_inter1 = coalesce(mov_inter1.x, mov_inter1.y),
    mov_inter2 = coalesce(mov_inter2.x, mov_inter2.y),
    mov_inter3 = coalesce(mov_inter3.x, mov_inter3.y),
    mov_inter4 = coalesce(mov_inter4.x, mov_inter4.y),
    mov_inter5 = coalesce(mov_inter5.x, mov_inter5.y),
    op_violencia1 = coalesce(op_violencia1.x, op_violencia1.y),
    justificacion_violencia1 = coalesce(justificacion_violencia1.x, justificacion_violencia1.y),
    justificacion_violencia2 = coalesce(justificacion_violencia2.x, justificacion_violencia2.y),
    justificacion_violencia3 = coalesce(justificacion_violencia3.x, justificacion_violencia3.y),
    justificacion_violencia4 = coalesce(justificacion_violencia4.x, justificacion_violencia4.y),
    justificacion_violencia5 = coalesce(justificacion_violencia5.x, justificacion_violencia5.y),
    justificacion_violencia7 = coalesce(justificacion_violencia7.x, justificacion_violencia7.y),
    justificacion_violencia8 = coalesce(justificacion_violencia8.x, justificacion_violencia8.y),
    justificacion_violencia9 = coalesce(justificacion_violencia9.x, justificacion_violencia9.y),
    op_matr5 = coalesce(op_matr5.x, op_matr5.y),
    op_matr1 = coalesce(op_matr1.x, op_matr1.y),
    op_matr6 = coalesce(op_matr6.x, op_matr6.y),
    op_matr11 = coalesce(op_matr11.x, op_matr11.y),
    op_matr7 = coalesce(op_matr7.x, op_matr7.y),
    op_trabajo_mujer2 = coalesce(op_trabajo_mujer2.x, op_trabajo_mujer2.y),
    op_trabajo_mujer14 = coalesce(op_trabajo_mujer14.x, op_trabajo_mujer14.y),
    op_trabajo_mujer16 = coalesce(op_trabajo_mujer16.x, op_trabajo_mujer16.y),
    op_trabajo_mujer15 = coalesce(op_trabajo_mujer15.x, op_trabajo_mujer15.y),
    op_aborto = coalesce(op_aborto.x, op_aborto.y),
    religion_encuestado = coalesce(religion_encuestado.x, religion_encuestado.y),
    religion_pareja = coalesce(religion_pareja.x, religion_pareja.y),
    religion_madre = coalesce(religion_madre.x, religion_madre.y),
    religion_padre = coalesce(religion_padre.x, religion_padre.y),
    religion_hijo = coalesce(religion_hijo.x, religion_hijo.y),
    creencia_dios = coalesce(creencia_dios.x, creencia_dios.y),
    define_como = coalesce(define_como.x, define_como.y),
    frecuencia_rezar2 = coalesce(frecuencia_rezar2.x, frecuencia_rezar2.y),
    frecuencia_misa_enc = coalesce(frecuencia_misa_enc.x, frecuencia_misa_enc.y),
    frecuencia_misa_pareja = coalesce(frecuencia_misa_pareja.x, frecuencia_misa_pareja.y),
    frecuencia_misa_madre = coalesce(frecuencia_misa_madre.x, frecuencia_misa_madre.y),
    frecuencia_misa_padre = coalesce(frecuencia_misa_padre.x, frecuencia_misa_padre.y),
    frecuencia_misa_hijo = coalesce(frecuencia_misa_hijo.x, frecuencia_misa_hijo.y),
    culto_virgen1 = coalesce(culto_virgen1.x, culto_virgen1.y),
    culto_virgen2 = coalesce(culto_virgen2.x, culto_virgen2.y),
    culto_virgen6 = coalesce(culto_virgen6.x, culto_virgen6.y),
    encomendar_familiares = coalesce(encomendar_familiares.x, encomendar_familiares.y),
    encomendarse_santo = coalesce(encomendarse_santo.x, encomendarse_santo.y),
    creencia_reencarnacion = coalesce(creencia_reencarnacion.x, creencia_reencarnacion.y),
    creencia_energia = coalesce(creencia_energia.x, creencia_energia.y),
    creencia_astrologia = coalesce(creencia_astrologia.x, creencia_astrologia.y),
    creencia_yoga = coalesce(creencia_yoga.x, creencia_yoga.y),
    creencia_milagros = coalesce(creencia_milagros.x, creencia_milagros.y),
    creencia_karma = coalesce(creencia_karma.x, creencia_karma.y),
    creencia_milagros_virgen = coalesce(creencia_milagros_virgen.x, creencia_milagros_virgen.y),
    creencia_jesucristo_hijo = coalesce(creencia_jesucristo_hijo.x, creencia_jesucristo_hijo.y),
    educ = coalesce(educ.x, educ.y),
    educ_madre = coalesce(educ_madre.x, educ_madre.y),
    educ_padre = coalesce(educ_padre.x, educ_padre.y),
    sexo = coalesce(sexo.x, sexo.y),
    ponderador = coalesce(ponderador.x, ponderador.y),
    tramoedad = coalesce(tramoedad.x, tramoedad.y),
    nse = coalesce(nse.x, nse.y),
    zona = coalesce(zona.x, zona.y),
    region = coalesce(as.character(region.x), as.character(region.y)),
    edad = coalesce(edad.x, edad.y),
    tipocomuna = coalesce(tipocomuna.x, tipocomuna.y),
    estado_civil = coalesce(estado_civil.x, estado_civil.y),
    id_comuna = coalesce(id_comuna.x, id_comuna.y),
    gse = coalesce(gse.x, gse.y),
    n_personas_hogar = coalesce(n_personas_hogar.x, n_personas_hogar.y),
    iden_pol = coalesce(iden_pol.x, iden_pol.y),
    actividad_ppal3 = coalesce(actividad_ppal3.x, actividad_ppal3.y),
    actividad_ppal4 = coalesce(actividad_ppal4.x, actividad_ppal4.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))
table(data_filtrada$prob_ascenso4, data_filtrada$ano)

#2023

d2023 <- d2023 %>%
  mutate(folio_unico = seq(32181, 33755, by = 1))

d2023 <- d2023 %>%
  mutate(folio = 1:n())

d2023 <- d2023 %>%
  mutate(ano = 2023)

d2023 <- d2023 %>%
  select(
    folio_unico, folio, ano,
    prob_ascenso1, prob_ascenso2, prob_ascenso3, prob_ascenso4, prob_ascenso5, prob_ascenso6, prob_ascenso7_2023,
    conf_inst2, conf_inst3, conf_inst9, conf_inst7, conf_inst6, conf_inst5, conf_inst1, conf_inst17_2023, conf_inst11,
    op_inmigrantes1, op_inmigrantes3, moral_econ1, moral_econ5, moral_econ6, moral_econ3, op_violencia,
    namigos, nvecinos, justificacion_violencia1, justificacion_violencia2, justificacion_violencia3,
    justificacion_violencia4, justificacion_violencia5, justificacion_violencia7, justificacion_violencia8,
    justificacion_violencia9, confianza1, op_matr5, op_matr1, op_matr6, op_matr11, op_matr7, op_aborto,
    religion_encuestado, religion_pareja, religion_madre, religion_padre, religion_hijo, religion_14años,
    creencia_dios, define_como, frecuencia_rezar2, frecuencia_misa_enc, frecuencia_misa_pareja,
    frecuencia_misa_madre, frecuencia_misa_padre, creencia_cat2, creencia_maldeojo, creencia_infierno,
    educ, educ_madre, educ_padre, sexo, ponderador, tramoedad, nse, region, edad, tipocomuna, estado_civil,
    id_comuna, gse, n_personas_hogar, iden_pol
  )

nuevas_filas <- data.frame(
  folio_unico = seq(32181, 33755),  # Generar folio_unico para 2021 (2002 casos)
  ano = rep(2023, 1575),
  folio = seq(1, 1575))

data_filtrada <- bind_rows(data_filtrada, nuevas_filas)

data_filtrada <- data_filtrada %>%
  left_join(d2023, by = c("folio_unico", "ano", "folio"))
table(data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    prob_ascenso1 = coalesce(prob_ascenso1.x, prob_ascenso1.y),
    prob_ascenso2 = coalesce(prob_ascenso2.x, prob_ascenso2.y),
    prob_ascenso3 = coalesce(prob_ascenso3.x, prob_ascenso3.y),
    prob_ascenso4 = coalesce(prob_ascenso4.x, prob_ascenso4.y),
    prob_ascenso5 = coalesce(prob_ascenso5.x, prob_ascenso5.y),
    prob_ascenso6 = coalesce(prob_ascenso6.x, prob_ascenso6.y),
    conf_inst2 = coalesce(conf_inst2.x, conf_inst2.y),
    conf_inst3 = coalesce(conf_inst3.x, conf_inst3.y),
    conf_inst9 = coalesce(conf_inst9.x, conf_inst9.y),
    conf_inst7 = coalesce(conf_inst7.x, conf_inst7.y),
    conf_inst6 = coalesce(conf_inst6.x, conf_inst6.y),
    conf_inst5 = coalesce(conf_inst5.x, conf_inst5.y),
    conf_inst1 = coalesce(conf_inst1.x, conf_inst1.y),
    conf_inst11 = coalesce(conf_inst11.x, conf_inst11.y),
    op_inmigrantes1 = coalesce(op_inmigrantes1.x, op_inmigrantes1.y),
    op_inmigrantes3 = coalesce(op_inmigrantes3.x, op_inmigrantes3.y),
    moral_econ1 = coalesce(moral_econ1.x, moral_econ1.y),
    moral_econ5 = coalesce(moral_econ5.x, moral_econ5.y),
    moral_econ6 = coalesce(moral_econ6.x, moral_econ6.y),
    moral_econ3 = coalesce(moral_econ3.x, moral_econ3.y),
    namigos = coalesce(namigos.x, namigos.y),
    nvecinos = coalesce(nvecinos.x, nvecinos.y),
    justificacion_violencia1 = coalesce(justificacion_violencia1.x, justificacion_violencia1.y),
    justificacion_violencia2 = coalesce(justificacion_violencia2.x, justificacion_violencia2.y),
    justificacion_violencia3 = coalesce(justificacion_violencia3.x, justificacion_violencia3.y),
    justificacion_violencia4 = coalesce(justificacion_violencia4.x, justificacion_violencia4.y),
    justificacion_violencia5 = coalesce(justificacion_violencia5.x, justificacion_violencia5.y),
    justificacion_violencia7 = coalesce(justificacion_violencia7.x, justificacion_violencia7.y),
    justificacion_violencia8 = coalesce(justificacion_violencia8.x, justificacion_violencia8.y),
    justificacion_violencia9 = coalesce(justificacion_violencia9.x, justificacion_violencia9.y),
    confianza1 = coalesce(confianza1.x, confianza1.y),
    op_matr5 = coalesce(op_matr5.x, op_matr5.y),
    op_matr1 = coalesce(op_matr1.x, op_matr1.y),
    op_matr6 = coalesce(op_matr6.x, op_matr6.y),
    op_matr11 = coalesce(op_matr11.x, op_matr11.y),
    op_matr7 = coalesce(op_matr7.x, op_matr7.y),
    op_aborto = coalesce(op_aborto.x, op_aborto.y),
    religion_encuestado = coalesce(religion_encuestado.x, religion_encuestado.y),
    religion_pareja = coalesce(religion_pareja.x, religion_pareja.y),
    religion_madre = coalesce(religion_madre.x, religion_madre.y),
    religion_padre = coalesce(religion_padre.x, religion_padre.y),
    religion_hijo = coalesce(religion_hijo.x, religion_hijo.y),
    religion_14años = coalesce(religion_14años.x, religion_14años.y),
    creencia_dios = coalesce(creencia_dios.x, creencia_dios.y),
    define_como = coalesce(define_como.x, define_como.y),
    frecuencia_rezar2 = coalesce(frecuencia_rezar2.x, frecuencia_rezar2.y),
    frecuencia_misa_enc = coalesce(frecuencia_misa_enc.x, frecuencia_misa_enc.y),
    frecuencia_misa_pareja = coalesce(frecuencia_misa_pareja.x, frecuencia_misa_pareja.y),
    frecuencia_misa_madre = coalesce(frecuencia_misa_madre.x, frecuencia_misa_madre.y),
    frecuencia_misa_padre = coalesce(frecuencia_misa_padre.x, frecuencia_misa_padre.y),
    creencia_cat2 = coalesce(creencia_cat2.x, creencia_cat2.y),
    creencia_maldeojo = coalesce(creencia_maldeojo.x, creencia_maldeojo.y),
    creencia_infierno = coalesce(creencia_infierno.x, creencia_infierno.y),
    educ = coalesce(educ.x, educ.y),
    educ_madre = coalesce(educ_madre.x, educ_madre.y),
    educ_padre = coalesce(educ_padre.x, educ_padre.y),
    sexo = coalesce(sexo.x, sexo.y),
    ponderador = coalesce(ponderador.x, ponderador.y),
    tramoedad = coalesce(tramoedad.x, tramoedad.y),
    nse = coalesce(nse.x, nse.y),
    region = coalesce(as.character(region.x), as.character(region.y)),
    edad = coalesce(edad.x, edad.y),
    tipocomuna = coalesce(tipocomuna.x, tipocomuna.y),
    estado_civil = coalesce(estado_civil.x, estado_civil.y),
    id_comuna = coalesce(id_comuna.x, id_comuna.y),
    gse = coalesce(gse.x, gse.y),
    n_personas_hogar = coalesce(n_personas_hogar.x, n_personas_hogar.y),
    iden_pol = coalesce(iden_pol.x, iden_pol.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))


#2024

d2024 <- d2024 %>%
  mutate(folio_unico = seq(33756, 35393, by = 1))

d2024 <- d2024 %>%
  mutate(folio = 1:n())

d2024 <- d2024 %>%
  mutate(ano = 2024)

d2024 <- d2024 %>%
  select(
    folio_unico, ano, folio,
    prob_ascenso1, prob_ascenso2, prob_ascenso3, prob_ascenso4, prob_ascenso5, prob_ascenso6, prob_ascenso7_2024,
    conf_inst2, conf_inst3, conf_inst9, conf_inst7, conf_inst6, conf_inst5, conf_inst1, conf_inst17_2024, conf_inst11,
    op_inmigrantes1, op_inmigrantes2, moral_econ1, moral_econ5, moral_econ6, moral_econ3, op_violencia1,
    op_matr5, op_matr1, op_matr6, op_matr11, op_matr7, op_aborto, nhijos, ideal_hijos,
    religion_encuestado, religion_pareja, religion_madre, religion_padre, religion_hijo, religion_14años,
    creencia_dios, define_como, frecuencia_rezar2, op_religion2, creencia_fe_siniglesia,
    frecuencia_misa_enc, frecuencia_misa_pareja, frecuencia_misa_madre, frecuencia_misa_padre,
    op_religion10, op_religion4, op_religion12, conoce_cura, confianza_parroquia,
    educ, educ_madre, educ_padre, sexo, ponderador, tramoedad, nse, region, comuna, zona, edad,
    tipocomuna, estado_civil, id_comuna, gse, n_personas_hogar, iden_pol, actividad_ppal3
  )

nuevas_filas <- data.frame(
  folio_unico = seq(33756, 35393),  # Generar folio_unico para 2021 (2002 casos)
  ano = rep(2024, 1638),
  folio = seq(1, 1638))

data_filtrada <- bind_rows(data_filtrada, nuevas_filas)

data_filtrada <- data_filtrada %>%
  left_join(d2024, by = c("folio_unico", "ano", "folio"))
table(data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    prob_ascenso1 = coalesce(prob_ascenso1.x, prob_ascenso1.y),
    prob_ascenso2 = coalesce(prob_ascenso2.x, prob_ascenso2.y),
    prob_ascenso3 = coalesce(prob_ascenso3.x, prob_ascenso3.y),
    prob_ascenso4 = coalesce(prob_ascenso4.x, prob_ascenso4.y),
    prob_ascenso5 = coalesce(prob_ascenso5.x, prob_ascenso5.y),
    prob_ascenso6 = coalesce(prob_ascenso6.x, prob_ascenso6.y),
    conf_inst2 = coalesce(conf_inst2.x, conf_inst2.y),
    conf_inst3 = coalesce(conf_inst3.x, conf_inst3.y),
    conf_inst9 = coalesce(conf_inst9.x, conf_inst9.y),
    conf_inst7 = coalesce(conf_inst7.x, conf_inst7.y),
    conf_inst6 = coalesce(conf_inst6.x, conf_inst6.y),
    conf_inst5 = coalesce(conf_inst5.x, conf_inst5.y),
    conf_inst1 = coalesce(conf_inst1.x, conf_inst1.y),
    conf_inst11 = coalesce(conf_inst11.x, conf_inst11.y),
    op_inmigrantes1 = coalesce(op_inmigrantes1.x, op_inmigrantes1.y),
    op_inmigrantes2 = coalesce(op_inmigrantes2.x, op_inmigrantes2.y),
    moral_econ1 = coalesce(moral_econ1.x, moral_econ1.y),
    moral_econ5 = coalesce(moral_econ5.x, moral_econ5.y),
    moral_econ6 = coalesce(moral_econ6.x, moral_econ6.y),
    moral_econ3 = coalesce(moral_econ3.x, moral_econ3.y),
    op_violencia1 = coalesce(op_violencia1.x, op_violencia1.y),
    op_matr5 = coalesce(op_matr5.x, op_matr5.y),
    op_matr1 = coalesce(op_matr1.x, op_matr1.y),
    op_matr6 = coalesce(op_matr6.x, op_matr6.y),
    op_matr11 = coalesce(op_matr11.x, op_matr11.y),
    op_matr7 = coalesce(op_matr7.x, op_matr7.y),
    op_aborto = coalesce(op_aborto.x, op_aborto.y),
    nhijos = coalesce(nhijos.x, nhijos.y),
    ideal_hijos = coalesce(ideal_hijos.x, ideal_hijos.y),
    religion_encuestado = coalesce(religion_encuestado.x, religion_encuestado.y),
    religion_pareja = coalesce(religion_pareja.x, religion_pareja.y),
    religion_madre = coalesce(religion_madre.x, religion_madre.y),
    religion_padre = coalesce(religion_padre.x, religion_padre.y),
    religion_hijo = coalesce(religion_hijo.x, religion_hijo.y),
    religion_14años = coalesce(religion_14años.x, religion_14años.y),
    creencia_dios = coalesce(creencia_dios.x, creencia_dios.y),
    define_como = coalesce(define_como.x, define_como.y),
    frecuencia_rezar2 = coalesce(frecuencia_rezar2.x, frecuencia_rezar2.y),
    op_religion2 = coalesce(op_religion2.x, op_religion2.y),
    creencia_fe_siniglesia = coalesce(creencia_fe_siniglesia.x, creencia_fe_siniglesia.y),
    frecuencia_misa_enc = coalesce(frecuencia_misa_enc.x, frecuencia_misa_enc.y),
    frecuencia_misa_pareja = coalesce(frecuencia_misa_pareja.x, frecuencia_misa_pareja.y),
    frecuencia_misa_madre = coalesce(frecuencia_misa_madre.x, frecuencia_misa_madre.y),
    frecuencia_misa_padre = coalesce(frecuencia_misa_padre.x, frecuencia_misa_padre.y),
    op_religion10 = coalesce(op_religion10.x, op_religion10.y),
    op_religion4 = coalesce(op_religion4.x, op_religion4.y),
    op_religion12 = coalesce(op_religion12.x, op_religion12.y),
    conoce_cura = coalesce(conoce_cura.x, conoce_cura.y),
    confianza_parroquia = coalesce(confianza_parroquia.x, confianza_parroquia.y),
    educ = coalesce(educ.x, educ.y),
    educ_madre = coalesce(educ_madre.x, educ_madre.y),
    educ_padre = coalesce(educ_padre.x, educ_padre.y),
    sexo = coalesce(sexo.x, sexo.y),
    ponderador = coalesce(ponderador.x, ponderador.y),
    tramoedad = coalesce(tramoedad.x, tramoedad.y),
    nse = coalesce(nse.x, nse.y),
    region = coalesce(as.character(region.x), as.character(region.y)),
    comuna = coalesce(as.character(comuna.x), as.character(comuna.y)),
    zona = coalesce(zona.x, zona.y),
    edad = coalesce(edad.x, edad.y),
    tipocomuna = coalesce(tipocomuna.x, tipocomuna.y),
    estado_civil = coalesce(estado_civil.x, estado_civil.y),
    id_comuna = coalesce(id_comuna.x, id_comuna.y),
    gse = coalesce(gse.x, gse.y),
    n_personas_hogar = coalesce(n_personas_hogar.x, n_personas_hogar.y),
    iden_pol = coalesce(iden_pol.x, iden_pol.y),
    actividad_ppal3 = coalesce(actividad_ppal3.x, actividad_ppal3.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

data_filtrada <- data_filtrada %>%
  mutate(conf_inst17 = coalesce(conf_inst17_2021, conf_inst17_2022, conf_inst17_2023, conf_inst17_2024)) %>%
  select(-conf_inst17_2021, -conf_inst17_2022, -conf_inst17_2023, -conf_inst17_2024)
table(data_filtrada$conf_inst17, data_filtrada$ano)
data_filtrada <- data_filtrada %>%
  mutate(prob_ascenso7 = coalesce(prob_ascenso7_2021, prob_ascenso7_2022, prob_ascenso7_2023, prob_ascenso7_2024)) %>%
  select(-prob_ascenso7_2021, -prob_ascenso7_2022, -prob_ascenso7_2023, -prob_ascenso7_2024)
table(data_filtrada$prob_ascenso7, data_filtrada$ano)

#Op inmigantes y religion

d2018 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2018_BBDD_BicentenarioUC.sav")

d2018 <- d2018 %>%
  mutate(folio_unico = seq(24217, 26227, by = 1))

d2018 <- d2018 %>%
  mutate(ano = 2018)

d2018 <- d2018 %>%
  rename(p1 = T_IN01_2)

d2018 <- d2018 %>%
  rename(p2 = T_IN01_3)

d2018 <- d2018 %>%
  rename(r1 = T_R01_1)

d2018 <- d2018 %>%
  rename(r2 = T_R01_2)

d2018 <- d2018 %>%
  rename(r3 = T_R01_3)

d2018 <- d2018 %>%
  rename(r4 = T_R01_4)

d2018 <- d2018 %>%
  rename(r5 = T_R01_5)

d2018 <- d2018 %>%
  mutate(
    r1 = case_when(
      r1 == 5 ~ 6,
      r1 == 6 ~ 5,
      TRUE ~ r1 # Mantener el valor original si no se cumple ninguna condición
    ),
    r2 = case_when(
      r2 == 5 ~ 6,
      r2 == 6 ~ 5,
      TRUE ~ r2 # Mantener el valor original si no se cumple ninguna condición
    ),
    r3 = case_when(
      r3 == 5 ~ 6,
      r3 == 6 ~ 5,
      TRUE ~ r3 # Mantener el valor original si no se cumple ninguna condición
    ),
    r4 = case_when(
      r4 == 5 ~ 6,
      r4 == 6 ~ 5,
      TRUE ~ r4 # Mantener el valor original si no se cumple ninguna condición
    ),
    r5 = case_when(
      r5 == 5 ~ 6,
      r5 == 6 ~ 5,
      TRUE ~ r5 # Mantener el valor original si no se cumple ninguna condición
    )
  )

table(d2018$r1)

d2018 <- d2018 %>%
  select(folio_unico, ano, p1, p2, r1, r2, r3, r4, r5)

data_filtrada <- data_filtrada %>%
  left_join(d2018, by = c("folio_unico", "ano"))

data_filtrada <- data_filtrada %>%
  mutate(op_inmigrantes1 = coalesce(op_inmigrantes1, p1)) %>%
  select(-p1)
table(data_filtrada$op_inmigrantes1, data_filtrada$ano)
data_filtrada <- data_filtrada %>%
  mutate(op_inmigrantes3 = coalesce(op_inmigrantes3, p2)) %>%
  select(-p2)
table(data_filtrada$op_inmigrantes3, data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    religion_encuestado = if_else(
      ano == 2018 & !is.na(r1),
      r1,  # Usa los valores recodificados de d2018
      religion_encuestado  # Mantén los valores originales
    )
  ) %>%
  select(-r1)
table(data_filtrada$religion_encuestado, data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    religion_pareja = if_else(
      ano == 2018 & !is.na(r2),
      r2,  # Usa los valores recodificados de d2018
      religion_pareja  # Mantén los valores originales
    )
  ) %>%
  select(-r2)

table(data_filtrada$religion_pareja, data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    religion_madre = if_else(
      ano == 2018 & !is.na(r3),
      r3,  # Usa los valores recodificados de d2018
      religion_madre  # Mantén los valores originales
    )
  ) %>%
  select(-r3)

table(data_filtrada$religion_madre, data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    religion_padre = if_else(
      ano == 2018 & !is.na(r4),
      r4,  # Usa los valores recodificados de d2018
      religion_padre  # Mantén los valores originales
    )
  ) %>%
  select(-r4)

table(data_filtrada$religion_padre, data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(
    religion_hijo = if_else(
      ano == 2018 & !is.na(r5),
      r5,  # Usa los valores recodificados de d2018
      religion_hijo  # Mantén los valores originales
    )
  ) %>%
  select(-r5)

table(data_filtrada$religion_hijo, data_filtrada$ano)

#Op violencia

d2013 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2013_BBDD_BicentenarioUC.sav")

d2013 <- d2013 %>%
  mutate(folio_unico = seq(14144, 16147, by = 1))

d2013 <- d2013 %>%
  mutate(ano = 2013)

d2013 <- d2013 %>%
  rename(p3 = Q144)

d2013 <- d2013 %>%
  select(folio_unico, ano, p3)

data_filtrada <- data_filtrada %>%
  left_join(d2013, by = c("folio_unico", "ano"))

data_filtrada <- data_filtrada %>%
  mutate(op_violencia = coalesce(op_violencia, p3, op_violencia1)) %>%
  select(-p3, -op_violencia1)
table(data_filtrada$op_violencia, data_filtrada$ano)

data_filtrada <- data_filtrada %>%
  mutate(iden_pol = coalesce(iden_pol, iden_pol1, iden_pol2)) %>%
  select(-iden_pol1, -iden_pol2)
table(data_filtrada$iden_pol, data_filtrada$ano)

#Creencia dios

d2015 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2015_BBDD_BicentenarioUC.sav")
table(d2015$R11)

d2015 <- d2015 %>%
  mutate(folio_unico = seq(18160, 20177, by = 1))

d2015 <- d2015 %>%
  mutate(ano = 2015)

d2015 <- d2015 %>%
  rename(p1 = R11)

d2015 <- d2015 %>%
  select(folio_unico, ano, p1)

d2015 <- d2015 %>%
  mutate(
    p1 = case_when(
      p1 == 3 ~ 2,
      p1 == 4 ~ 3,
      TRUE ~ p1 # Mantener el valor original si no se cumple ninguna condición
    ))

data_filtrada <- data_filtrada %>%
  left_join(d2015, by = c("folio_unico", "ano"))

data_filtrada <- data_filtrada %>%
  mutate(
    creencia_dios = if_else(
      ano == 2015 & !is.na(p1),
      p1,  # Usa los valores recodificados de d2018
      creencia_dios  # Mantén los valores originales
    )
  ) %>%
  select(-p1)

table(data_filtrada$creencia_dios, data_filtrada$ano)

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")
save(data_filtrada, file = "base_pt2.RData")

write_sav(data_filtrada, "base_sav.sav")
