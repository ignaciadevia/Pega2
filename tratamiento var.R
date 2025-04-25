rm(list = ls())
library(dplyr)
library(sjlabelled)
library(labelled)
library(ggplot2)
library(haven)

load("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_pt2.RData")

datos <- data_filtrada

datos <- datos %>%
  mutate(across(where(haven::is.labelled), as.character))

#Servicio religioso pareja 2014 y 2015
d2014 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2014_BBDD_BicentenarioUC.sav")
d2015 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2015_BBDD_BicentenarioUC.sav")

table(d2014$r7)
table(d2015$R8)
table(datos$frecuencia_misa_pareja)

d2014 <- d2014 %>%
  mutate(folio_unico = seq(16148, 18159, by = 1))

d2014 <- d2014 %>%
  mutate(ano = 2014)

d2014 <- d2014 %>%
  rename(misa1 = r7)

d2014 <- d2014 %>%
  select(folio_unico, ano, misa1)

d2015 <- d2015 %>%
  mutate(folio_unico = seq(18159, 20176, by = 1))

d2015 <- d2015 %>%
  mutate(ano = 2015)

d2015 <- d2015 %>%
  rename(misa2 = R8)

d2015 <- d2015 %>%
  select(folio_unico, ano, misa2)

datos <- datos %>%
  left_join(d2014, by = c("folio_unico", "ano"))

datos <- datos %>%
  left_join(d2015, by = c("folio_unico", "ano"))

datos <- datos %>%
  mutate(
    frecuencia_misa_pareja = coalesce(
      as.character(frecuencia_misa_pareja),
      as.character(misa1),
      as.character(misa2)
    )
  ) %>%
  select(-misa1, -misa2)

table(datos$frecuencia_misa_pareja, datos$ano)

#Religion

variables_religion <- c("religion_encuestado", "religion_pareja", "religion_madre", "religion_padre", "religion_hijo", "religion_14años")

datos <- datos %>%
  mutate(across(all_of(variables_religion), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_religion),
    ~ case_when(
      . > 6 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_religion),
    ~ factor(., 
             levels = 1:6, 
             labels = c(
               "Católica", 
               "Evangélica", 
               "Protestante", 
               "Otra religión", 
               "Ateo, agnóstico", 
               "Ninguna religión"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

table(datos$religion_encuestado, datos$ano)

datos$religion_encuestado <- set_label(datos$religion_encuestado,  "Religión que profesa Ud.")
datos$religion_madre <- set_label(datos$religion_madre,  "Religión que profesa su madre")
datos$religion_padre <- set_label(datos$religion_padre,  "Religión que profesa su padre")
datos$religion_pareja <- set_label(datos$religion_pareja,  "Religión que profesa su cónyuge o pareja")
datos$religion_hijo <- set_label(datos$religion_hijo,  "Religión que profesa su hijo/a mayor")
datos$religion_14años <- set_label(datos$religion_14años,  "Religión profesaba Ud. cuando tenía 14 años")

#Frecuencia misa 

variables_misa <- c("frecuencia_misa_enc", "frecuencia_misa_madre", "frecuencia_misa_padre", "frecuencia_misa_pareja", "frecuencia_misa_hijo", "frecuencia_misa_14años")

datos <- datos %>%
  mutate(across(all_of(variables_misa), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_misa),
    ~ case_when(
      . > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_misa),
    ~ factor(., 
             levels = 1:5, 
             labels = c(
               "Más de una vez a la semana",
               "Una vez a la semana",
               "Algunas veces al mes",
               "Solo en fechas religiosas",
               "Nunca o casi nunca"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

table(datos$frecuencia_misa_enc, datos$ano)
table(datos$frecuencia_misa_pareja, datos$ano)
table(datos$frecuencia_misa_madre, datos$ano)

datos$frecuencia_misa_enc <- set_label(datos$frecuencia_misa_enc,  "Con qué frecuencia asiste a misa o servicios religiosos de su iglesia, sin contar bautizos, matrimonios o funerales Ud.")
datos$frecuencia_misa_madre <- set_label(datos$frecuencia_misa_madre,  "Con qué frecuencia asiste a misa o servicios religiosos de su iglesia, sin contar bautizos, matrimonios o funerales su madre")
datos$frecuencia_misa_padre <- set_label(datos$frecuencia_misa_padre,  "Con qué frecuencia asiste a misa o servicios religiosos de su iglesia, sin contar bautizos, matrimonios o funerales su padre")
datos$frecuencia_misa_pareja <- set_label(datos$frecuencia_misa_pareja,  "Con qué frecuencia asiste a misa o servicios religiosos de su iglesia, sin contar bautizos, matrimonios o funerales su cónyuge o pareja")
datos$frecuencia_misa_hijo <- set_label(datos$frecuencia_misa_hijo,  "Con qué frecuencia asiste a misa o servicios religiosos de su iglesia, sin contar bautizos, matrimonios o funerales su hijo/a mayor")
datos$frecuencia_misa_14años <- set_label(datos$frecuencia_misa_14años,  "Con qué frecuencia asistía a misa o servicios religiosos de su iglesia, sin contar bautizos, matrimonios o funerales Ud. cuando tenía 14 años")

#Frecuencia reza

variables_rezar <- c("frecuencia_rezar1", "frecuencia_rezar2")

datos <- datos %>%
  mutate(across(all_of(variables_rezar), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_rezar),
    ~ case_when(
      . > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

datos <- datos %>%
  mutate(frecuencia_rezar1 = factor(frecuencia_rezar1, 
                                      levels = 1:5, 
                                      labels = c(
                                        "Más de una vez a la semana",
                                        "Una vez a la semana",
                                        "Algunas veces al mes",
                                        "Solo en fechas religiosas",
                                        "Nunca o casi nunca"
                                      )))

table(datos$frecuencia_rezar1, datos$ano)

datos <- datos %>%
  mutate(frecuencia_rezar2 = factor(frecuencia_rezar2, 
                                    levels = 1:5, 
                                    labels = c(
                                      "Varias veces durante el día",
                                      "Al menos una vez al día",
                                      "Algunos días si y otros no",
                                      "Solo en fechas religiosas",
                                      "Nunca o casi nunca"
                                    )))


table(datos$frecuencia_rezar2, datos$ano)

datos$frecuencia_rezar1 <- set_label(datos$frecuencia_rezar1,  "Con qué frecuencia hace oración (o reza) fuera de un servicio religoso")
datos$frecuencia_rezar2 <- set_label(datos$frecuencia_rezar2, "Cuan frecuentemente Ud. hace oración (o reza) fuera de un servicio religioso?")

#Creencia en Dios

datos <- datos %>%
  mutate(creencia_dios = as.numeric(as.character(creencia_dios))) 

datos <- datos %>%
  mutate(creencia_dios = case_when(
    creencia_dios > 3 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ creencia_dios          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(creencia_dios = factor(creencia_dios, 
                                levels = 1:3, 
                                labels = c(
                                  "Si, y no tiene dudas",
                                  "A momentos sí, en otros no",
                                  "No cree"
                                )))

table(datos$creencia_dios, datos$ano)
datos$creencia_dios <- set_label(datos$creencia_dios, "¿Cómo calificaría su creencia en Dios?")

#Creencia milagros

datos <- datos %>%
  mutate(creencia_milagros = as.numeric(as.character(creencia_milagros))) 

datos <- datos %>%
  mutate(creencia_milagros = case_when(
    creencia_milagros > 3 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ creencia_milagros          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(creencia_milagros = factor(creencia_milagros, 
                                levels = 1:3, 
                                labels = c(
                                  "Si, y no tiene dudas",
                                  "A momentos sí, en otros no",
                                  "No cree"
                                )))

table(datos$creencia_milagros, datos$ano)
datos$creencia_dios <- set_label(datos$creencia_dios, "¿Cree en los milagros?")


#Op religion 4

datos <- datos %>%
  mutate(op_religion4 = as.numeric(as.character(op_religion4))) 

datos <- datos %>%
  mutate(op_religion4 = case_when(
    op_religion4 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ op_religion4          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(op_religion4 = factor(op_religion4, 
                                    levels = 1:5, 
                                    labels = c(
                                      "Muy de acuerdo",
                                      "De acuerdo",
                                      "Ni acuerdo ni desacuerdo",
                                      "En desacuerdo",
                                      "Muy en desacuerdo"
                                    )))

table(datos$op_religion4, datos$ano)
datos$op_religion4 <- set_label(datos$op_religion4, "Personalmente trato de dar testimonio o de convencer a otros acerca de mi fe")

#Op religion 8

datos <- datos %>%
  mutate(op_religion8 = as.numeric(as.character(op_religion8))) 

datos <- datos %>%
  mutate(op_religion8 = case_when(
    op_religion8 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ op_religion8          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(op_religion8 = factor(op_religion8, 
                               levels = 1:5, 
                               labels = c(
                                 "Muy de acuerdo",
                                 "De acuerdo",
                                 "Ni acuerdo ni desacuerdo",
                                 "En desacuerdo",
                                 "Muy en desacuerdo"
                               )))

table(datos$op_religion8, datos$ano)
datos$op_religion8 <- set_label(datos$op_religion8, "Se debería tomar más en cuenta a la Iglesia Catolica a la hora de tomar decisiones públicas")

#Op religion 10

datos <- datos %>%
  mutate(op_religion10 = as.numeric(as.character(op_religion10))) 

datos <- datos %>%
  mutate(op_religion10 = case_when(
    op_religion10 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ op_religion10          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(op_religion10 = factor(op_religion10, 
                               levels = 1:5, 
                               labels = c(
                                 "Muy de acuerdo",
                                 "De acuerdo",
                                 "Ni acuerdo ni desacuerdo",
                                 "En desacuerdo",
                                 "Muy en desacuerdo"
                               )))

table(datos$op_religion10, datos$ano)
datos$op_religion10 <- set_label(datos$op_religion10, "Los valores cristianos deberían jugar un rol más importante en la sociedad actual")

#Culto virgen 1

datos <- datos %>%
  mutate(culto_virgen1 = as.numeric(as.character(culto_virgen1))) 

datos <- datos %>%
  mutate(culto_virgen1 = case_when(
    culto_virgen1 > 3 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ culto_virgen1          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(culto_virgen1 = factor(culto_virgen1, 
                                levels = 1:2, 
                                labels = c(
                                  "Si",
                                  "No"
                                )))

table(datos$culto_virgen1, datos$ano)
datos$culto_virgen1 <- set_label(datos$culto_virgen1, "Tiene la costumbre de detenerse y rezar a la Virgen en un oratorio, gruta o cualquier imagen pública expuesta")

#Culto virgen 2

datos <- datos %>%
  mutate(culto_virgen2 = as.numeric(as.character(culto_virgen2))) 

datos <- datos %>%
  mutate(culto_virgen2 = case_when(
    culto_virgen2 > 3 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ culto_virgen2          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(culto_virgen2 = factor(culto_virgen2, 
                                levels = 1:2, 
                                labels = c(
                                  "Si",
                                  "No"
                                )))

table(datos$culto_virgen2, datos$ano)
datos$culto_virgen2 <- set_label(datos$culto_virgen2, "Tiene la costumbre de hacer una manda a la Virgen y después paga con oraciones, homenajes o peregrinación")

#Culto virgen 6

datos <- datos %>%
  mutate(
    culto_virgen6_2008 = if_else(ano == 2008, as.numeric(as.character(culto_virgen6)), NA_real_),
    culto_virgen6_2008 = case_when(
      culto_virgen6_2008 == 1 ~ 1,
      culto_virgen6_2008 == 2 ~ 1,
      culto_virgen6_2008 == 3 ~ 2,
      TRUE ~ NA_real_ # Por si hay valores fuera del rango
    )
  ) %>%
  mutate(
    culto_virgen6 = if_else(ano == 2008, as.character(culto_virgen6_2008), as.character(culto_virgen6))
  ) %>%
  select(-culto_virgen6_2008)

table(datos$culto_virgen6, datos$ano)

datos <- datos %>%
  mutate(culto_virgen6 = as.numeric(as.character(culto_virgen6))) 

datos <- datos %>%
  mutate(culto_virgen6 = case_when(
    culto_virgen6 > 3 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ culto_virgen6          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(culto_virgen6 = factor(culto_virgen6, 
                                levels = 1:2, 
                                labels = c(
                                  "Si",
                                  "No"
                                )))

table(datos$culto_virgen6, datos$ano)
datos$culto_virgen6 <- set_label(datos$culto_virgen6, "Tiene costumbre de visitar el cementerio el día de los muertos a personas cercanas a usted o que han muerto")


#Define como

datos <- datos %>%
  mutate(
    define_como2 = if_else(ano %in% c(2018, 2019, 2021), as.numeric(as.character(define_como)), NA_real_)
  )

table(datos$define_como2, datos$ano)
table(datos$define_como, datos$ano)

datos$define_como2 <- case_when(
  datos$define_como2  == 1 ~ 1,
  datos$define_como2  == 2 ~ 1,
  datos$define_como2  == 3 ~ 2,
  datos$define_como2  == 4 ~ 3,
  datos$define_como2  == 5 ~ 4,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

datos$define_como <- case_when(
  datos$define_como  == 1 ~ 1,
  datos$define_como  == 3 ~ 2,
  datos$define_como  == 4 ~ 3,
  datos$define_como  == 5 ~ 4,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)

datos<- datos %>%
  mutate(define_como = coalesce(define_como, define_como2)) %>%
  select(-define_como2)

datos <- datos %>%
  mutate(define_como = as.numeric(as.character(define_como))) 

datos <- datos %>%
  mutate(define_como = case_when(
    define_como > 4 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ define_como         # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(define_como = factor(define_como, 
                                levels = 1:4, 
                                labels = c(
                                  "Bastante religiosa",
                                  "Más o menos religiosa",
                                  "Poco religiosa",
                                  "Nada religiosa"
                                )))

table(datos$define_como, datos$ano)
datos$define_como <- set_label(datos$define_como, "Se define como una persona…")


#Encomendarse santo

datos <- datos %>%
  mutate(encomendarse_santo = as.numeric(as.character(encomendarse_santo))) 

datos <- datos %>%
  mutate(encomendarse_santo = case_when(
    encomendarse_santo > 3 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ encomendarse_santo          # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(encomendarse_santo = factor(encomendarse_santo, 
                                levels = 1:2, 
                                labels = c(
                                  "Si",
                                  "No"
                                )))

table(datos$encomendarse_santo, datos$ano)
datos$encomendarse_santo <- set_label(datos$encomendarse_santo, "Tiene la costumbre de encomendarse a algún santo")


#Creencia cat 2

datos <- datos %>%
  mutate(creencia_cat2 = as.numeric(as.character(creencia_cat2))) 

datos <- datos %>%
  mutate(creencia_cat2 = case_when(
    creencia_cat2 > 4 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ creencia_cat2         # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(creencia_cat2 = factor(creencia_cat2, 
                                     levels = 1:3, 
                                     labels = c(
                                       "Si, y no tiene dudas",
                                       "A momentos sí, en otros no",
                                       "No cree"
                                     )))

table(datos$creencia_cat2, datos$ano)
datos$creencia_cat2 <- set_label(datos$creencia_cat2, "Cree que los sacerdotes pueden perdonar los pecados en el nombre de Dios")


#Creencia fe sin iglesia

datos <- datos %>%
  mutate(creencia_fe_siniglesia = as.numeric(as.character(creencia_fe_siniglesia))) 

datos <- datos %>%
  mutate(creencia_fe_siniglesia = case_when(
    creencia_fe_siniglesia > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
    TRUE ~ creencia_fe_siniglesia        # Mantener los valores dentro del rango permitido
  )) %>%
  mutate(creencia_fe_siniglesia = factor(creencia_fe_siniglesia, 
                                levels = 1:4, 
                                labels = c(
                                  "Si, absolutamente",
                                  "Si, con dificultades",
                                  "Probablemente no",
                                  "Definitivamente no"
                                )))

table(datos$creencia_fe_siniglesia, datos$ano)
datos$creencia_fe_siniglesia <- set_label(datos$creencia_fe_siniglesia, "Cree que la fe se puede vivir sin pertenecer a ninguna Iglesia")

#N personas hogar
datos <- datos %>%
  mutate(
    n_personas_hogar = ifelse(n_personas_hogar %in% c(99, 999), NA, n_personas_hogar)  # Convertir a NA
  )  # Ordenar los datos por la variable n_personas_hogar

datos$n_personas_hogar <- factor(datos$n_personas_hogar, 
                                 levels = sort(unique(as.numeric(datos$n_personas_hogar))))

datos$n_personas_hogar <- set_label(datos$n_personas_hogar, "Número de personas en el hogar")

#NSE Y GSE
# Primero, crea las variables gse_2 y nse_2 según la condición original
datos$gse_2 <- ifelse(datos$ano %in% c(2021, 2022, 2023, 2024), datos$nse, NA)
table(datos$gse_2, datos$ano)

datos$nse_2 <- ifelse(datos$ano %in% c(2021, 2022, 2023, 2024), datos$gse, NA)
table(datos$nse_2, datos$ano)

# Mutamos las variables nse y gse
datos <- datos %>%
  mutate(
    nse = if_else(
      ano %in% c(2021, 2022, 2023, 2024) & !is.na(nse_2),
      nse_2,
      nse
    )
  ) %>%
  select(-nse_2)

datos <- datos %>%
  mutate(
    gse = if_else(
      ano %in% c(2021, 2022, 2023, 2024) & !is.na(gse_2),
      gse_2,
      gse
    )
  ) %>%
  select(-gse_2)  

# No eliminar 2021, solo mantén las filas donde gse no sea NA
# Filtramos solo si el año no es 2021 y no es necesario mantener gse
datos <- datos %>%
  filter(!(ano == 2021 & is.na(gse)))

# Verifica el resultado con la tabla
table(datos$gse, datos$ano)

# Etiquetamos las variables
datos$nse <- set_label(datos$nse, "NSE")
datos$gse <- set_label(datos$gse, "GSE")

#Nivel educacional

table(datos$educ, datos$ano)

datos <- datos %>%
  mutate(across(
    .cols = c("educ", "educ_pareja", "educ_madre", "educ_padre"),
    .fns = ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ 2,
      . %in% c(3, 4) ~ 3,
      . == 5 ~ 4,
      . == 6 ~ 5,
      . == 7 ~ 6,
      . == 8 ~ 7,
      TRUE ~ NA_real_ # Mantén esto para manejar valores que no están especificados
    )
  ))


variables_educ <- c("educ", "educ_pareja", "educ_madre", "educ_padre")

datos <- datos %>%
  mutate(across(all_of(variables_educ), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_educ),
    ~ case_when(
      . > 7 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_educ),
    ~ factor(., 
             levels = 1:7, 
             labels = c(
               "Sin Estudios / Básica incompleta / Pre Básica / Especial-Diferencial", 
               "Básica completa", 
               "Media o Humanidades", 
               "Centro de Formación Técnica o Instituto Profesional incompleto", 
               "Centro de Formación Técnica o Instituto Profesional completo / Universitaria incompleta",
               "Universitaria completa",
               "Post Grado (Magíster o Doctorado)"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

table(datos$educ, datos$ano)

datos$educ <- set_label(datos$educ, "Nivel educación encuestado")
datos$educ_madre<- set_label(datos$educ_madre, "Nivel educación madre")
datos$educ_padre <- set_label(datos$educ_padre, "Nivel educación padre")
datos$educ_pareja <- set_label(datos$educ_pareja, "Nivel educación pareja")

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")
save(datos, file = "base_pt3.RData")

load("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_pt3.RData")

#Sexo

table(datos$sexo, datos$ano)

datos <- datos %>%
  mutate(sexo = factor(sexo, 
                       levels = 1:2, 
                       labels = c(
                         "Hombre",
                         "Mujer"
                       )))

datos$sexo <- set_label(datos$sexo, "Sexo encuestado")

#Edad

table(datos$tramoedad, datos$ano)
datos <- datos %>%
  mutate(tramoedad = factor(tramoedad, 
                       levels = 1:5, 
                       labels = c(
                         "18 a 24",
                         "25 a 34",
                         "35 a 44",
                         "45 a 54",
                         "55 o más"
                       )))

datos$tramoedad <- set_label(datos$tramoedad, "Tramo etario")
datos$edad <- set_label(datos$edad, "Edad encuestado")

#Zona
table(datos$zona, datos$ano)
datos <- datos %>%
  mutate(zona = factor(zona, 
                            levels = 1:4, 
                            labels = c(
                              "Norte",
                              "Centro",
                              "Sur",
                              "RM"
                            )))

datos$zona <- set_label(datos$zona, "Zona Chile")

#Tipo comuna
table(datos$tipocomuna, datos$ano)
datos <- datos %>%
  mutate(tipocomuna = factor(tipocomuna, 
                            levels = 1:4, 
                            labels = c(
                              "A",
                              "B",
                              "C",
                              "D"
                            )))

datos$tipocomuna <- set_label(datos$tipocomuna, "Tipo comuna")

#Estado civil
table(datos$estado_civil, datos$ano)
datos <- datos %>%
  mutate(
    estado_civil = case_when(
      estado_civil > 6 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ as.numeric(estado_civil)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    estado_civil = factor(
      estado_civil,
      levels = 1:6,
      labels = c(
        "Casado",
        "Conviviente sin acuerdo",
        "Conviviente con acuerdo",
        "Anulado/Sepado/Divorciado",
        "Viudo",
        "Soltero"
      )
    )
  )

datos$estado_civil <- set_label(datos$estado_civil, "Estado civil")

#Op matrimonio
table(datos$op_matr1, datos$ano)
table(datos$op_matr5, datos$ano)
table(datos$op_matr6, datos$ano)
table(datos$op_matr7, datos$ano)
table(datos$op_matr11, datos$ano)

datos <- datos %>%
  mutate(
    op_matr1_nueva = if_else(
      ano %in% c(2018, 2022, 2023, 2024),
      op_matr1,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_matr5_nueva = if_else(
      ano %in% c(2018, 2022, 2023, 2024),
      op_matr5,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_matr6_nueva = if_else(
      ano %in% c(2018, 2022, 2023, 2024),
      op_matr6,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_matr7_nueva = if_else(
      ano %in% c(2018, 2022, 2023, 2024),
      op_matr7,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_matr11_nueva = if_else(
      ano %in% c(2018, 2022, 2023, 2024),
      op_matr11,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_matr1_nueva = recode(op_matr1_nueva, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_),
    op_matr5_nueva = recode(op_matr5_nueva, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_),
    op_matr6_nueva = recode(op_matr6_nueva, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_),
    op_matr7_nueva = recode(op_matr7_nueva, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_),
    op_matr11_nueva = recode(op_matr11_nueva, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_)
  )

table(datos$op_matr1_nueva, datos$ano)

datos <- datos %>%
  mutate(
    op_matr1 = if_else(
      ano %in% c(2018, 2022, 2023, 2024) & !is.na(op_matr1_nueva),
      as.double(op_matr1_nueva),  # Convierte a tipo numérico
      as.double(op_matr1)  # Mantén el valor original y conviértelo a numérico
    )
  ) %>%
  select(-op_matr1_nueva)

table(datos$op_matr1, datos$ano)

datos <- datos %>%
  mutate(
    op_matr5 = if_else(
      ano %in% c(2018, 2022, 2023, 2024) & !is.na(op_matr5_nueva),
      as.double(op_matr5_nueva),  # Convierte a tipo numérico
      as.double(op_matr5)  # Mantén el valor original y conviértelo a numérico
    )
  ) %>%
  select(-op_matr5_nueva)

table(datos$op_matr5, datos$ano)

datos <- datos %>%
  mutate(
    op_matr6 = if_else(
      ano %in% c(2018, 2022, 2023, 2024) & !is.na(op_matr6_nueva),
      as.double(op_matr6_nueva),  # Convierte a tipo numérico
      as.double(op_matr6)  # Mantén el valor original y conviértelo a numérico
    )
  ) %>%
  select(-op_matr6_nueva)

table(datos$op_matr6, datos$ano)

datos <- datos %>%
  mutate(
    op_matr7 = if_else(
      ano %in% c(2018, 2022, 2023, 2024) & !is.na(op_matr7_nueva),
      as.double(op_matr7_nueva),  # Convierte a tipo numérico
      as.double(op_matr7)  # Mantén el valor original y conviértelo a numérico
    )
  ) %>%
  select(-op_matr7_nueva)

table(datos$op_matr7, datos$ano)

datos <- datos %>%
  mutate(
    op_matr11 = if_else(
      ano %in% c(2018, 2022, 2023, 2024) & !is.na(op_matr11_nueva),
      as.double(op_matr11_nueva),  # Convierte a tipo numérico
      as.double(op_matr11)  # Mantén el valor original y conviértelo a numérico
    )
  ) %>%
  select(-op_matr11_nueva)

table(datos$op_matr11, datos$ano)


variables_matrimonio <- c("op_matr1", "op_matr5", "op_matr6", "op_matr7", "op_matr11")

datos <- datos %>%
  mutate(across(all_of(variables_matrimonio), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_matrimonio),
    ~ case_when(
      . > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_matrimonio),
    ~ factor(., 
             levels = 1:5, 
             labels = c(
               "Muy de acuerdo", 
               "De acuerdo", 
               "Ni de acuerdo ni en desacuerdo", 
               "En desacuerdo", 
               "Muy en desacuerdo"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))
table(datos$op_matr1, datos$ano)
table(datos$op_matr5, datos$ano)
table(datos$op_matr6, datos$ano)
table(datos$op_matr7, datos$ano)
table(datos$op_matr11, datos$ano)

#Opinion aborto
table(datos$op_aborto, datos$ano)
datos <- datos %>%
  mutate(
    op_aborto = case_when(
      op_aborto > 3 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ as.numeric(op_aborto)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    op_aborto = factor(
      op_aborto,
      levels = 1:3,
      labels = c(
        "Bajo cualquier circunstancia",
        "Solo bajo algunas circunstancias",
        "Bajo ninguna circunstancia"
      )
    )
  )

datos$op_matr1 <- set_label(datos$op_matr1,  "El matrimonio es un compromiso para toda la vida")
datos$op_matr5 <- set_label(datos$op_matr5,  "Las parejas que conviven debieran casarse cuando deciden tener hijos")
datos$op_matr6 <- set_label(datos$op_matr6,  "Cuando hay niños de por medio, los padres deben permanecer juntos aun cuando no se lleven bien")
datos$op_matr7 <- set_label(datos$op_matr7,  "Las parejas homosexuales deberían tener derecho de casarse")
datos$op_matr11 <- set_label(datos$op_matr11,  "Las parejas homosexuales deberían tener derecho a adoptar")


#Op trabajo mujeres
table(datos$op_trabajo_mujer1, datos$ano)
table(datos$op_trabajo_mujer2, datos$ano)
table(datos$op_trabajo_mujer3, datos$ano)

datos <- datos %>%
  mutate(
    op_trabajo_mujer1_nueva = if_else(
      ano %in% c(2018),
      op_trabajo_mujer1,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_trabajo_mujer3_nueva = if_else(
      ano %in% c(2018),
      op_trabajo_mujer3,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_trabajo_mujer1_nueva = recode(op_trabajo_mujer1_nueva, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_),
    op_trabajo_mujer2 = recode(op_trabajo_mujer2, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_),
    op_trabajo_mujer3_nueva = recode(op_trabajo_mujer3_nueva, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, .default = NA_real_)
  )

datos <- datos %>%
  mutate(
    op_trabajo_mujer1 = if_else(
      ano %in% c(2018) & !is.na(op_trabajo_mujer1_nueva),
      as.double(op_trabajo_mujer1_nueva),  # Convierte a tipo numérico
      as.double(op_trabajo_mujer1)  # Mantén el valor original y conviértelo a numérico
    )
  ) %>%
  select(-op_trabajo_mujer1_nueva)


datos <- datos %>%
  mutate(
    op_trabajo_mujer3 = if_else(
      ano %in% c(2018) & !is.na(op_trabajo_mujer3_nueva),
      as.double(op_trabajo_mujer3_nueva),  # Convierte a tipo numérico
      as.double(op_trabajo_mujer3)  # Mantén el valor original y conviértelo a numérico
    )
  ) %>%
  select(-op_trabajo_mujer3_nueva)

variables_trabajo <- c("op_trabajo_mujer1", "op_trabajo_mujer2", "op_trabajo_mujer3")

datos <- datos %>%
  mutate(across(all_of(variables_trabajo), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_trabajo),
    ~ case_when(
      . > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_trabajo),
    ~ factor(., 
             levels = 1:5, 
             labels = c(
               "Muy de acuerdo", 
               "De acuerdo", 
               "Ni de acuerdo ni en desacuerdo", 
               "En desacuerdo", 
               "Muy en desacuerdo"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

table(datos$op_trabajo_mujer1, datos$ano)
table(datos$op_trabajo_mujer2, datos$ano)
table(datos$op_trabajo_mujer3, datos$ano)

datos$op_trabajo_mujer1 <- set_label(datos$op_trabajo_mujer1,  "Una madre que trabaja establece una relación igual de cercana con sus hijos que una que no trabaja")
datos$op_trabajo_mujer2 <- set_label(datos$op_trabajo_mujer2,  "La familia se descuida si la mujer tiene un trabajo de tiempo completo")
datos$op_trabajo_mujer3 <- set_label(datos$op_trabajo_mujer3,  "Ser dueña de casa es tan satisfactorio como trabajar por un sueldo")

#Moral economica
table(datos$moral_econ1, datos$ano)

datos <- datos %>%
  mutate(
    moral_econ1_5nueva = if_else(
      ano %in% c(2018),
      moral_econ1_5,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    moral_econ1_5nueva = recode(moral_econ1_5nueva, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5, .default = NA_real_))

datos <- datos %>%
  mutate(
    moral_econ1 = case_when(
      ano == 2018 & !is.na(moral_econ1_5nueva) ~ as.double(moral_econ1_5nueva),  # Año 2018
      ano == 2019 & !is.na(moral_econ1_5) ~ as.double(moral_econ1_5),  # Año 2019
      TRUE ~ as.double(moral_econ1)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-moral_econ1_5nueva, -moral_econ1_5)  # Elimina las columnas auxiliares

datos <- datos %>%
  mutate(
    moral_econ1 = case_when(
      moral_econ1 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
      TRUE ~ as.numeric(moral_econ1)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    moral_econ1 = factor(
      moral_econ1,
      levels = 1:5,
      labels = c(
        "Cada persona debería preocuparse y responsabilizarse por su propio bienestar", # Para 1
        2, # Sin etiqueta para 2
        3, # Sin etiqueta para 3
        4, # Sin etiqueta para 4
        "El Estado debería preocuparse y hacerse responsable por el bienestar de las personas" # Para 5
      )
    )
  )

datos$moral_econ1 <- set_label(datos$moral_econ1,  " ¿Dónde se situaría Ud. entre estos pares de afirmaciones?")

table(datos$moral_econ3, datos$ano)
datos <- datos %>%
  mutate(
    moral_econ3 = case_when(
      moral_econ3 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
      TRUE ~ as.numeric(moral_econ3)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    moral_econ3 = factor(
      moral_econ3,
      levels = 1:5,
      labels = c(
        "Lo mejor para el país es que haya crecimiento económico alto y sostenido", # Para 1
        2, # Sin etiqueta para 2
        3, # Sin etiqueta para 3
        4, # Sin etiqueta para 4
        "Lo mejor para el país es que haya igualdad social y una distribución de los ingresos más equitativa" # Para 5
      )
    )
  )

datos$moral_econ3 <- set_label(datos$moral_econ3,  " ¿Dónde se situaría Ud. entre estos pares de afirmaciones?")

table(datos$moral_econ5, datos$ano)
datos <- datos %>%
  mutate(
    moral_econ5 = case_when(
      moral_econ5 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
      TRUE ~ as.numeric(moral_econ5)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    moral_econ5 = factor(
      moral_econ5,
      levels = 1:5,
      labels = c(
        "La ayuda del Estado debe destinarse sólo a los más pobres y vulnerables", # Para 1
        2, # Sin etiqueta para 2
        3, # Sin etiqueta para 3
        4, # Sin etiqueta para 4
        "Todos los ciudadanos deben recibir la misma ayuda del Estado" # Para 5
      )
    )
  )

datos$moral_econ5 <- set_label(datos$moral_econ5,  " ¿Dónde se situaría Ud. entre estos pares de afirmaciones?")

table(datos$moral_econ6, datos$ano)
datos <- datos %>%
  mutate(
    moral_econ6 = case_when(
      moral_econ6 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
      TRUE ~ as.numeric(moral_econ6)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    moral_econ6 = factor(
      moral_econ6,
      levels = 1:5,
      labels = c(
        "La mejor forma de progresar en la vida es esforzarse por emprender, capacitarse y trabajar", # Para 1
        2, # Sin etiqueta para 2
        3, # Sin etiqueta para 3
        4, # Sin etiqueta para 4
        "Para progresar en la vida se requieren garantías del Estado de buena educación y trabajo" # Para 5
      )
    )
  )

datos$moral_econ6 <- set_label(datos$moral_econ6,  " ¿Dónde se situaría Ud. entre estos pares de afirmaciones?")


#Probabilidad de ascenso
table(datos$prob_ascenso1, datos$ano)
table(datos$prob_ascenso2, datos$ano)
table(datos$prob_ascenso3, datos$ano)
table(datos$prob_ascenso4, datos$ano)
table(datos$prob_ascenso5, datos$ano)
table(datos$prob_ascenso6, datos$ano)

datos <- datos %>%
  mutate(
    prob_ascenso4_nueva = if_else(
      ano %in% c(2013, 2014),
      prob_ascenso4,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    prob_ascenso4_nueva = recode(prob_ascenso4_nueva, `5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5, .default = NA_real_))

datos <- datos %>%
  mutate(
    prob_ascenso4 = case_when(
      ano == 2013 & !is.na(prob_ascenso4_nueva) ~ as.double(prob_ascenso4_nueva),  # Año 2018
      ano == 2014 & !is.na(prob_ascenso4_nueva) ~ as.double(prob_ascenso4_nueva),  # Año 2019
      TRUE ~ as.double(prob_ascenso4)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-prob_ascenso4_nueva)  # Elimina las columnas auxiliares

datos <- datos %>%
  mutate(
    prob_ascenso5_nueva = if_else(
      ano %in% c(2013, 2014),
      prob_ascenso5,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    prob_ascenso5_nueva = recode(prob_ascenso5_nueva, `5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5, .default = NA_real_))

datos <- datos %>%
  mutate(
    prob_ascenso5 = case_when(
      ano == 2013 & !is.na(prob_ascenso5_nueva) ~ as.double(prob_ascenso5_nueva),  # Año 2018
      ano == 2014 & !is.na(prob_ascenso5_nueva) ~ as.double(prob_ascenso5_nueva),  # Año 2019
      TRUE ~ as.double(prob_ascenso5)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-prob_ascenso5_nueva)

datos <- datos %>%
  mutate(
    prob_ascenso6_nueva = if_else(
      ano %in% c(2013, 2014),
      prob_ascenso6,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    prob_ascenso6_nueva = recode(prob_ascenso6_nueva, `5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5, .default = NA_real_))

datos <- datos %>%
  mutate(
    prob_ascenso6 = case_when(
      ano == 2013 & !is.na(prob_ascenso6_nueva) ~ as.double(prob_ascenso6_nueva),  # Año 2018
      ano == 2014 & !is.na(prob_ascenso6_nueva) ~ as.double(prob_ascenso6_nueva),  # Año 2019
      TRUE ~ as.double(prob_ascenso6)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-prob_ascenso6_nueva)

variables_moral <- c("prob_ascenso1", "prob_ascenso2", "prob_ascenso3", "prob_ascenso4", "prob_ascenso5", "prob_ascenso6")

datos <- datos %>%
  mutate(across(all_of(variables_moral), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_moral),
    ~ case_when(
      . > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_moral),
    ~ factor(., 
             levels = 1:5, 
             labels = c(
               "Muy alta", 
               "Bastante alta", 
               "Ni alta ni baja", 
               "Bastante baja", 
               "Muy baja"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

datos$prob_ascenso1 <- set_label(datos$prob_ascenso1,  "Probabilidad o chance que tiene en este país: Un pobre de salir de la pobreza")
datos$prob_ascenso2 <- set_label(datos$prob_ascenso2,  "Probabilidad o chance que tiene en este país: Una persona de clase media de llegar a tener muy buena situación económica")
datos$prob_ascenso3 <- set_label(datos$prob_ascenso3,  "Probabilidad o chance que tiene en este país: Un joven inteligente pero sin recursos de ingresar a la universidad")
datos$prob_ascenso4 <- set_label(datos$prob_ascenso4,  "Probabilidad o chance que tiene en este país: Cualquier persona de inciar su propio negocio y establecerse independientemente")
datos$prob_ascenso5 <- set_label(datos$prob_ascenso5,  "Probabilidad o chance que tiene en este país: Alguien que tiene un negocio o empresa pequeña de convertirla en una empresa grande y exitosa")
datos$prob_ascenso6 <- set_label(datos$prob_ascenso6,  "Probabilidad o chance que tiene en este país: Cualquier trabajador de adquirir su propia vivienda en un tiempo razonable")

#Confianza

table(datos$confianza1, datos$ano)
datos <- datos %>%
  mutate(
    confianza1 = case_when(
      confianza1 > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ as.numeric(confianza1)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    confianza1 = factor(
      confianza1,
      levels = 1:5,
      labels = c(
        "Muy en desacuerdo", 
        "En desacuerdo", 
        "Ni de acuerdo ni en desacuerdo", 
        "De acuerdo", 
        "Muy de acuerdo"
      )
    )
  )
datos$confianza1 <- set_label(datos$confianza1,  "Se puede confiar en la mayor parte de las personas")

#Confianza institucional
table(datos$conf_inst1, datos$ano)
table(datos$conf_inst2, datos$ano)
table(datos$conf_inst3, datos$ano)
table(datos$conf_inst4, datos$ano)
table(datos$conf_inst5, datos$ano)
table(datos$conf_inst6, datos$ano)
table(datos$conf_inst7, datos$ano)
table(datos$conf_inst8, datos$ano)
table(datos$conf_inst9, datos$ano)
table(datos$conf_inst11, datos$ano) #2015

datos <- datos %>%
  mutate(
    conf_inst11_nueva = if_else(
      ano %in% c(2015),
      conf_inst11,  
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    conf_inst11_nueva = recode(conf_inst11_nueva, `5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5, .default = NA_real_))

datos <- datos %>%
  mutate(
    conf_inst11 = case_when(
      ano == 2015 & !is.na(conf_inst11_nueva) ~ as.double(conf_inst11_nueva),  # Año 2018
      TRUE ~ as.double(conf_inst11)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-conf_inst11_nueva)

variables_inst <- c("conf_inst1", "conf_inst2", "conf_inst3", "conf_inst4", "conf_inst5", "conf_inst6", "conf_inst7", "conf_inst8", "conf_inst9", "conf_inst11")

datos <- datos %>%
  mutate(across(all_of(variables_inst), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_inst),
    ~ case_when(
      . > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_inst),
    ~ factor(., 
             levels = 1:5, 
             labels = c(
               "Nada",
               "Poco",
               "Algo",
               "Bastante",
               "Mucho"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

datos$conf_inst1 <- set_label(datos$conf_inst1,  "Confianza en la Iglesia Católica")
datos$conf_inst2 <- set_label(datos$conf_inst2,  "Confianza en las Fuerzas Armadas")
datos$conf_inst3 <- set_label(datos$conf_inst3,  "Confianza en el Gobierno")
datos$conf_inst4 <- set_label(datos$conf_inst4,  "Confianza en Iglesias Evangélicas")
datos$conf_inst5 <- set_label(datos$conf_inst5,  "Confianza en Empresarios")
datos$conf_inst6 <- set_label(datos$conf_inst6,  "Confianza en Tribunales de Justicia")
datos$conf_inst7 <- set_label(datos$conf_inst7,  "Confianza en Parlamentarios")
datos$conf_inst8 <- set_label(datos$conf_inst8,  "Confianza en Medios de Comunicación")
datos$conf_inst9 <- set_label(datos$conf_inst9,  "Confianza en Partidos Políticos")
datos$conf_inst11 <- set_label(datos$conf_inst11,  "Confianza en las Universidades")

#Opinion inmigrantes
table(datos$op_inmigrantes1, datos$ano)
table(datos$op_inmigrantes3, datos$ano)

datos <- datos %>%
  mutate(
    op_inmigrantes3_nueva = if_else(
      ano %in% c(2019),
      op_inmigrantes3,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    op_inmigrantes3_nueva = recode(op_inmigrantes3_nueva, `5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5, .default = NA_real_))

datos <- datos %>%
  mutate(
    op_inmigrantes3 = case_when(
      ano == 2019 & !is.na(op_inmigrantes3_nueva) ~ as.double(op_inmigrantes3_nueva),  # Año 2018
      TRUE ~ as.double(op_inmigrantes3)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-op_inmigrantes3_nueva)

variables_inmigrantes <- c("op_inmigrantes1", "op_inmigrantes3")

datos <- datos %>%
  mutate(across(all_of(variables_inmigrantes), ~ as.numeric(as.character(.))))

datos <- datos %>%
  mutate(across(
    all_of(variables_inmigrantes),
    ~ case_when(
      . > 5 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ .           # Mantener los valores dentro del rango permitido
    ),
    .names = "{col}"  # Mantener el nombre de la columna original
  )) %>%
  mutate(across(
    all_of(variables_inmigrantes),
    ~ factor(., 
             levels = 1:5, 
             labels = c(
               "Muy en desacuerdo", 
               "En desacuerdo", 
               "Ni de acuerdo ni en desacuerdo", 
               "De acuerdo", 
               "Muy de acuerdo"
             )),
    .names = "{col}"  # Mantener el nombre de la columna original
  ))

table(datos$op_inmigrantes1, datos$ano)
table(datos$op_inmigrantes3, datos$ano)
datos$op_inmigrantes1 <- set_label(datos$op_inmigrantes1,  "Los inmigrantes limitan las posibilidades de encontrar trabajo de los chilenos")
datos$op_inmigrantes3 <- set_label(datos$op_inmigrantes3,  "Los inmigrantes con su situación legal al día, deberían tener los mismos derechos que los chilenos para acceder a beneficios de salud, educación y vivienda")

#Opinion violencia
table(datos$op_violencia, datos$ano)
datos <- datos %>%
  mutate(
    op_violencia = case_when(
      op_violencia > 4 ~ NA_real_,  # Convertir valores mayores a 6 en NA
      TRUE ~ as.numeric(confianza1)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    op_violencia = factor(
      op_violencia,
      levels = 1:4,
      labels = c(
        "Existe violencia en el país, pero proviene de pequeños grupos sin importancia", 
        "Existe violencia en el país y hay que darle importancia, pero sin exagerar", 
        "Existe violencia en el país y hay que darle mucha importancia porque amenaza con destruir el orden institucional", 
        "No existe violencia en el país; no hay que darle importancia a hechos aislados"
      )
    )
  )
datos$op_violencia <- set_label(datos$op_violencia,  "Dígame, por favor, con cuál de estas frases está más de acuerdo:")


#Identificacion politica
table(datos$iden_pol, datos$ano)

datos <- datos %>%
  mutate(
    iden_pol_nueva = if_else(
      ano %in% c(2018),
      iden_pol,  # Asigna los valores originales de op_matr1
      NA_character_  # Deja NA para otros años (carácter)
    )
  )

datos <- datos %>%
  mutate(
    iden_pol_nueva = recode(iden_pol_nueva, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5, .default = NA_real_))

datos <- datos %>%
  mutate(
    iden_pol = case_when(
      ano == 2018 & !is.na(iden_pol_nueva) ~ as.double(iden_pol_nueva),  # Año 2018
      TRUE ~ as.double(iden_pol)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-iden_pol_nueva)

datos <- datos %>%
  mutate(
    iden_pol = case_when(
      iden_pol > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
      TRUE ~ as.numeric(iden_pol)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    iden_pol = factor(
      iden_pol,
      levels = 1:5,
      labels = c(
        "Izquierda", # Para 1
        2, # Sin etiqueta para 2
        3, # Sin etiqueta para 3
        4, # Sin etiqueta para 4
        "Derecha" # Para 5
      )
    )
  )

datos$iden_pol <- set_label(datos$iden_pol,  "Posición política")

#Actividades
table(datos$actividad_ppal, datos$ano)
table(datos$actividad_ppal2, datos$ano)

datos <- datos %>%
  mutate(
    actividad_ppal = case_when(
      ano == 2013 & !is.na(actividad_ppal2) ~ as.double(actividad_ppal2),  # Año 2018
      TRUE ~ as.double(actividad_ppal)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-actividad_ppal2)

datos <- datos %>%
  mutate(
    actividad_ppal = ifelse(actividad_ppal %in% c(11, 99), NA, actividad_ppal)  # Convertir a NA
  )  # Ordenar los datos por la variable n_personas_hogar

datos$actividad_ppal <- factor(datos$actividad_ppal, 
                                 levels = sort(unique(as.numeric(datos$actividad_ppal))))

datos <- datos %>%
  mutate(
    actividad_ppal = factor(
      actividad_ppal,
      levels = 1:10,
      labels = c(
        "Trabaja jornada completa, 44 horas o más en la semana", 
        "Trabaja jornada parcial, menos de 44 horas en la semana", 
        "Tiene trabajo, pero no está trabajando temporalmente por lic", 
        "Está sin trabajo, pero está buscando trabajo", 
        "Dedicada a labores del hogar o dueña de casa",
        "Estudia en alguna universidad, instituto o centro de formaci",
        "Estudia en algún establecimiento de educación media",
        "Retirado, jubilado o pensionado",
        "Incapacitado para trabajar por enfermedad crónica o invalide",
        "No está haciendo nada"
      )
    )
  )

datos$actividad_ppal <- set_label(datos$actividad_ppal,  "Actividad principal que tiene: Ud.")

table(datos$actividad_ppal3, datos$ano)
datos <- datos %>%
  mutate(
    actividad_ppal3 = case_when(
      actividad_ppal3 > 6 ~ NA_real_,  # Convertir valores mayores a 5 en NA
      TRUE ~ as.numeric(actividad_ppal3)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    actividad_ppal3 = factor(
      actividad_ppal3,
      levels = 1:6,
      labels = c(
        "Patrón o empleador", 
        "Trabajador por cuenta propia",
        "Empleado u obrero", 
        "Servicio doméstico",
        "FF.AA. y del Orden",
        "Familiar no remunerado"
      )
    )
  )

datos$actividad_ppal3 <- set_label(datos$actividad_ppal3,  "En su trabajo o negocio principal, ¿usted trabaja como?")

table(datos$actividad_ppal4, datos$ano)
table(datos$actividad_ppal5, datos$ano)

datos <- datos %>%
  mutate(
    actividad_ppal4 = ifelse(actividad_ppal4 %in% c(5, 8, 9), NA, actividad_ppal4)  # Convertir a NA
  ) 

datos <- datos %>%
  mutate(
    actividad_ppal4 = case_when(
      ano == 2018 & !is.na(actividad_ppal5) ~ as.double(actividad_ppal5),  # Año 2018
      TRUE ~ as.double(actividad_ppal4)  # Para otros casos, se mantiene el valor original
    )
  ) %>%
  select(-actividad_ppal5)

datos <- datos %>%
  mutate(
    actividad_ppal4 = case_when(
      actividad_ppal4 > 5 ~ NA_real_,  # Convertir valores mayores a 5 en NA
      TRUE ~ as.numeric(actividad_ppal4)  # Mantener los valores como numéricos
    )
  ) %>%
  mutate(
    actividad_ppal4 = factor(
      actividad_ppal4,
      levels = 1:5,
      labels = c(
        "Trabajando", 
        "Cesante",
        "Buscando trabajo por primera vez", 
        "Inactivo",
        "Jubilado"
      )
    )
  )

datos$actividad_ppal4 <- set_label(datos$actividad_ppal4,  "Pensando en la última semana, ¿en cuál de las siguientes situaciones laborales se encuentra?")

#Nombres variables

datos$ano <- set_label(datos$ano, "Año")
datos$region <- set_label(datos$region, "Region")
datos$ciudad <- set_label(datos$ciudad, "Ciudad")
datos$comuna <- set_label(datos$comuna, "Comuna")
datos$comuna_aux <- set_label(datos$comuna_aux, "Comuna")
datos$id_comuna <- set_label(datos$id_comuna, "Código Comuna")
datos$namigos <- set_label(datos$namigos, "¿Cuántos amigos cercanos diría Ud. que tiene?")
datos$nvecinos <- set_label(datos$nvecinos, "¿A cuántos vecinos conoce Ud. por su nombre?")
datos$folio <- set_label(datos$folio, "Folio")
datos$ponderador <- set_label(datos$ponderador, "Ponderador")

datos$creencia_jesucristo_hijo[datos$creencia_jesucristo_hijo > 3] <- NA
datos$creeencia_resurreccion[datos$creeencia_resurreccion > 3] <- NA
datos$creencia_milagros_virgen[datos$creencia_milagros_virgen > 3] <- NA
datos$creencia_reencarnacion[datos$creencia_reencarnacion > 3] <- NA
datos$creencia_astrologia[datos$creencia_astrologia > 3] <- NA
datos$creencia_karma[datos$creencia_karma > 3] <- NA
datos$creencia_yoga[datos$creencia_yoga > 3] <- NA
datos$creencia_meditacion[datos$creencia_meditacion > 3] <- NA
datos$creencia_finmundo[datos$creencia_finmundo > 3] <- NA
datos$creencia_energia[datos$creencia_energia > 3] <- NA
datos$creencia_naturaleza[datos$creencia_naturaleza > 3] <- NA
datos$creencia_animitas[datos$creencia_animitas > 3] <- NA
datos$encomendar_familiares[datos$encomendar_familiares > 2] <- NA

datos <- datos %>%
  select(
    folio, folio_unico, ano, ponderador, region, ciudad, comuna, id_comuna, comuna_aux, 
    sexo, edad, tramoedad, educ, educ_pareja, educ_madre, educ_padre, nse, gse, 
    estado_civil, n_personas_hogar, namigos, nvecinos, zona, tipocomuna, 
    religion_encuestado, religion_pareja, religion_madre, religion_padre, 
    religion_hijo, religion_14años, frecuencia_misa_enc, frecuencia_misa_madre, 
    frecuencia_misa_padre, frecuencia_misa_pareja, frecuencia_misa_hijo, 
    frecuencia_misa_14años, frecuencia_rezar1, frecuencia_rezar2, creencia_dios, 
    creencia_milagros, op_religion4, op_religion8, op_religion10, culto_virgen1, 
    culto_virgen2, culto_virgen6, define_como, encomendarse_santo, creencia_cat2, 
    creencia_fe_siniglesia, actividad_ppal, actividad_ppal3, actividad_ppal4, 
    op_matr1, op_matr5, op_matr6, op_matr7, op_matr11, op_aborto, 
    op_trabajo_mujer1, op_trabajo_mujer2, op_trabajo_mujer3, moral_econ1, 
    moral_econ3, moral_econ5, moral_econ6, prob_ascenso1, prob_ascenso2, 
    prob_ascenso3, prob_ascenso4, prob_ascenso5, prob_ascenso6, confianza1, 
    conf_inst1, conf_inst2, conf_inst3, conf_inst4, conf_inst5, conf_inst6, 
    conf_inst7, conf_inst8, conf_inst9, conf_inst11, op_inmigrantes1, 
    op_inmigrantes3, op_violencia, iden_pol,
    everything()  # Coloca el resto de las variables al final
  )

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")
save(datos, file = "base_pt3.RData")

write_sav(data_filtrada, "base_sav.sav")

#Tablas y etiquetas
table(datos$nvecinos, datos$ano)
var_label(datos$n_personas_hogar)
etiquetas <- get_labels(datos$n_educacion_enc)
valores <- get_values(datos$n_educacion_enc)
tabla <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla)