rm(list = ls())
library(dplyr)
library(sjlabelled)
library(labelled)
library(ggplot2)
library(haven)
library(nnet)
library(texreg)
library(marginaleffects)
library(stringr)


load("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_pt3.RData")

#Transmision religiosa

variables_religion <- c("religion_encuestado", "religion_pareja", "religion_madre", "religion_padre", "religion_hijo", "religion_14años")

datos <- datos %>%
  mutate(
    across(
      all_of(variables_religion),
      ~ case_when(
        .x == "Católica" ~ "Católica",
        .x == "Evangélica" ~ "Evangélica",
        .x %in% c("Ateo, agnóstico", "Ninguna religión") ~ "Ninguna religión",
        TRUE ~ NA_character_  # Otras religiones quedan como NA
      ),
      .names = "reli_{.col}"  # Crea nuevas variables con el prefijo 'reli_'
    )
  )

table(datos$reli_encuestado)
table(datos$religion_encuestado)
datos <- datos %>%
  rename(
    reli_encuestado = reli_religion_encuestado,
    reli_pareja = reli_religion_pareja,
    reli_madre = reli_religion_madre,
    reli_padre = reli_religion_padre,
    reli_hijo = reli_religion_hijo,
    reli_14años = reli_religion_14años,
  )

variables_religion2 <- c("religion_encuestado", "religion_14años")

datos <- datos %>%
  mutate(
    across(
      all_of(variables_religion2),
      ~ case_when(
        .x == "Católica" ~ "Católica",
        .x == "Evangélica" ~ "Evangélica",
        .x %in% c("Ateo, agnóstico", "Ninguna religión") ~ "Ninguna religión",
        .x %in% c("Protestante", "Otra religión") ~ "Otra religión",
        TRUE ~ NA_character_  
      ),
      .names = "{.col}2"  # Crea nuevas variables con el prefijo 'reli_'
    )
  )

table(datos$religion_encuestado2)

datos <- datos %>%
  mutate(
    transf_rel = case_when(
      reli_padre == "Católica" & reli_madre == "Católica" ~ "Padres católicos",
      reli_padre == "Católica" & reli_madre == "Ninguna religión" ~ "Padre católico - Madre sin religión",
      reli_padre == "Ninguna religión" & reli_madre == "Católica" ~ "Madre católica - Padre sin religión",
      reli_padre == "Ninguna religión" & reli_madre == "Ninguna religión" ~ "Padres sin religión",
      reli_padre == "Evangélica" & reli_madre == "Evangélica" ~ "Padres evangélicos",
      reli_padre == "Evangélica" & reli_madre == "Ninguna religión" ~ "Padre evangélico - Madre sin religión",
      reli_padre == "Ninguna religión" & reli_madre == "Evangélica" ~ "Madre evangélica - Padre sin religión",
      reli_padre == "Católica" & reli_madre == "Evangélica" ~ "Padre católico - Madre evangélica",
      reli_padre == "Evangélica" & reli_madre == "Católica" ~ "Madre católica - Padre evangélico",
      TRUE ~ NA_character_
    )
  )

table(datos$transf_rel)

datos <- datos %>%
  mutate(
    transf_rel2 = case_when(
      reli_encuestado == "Católica" & reli_pareja == "Católica" ~ "Padres católicos",
      reli_encuestado == "Católica" & reli_pareja == "Ninguna religión" ~ "Padre católico - Madre sin religión",
      reli_encuestado == "Ninguna religión" & reli_pareja == "Católica" ~ "Madre católica - Padre sin religión",
      reli_encuestado == "Ninguna religión" & reli_pareja == "Ninguna religión" ~ "Padres sin religión",
      reli_encuestado == "Evangélica" & reli_pareja == "Evangélica" ~ "Padres evangélicos",
      reli_encuestado == "Evangélica" & reli_pareja == "Ninguna religión" ~ "Padre evangélico - Madre sin religión",
      reli_encuestado == "Ninguna religión" & reli_pareja == "Evangélica" ~ "Madre evangélica - Padre sin religión",
      reli_encuestado == "Católica" & reli_pareja == "Evangélica" ~ "Padre católico - Madre evangélica",
      reli_encuestado == "Evangélica" & reli_pareja == "Católica" ~ "Madre católica - Padre evangélico",
      TRUE ~ NA_character_
    )
  )

table(datos$transf_rel2)

datos <- datos %>%
  mutate(
    transf_rel3 = case_when(
      reli_padre == "Católica" & reli_madre == "Católica" ~ "Padres católicos",
      reli_padre == "Católica" & reli_madre == "Ninguna religión" ~ "Padre católico",
      reli_padre == "Ninguna religión" & reli_madre == "Católica" ~ "Madre católica",
      reli_padre == "Ninguna religión" & reli_madre == "Ninguna religión" ~ "Padres sin religión",
      reli_padre == "Evangélica" & reli_madre == "Evangélica" ~ "Padres evangélicos",
      reli_padre == "Evangélica" & reli_madre == "Ninguna religión" ~ "Otra",
      reli_padre == "Ninguna religión" & reli_madre == "Evangélica" ~ "Otra",
      reli_padre == "Católica" & reli_madre == "Evangélica" ~ "Padre católico",
      reli_padre == "Evangélica" & reli_madre == "Católica" ~ "Madre católica",
      TRUE ~ NA_character_
    )
  )
table(datos$transf_rel3)


#Actividad religiosa

variables_misa <- c("frecuencia_misa_enc", "frecuencia_misa_madre", "frecuencia_misa_padre", "frecuencia_misa_pareja", "frecuencia_misa_hijo", "frecuencia_misa_14años")

datos <- datos %>%
  mutate(across(
    .cols = all_of(variables_misa), 
    .fns = ~ case_when(
      . %in% c("Más de una vez a la semana", "Una vez a la semana", "Algunas veces al mes") ~ 1,
      . %in% c("Solo en fechas religiosas", "Nunca o casi nunca") ~ 0,
      TRUE ~ NA_real_  # Mantener los NA para valores no especificados
    ),
    .names = "rec_{.col}" # Crear nuevas columnas con prefijo "rec_"
  )) %>%
  mutate(across(
    .cols = starts_with("rec_"), 
    .fns = ~ factor(., levels = c(0, 1), labels = c("Inactivo", "Activo")),
    .names = "{.col}" 
  ))

table(datos$rec_frecuencia_misa_enc)

datos <- datos %>%
  rename(
    misa_encuestado = rec_frecuencia_misa_enc,
    misa_pareja = rec_frecuencia_misa_pareja,
    misa_madre = rec_frecuencia_misa_madre,
    misa_padre = rec_frecuencia_misa_padre,
    misa_hijo = rec_frecuencia_misa_hijo,
    misa_14años = rec_frecuencia_misa_14años,
  )


datos <- datos %>%
  mutate(
    act_rel = case_when(
      misa_padre == "Activo" & misa_madre == "Activo" ~ "Padres activos",
      misa_padre == "Activo" & misa_madre == "Inactivo" ~ "Padre activo - Madre inactiva",
      misa_padre == "Inactivo" & misa_madre == "Activo" ~ "Madre activa - Padre inactivo",
      misa_padre == "Inactivo" & misa_madre == "Inactivo" ~ "Padres inactivos"
    )
  )

table(datos$act_rel)

#Actividad religiosa 2

variables_misa <- c("frecuencia_misa_enc", "frecuencia_misa_madre", "frecuencia_misa_padre", "frecuencia_misa_pareja", "frecuencia_misa_hijo", "frecuencia_misa_14años")

datos <- datos %>%
  mutate(across(
    .cols = all_of(variables_misa), 
    .fns = ~ case_when(
      . %in% c("Más de una vez a la semana", "Una vez a la semana") ~ 1,
      . %in% c("Solo en fechas religiosas", "Nunca o casi nunca") ~ 0,
      . %in% c("Algunas veces al mes") ~ NA_real_,  
      TRUE ~ NA_real_  
    ),
    .names = "rec2_{.col}"  
  )) %>%
  mutate(across(
    .cols = starts_with("rec2_"),  # Corregido para que coincida con el prefijo correcto
    .fns = ~ factor(., levels = c(0, 1), labels = c("Inactivo", "Activo")),
    .names = "{.col}" 
  ))


table(datos$rec2_frecuencia_misa_enc)

datos <- datos %>%
  rename(
    misa_encuestado2 = rec2_frecuencia_misa_enc,
    misa_pareja2 = rec2_frecuencia_misa_pareja,
    misa_madre2 = rec2_frecuencia_misa_madre,
    misa_padre2 = rec2_frecuencia_misa_padre,
    misa_hijo2 = rec2_frecuencia_misa_hijo,
    misa_14años2 = rec2_frecuencia_misa_14años,
  )

datos <- datos %>%
  mutate(
    act_rel2 = case_when(
      misa_padre2 == "Activo" & misa_madre2 == "Activo" ~ "Padres activos",
      misa_padre2 == "Activo" & misa_madre2 == "Inactivo" ~ "Padre activo - Madre inactiva",
      misa_padre2 == "Inactivo" & misa_madre2 == "Activo" ~ "Madre activa - Padre inactivo",
      misa_padre2 == "Inactivo" & misa_madre2 == "Inactivo" ~ "Padres inactivos"
    )
  )



#Rezar
datos <- datos %>%
  mutate(
    frec_rezar2 = case_when(
      frecuencia_rezar2 %in% c("Varias veces durante el día", "Al menos una vez al día", "Algunos días sí y otros no") ~ 1,
      frecuencia_rezar2 %in% c("Solo en fechas religiosas", "Nunca o casi nunca") ~ 0,
      TRUE ~ NA_real_
    ),
    frec_rezar2 = factor(frec_rezar2, levels = c(0, 1), labels = c("Inactivo", "Activo"))
  )

#Cree en dios

table(datos$creencia_dios)

datos <- datos %>%
  mutate(cree_dios = case_when(
    creencia_dios == "Si, y no tiene dudas" ~ 1,  # Si y no tiene dudas, asignar 1
    creencia_dios == "A momentos sí, en otros no" | creencia_dios == "No cree" ~ 0,  # A momentos sí o no cree, asignar 0
    TRUE ~ NA_real_  # En otros casos asignar NA (si es necesario)
  )) %>%
  mutate(cree_dios = factor(cree_dios, levels = c(0, 1), labels = c("No", "Sí")))

table(datos$cree_dios)

#Variables de control

datos <- datos %>%
  mutate(
    educ_enc = case_when(
      educ %in% c("Sin Estudios / Básica incompleta / Pre Básica / Especial-Diferencial", 
                  "Básica completa") ~ "Básica",
      educ %in% c("Media o Humanidades") ~ "Media",
      educ %in% c("Centro de Formación Técnica o Instituto Profesional incompleto", 
                  "Centro de Formación Técnica o Instituto Profesional completo / Universitaria incompleta",
                  "Universitaria completa",
                  "Post Grado (Magíster o Doctorado)") ~ "Universitaria",
      TRUE ~ NA_character_  # Para valores no especificados
    )
  )

table(datos$educ_enc)

table(datos$nse, datos$ano) #2019
d2019 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2019_BBDD_BicentenarioUC.sav")
table(d2019$cvar11)

d2019 <- d2019 %>%
  mutate(folio_unico = seq(26228, 28274, by = 1))

d2019 <- d2019 %>%
  mutate(ano = 2019)

d2019 <- d2019 %>%
  rename(nse1 = cvar11)

d2019 <- d2019 %>%
  select(folio_unico, ano, nse1)

datos <- datos %>%
  left_join(d2019, by = c("folio_unico", "ano"))

datos <- datos %>%
  mutate(
    nse = if_else(
      ano == 2019 & !is.na(nse1),
      as.character(nse1),  # Convierte nse1 a carácter
      as.character(nse)    # Convierte nse a carácter
    )
  ) %>%
  select(-nse1)  # Elimina la variable nse1

datos <- datos %>%
  mutate(
    nse = factor(
      nse,
      levels = 1:5,  # Define los niveles originales
      labels = c("ABC1", "C2", "C3", "D", "E")  # Asigna etiquetas
    )
  )

table(datos$nse, datos$ano)

datos <- datos %>%
  mutate(
    nse1 = case_when(
      nse == "ABC1" ~ "Alto",    # Recodifica ABC1 como alto
      nse %in% c("C2", "C3") ~ "Medio",  # Recodifica C2 y C3 como medio
      nse %in% c("D", "E") ~ "Bajo"      # Recodifica D y E como bajo
    )
  )
table(datos$nse1, datos$ano)

table(datos$tramoedad, datos$ano)
datos <- datos %>%
  mutate(
    año_nacimiento = ano - edad
  )
table(datos$año_nacimiento)
datos <- datos %>%
  mutate(
    cohorte_nac = case_when(
      año_nacimiento >= 1910 & año_nacimiento <= 1919 ~ "1910-1919",
      año_nacimiento >= 1920 & año_nacimiento <= 1929 ~ "1920-1929",
      año_nacimiento >= 1930 & año_nacimiento <= 1939 ~ "1930-1939",
      año_nacimiento >= 1940 & año_nacimiento <= 1949 ~ "1940-1949",
      año_nacimiento >= 1950 & año_nacimiento <= 1959 ~ "1950-1959",
      año_nacimiento >= 1960 & año_nacimiento <= 1969 ~ "1960-1969",
      año_nacimiento >= 1970 & año_nacimiento <= 1979 ~ "1970-1979",
      año_nacimiento >= 1980 & año_nacimiento <= 1989 ~ "1980-1989",
      año_nacimiento >= 1990 & año_nacimiento <= 1999 ~ "1990-1999",
      año_nacimiento >= 2000 & año_nacimiento <= 2009 ~ "2000-2009",
      TRUE ~ "Otro"  # Para cubrir otros casos fuera del rango de interés
    )
  )

table(datos$cohorte_nac, datos$ano)

datos <- datos %>%
  mutate(asistencia_virgen2 = ifelse(
    ano == 2017 & rowSums(select(., starts_with("asistencia_fiesta")) == 1, na.rm = TRUE) > 0, 
    1, 
    ifelse(ano == 2017, 2, NA)
  ))

datos <- datos %>%
  mutate(aasistencia_fiestamariana1 = as.numeric(aasistencia_fiestamariana1),  # Convertir a numérico
         asistencia_virgen3 = case_when(
           aasistencia_fiestamariana1 == 1 ~ 2,
           aasistencia_fiestamariana1 >= 2 ~ 1,
           TRUE ~ as.numeric(NA)  # Mantener NA si hay valores faltantes
         ))


table(datos$asistencia_virgen2)
table(datos$asistencia_virgen3)
table(datos$asistencia_virgen, datos$ano)
datos <- datos %>%
  mutate(asistencia_virgen2 = as.character(asistencia_virgen2),
         asistencia_virgen3 = as.character(asistencia_virgen3), 
         asistencia_virgen = coalesce(asistencia_virgen, asistencia_virgen2, asistencia_virgen3))
datos <- datos %>%
  mutate(asistencia_virgen = case_when(
    asistencia_virgen == 1 ~ "Si",
    asistencia_virgen == 2 ~ "No",
    TRUE ~ NA_character_  # Mantener los NA
  ))


table(datos$frecuencia_santuarios, datos$ano)
datos <- datos %>%
  mutate(frecuencia_santuarios = ifelse(frecuencia_santuarios > 5, NA, frecuencia_santuarios))

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")
save(datos, file = "base_pt4.RData")
write_sav(datos, "base_sav.sav")

rm(list = ls())
load("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_pt4.RData")

#Modelo 1
m1 <- multinom(
  relevel(factor(reli_encuestado), ref = "Ninguna religión") ~ 
    relevel(factor(transf_rel), ref = "Padres sin religión") + 
    relevel(factor(act_rel2), ref = "Padres inactivos") + 
    relevel(factor(cohorte_nac), ref = "1910-1919") + 
    relevel(factor(nse1), ref = "Bajo") +
    cree_dios + educ_enc + sexo + ano, 
  data = datos, 
  trace = FALSE
)
screenreg(m1)
predict(m1, type="prob")

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")

unique(datos$act_rel)


# Separar los modelos internos del modelo multinomial
m1_catolica <- summary(m1)$coefficients[, 1]  # Coeficientes para "Católica"
m1_evangelica <- summary(m1)$coefficients[, 2]  # Coeficientes para "Evangélica"

# Extraer los modelos con extract() para texreg
m1_models <- extract(m1, beside = TRUE, 
                     custom.model.names = c("Católica", "Evangélica"))

# Personalizar nombres de variables
custom.coef.names <- c(
  "(Intercept)" = "Intercepto",
  "transf_relMadre católica - Padre evangélico" = "Madre católica - Padre evangélico",
  "transf_relMadre católica - Padre sin religión" = "Madre católica - Padre sin religión",
  "transf_relMadre evangélica - Padre sin religión" = "Madre evangélica - Padre sin religión",
  "transf_relPadre católico - Madre evangélica" = "Padre católico - Madre evangélica",
  "transf_relPadre católico - Madre sin religión" = "Padre católico - Madre sin religión",
  "transf_relPadre evangélico - Madre sin religión" = "Padre evangélico - Madre sin religión",
  "transf_relPadres católicos" = "Padres católicos",
  "transf_relPadres evangélicos" = "Padres evangélicos",
  "act_relMadre activa - Padre inactivo" = "Madre activa - Padre inactivo",
  "act_relPadre activo - Madre inactiva" = "Padre activo - Madre inactiva",
  "act_relPadres activos" = "Padres activos",
  "cohorte_nac1920-1929" = "1920-1929",
  "cohorte_nac1930-1939" = "1930-1939",
  "cohorte_nac1940-1949" = "1940-1949",
  "cohorte_nac1950-1959" = "1950-1959",
  "cohorte_nac1960-1969" = "1960-1969",
  "cohorte_nac1970-1979" = "1970-1979",
  "cohorte_nac1980-1989" = "1980-1989",
  "cohorte_nac1990-1999" = "1990-1999",
  "cohorte_nac2000-2009" = "2000-2009",
  "nse1Alto" = "NSE Alto",
  "nse1Medio" = "NSE Medio",
  "cree_diosSí" = "Cree en Dios",
  "educ_encMedia" = "Educación Media",
  "educ_encUniversitaria" = "Educación Universitaria",
  "sexoMujer" = "Mujer",
  "ano" = "Año"
)

htmlreg(m1_models,
        custom.coef.names = custom.coef.names,
        custom.model.names = c("Católica", "Evangélica"),
        stars = c(0.01, 0.05, 0.1),
        star.symbol = "*",
        leading.zero = TRUE,
        digits = 3,
        inline.css = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        caption = "Resultados del Modelo Multinomial",
        caption.above = TRUE,
        custom.note = "Nota: *p<0.1; **p<0.05; ***p<0.01",
        file = "modelo_multinomial.html")

# 1. Extraer los coeficientes del modelo
m1_or <- m1_models  # Crear una copia del modelo

# 2. Exponenciar los coeficientes para obtener Odds Ratios
for (i in seq_along(m1_or)) {
  m1_or[[i]]@coef <- exp(m1_or[[i]]@coef)  # Exponenciar los coeficientes
  m1_or[[i]]@se <- exp(m1_or[[i]]@se)      # Exponenciar los errores estándar
}

# 3. Exportar la tabla con Odds Ratios en HTML
htmlreg(m1_or,
        custom.coef.names = custom.coef.names,
        custom.model.names = c("Católica", "Evangélica"),
        stars = c(0.01, 0.05, 0.1),
        star.symbol = "*",
        leading.zero = TRUE,
        digits = 3,
        inline.css = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        caption = "Odds Ratios del Modelo Multinomial",
        caption.above = TRUE,
        custom.note = "Nota: *p<0.1; **p<0.05; ***p<0.01",
        file = "modelo_multinomial_oddsratio.html")  # Archivo HTML

##Modelos Nones
rm(list = ls())
load("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_profe.RData")
datos1 <- base
datos <- datos %>%
  mutate(
    nones = case_when(
      religion_encuestado %in% c("Ateo, agnóstico", "Ninguna religión") ~ 1,
      religion_encuestado %in% c("Católica", "Evangélica", "Otra religión") ~ 0,
      TRUE ~ NA_real_
    )
  )

datos <- datos %>%
  mutate(tronco_religioso = case_when(
    transf_rel == "Madre católica - Padre evangélico" ~ "Tronco religioso",
    transf_rel == "Madre católica - Padre sin religión" ~ "Tronco religioso",
    transf_rel == "Madre evangélica - Padre sin religión" ~ "Tronco religioso",
    transf_rel == "Padre católico - Madre evangélica" ~ "Tronco religioso",
    transf_rel == "Padre católico - Madre sin religión" ~ "Tronco no religioso",
    transf_rel == "Padre evangélico - Madre sin religión" ~ "Tronco no religioso",
    transf_rel == "Padres católicos" ~ "Tronco religioso",
    transf_rel == "Padres evangélicos" ~ "Tronco religioso",
    transf_rel == "Padres sin religión" ~ "Tronco no religioso",
    TRUE ~ NA_character_ # Asignar NA en caso de que no coincida con ninguna categoría
  ))

datos <- datos %>%
  mutate(tronco_activo = case_when(
    act_rel == "Madre activa - Padre inactivo" ~ "Tronco activo",
    act_rel == "Padre activo - Madre inactiva" ~ "Tronco inactivo",
    act_rel == "Padres activos" ~ "Tronco activo",
    act_rel == "Padres inactivos" ~ "Tronco inactivo",
    TRUE ~ NA_character_ # Asignar NA en caso de que no coincida con ninguna categoría
  ))

datos <- datos %>%
  mutate(
    educ_enc = case_when(
      str_trim(educ_enc) %in% c("Básica") ~ "Básica",
      str_trim(educ_enc) %in% c("Media") ~ "Media",
      str_trim(educ_enc) %in% c("Universitaria") ~ "Universitaria",
      TRUE ~ NA_character_  # Para todo lo que no coincide, poner NA
    )
  )

table(datos$estado_civil)
datos <- datos %>%
  mutate(estado_civil2 = case_when(
    estado_civil %in% c("Conviviente sin acuerdo", "Conviviente con acuerdo") ~ "Conviviente",
    TRUE ~ estado_civil
  ))
table(datos$estado_civil2)

#Primer modelo
summary(m1)
m1 <- lm(
  nones ~ 
    relevel(factor(tronco_religioso), ref = "Tronco no religioso") +
    relevel(factor(tronco_activo), ref = "Tronco inactivo") + 
    relevel(factor(sexo), ref = "Hombre") + 
    relevel(factor(tramoedad), ref = "18 a 24") + 
    relevel(factor(educ_enc), ref = "Universitaria") +
    estado_civil2,
  data = datos
)
summary(m1)

#Modelo 2
m2 <- lm(
  nones ~ 
    relevel(factor(iden_pol), ref = "Izquierda") + 
    relevel(factor(sexo), ref = "Hombre") + 
    relevel(factor(tramoedad), ref = "18 a 24") + 
    relevel(factor(educ_enc), ref = "Universitaria") +
    estado_civil2,
  data = datos
)
summary(m2)

#Modelo 3
m3 <- lm(
  nones ~ 
    relevel(factor(op_aborto), ref = "Bajo cualquier circunstancia") + 
    relevel(factor(op_matr7), ref = "Muy de acuerdo") + 
    relevel(factor(op_matr11), ref = "Muy de acuerdo") + 
    relevel(factor(sexo), ref = "Hombre") + 
    relevel(factor(tramoedad), ref = "18 a 24") + 
    relevel(factor(educ_enc), ref = "Universitaria") +
    estado_civil2,
  data = datos
)
summary(m3)

