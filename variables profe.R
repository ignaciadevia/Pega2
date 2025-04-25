rm(list = ls())
library(haven)
library(dplyr)

datos1 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_rel1sav.sav")
load("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_pt4.RData")

#Agregar año 2021
d2021 <- datos %>% filter(ano == 2021)

nuevas_filas <- data.frame(
  folio_unico = seq(28275, 30276),  # Generar folio_unico para 2021 (2002 casos)
  ano = rep(2021, 2002),
  folio = seq(1, 2002))

datos2 <- bind_rows(datos1, nuevas_filas)

datos1 <- datos1 %>%
  mutate(across(where(is.character), as.factor))
d2021 <- d2021 %>%
  mutate(across(where(is.character), as.factor))
datos1 <- datos1 %>%
  mutate(across(where(is.labelled), ~ as_factor(.)))
d2021 <- d2021 %>%
  mutate(across(where(is.labelled), ~ as_factor(.)))

# Verificar columnas que están en datos1 pero no en d2021
setdiff(names(datos1), names(d2021))

d2021 <- d2021 %>%
  rename(
    nse3 = nse1,
    nse5 = nse,
    )
#Crear relmother, relego, relmadre, nones1, nones2

# Verificar columnas que están en d2021 pero no en datos1
setdiff(names(d2021), names(datos1))

# Lista de nombres de las variables a eliminar
variables_a_eliminar <- c("region", "ciudad", "comuna", "id_comuna", "comuna_aux", "educ_pareja", 
                          "gse", "n_personas_hogar", "namigos", "nvecinos", "zona", "tipocomuna", 
                          "actividad_ppal", "actividad_ppal3", "actividad_ppal4", "op_trabajo_mujer1", 
                          "op_trabajo_mujer2", "op_trabajo_mujer3", "moral_econ1", "moral_econ3", 
                          "moral_econ5", "moral_econ6", "prob_ascenso1", "prob_ascenso2", "prob_ascenso3", 
                          "prob_ascenso4", "prob_ascenso5", "prob_ascenso6", "confianza1", 
                          "conf_inst2", "conf_inst3", "conf_inst4", "conf_inst5", "conf_inst6", "conf_inst7", 
                          "conf_inst8", "conf_inst9", "conf_inst11", "op_inmigrantes1", "op_inmigrantes3", 
                          "op_violencia", "dfolio", "id", "reli", "pareja", "rr10", "educ_hijo1", 
                          "educ_hijo2", "educ_hija1", "trabajo", "profesion_padre", "profesion_madre", 
                          "vivia_15años", "madre_era", "op_matr2", "op_matr3", "op_matr4", "op_matr8", 
                          "op_matr9", "op_matr10", "op_matr12", "op_matr13", "op_familia1", "op_familia2", 
                          "op_familia4", "op_familia6", "disciplina1", "disciplina2", "ev_relacion_pareja", 
                          "frecuencia_biblia", "frecuencia_confiesa", "frecuencia_mandas", "frecuencia_santuarios", 
                          "frecuencia_comulga", "creencia_brujeria", "creencia_vidamuerte", "creencia_virgen", 
                          "creencia_horoscopo", "creencia_santos", "valrel_hijos1", "valrel_hijos2", 
                          "valrel_hijos3", "valrel_hijos4", "valrel_hijos5", "valrel_hijos6", "op_religion1", 
                          "op_religion3", "op_religion5", "op_religion6", "op_religion7", "op_religion11", 
                          "op_religion13", "op_nathum1", "op_nathum2", "op_nathum3", "op_nathum4", 
                          "rezar_virgen", "culto_virgen3", "culto_virgen4", "culto_virgen5", "culto_virgen7", 
                          "aasistencia_fiestamariana1", "asistencia_fiestamariana2", "asistencia_fiestamariana3", 
                          "asistencia_fiestamariana4", "asistencia_fiesta1", "asistencia_fiesta2", 
                          "asistencia_fiesta3", "asistencia_fiesta4", "asistencia_fiesta5", "asistencia_fiesta6", 
                          "asistencia_fiesta7", "asistencia_fiesta8", "asistencia_fiesta9", "asistencia_fiesta10", 
                          "asistencia_fiesta11", "asistencia_fiesta12", "atributos_virgen1", "atributos_virgen2", 
                          "santo1", "santo2", "santo3", "santo4", "santuario_padre_hurtado", "santuario_padre_hurtado1", 
                          "santuario_padre_hurtado2", "santuario_padre_hurtado3", "santuario_santa_teresita", 
                          "santuario_santa_teresita1", "santuario_santa_teresita2", "santuario_santa_teresita3", 
                          "describir_cambio_religion1", "describir_cambio_religion2", "edad_cambio", "veces_cambio", 
                          "motivo_cambio1", "motivo_cambio2", "motivo_cambio3", "motivo_cambio4", "evento_compromiso", 
                          "edad_evento_compromiso", "evento_compromiso1", "evento_compromiso2", "evento_compromiso3", 
                          "evento_compromiso4", "evento_compromiso5", "publicidad_religion", "asisitencia_misa_catolica", 
                          "asisitencia_misa_evangelica", "asisitencia_misa_protestante", "asisitencia_misa_testigos", 
                          "asisitencia_misa_mormones", "asisitencia_misa_otra", "frecuencia_misa_catolica", 
                          "frecuencia_misa_evangelica", "frecuencia_misa_protestante", "frecuencia_misa_testigos", 
                          "frecuencia_misa_mormones", "frecuencia_misa_otra", "frec_actrel1", "frec_actrel2", 
                          "frec_actrel3", "frec_actrel4", "frec_actrel5", "frec_actrel6", "frec_actrel7", "frec_actrel8", 
                          "frec_actrel9", "frec_actrel10", "frec_actrel11", "asistencia_virgen", "cuan_religioso", 
                          "devenir_religioso", "cambio_religion", "temor_muerte_propia", "op_politica1", "op_religion9", 
                          "tema_iglesia1", "tema_iglesia2", "tema_iglesia3", "tema_iglesia4", "tema_iglesia5", 
                          "tema_iglesia6", "dedicacion_sacedrote", "consejos_papa", "desempeño_iglesia1", 
                          "desempeño_iglesia2", "desempeño_iglesia3", "desempeño_iglesia4", "desempeño_iglesia5", 
                          "desempeño_sacerdote1", "desempeño_sacerdote2", "desempeño_sacerdote3", "desempeño_sacerdote4", 
                          "desempeño_sacerdote5", "creencia_cat1", "creencia_cat3", "creencia_cat4", "feliz_fe", 
                          "creencia_fe_sinritos", "uncion_muertos", "op_iglesia4", "muerte_piensa", "muerte_conversa", 
                          "temor_muerte_padres", "temor_muerte_amigos", "temor_muerte_hijos", "temor_muerte_pareja", 
                          "sepultado", "mirar_muertos", "salvacion_muerte1", "salvacion_muerte2", "muerte_identificacion1", 
                          "muerte_identificacion2", "muerte_identificacion3", "muerte_identificacion4", 
                          "muerte_identificacion5", "muerte_identificacion6", "muerte_identificacion7", "tipo_igualdad", 
                          "op_trabajo_mujer4", "op_trabajo_mujer5", "op_trabajo_mujer6", "op_trabajo_mujer7", 
                          "op_trabajo_mujer8", "op_trabajo_mujer9", "op_trabajo_mujer10", "op_trabajo_mujer11", 
                          "op_trabajo_mujer12", "op_trabajo_mujer13", "moral_econ2", "moral_econ4", "causas_riqueza1", 
                          "causas_riqueza2", "causas_pobreza1", "causas_pobreza2", "op_rol_estado1", "op_rol_estado2", 
                          "op_rol_estado3", "percep_mov", "membersia1", "membersia2", "membersia3", "membersia4", 
                          "membersia5", "membersia6", "membersia7", "membersia8", "membersia9", "membersia10", 
                          "membersia11", "membersia12", "membersia13", "membersia14", "membersia15", "membersia16", 
                          "membersia17", "membersia18", "confianza2", "confianza3", "confianza4", "confianza5", 
                          "confianza6", "confianza7", "conf_inst10", "conf_inst12", "conf_inst13", "conf_inst14", 
                          "conf_inst15", "op_politica2", "op_politica4", "op_politica5", "tolerancia1", "tolerancia2", "tolerancia3", 
                          "tolerancia4", "tolerancia5", "op_chile1", "op_chile3", "orgullo_chileno", "percep_clase", 
                          "op_inmigrantes4", "op_inmigrantes5", "op_inmigrantes6", "op_inmigrantes7", "op_inmigrantes8", 
                          "op_inmigrantes9", "salario_suficiente", "justificacion_violencia6", "democracia1", "op_mujeres1", 
                          "op_mujeres2", "papa_francisco1", "papa_francisco2", "papa_francisco3", "papa_francisco4", 
                          "papa_francisco5", "cambios_papa1", "cambios_papa2", "cambios_papa3", "cambios_papa4", "cambios_papa5", 
                          "cambios_papa6", "cambios_papa7", "cambios_papa8", "op_iglesia", "tiene_hijos", "perdida_influencia", 
                          "marihuana", "confianza_inter1", "confianza_inter2", "confianza_inter3", "confianza_inter4", 
                          "confianza_inter5", "confianza_inter6", "membersia19", "causa_abusos1", "causa_abusos2", "causa_abusos3", 
                          "causa_abusos4", "dios_presente1", "dios_presente2", "dios_presente3", "dios_presente4", "evento_cambio2", 
                          "evento_cambio3", "pp_indispensables", "op_chile2", "op_chile4", "op_chile6", "op_chile5", "expectativa_mov", 
                          "mov_inter2", "mov_inter5", "op_trabajo_mujer14", "op_trabajo_mujer16", "op_trabajo_mujer15", 
                          "justificacion_violencia1", "justificacion_violencia2", "justificacion_violencia3", "justificacion_violencia4", 
                          "justificacion_violencia5", "justificacion_violencia7", "justificacion_violencia8", "justificacion_violencia9", 
                          "op_inmigrantes2", "nhijos", "ideal_hijos", "prob_ascenso7", "reli_14años", "religion_14años2", 
                          "misa_encuestado2", "misa_madre2", "misa_padre2", "misa_pareja2", "misa_hijo2", "misa_14años2", 
                          "act_rel2", "frec_rezar2", "asistencia_virgen2", "conf_inst1")

d2021 <- subset(d2021, select = setdiff(names(d2021), variables_a_eliminar))

datos2 <- bind_rows(datos1, d2021)
datos2 <- datos2 %>% arrange(folio_unico)

#Meter confianza iglesia
datos2 <- merge(datos2, datos[, c("folio_unico", "conf_inst1")], by = "folio_unico", all.x = TRUE)

##Arreglar otras variables
#creencia_resurrección
datos2 <- datos2 %>%
  mutate(creencia_resureccion = coalesce(creencia_resureccion, creeencia_resurreccion)) %>%
  select(-creeencia_resurreccion)

datos2 <- datos2 %>% select(-conf_inst17)

datos2 <- datos2 %>%
  mutate(consejo_sacerdote = coalesce(consejo_sacerdote, recurrir_sacerdote)) %>%
  select(-recurrir_sacerdote)

#Op_religion8, 2024
datos2$op_religion2 <- ifelse(datos2$ano %in% c(2024), datos2$op_religion2, NA)
datos2 <- datos2 %>%
  mutate(op_religion2 = as.numeric(as.character(op_religion2))) %>%  # convertir a numérico
  mutate(op_religion2 = case_when(
    op_religion2 > 5 ~ NA_real_,
    TRUE ~ op_religion2
  )) %>%
  mutate(op_religion2 = factor(op_religion2, 
                               levels = 1:5, 
                               labels = c(
                                 "Muy de acuerdo",
                                 "De acuerdo",
                                 "Ni acuerdo ni desacuerdo",
                                 "En desacuerdo",
                                 "Muy en desacuerdo"
                               )))
table(datos2$op_religion2, datos2$ano)

datos2 <- datos2 %>%
  mutate(
    op_religion8 = coalesce(as.character(op_religion8), as.character(op_religion2))
  ) %>%
  select(-op_religion2)
table(datos2$op_religion8, datos2$ano)

#Cambio religion 2009, 2018
#2009
d2009 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2009_BBDD_BicentenarioUC.sav")

d2009 <- d2009 %>% 
  filter(id >= 0000 & id <= 2012)

table(d2009$q80)
d2009 <- d2009 %>%
  mutate(folio_unico = seq(6105, 8116, by = 1))

d2009 <- d2009 %>%
  mutate(ano = 2009)

d2009 <- d2009 %>%
  rename(cambio_r1 = q80)

d2009 <- d2009 %>%
  select(folio_unico, ano, cambio_r1)

d2009 <- d2009 %>%
  mutate(cambio_r1 = factor(cambio_r1,
                            levels = 1:11,
                            labels = c(
                              "Sí, de católico a no creyente",
                              "Sí, de católico a evangélico",
                              "Sí, de no creyente a católico",
                              "Sí, de no creyente a evangélico",
                              "Sí, de evangélico a católico",
                              "Sí, de evangélico a no creyente",
                              "Sí, de católico a otra religión no evangélica",
                              "Sí, de otra religión no evangélica a católico",
                              "Sí, de no creyente a otra religión no evangélica",
                              "Sí, otro cambio no contemplado en las respuestas anteriores",
                              "No, no he cambiado"
                            )))

d2018 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2018_BBDD_BicentenarioUC.sav")
table(d2018$R08_1)
table(d2018$R08_2)

d2018 <- d2018 %>%
  mutate(folio_unico = seq(24217, 26227, by = 1))

d2018 <- d2018 %>%
  mutate(ano = 2018)

d2018 <- d2018 %>%
  rename(cambio_antes = R08_1)

d2018 <- d2018 %>%
  rename(cambio_ahora = R08_2)

d2018 <- d2018 %>%
  select(folio_unico, cambio_ahora, cambio_antes, ano)

variables_religion <- c("cambio_ahora", "cambio_antes")

variables_religion <- c("cambio_ahora", "cambio_antes")

d2018 <- d2018 %>%
  mutate(
    across(
      all_of(variables_religion),
      ~ case_when(
        .x == 1 ~ "Católica",
        .x == 2 ~ "Evangélica",
        .x %in% c(3, 4) ~ "Otra religión",
        .x %in% c(5, 6) ~ "Ninguna religión",
        TRUE ~ NA_character_
      ),
      .names = "{.col}2"
    )
  )

d2018 <- d2018 %>%
  mutate(
    cambio_r2 = case_when(
      cambio_antes2 == "Católica" & cambio_ahora2 == "Católica" ~ "No, no he cambiado",
      cambio_antes2 == "Católica" & cambio_ahora2 == "Ninguna religión" ~ "Sí, de católico a no creyente",
      cambio_antes2 == "Ninguna religión" & cambio_ahora2 == "Católica" ~ "Sí, de no creyente a católico",
      cambio_antes2 == "Ninguna religión" & cambio_ahora2 == "Ninguna religión" ~ "No, no he cambiado",
      cambio_antes2 == "Evangélica" & cambio_ahora2 == "Evangélica" ~ "No, no he cambiado",
      cambio_antes2 == "Evangélica" & cambio_ahora2 == "Ninguna religión" ~ "Sí, de evangélico a no creyente",
      cambio_antes2 == "Ninguna religión" & cambio_ahora2 == "Evangélica" ~ "Sí, de no creyente a evangélico",
      cambio_antes2 == "Católica" & cambio_ahora2 == "Evangélica" ~ "Sí, de católico a evangélico",
      cambio_antes2 == "Evangélica" & cambio_ahora2 == "Católica" ~ "Sí, de evangélico a católico",
      cambio_antes2 == "Católica" & cambio_ahora2 == "Otra religión" ~ "Sí, de católico a otra religión no evangélica",
      cambio_antes2 == "Otra religión" & cambio_ahora2 == "Católica" ~ "Sí, de otra religión no evangélica a católico",
      cambio_antes2 == "Ninguna religión" & cambio_ahora2 == "Otra religión" ~ "Sí, de no creyente a otra religión no evangélica",
      cambio_antes2 == "Evangélica" & cambio_ahora2 == "Otra religión" ~ "Sí, otro cambio no contemplado en las respuestas anteriores",
      cambio_antes2 == "Otra religión" & cambio_ahora2 == "Evangélica" ~ "Sí, otro cambio no contemplado en las respuestas anteriores",
      cambio_antes2 == "Otra religión" & cambio_ahora2 == "Ninguna religión" ~ "Sí, otro cambio no contemplado en las respuestas anteriores",
      cambio_antes2 == "Otra religión" & cambio_ahora2 == "Otra religión" ~ "No, no he cambiado",
      TRUE ~ NA_character_
    )
  )

table(d2018$cambio_r2)
table(d2018$cambio_antes, d2018$cambio_ahora)
d2018 <- d2018 %>%
  select(folio_unico, cambio_r2, ano)

datos2 <- datos2 %>%
  left_join(d2009, by = c("folio_unico", "ano"))

datos2 <- datos2 %>%
  left_join(d2018, by = c("folio_unico", "ano"))

datos2 <- datos2 %>%
  mutate(cambio_religion2 = coalesce(cambio_r1, cambio_r2)) %>%
  select(-cambio_r1, cambio_r2)

table(datos2$cambio_religion2, datos2$ano)

#Preguntas 2021
d2021 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2021_BBDD_BicentenarioUC.sav")
table(d2021$P68)
table(d2021$P69)
table(d2021$I_1_P70)
d2021 <- d2021 %>%
  mutate(folio_unico = seq(28275, 30276, by = 1))

d2021 <- d2021 %>%
  mutate(ano = 2021)

d2021 <- d2021 %>%
  rename(
    abusos_identificarse = P68,
    abusos_noir_iglesia = P69,
    consistencia_derecho_vida1 = I_1_P70,
    consistencia_derecho_vida2 = I_2_P70,
    consistencia_derecho_vida3 = I_3_P70,
    consistencia_derecho_vida4 = I_4_P70,
    consistencia_derecho_vida5 = I_5_P70,
    consistencia_derecho_vida6 = I_6_P70,
    consistencia_derecho_vida7 = I_7_P70)

d2021 <- d2021 %>%
  select(folio_unico, ano, abusos_identificarse, abusos_noir_iglesia, consistencia_derecho_vida1, consistencia_derecho_vida2, consistencia_derecho_vida3, consistencia_derecho_vida4, consistencia_derecho_vida5, consistencia_derecho_vida6, consistencia_derecho_vida7)

datos2 <- datos2 %>%
  left_join(d2021, by = c("folio_unico", "ano"))

#Preguntas 2019
d2019 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2019_BBDD_BicentenarioUC.sav")
table(d2019$t17_1)
table(d2019$t17_2)
table(d2019$t17_3)
table(d2019$t17_4)
d2019 <- d2019 %>%
  mutate(folio_unico = seq(26228, 28274, by = 1))

d2019 <- d2019 %>%
  mutate(ano = 2019)

d2019 <- d2019 %>%
  rename(
    temor_dios = t17_1,
    temor_espiritu_muertos = t17_2,
    temor_diablo = t17_3,
    temor_infierno = t17_4)

d2019 <- d2019 %>%
  select(folio_unico, ano, temor_dios, temor_espiritu_muertos, temor_diablo, temor_infierno)

datos2 <- datos2 %>%
  left_join(d2019, by = c("folio_unico", "ano"))

#Movilidad intergeneracional
table(datos2$mov_inter1, datos2$ano)
table(datos2$mov_inter3, datos2$ano)
table(datos2$mov_inter4, datos2$ano)

datos2 <- datos2 %>%
  mutate(mov_inter1 = case_when(
    ano == 2007 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2007 & mov_inter1 == 2 ~ "Peor",
    ano == 2007 & mov_inter1 == 3 ~ "Igual",
    ano == 2007 & mov_inter1 == 4 ~ "Mejor",
    ano == 2007 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2007 & mov_inter1 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2012 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2012 & mov_inter1 == 2 ~ "Peor",
    ano == 2012 & mov_inter1 == 3 ~ "Igual",
    ano == 2012 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2012 & mov_inter1 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2015 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2015 & mov_inter1 == 2 ~ "Peor",
    ano == 2015 & mov_inter1 == 3 ~ "Igual",
    ano == 2015 & mov_inter1 == 4 ~ "Mejor",
    ano == 2015 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2015 & mov_inter1 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2016 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2016 & mov_inter1 == 2 ~ "Peor",
    ano == 2016 & mov_inter1 == 3 ~ "Igual",
    ano == 2016 & mov_inter1 == 4 ~ "Mejor",
    ano == 2016 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2016 & mov_inter1 %in% c(6, 7, 8, 9, 10) ~ NA_character_,
    ano == 2018 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2018 & mov_inter1 == 2 ~ "Peor",
    ano == 2018 & mov_inter1 == 3 ~ "Igual",
    ano == 2018 & mov_inter1 == 4 ~ "Mejor",
    ano == 2018 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2018 & mov_inter1 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2019 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2019 & mov_inter1 == 2 ~ "Peor",
    ano == 2019 & mov_inter1 == 3 ~ "Igual",
    ano == 2019 & mov_inter1 == 4 ~ "Mejor",
    ano == 2019 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2019 & mov_inter1 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2021 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2021 & mov_inter1 == 2 ~ "Peor",
    ano == 2021 & mov_inter1 == 3 ~ "Igual",
    ano == 2021 & mov_inter1 == 4 ~ "Mejor",
    ano == 2021 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2021 & mov_inter1 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2022 & mov_inter1 == 1 ~ "Mucho peor",
    ano == 2022 & mov_inter1 == 2 ~ "Peor",
    ano == 2022 & mov_inter1 == 3 ~ "Igual",
    ano == 2022 & mov_inter1 == 4 ~ "Mejor",
    ano == 2022 & mov_inter1 == 5 ~ "Mucho mejor",
    ano == 2022 & mov_inter1 %in% c(6, 7, 8, 9) ~ NA_character_,
    TRUE ~ as.character(mov_inter1)  # conserva los valores originales como texto si no hay cambio
  ))
table(datos2$mov_inter1, datos2$ano)

datos2 <- datos2 %>%
  mutate(mov_inter3 = case_when(
    ano == 2007 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2007 & mov_inter3 == 2 ~ "Peor",
    ano == 2007 & mov_inter3 == 3 ~ "Igual",
    ano == 2007 & mov_inter3 == 4 ~ "Mejor",
    ano == 2007 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2007 & mov_inter3 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2012 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2012 & mov_inter3 == 2 ~ "Peor",
    ano == 2012 & mov_inter3 == 3 ~ "Igual",
    ano == 2012 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2012 & mov_inter3 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2015 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2015 & mov_inter3 == 2 ~ "Peor",
    ano == 2015 & mov_inter3 == 3 ~ "Igual",
    ano == 2015 & mov_inter3 == 4 ~ "Mejor",
    ano == 2015 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2015 & mov_inter3 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2016 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2016 & mov_inter3 == 2 ~ "Peor",
    ano == 2016 & mov_inter3 == 3 ~ "Igual",
    ano == 2016 & mov_inter3 == 4 ~ "Mejor",
    ano == 2016 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2016 & mov_inter3 %in% c(6, 7, 8, 9, 10) ~ NA_character_,
    ano == 2018 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2018 & mov_inter3 == 2 ~ "Peor",
    ano == 2018 & mov_inter3 == 3 ~ "Igual",
    ano == 2018 & mov_inter3 == 4 ~ "Mejor",
    ano == 2018 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2018 & mov_inter3 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2019 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2019 & mov_inter3 == 2 ~ "Peor",
    ano == 2019 & mov_inter3 == 3 ~ "Igual",
    ano == 2019 & mov_inter3 == 4 ~ "Mejor",
    ano == 2019 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2019 & mov_inter3 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2021 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2021 & mov_inter3 == 2 ~ "Peor",
    ano == 2021 & mov_inter3 == 3 ~ "Igual",
    ano == 2021 & mov_inter3 == 4 ~ "Mejor",
    ano == 2021 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2021 & mov_inter3 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2022 & mov_inter3 == 1 ~ "Mucho peor",
    ano == 2022 & mov_inter3 == 2 ~ "Peor",
    ano == 2022 & mov_inter3 == 3 ~ "Igual",
    ano == 2022 & mov_inter3 == 4 ~ "Mejor",
    ano == 2022 & mov_inter3 == 5 ~ "Mucho mejor",
    ano == 2022 & mov_inter3 %in% c(6, 7, 8, 9) ~ NA_character_,
    TRUE ~ as.character(mov_inter3)  # conserva los valores originales como texto si no hay cambio
  ))
table(datos2$mov_inter3, datos2$ano)

datos2 <- datos2 %>%
  mutate(mov_inter4 = case_when(
    ano == 2007 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2007 & mov_inter4 == 2 ~ "Peor",
    ano == 2007 & mov_inter4 == 3 ~ "Igual",
    ano == 2007 & mov_inter4 == 4 ~ "Mejor",
    ano == 2007 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2007 & mov_inter4 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2012 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2012 & mov_inter4 == 2 ~ "Peor",
    ano == 2012 & mov_inter4 == 3 ~ "Igual",
    ano == 2012 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2012 & mov_inter4 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2015 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2015 & mov_inter4 == 2 ~ "Peor",
    ano == 2015 & mov_inter4 == 3 ~ "Igual",
    ano == 2015 & mov_inter4 == 4 ~ "Mejor",
    ano == 2015 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2015 & mov_inter4 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2016 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2016 & mov_inter4 == 2 ~ "Peor",
    ano == 2016 & mov_inter4 == 3 ~ "Igual",
    ano == 2016 & mov_inter4 == 4 ~ "Mejor",
    ano == 2016 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2016 & mov_inter4 %in% c(6, 7, 8, 9, 10) ~ NA_character_,
    ano == 2018 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2018 & mov_inter4 == 2 ~ "Peor",
    ano == 2018 & mov_inter4 == 3 ~ "Igual",
    ano == 2018 & mov_inter4 == 4 ~ "Mejor",
    ano == 2018 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2018 & mov_inter4 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2019 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2019 & mov_inter4 == 2 ~ "Peor",
    ano == 2019 & mov_inter4 == 3 ~ "Igual",
    ano == 2019 & mov_inter4 == 4 ~ "Mejor",
    ano == 2019 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2019 & mov_inter4 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2021 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2021 & mov_inter4 == 2 ~ "Peor",
    ano == 2021 & mov_inter4 == 3 ~ "Igual",
    ano == 2021 & mov_inter4 == 4 ~ "Mejor",
    ano == 2021 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2021 & mov_inter4 %in% c(6, 7, 8, 9) ~ NA_character_,
    ano == 2022 & mov_inter4 == 1 ~ "Mucho peor",
    ano == 2022 & mov_inter4 == 2 ~ "Peor",
    ano == 2022 & mov_inter4 == 3 ~ "Igual",
    ano == 2022 & mov_inter4 == 4 ~ "Mejor",
    ano == 2022 & mov_inter4 == 5 ~ "Mucho mejor",
    ano == 2022 & mov_inter4 %in% c(6, 7, 8, 9) ~ NA_character_,
    TRUE ~ as.character(mov_inter4)  # conserva los valores originales como texto si no hay cambio
  ))
table(datos2$mov_inter4, datos2$ano)

#NA y formato variables
table(datos2$creencia_animitas, datos2$ano)
table(datos2$religion_encuestado, datos2$ano)
class(datos2$mov_inter1)
class(datos2$cercania_iglesia1)
class(datos2$religion_encuestado)

datos2 <- datos2 %>%
  mutate(across(where(is.factor), ~ as.character(.)))

datos2 <- datos2 %>%
  mutate(across(where(~ is.character(.) | is.factor(.)), 
                ~ na_if(., "")))

base <- datos2 %>%
  mutate(across(where(is.character), ~ factor(.)))

factor_to_labelled <- function(x) {
  lbls <- setNames(seq_along(levels(x)), levels(x))
  labelled(as.integer(x), labels = lbls)
}

base <- base %>%
  mutate(across(where(is.factor), factor_to_labelled))

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")
save(base, file = "base_profe.RData")
write_sav(base, "base_final.sav")

rm(list = ls())
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_final.sav")
table(datos$creencia_animitas, datos$ano)
table(datos$religion_encuestado, datos$ano)
table(datos$mov_inter1, datos$ano)
class(datos$mov_inter1)
class(datos$cercania_iglesia1)
class(datos$religion_encuestado)
attributes(datos$religion_encuestado)
attributes(datos$mov_inter1)
