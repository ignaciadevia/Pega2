setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario")
rm(list = ls())
library(haven)
library(dplyr)
library(tidyverse)
library(readxl)
library(sjlabelled)
library(labelled)
options(scipen = 999)

datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/CONSOLIDADO BICENTENARIO 2006-2017.sav")
data <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/CONSOLIDADO_BICENTENARIO_2006_2020.sav")

d2016 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2016_BBDD_BicentenarioUC.sav")
d2018 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2018_BBDD_BicentenarioUC.sav")
d2019 <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bases Bicentenario/2019_BBDD_BicentenarioUC.sav")

data <- data %>% filter(ano != 2020)
table(data$ano)

data <- data %>%
  rename(
    tipocomuna = v1,
    tramoedad = v2,
    educ_pareja = v6,
    educ_hijo1 = v7,
    educ_hijo2 = v8,
    educ_hija1 = v9,
    actividad_ppal = v22,
    actividad_ppal2 = q225,
    actividad_ppal3 = v1401,
    actividad_ppal4 = v1417,
    actividad_ppal5 = v1417_2,
    profesion_padre = v20,
    profesion_madre = v21,
    vivia_15años = v69,
    madre_era = v70,
    op_matr1 = v84, #El matrimonio es un compromiso para toda la vida"
    op_matr2 = v85, #Yo le aconsejaría a mis hijos que no convivan antes de casarse"
    op_matr3 = v86, #A veces es aceptable tener una aventura ocasional o pasajera estando casado"
    op_matr4 = v87, #Yo le aconsejaría a mis hijos no tener relaciones sexuales antes de casarse"
    op_matr5 = v88, #Las parejas que conviven debieran casarse cuando deciden tener hijos"
    op_matr6 = v89, #Cuando hay niños de por medio, los padres deben permanecer juntos aun cuando no"
    op_matr7 = v90, #Las parejas homosexuales deberían tener derecho de casarse"
    op_matr8 = v91, #En general las personas casadas son más felices que las solteras"
    op_matr9 = v92, #Observar cómo creecen los niños es la mayor felicidad en la vida de una person"
    op_matr10 = v93, #En general, las personas casadas se separan menos que las que conviven"
    op_matr11 = v94, #Las parejas homosexuales deberían tener derecho a adoptar niños
    op_matr12 = v1234, #Es aceptable que las parejas tengan relaciones sexuales antes del matrimonio
    op_matr13 = v1235, #Que exista ley de divorcio en Chile"
    op_aborto = v95, #No hay 2020
    op_familia1 = v153, #Las personas deben permanecer en contacto con su familia más cercana, aun cuand"
    op_familia2 = v154, #Las personas deben permanecer en contacto con su familia más lejana como tíos,"
    op_familia4 = v156, #Cuando los hijos se van de la casa, no deberían esperar que sus padres los siga"
    op_familia6 = v158, #Cuando los padres envejecen, deberían esperar que los hijos se hagan cargo de e"
    disciplina1 = v214,
    disciplina2 = v215,
    ev_relacion_pareja = v228,
    religion_encuestado = v419,
    religion_pareja = v420,
    religion_madre = v421,
    religion_padre = v422,
    religion_hijo = v423,
    religion_14años = v424,
    frecuencia_misa_enc = v425,
    frecuencia_misa_pareja = v426,
    frecuencia_misa_madre = v427,
    frecuencia_misa_padre = v428, 
    frecuencia_misa_hijo = v429,
    frecuencia_misa_14años = v430,
    frecuencia_biblia = v431, 
    frecuencia_confiesa = v433, 
    frecuencia_mandas = v434,
    frecuencia_santuarios = v435,
    frecuencia_comulga = v436,
    creencia_dios = v437,
    creencia_infierno = v438,
    creencia_brujeria = v439,
    creencia_vidamuerte = v440,
    creencia_virgen = v441,
    creencia_horoscopo = v443,
    creencia_santos = v444,
    creencia_virgen_chile = v446,
    creeencia_resurreccion = rr27,
    valrel_hijos1 = v449, #Que tengan un matrimonio religioso"
    valrel_hijos2 = v450, #Que asistan a un colegio religioso"
    valrel_hijos3 = v451, #Que practiquen activamente su religión"
    valrel_hijos4 = v452, #Que alguno opte por la vida religiosa ( sacerdote, monja, pastor)"
    valrel_hijos5 = v453, #Que compartiera la religión de sus padres"
    valrel_hijos6 = v454, #Que sean creyentes"
    op_religion1 = v455,
    op_religion2 = v456,
    op_religion3 = v457,
    op_religion4 = v458,
    op_religion5 = v459,
    op_religion6 = v460,
    op_religion7 = v461,
    op_religion8 = v462,
    op_religion10 = v464,
    op_religion11 = v465,
    op_religion12 = v466,
    op_religion13 = v467,
    op_nathum1 = v475,
    op_nathum2 = v476,
    op_nathum3 = v477,
    op_nathum4 = v478,
    rezar_virgen = v479,
    culto_virgen1 = v480,
    culto_virgen2 = v481,
    culto_virgen3 = v482,
    culto_virgen4 = v483,
    culto_virgen5 = v484,
    culto_virgen6 = v485,
    culto_virgen7 = v486,
    aasistencia_fiestamariana1 = v487,
    asistencia_fiestamariana2 = v488,
    asistencia_fiestamariana3 = v489,
    asistencia_fiestamariana4 = v490,
    asistencia_fiesta1 = x2017_r06_1,
    asistencia_fiesta2 = x2017_r06_2,
    asistencia_fiesta3 = x2017_r06_3,
    asistencia_fiesta4 = x2017_r06_4,
    asistencia_fiesta5 = x2017_r06_5,
    asistencia_fiesta6 = x2017_r06_6,
    asistencia_fiesta7 = x2017_r06_7,
    asistencia_fiesta8 = x2017_r06_8,
    asistencia_fiesta9 = x2017_r06_9,
    asistencia_fiesta10 = x2017_r06_10,
    asistencia_fiesta11 = x2017_r06_oth_coded_1,
    asistencia_fiesta12 = x2017_r06_oth_coded_2,
    atributos_virgen1 = v502,
    atributos_virgen2 = v503,
    santo1 = v504,
    santo2 = v505,
    santo3 = v506,
    santo4 = v507, 
    santuario_padre_hurtado = v508,
    santuario_padre_hurtado1 = v509,
    santuario_padre_hurtado2 = v510,
    santuario_padre_hurtado3 = v511,
    santuario_santa_teresita = v512,
    santuario_santa_teresita1 = v513,
    santuario_santa_teresita2 = v514,
    santuario_santa_teresita3 = v515,
    describir_cambio_religion1 = v1508,
    describir_cambio_religion2 = v1509,
    edad_cambio = v517,
    veces_cambio = v518,
    motivo_cambio1 = v519,
    motivo_cambio2 = v520,
    motivo_cambio3 = v521,
    motivo_cambio4 = v522,
    evento_compromiso = v523,
    edad_evento_compromiso = v524,
    evento_compromiso1 = v525, 
    evento_compromiso2 = v526, 
    evento_compromiso3 = v527, 
    evento_compromiso4 = v528, 
    evento_compromiso5 = v529,
    conservar_religion1 = v530,
    conservar_religion2 = v531,
    conservar_religion3 = v532,
    publicidad_religion = v533,
    asisitencia_misa_catolica = v534,
    asisitencia_misa_evangelica = v535,
    asisitencia_misa_protestante = v536,
    asisitencia_misa_testigos = v537,
    asisitencia_misa_mormones = v538,
    asisitencia_misa_otra = v539,
    frecuencia_misa_catolica = v540,
    frecuencia_misa_evangelica = v541,
    frecuencia_misa_protestante = v542,
    frecuencia_misa_testigos = v543,
    frecuencia_misa_mormones = v544,
    frecuencia_misa_otra = v545,
    imagen_dios = v546,
    creencia_reencarnacion = v547,
    creencia_energia = v548,
    creencia_astrologia = v549,
    creencia_yoga = v550,
    creencia_maldeojo = v551,
    creencia_fantasmas = v552,
    creencia_telepatia = v553,
    creencia_clarividencia = v554,
    creencia_extraterrestes = v555,
    creencia_comunicacion_muerto = v556,
    creencia_brujas = v557,
    creencia_espiritu_poseer = v558,
    creencia_casa_poseida = v559, 
    creencia_curacion = v560,
    creencia_karma = v561,
    creencia_nirvana = v562, 
    creencia_piedas = rr21,
    actrel1 = v563,
    actrel2 = v564, 
    actrel3 = v565,
    actrel4 = v566, 
    actrel5 = v567,
    actrel6 = v568,
    actrel7 = v569,
    actrel8 = v570, 
    actrel9 = v571,
    actrel10 = v572,
    actrel11 = v573,
    frec_actrel1 = v574,
    frec_actrel2 = v575, 
    frec_actrel3 = v576,
    frec_actrel4 = v577, 
    frec_actrel5 = v578,
    frec_actrel6 = v579,
    frec_actrel7 = v580,
    frec_actrel8 = v581, 
    frec_actrel9 = v582,
    frec_actrel10 = v583,
    frec_actrel11 = v584,
    asistencia_virgen = v585,
    encomendarse_santo = v586,
    cuan_religioso = v587,
    devenir_religioso = v1507,
    tema_iglesia1 = v588,
    tema_iglesia2 = v589,
    tema_iglesia3 = v590,
    tema_iglesia4 = v591,
    tema_iglesia5 = v592,
    tema_iglesia6 = v593,
    cercania_iglesia1 = v594, 
    cercania_iglesia2 = v595, 
    cercania_iglesia3 = v596, 
    cercania_iglesia4 = v597, 
    cercania_iglesia5 = v598,
    recurrir_sacerdote = v599,
    dedicacion_sacedrote = v600,
    consejos_papa = v601,
    desempeño_iglesia1 = v602,
    desempeño_iglesia2 = v603,
    desempeño_iglesia3 = v604,
    desempeño_iglesia4 = v605,
    desempeño_iglesia5 = v606,
    desempeño_sacerdote1 = v607,
    desempeño_sacerdote2 = v608,
    desempeño_sacerdote3 = v609,
    desempeño_sacerdote4 = v610,
    desempeño_sacerdote5 = v611,
    creencia_cat1 = v612,
    creencia_cat3 = v614,
    creencia_cat4 = v615, 
    feliz_fe = v616,
    creencia_personas_justas = v617,
    creencia_fe_siniglesia = v618,
    creencia_fe_sinritos = v619,
    frecuencia_confesion = v620,
    frecuencia_comulgar = v621, 
    uncion_muertos = v622,
    importancia_sacramentos = v623,
    sacramento_mas_importante = v624,
    sacramento_menos_importante = v625,
    creencia_cat5 = v626,
    creencia_cat6 = v627,
    creencia_cat7 = v628,
    creencia_cat8 = v629,
    op_iglesia1 = v630,
    op_iglesia2 = v631,
    op_iglesia3 = v632,
    op_iglesia4 = v633,
    op_iglesia5 = v634,
    muerte_piensa = v635, 
    muerte_conversa = v636,
    temor_muerte_padres = v637,
    temor_muerte_amigos = v639,
    temor_muerte_hijos = v640, 
    temor_muerte_pareja = v641,
    sepultado = v642,
    mirar_muertos = v643, 
    asistir_cementerio = v644,
    rezar_familiares = v645,
    salvacion_muerte1 = v647,
    salvacion_muerte2 = v648,
    muerte_identificacion1 = v649,
    muerte_identificacion2 = v650,
    muerte_identificacion3 = v651,
    muerte_identificacion4 = v652,
    muerte_identificacion5 = v653,
    muerte_identificacion6 = v654,
    muerte_identificacion7 = v655,
    seguir_enseñanzas = v1624,
    consejo_sacerdote = v1542,
    tipo_igualdad = v656, #Cuál es a su juicio la igualdad que se debe alcanzar en el país"
    op_trabajo_mujer1 = v657, #Una madre que trabaja establece una relación igual de cercana con sus hijos que"
    op_trabajo_mujer2 = v658, #La familia se descuida si la mujer tiene un trabajo de tiempo completo"
    op_trabajo_mujer3 = v659, #Ser dueña de casa es tan satisfactorio como trabajar por un sueldo"
    op_trabajo_mujer4 = v660, #Es importante que la mujer apoye la carrera profesional de su marido aunque sea"
    op_trabajo_mujer5 = v661, #Es mejor para la familia si el hombre trabaja y la mujer se queda en la casa"
    op_trabajo_mujer6 = v662, #Los hijos se benefician si la mujer trabaja"
    op_trabajo_mujer7 = v663, #Si mi pareja ganara lo suficiente, yo no trabajaría remuneradamente"
    op_trabajo_mujer8 = v664, #Me parece bien que una mujer trabaje, pero en realidad lo que debe hacer es cuid"
    op_trabajo_mujer9 = v665, #Un niño de edad pre-escolar sufrirá si su madre trabaja"
    op_trabajo_mujer10 = v666, #En el trabajo, la autoridad de un hombre da más seguridad y estabilidad que la"
    op_trabajo_mujer11 = v667, #La mejor manera de que una mujer sea una persona independiente es teniendo un tr"
    op_trabajo_mujer12 = v668, #La igualdad entre hombres y mujeres ya se ha logrado en su mayor parte."
    op_trabajo_mujer13 = v669, #La igualdad entre hombres y mujeres ha terminado perjudicando a la mujer."
    op_trabajo_mujer14 = v1532, #(El ser únicamente dueña de casa le impide a una mujer realizarse plenamente)"
    op_trabajo_mujer15 = v1533, #(La mujer debería permanecer con sus hijos(as) en casa mientras son pequeños(a"
    moral_econ1 = v685,
    moral_econ1_5 = v1431,
    moral_econ2 = v686, 
    moral_econ3 = v687,
    moral_econ4 = v688,
    moral_econ5 = v689,
    moral_econ6 = v690,
    causas_riqueza1 = v691,
    causas_riqueza2 = v692, 
    causas_pobreza1 = v693,
    causas_pobreza2 = v694,
    prob_ascenso1 = v695,
    prob_ascenso2 = v696,
    prob_ascenso3 = v697,
    prob_ascenso4 = v698,
    prob_ascenso5 = v699,
    prob_ascenso6 = v700,
    op_rol_estado1 = v704,
    op_rol_estado2 = v705,
    op_rol_estado3 = v706,
    percep_mov = v714,
    expectativa_mov = v715,
    membersia1 = v744, 
    membersia2 = v745, 
    membersia3 = v746, 
    membersia4 = v747, 
    membersia5 = v748, 
    membersia6 = v749, 
    membersia7 = v750, 
    membersia8 = v751, 
    membersia9 = v752, 
    membersia10 = v753, 
    membersia11 = v754,
    membersia12 = v755, 
    membersia13 = v756,
    membersia14 = v757, 
    membersia15 = v758,
    membersia16 = v759, 
    membersia17 = v760,
    membersia18 = v761,
    confianza2 = v799,
    confianza3 = v800,
    confianza4 = v801,
    confianza5 = v802,
    confianza6 = v803,
    confianza7 = v804,
    confianza_obispos = v1535,
    confianza_sacerdotes = v1536,
    conf_inst1 = v837, 
    conf_inst2 = v838, #fa
    conf_inst3 = v839, #gob
    conf_inst4 = v840,
    conf_inst5 = v841,
    conf_inst6 = v842, #tribunales
    conf_inst7 = v843, #parlamentarios
    conf_inst8 = v844, #medios 
    conf_inst9 = v845,
    conf_inst10 = v846,
    conf_inst11 = v847,
    conf_inst12 = v848,
    conf_inst13 = v849,
    conf_inst14 = v850,
    conf_inst15 = v851,
    op_politica2 = v853,
    op_politica4 = v855,
    op_politica5 = v856,
    tolerancia1 = v857,
    tolerancia2 = v858,
    tolerancia3 = v859,
    tolerancia4 = v860,
    tolerancia5 = v861,
    op_chile1 = v965,
    op_chile3 = v967,
    op_chile5 = v969,
    op_chile6 = v970,
    orgullo_chileno = v971,
    percep_clase = v1037, 
    op_inmigrantes1 = v1136, 
    op_inmigrantes2 = v1137,
    op_inmigrantes3 = v1138,
    op_inmigrantes4 = v1139,
    op_inmigrantes5 = v1408,
    op_inmigrantes6 = v1409,
    op_inmigrantes7 = v1423,
    op_inmigrantes8 = x2017_i04_1,
    op_inmigrantes9 = x2017_i04_4,
    op_violencia1 = v1166, 
    pp_indispensables = v1167,
    salario_suficiente = v1168,
    justificacion_violencia1 = v1189,
    justificacion_violencia2 = v1190,
    justificacion_violencia3 = v1191,
    justificacion_violencia4 = v1192,
    justificacion_violencia5 = v1193,
    justificacion_violencia6 = v1194,
    justificacion_violencia7 = v1195,
    justificacion_violencia8 = v1196,
    justificacion_violencia9 = v1197,
    democracia1 = ds54,
    op_trabajo_mujer16 = v1412,
    op_mujeres1 = v1430,
    op_mujeres2 =df43,
    papa_francisco1 = dr1,
    papa_francisco2 = dr20,
    papa_francisco3 = dr21, 
    papa_francisco4 = dr22, 
    papa_francisco5 = dr23,
    cambios_papa1 = dr24,
    cambios_papa2 = dr25, 
    cambios_papa3 = dr26,
    cambios_papa4 = dr27,
    cambios_papa5 = dr28,
    cambios_papa6 = dr29,
    cambios_papa7 = dr30,
    cambios_papa8 = dr31,
    op_iglesia = dr36, 
    tiene_hijos = rf1,
    perdida_influencia = rr30,
    op_iglesia6 = rr31, 
    op_iglesia7 = rr32,
    op_iglesia8 = rr33,
    op_iglesia9 = rr34,
    op_iglesia10 = rr35,
    op_iglesia11 = rr36,
    marihuana = v1233,
    confianza_inter1 = q3,
    confianza_inter2 = q4,
    confianza_inter3 = q5,
    confianza_inter4 = q6,
    confianza_inter5 = q7,
    confianza_inter6 = q8,
    membersia19 = q9o,
    confianza_parroquia = v1543,
    afectados_abusos_sacerdotes = v1544,
    abusos_comunes_iglesia = v1545,
    causa_abusos1 = v1546,
    causa_abusos2 = v1547,
    causa_abusos3 = v1548,
    causa_abusos4 = v1541,
    iglesia_parroquia = q102,
    grupo_catolico = q103,
    mov_catolico = q104, 
    confianza_sacerdotes_parroquia = q105,
    dios_presente1 = x2017_r08,
    dios_presente2 = x2017_r09,
    dios_presente3 = x2017_r10,
    dios_presente4 = x2017_r11,
    abusos_sexuales1 = v1510,
    cambio_practica = v1511,
    describir_cambio_practica = v1512,
    evento_cambio1 = v1515, 
    evento_cambio2 = v1516, 
    evento_cambio3 = v1517, 
    abuso_sexual2 = v1518,
    iden_pol = v833,
    iden_pol1 = v1435,
    iden_pol2 = v1435_2,
    define_como1 = v448,
    encomendar_familiares = rr18,
    invocar_familiares = v646,
    frecuencia_rezar1 = v432,
    frecuencia_rezar2 = v1413,
    )
#2016
d2016 <- d2016 %>%
  mutate(folio_unico = seq(20178, 22193, by = 1))

d2016 <- d2016 %>%
  mutate(ano = 2016)

d2016 <- d2016 %>%
  rename(e07 = E07)

d2016 <- d2016 %>%
  select(folio_unico, e07, ano)
head(d2016)

data<- data %>%
  left_join(d2016, by = c("folio_unico", "ano"))

#2018
d2018 <- d2018 %>%
  mutate(folio = 1:n())

d2018 <- d2018 %>%
  mutate(folio_unico = seq(24217, 26227, by = 1))

d2018 <- d2018 %>%
  mutate(ano = 2018)

d2018 <- d2018 %>%
  rename(v3_2018 = CP01)

d2018 <- d2018 %>%
  select(folio, folio_unico, v3_2018, ano)
head(d2018)

data<- data %>%
  left_join(d2018, by = c("folio_unico", "folio", "ano"))

#2019
d2019 <- d2019 %>%
  mutate(folio = 1:n())

d2019 <- d2019 %>%
  mutate(folio_unico = seq(26228, 28274, by = 1))

d2019 <- d2019 %>%
  mutate(ano = 2019)

d2019 <- d2019 %>%
  rename(v3_2019 = cp01)

d2019 <- d2019 %>%
  select(folio, folio_unico, v3_2019, ano)
head(d2019)

data<- data %>%
  left_join(d2019, by = c("folio_unico", "folio", "ano"))


#Educacion
data <- data %>%
  mutate(v3 = ifelse(ano %in% c(2018, 2019, 2020), NA, v3))
data <- data %>%
  mutate(v3 = case_when(
    v3 == 1 ~ 1,
    v3 == 2 ~ 1,
    v3 == 3 ~ 2,
    v3 == 4 ~ 3,
    v3 == 5 ~ 4,
    v3 == 6 ~ 5,
    v3 == 7 ~ 6,
    v3 == 8 ~ 7,
    v3 == 9 ~ 8,
    v3 == 88 ~ 9,
    v3 == 99 ~ 9,
    TRUE ~ v3
  ))

data <- data %>%
  mutate(q217 = case_when(
    q217 == 1 ~ 1,
    q217 == 2 ~ 1,
    q217 == 3 ~ 2,
    q217 == 4 ~ 3,
    q217 == 5 ~ 4,
    q217 == 6 ~ 5,
    q217 == 7 ~ 6,
    q217 == 8 ~ 6,
    q217 == 9 ~ 7,
    q217 == 10 ~ 8,
    q217 == 99 ~ 9,
    TRUE ~ q217
  ))

data$v3_2018 <- as.numeric(data$v3_2018)
data$v3_2019 <- as.numeric(data$v3_2019)

data <- data %>%
  mutate(
    v3_2018 = case_when(
      v3_2018 == 1 ~ 1,
      v3_2018 == 2 ~ 1,
      v3_2018 == 3 ~ 1,
      v3_2018 == 4 ~ 1,
      v3_2018 == 5 ~ 1,
      v3_2018 == 6 ~ 2,
      v3_2018 == 7 ~ 2,
      v3_2018 == 8 ~ 4,
      v3_2018 == 9 ~ 4,
      v3_2018 == 10 ~ 4,
      v3_2018 == 11 ~ 4,
      v3_2018 == 12 ~ 5,
      v3_2018 == 13 ~ 6,
      v3_2018 == 14 ~ 6,
      v3_2018 == 15 ~ 7,
      v3_2018 == 16 ~ 8,
      v3_2018 == 17 ~ 8,
      v3_2018 == 99 ~ 9,  
    ),
    v3_2019 = case_when(
      v3_2019 == 1 ~ 1,
      v3_2019 == 2 ~ 1,
      v3_2019 == 3 ~ 1,
      v3_2019 == 4 ~ 1,
      v3_2019 == 5 ~ 1,
      v3_2019 == 6 ~ 2,
      v3_2019 == 7 ~ 2,
      v3_2019 == 8 ~ 4,
      v3_2019 == 9 ~ 4,
      v3_2019 == 10 ~ 4,
      v3_2019 == 11 ~ 4,
      v3_2019 == 12 ~ 5,
      v3_2019 == 13 ~ 6,
      v3_2019 == 14 ~ 6,
      v3_2019 == 15 ~ 7,
      v3_2019 == 16 ~ 8,
      v3_2019 == 17 ~ 8,
      v3_2019 == 88 ~ 9,
      v3_2019 == 99 ~ 9,
    )
  )

val_labels(data$q217) <- val_labels(data$n_educacion_enc)
val_labels(data$v3) <- val_labels(data$n_educacion_enc)
val_labels(data$v3_2018) <- val_labels(data$n_educacion_enc)
val_labels(data$v3_2019) <- val_labels(data$n_educacion_enc)
data$educ <- coalesce(data$v3, data$q217, data$n_educacion_enc, data$v3_2018, data$v3_2019)
data$educ <- set_label(data$educ, "Educación encuestado")
table(data$educ, data$ano)

#Educacion padre y madre
data <- data %>%
  mutate(
    v4_2 = if_else(ano >= 2006 & ano <= 2010, v4, NA_real_) # Crear v4_2 con los valores de v4 solo para 2006-2010
  ) %>%
  mutate(
    v4 = if_else(ano >= 2018 & ano <= 2019, v4, NA_real_) # Mantener v4 solo para 2018-2019
  )

data <- data %>%
  mutate(
    v5_2 = if_else(ano >= 2006 & ano <= 2010, v5, NA_real_) # Crear v4_2 con los valores de v4 solo para 2006-2010
  ) %>%
  mutate(
    v5 = if_else(ano >= 2018 & ano <= 2019, v5, NA_real_) # Mantener v4 solo para 2018-2019
  )


data <- data %>%
  mutate(
    v4 = case_when(
      v4 == 1 ~ 1,
      v4 == 2 ~ 1,
      v4 == 3 ~ 1,
      v4 == 4 ~ 1,
      v4 == 5 ~ 1,
      v4 == 6 ~ 2,
      v4 == 7 ~ 2,
      v4 == 8 ~ 4,
      v4 == 9 ~ 4,
      v4 == 10 ~ 4,
      v4 == 11 ~ 4,
      v4 == 12 ~ 5,
      v4 == 13 ~ 6,
      v4 == 14 ~ 6,
      v4 == 15 ~ 7,
      v4 == 16 ~ 8,
      v4 == 17 ~ 8,
      v4 == 88 ~ 9,
      v4 == 99 ~ 9,  
    ),
    v5 = case_when(
      v5 == 1 ~ 1,
      v5 == 2 ~ 1,
      v5 == 3 ~ 1,
      v5 == 4 ~ 1,
      v5 == 5 ~ 1,
      v5 == 6 ~ 2,
      v5 == 7 ~ 2,
      v5 == 8 ~ 4,
      v5 == 9 ~ 4,
      v5 == 10 ~ 4,
      v5 == 11 ~ 4,
      v5 == 12 ~ 5,
      v5 == 13 ~ 6,
      v5 == 14 ~ 6,
      v5 == 15 ~ 7,
      v5 == 16 ~ 8,
      v5 == 17 ~ 8,
      v5 == 88 ~ 9,
      v5 == 99 ~ 9,
    ),
    x2017_cp03 = case_when(
      x2017_cp03 == 1 ~ 1,
      x2017_cp03 == 2 ~ 1,
      x2017_cp03 == 3 ~ 1,
      x2017_cp03 == 4 ~ 1,
      x2017_cp03 == 5 ~ 1,
      x2017_cp03 == 6 ~ 2,
      x2017_cp03 == 7 ~ 2,
      x2017_cp03 == 8 ~ 4,
      x2017_cp03 == 9 ~ 4,
      x2017_cp03 == 10 ~ 4,
      x2017_cp03 == 11 ~ 4,
      x2017_cp03 == 12 ~ 5,
      x2017_cp03 == 13 ~ 6,
      x2017_cp03 == 14 ~ 6,
      x2017_cp03 == 15 ~ 7,
      x2017_cp03 == 16 ~ 8,
      x2017_cp03 == 17 ~ 8,
      x2017_cp03 == 18 ~ 9,
    ),
    x2017_cp02 = case_when(
      x2017_cp02 == 1 ~ 1,
      x2017_cp02 == 2 ~ 1,
      x2017_cp02 == 3 ~ 1,
      x2017_cp02 == 4 ~ 1,
      x2017_cp02 == 5 ~ 1,
      x2017_cp02 == 6 ~ 2,
      x2017_cp02 == 7 ~ 2,
      x2017_cp02 == 8 ~ 4,
      x2017_cp02 == 9 ~ 4,
      x2017_cp02 == 10 ~ 4,
      x2017_cp02 == 11 ~ 4,
      x2017_cp02 == 12 ~ 5,
      x2017_cp02 == 13 ~ 6,
      x2017_cp02 == 14 ~ 6,
      x2017_cp02 == 15 ~ 7,
      x2017_cp02 == 16 ~ 8,
      x2017_cp02 == 17 ~ 8,
      x2017_cp02 == 18 ~ 9,
    )
  )
val_labels(data$v4) <- val_labels(data$n_educacion_enc)
val_labels(data$v5) <- val_labels(data$n_educacion_enc)
val_labels(data$v4_2) <- val_labels(data$n_educacion_enc)
val_labels(data$v5_2) <- val_labels(data$n_educacion_enc)
val_labels(data$x2017_cp02) <- val_labels(data$n_educacion_enc)
val_labels(data$x2017_cp03) <- val_labels(data$n_educacion_enc)
data$educ_padre <- coalesce(data$v4, data$x2017_cp03, data$v4_2)
data$educ_madre <- coalesce(data$v5, data$x2017_cp02, data$v5_2)
data$educ_padre <- set_label(data$educ_padre, "Educación  alcanzado por su padre")
table(data$educ_padre, data$ano)
data$educ_madre <- set_label(data$educ_madre, "Educación  alcanzado por su madre")
table(data$educ_madre, data$ano)


#Estado civil
data <- data %>%
  mutate(
    v55 = case_when(
      v55 == 1 ~ 6,
      v55 == 2 ~ 2,
      v55 == 3 ~ 1,
      v55 == 4 ~ 1,
      v55 == 5 ~ 4,
      v55 == 6 ~ 2,
      v55 == 7 ~ 5,
      v55 == 8 ~ 5,
      v55 == 9 ~ 9,  
    ),
    v55_2 = case_when(
      v55_2 == 1 ~ 1,
      v55_2 == 2 ~ 2,
      v55_2 == 3 ~ 3,
      v55_2 == 4 ~ 4,
      v55_2 == 5 ~ 4,
      v55_2 == 6 ~ 4,
      v55_2 == 7 ~ 5,
      v55_2 == 8 ~ 6,
      v55_2 == 99 ~ 9,
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
    ))
labels <- c(
  "Casado" = 1,
  "Conviviente sin acuerdo" = 2,
  "Conviviente con acuerdo" = 3,
  "Anulado/Sepado/Divorciado" = 4,
  "Viudo" = 5,
  "Soltero" = 6
)
data$v55 <- labelled(data$v55, labels)
data$v55_2 <- labelled(data$v55_2, labels)
data$cp05 <- labelled(data$cp05, labels)
data$estado_civil <- coalesce(data$v55, data$v55_2, data$cp05)
data$estado_civil <- set_label(data$estado_civil, "Actualmente Ud. es...")
table(data$estado_civil, data$ano)

#Numero hijos
data$nhijos <- coalesce(data$v61, data$q222)
data$nhijos <- set_label(data$nhijos, "Cuántos hijos tiene Ud.")
table(data$nhijos, data$ano)

#Ideal hijos
data$v62 <- as.numeric(data$v62)
data$q223 <- as.numeric(data$q223)
data$ideal_hijos <- coalesce(data$v62, data$q223)
table(data$ideal_hijos, data$ano)

#Numero personas hogar
data$n_personas_hogar <- coalesce(data$v76, data$q218, data$v1418, data$v1521)
data$n_personas_hogar <- set_label(data$n_personas_hogar,  "¿Cuántas personas viven en este hogar?")
table(data$n_personas_hogar, data$ano)

#Numero amigos y vecinos
data$namigos <- coalesce(data$v742, data$da1, data$e07)
data$namigos <- set_label(data$namigos,  "Cantidad de amigos cercanos que tiene")
table(data$namigos, data$ano)

data$nvecinos <- coalesce(data$v743, data$ds52)
data$nvecinos <- set_label(data$nvecinos,  "Cuántos vecinos conoce de nombre")
table(data$nvecinos, data$ano)

#Creencias
val_labels(data$v445) <- val_labels(data$rr26)
val_labels(data$v442) <- val_labels(data$rr24)
val_labels(data$v447) <- val_labels(data$rr28)
data$creencia_milagros_virgen <- coalesce(data$v445, data$rr26)
data$creencia_milagros_virgen <- set_label(data$creencia_milagros_virgen,  "Creencia milagros Virgen")
table(data$creencia_milagros_virgen, data$ano)
data$creencia_milagros <- coalesce(data$v442, data$rr24)
data$creencia_milagros <- set_label(data$creencia_milagros,  "Creencia en los milagros")
table(data$creencia_milagros, data$ano)
data$creencia_jesucristo_hijo <- coalesce(data$v447, data$rr28)
data$creencia_jesucristo_hijo <- set_label(data$creencia_jesucristo_hijo,  "Creencia en Jesucristo como hijo de Dios")
table(data$creencia_jesucristo_hijo, data$ano)

#Cambio religion
data <- data %>%
  mutate(v516 = case_when(
    v516 == 1 ~ 1,
    v516 == 2 ~ 1,
    v516 == 3 ~ 1,
    v516 == 4 ~ 1,
    v516 == 5 ~ 1,
    v516 == 6 ~ 1,
    v516 == 7 ~ 1,
    v516 == 8 ~ 1,
    v516 == 9 ~ 1,
    v516 == 10 ~ 1,
    v516 == 11 ~ 2,
    v516 == 99 ~ 9,
    TRUE ~ v516
  ))
val_labels(data$v516) <- val_labels(data$v516_2)
data$cambio_religion <- coalesce(data$v516, data$v516_2)
data$cambio_religion <- set_label(data$cambio_religion,  "Ha cambiado alguna vez su preferencia o identificación religiosa")
table(data$cambio_religion, data$ano)

#Definición religioso
data <- data %>%
  mutate(
    r05_2016 = if_else(ano == 2016, r05, NA_real_),
    r05_2017 = if_else(ano == 2017, r05, NA_real_)
  )

data <- data %>%
  mutate(r05_2016 = case_when(
    r05_2016 == 1 ~ 1,
    r05_2016 == 2 ~ 3,
    r05_2016 == 3 ~ 4,
    r05_2016 == 5 ~ 9,
    TRUE ~ r05_2016
  ))

data <- data %>%
  mutate(r05_2017 = case_when(
    r05_2017 == 1 ~ 1,
    r05_2017 == 2 ~ 3,
    r05_2017 == 3 ~ 4,
    r05_2017 == 4 ~ 5,
    r05_2017 == 9 ~ 9,
    TRUE ~ r05_2017
  ))

variables <- c("r05_2016", "r05_2017", "v448_2")

# Etiquetas
labels <- c(
  "Una persona muy religiosa" = 1,
  "Una persona bastante religiosa" = 2,
  "Una persona más o menos religiosa" = 3,
  "Una persona poco religiosa" = 4,
  "Una persona nada religiosa" = 5,
  "NS/NR (No leer)" = 9
)

# Aplicar etiquetas a cada variable en la lista
for (var in variables) {
  data[[var]] <- labelled(data[[var]], labels = labels)
}
data$define_como <- coalesce(data$r05_2016, data$r05_2017, data$v448_2)
data$define_como <- set_label(data$define_como,  "Ud. se definiría como…")
table(data$define_como, data$ano)

#Conoce cura
data <- data %>%
  mutate(v1415 = case_when(
    v1415 == 9 ~ 3,
    TRUE ~ v1415
  ))

data <- data %>%
  mutate(v1415_2 = case_when(
    v1415_2 == 8 ~ 9,
    TRUE ~ v1415_2
  ))

variables <- c("v1415_2", "v1415")

labels <- c(
  "Sí, puede decir su nombre" = 1,
  "Sí, lo conoce, pero no sabe su nombre" = 2,
  "No lo conoce, no sabe quién es" = 3
  )
for (var in variables) {
  data[[var]] <- labelled(data[[var]], labels = labels)
}

data$conoce_cura <- coalesce(data$v1415_2, data$v1415)
data$conoce_cura <- set_label(data$conoce_cura,  "¿Conoce Ud. al cura o pastor de su parroquia o templo?")
table(data$conoce_cura, data$ano)

#Temor muerte propia
data <- data %>%
  mutate(v638 = case_when(
    v638 == 1 ~ 5,
    v638 == 2 ~ 4,
    v638 == 3 ~ 2,
    v638 == 4 ~ 1,
    v638 == 8 ~ 9,
    TRUE ~ v638
  ))

data <- data %>%
  mutate(
    v1598_2019 = if_else(ano == 2019, v1598, NA_real_),
  )

variables <- c("v1598_2019", "v638")

labels <- c(
  "Mucho" = 1,
  "Bastante" = 2,
  "Algo" = 3,
  "Poco" = 4,
  "Nada" =5
)
for (var in variables) {
  data[[var]] <- labelled(data[[var]], labels = labels)
}
data$temor_muerte_propia <- coalesce(data$v1598_2019, data$v638)
data$temor_muerte_propia <- set_label(data$temor_muerte_propia,  "(Su propia muerte) ¿Cuánto temor le producen las siguientes situaciones? ")
table(data$temor_muerte_propia, data$ano)


#Movilidad intergeneracional

variables <- c("v1265", "v1266", "v1267", "v1268", "v1269", "v709", "v710", "v711", "v712", "v713")

labels <- c(
  "Mucho peor" = 1,
  "Peor" = 2,
  "Igual" = 3,
  "Mejor" = 4,
  "Mucho mejor" = 5,
  "Ns/nr" = 9,
  "No aplica" = 10
)
for (var in variables) {
  data[[var]] <- labelled(data[[var]], labels = labels)
}

data <- data %>%
  mutate(across(c(v1265, v1266, v1267, v1268, v1269), ~ ifelse(. == 6, 10, .)))
data$mov_inter1 <- coalesce(data$v709, data$v1265)
data$mov_inter2 <- coalesce(data$v710, data$v1266)
data$mov_inter3 <- coalesce(data$v711, data$v1267)
data$mov_inter4 <- coalesce(data$v712, data$v1268)
data$mov_inter5 <- coalesce(data$v713, data$v1269)
data$mov_inter1 <- set_label(data$mov_inter1,  "Comparándose con sus padres, el nivel de ingresos suyo es")
table(data$mov_inter1, data$ano)
data$mov_inter2 <- set_label(data$mov_inter2,  "Comparándose con sus padres, le parece que su casa es:")
table(data$mov_inter2, data$ano)
data$mov_inter3 <- set_label(data$mov_inter3,  "Comparándose con sus padres, su trabajo le parece")
table(data$mov_inter3, data$ano)
data$mov_inter4 <- set_label(data$mov_inter4,  "Comparándose con sus padres,le parece que su vida familiar es hoy día")
table(data$mov_inter4, data$ano)
data$mov_inter5 <- set_label(data$mov_inter5,  "Comparándose con sus padres, le parece que su tiempo libre es")
table(data$mov_inter5, data$ano)


#Sacerdotes perdonan pecados
data <- data %>%
  mutate(rr29 = case_when(
    rr29 == 2 ~ 3,
    rr29 == 3 ~ 2,
    TRUE ~ rr29
  ))
data <- data[!data$v613 %in% 4:8, ]

variables <- c("v613", "rr29")

labels <- c(
  "Cree" = 1,
  "No cree" = 2,
  "Tiene dudas" = 3
)
for (var in variables) {
  data[[var]] <- labelled(data[[var]], labels = labels)
}
data$creencia_cat2 <- coalesce(data$v613, data$rr29)
data$creencia_cat2 <- set_label(data$creencia_cat2,  "Cree que los sacerdotes pueden perdonar los pecados en el nombre de Dios")
table(data$creencia_cat2, data$ano)

#Opinion de chile

data <- data %>%
  mutate(v1443 = case_when(
    v1443 == 1 ~ 5,
    v1443 == 2 ~ 4,
    v1443 == 3 ~ 3,
    v1443 == 4 ~ 2,
    v1443 == 5 ~ 1,
    v1443 == 99 ~ 9,
    TRUE ~ v1443
  ))
data <- data %>%
  mutate(v1442 = case_when(
    v1442 == 1 ~ 5,
    v1442 == 2 ~ 4,
    v1442 == 3 ~ 3,
    v1442 == 4 ~ 2,
    v1442 == 5 ~ 1,
    v1442 == 99 ~ 9,
    TRUE ~ v1442
  ))
val_labels(data$v1443) <- val_labels(data$v966)
val_labels(data$v1442) <- val_labels(data$v968)
data$op_chile2 <- coalesce(data$v966, data$v1443)
data$op_chile2 <- set_label(data$op_chile2,  "Chile es el mejor país para vivir dentro de América Latina")
table(data$op_chile2, data$ano)
data$op_chile4 <- coalesce(data$v968, data$v1442)
data$op_chile4 <- set_label(data$op_chile2,  "Tomando todo lo bueno y lo malo de nuestra historia, me siento orgulloso de la h")
table(data$op_chile4, data$ano)

#Confiar personas
val_labels(data$v1436) <- val_labels(data$v798)
data$confianza1 <- coalesce(data$v798, data$v1436)
data$confianza1 <- set_label(data$confianza1,  "Se puede confiar en la mayor parte de las personas")
table(data$confianza1, data$ano)

#Opinion politica

data$ds59i <- case_when(
  data$ds59 == 1 ~ 5,
  data$ds59 == 2 ~ 4,
  data$ds59 == 3 ~ 3,
  data$ds59 == 4 ~ 2,
  data$ds59 == 5 ~ 1,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)
val_labels(data$ds59i) <- val_labels(data$v852)
data$op_politica1 <- coalesce(data$v852, data$ds59i)
data$op_politica1 <- set_label(data$op_politica1, "Tener el mismo pensamiento político es importante a la hora de hacer una amista")
table(data$op_politica1, data$ano)


#Opinion religion

data$dr19i <- case_when(
  data$dr19 == 1 ~ 5,
  data$dr19 == 2 ~ 4,
  data$dr19 == 3 ~ 3,
  data$dr19 == 4 ~ 2,
  data$dr19 == 5 ~ 1,
  TRUE ~ NA_real_ # Por si hay valores fuera del rango
)
val_labels(data$dr19i) <- val_labels(data$v463)
data$op_religion9 <- coalesce(data$v463, data$dr19i)
data$op_religion9 <- set_label(data$op_religion9, "En general, me siento parte de la Iglesia Católica")
table(data$op_religion9, data$ano)

#seguir_enseñanzas, consejo_sacerdote
table(data$v419, data$ano)
var_label(data_filtrada$op_violencia1)
etiquetas <- get_labels(data_filtrada$gse)
valores <- get_values(data_filtrada$gse)
tabla <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla)

data_filtrada <- data %>%
  select(folio, folio_unico, ano, ponderador, zona, sexo, edad, tramoedad, tipocomuna, 
         nse, gse, dfolio, id, ciudad, region, id_comuna, comuna, comuna_aux, educ, 
         reli, educ_padre, educ_madre, estado_civil, pareja, nhijos, ideal_hijos, 
         rr10, n_personas_hogar, namigos, nvecinos,
    educ_pareja, educ_hijo1, educ_hijo2, educ_hija1, 
    actividad_ppal, actividad_ppal2, actividad_ppal3, actividad_ppal4, actividad_ppal5, trabajo,
    profesion_padre, profesion_madre, vivia_15años, madre_era,
    op_matr1, op_matr2, op_matr3, op_matr4, op_matr5, op_matr6, op_matr7, 
    op_matr8, op_matr9, op_matr10, op_matr11, op_matr12, op_matr13, op_aborto, 
    op_familia1, op_familia2, op_familia4, op_familia6, disciplina1, disciplina2, 
    ev_relacion_pareja, religion_encuestado, religion_pareja, religion_madre, religion_padre, 
    religion_hijo, religion_14años, frecuencia_misa_enc, frecuencia_misa_pareja, 
    frecuencia_misa_madre, frecuencia_misa_padre, frecuencia_misa_hijo, frecuencia_misa_14años,
    frecuencia_biblia, frecuencia_confiesa, frecuencia_mandas, frecuencia_santuarios, 
    frecuencia_comulga, creencia_dios, creencia_infierno, creencia_brujeria, 
    creencia_vidamuerte, creencia_virgen, creencia_horoscopo, creencia_santos, 
    creencia_virgen_chile, creeencia_resurreccion, valrel_hijos1, valrel_hijos2, 
    valrel_hijos3, valrel_hijos4, valrel_hijos5, valrel_hijos6, op_religion1, 
    op_religion2, op_religion3, op_religion4, op_religion5, op_religion6, op_religion7, 
    op_religion8, op_religion10, op_religion11, op_religion12, op_religion13, op_nathum1, 
    op_nathum2, op_nathum3, op_nathum4, rezar_virgen, culto_virgen1, culto_virgen2, 
    culto_virgen3, culto_virgen4, culto_virgen5, culto_virgen6, culto_virgen7, 
    aasistencia_fiestamariana1, asistencia_fiestamariana2, asistencia_fiestamariana3, 
    asistencia_fiestamariana4, asistencia_fiesta1, asistencia_fiesta2, asistencia_fiesta3, 
    asistencia_fiesta4, asistencia_fiesta5, asistencia_fiesta6, asistencia_fiesta7, 
    asistencia_fiesta8, asistencia_fiesta9, asistencia_fiesta10, asistencia_fiesta11, 
    asistencia_fiesta12, atributos_virgen1, atributos_virgen2, santo1, santo2, santo3, 
    santo4, santuario_padre_hurtado, santuario_padre_hurtado1, santuario_padre_hurtado2, 
    santuario_padre_hurtado3, santuario_santa_teresita, santuario_santa_teresita1, 
    santuario_santa_teresita2, santuario_santa_teresita3, describir_cambio_religion1, 
    describir_cambio_religion2, edad_cambio, veces_cambio, motivo_cambio1, motivo_cambio2, 
    motivo_cambio3, motivo_cambio4, evento_compromiso, edad_evento_compromiso, 
    evento_compromiso1, evento_compromiso2, evento_compromiso3, evento_compromiso4, 
    evento_compromiso5, conservar_religion1, conservar_religion2, conservar_religion3, 
    publicidad_religion, asisitencia_misa_catolica, asisitencia_misa_evangelica, 
    asisitencia_misa_protestante, asisitencia_misa_testigos, asisitencia_misa_mormones, 
    asisitencia_misa_otra, frecuencia_misa_catolica, frecuencia_misa_evangelica, 
    frecuencia_misa_protestante, frecuencia_misa_testigos, frecuencia_misa_mormones, 
    frecuencia_misa_otra, imagen_dios, creencia_reencarnacion, creencia_energia, 
    creencia_astrologia, creencia_yoga, creencia_maldeojo, creencia_fantasmas, 
    creencia_telepatia, creencia_clarividencia, creencia_extraterrestes, 
    creencia_comunicacion_muerto, creencia_brujas, creencia_espiritu_poseer, 
    creencia_casa_poseida, creencia_curacion, creencia_karma, creencia_nirvana, 
    creencia_piedas, actrel1, actrel2, actrel3, actrel4, actrel5, actrel6, actrel7, 
    actrel8, actrel9, actrel10, actrel11, frec_actrel1, frec_actrel2, frec_actrel3, 
    frec_actrel4, frec_actrel5, frec_actrel6, frec_actrel7, frec_actrel8, frec_actrel9, 
    frec_actrel10, frec_actrel11, asistencia_virgen, encomendarse_santo, cuan_religioso, 
    devenir_religioso, creencia_milagros_virgen, creencia_milagros, 
    creencia_jesucristo_hijo, cambio_religion, define_como, conoce_cura, temor_muerte_propia,
    mov_inter1, mov_inter2, mov_inter3, mov_inter4, mov_inter5, creencia_cat2, op_chile2, op_chile4, 
    confianza1, op_politica1, op_religion9, tema_iglesia1, tema_iglesia2, tema_iglesia3, tema_iglesia4, tema_iglesia5, tema_iglesia6,
    cercania_iglesia1, cercania_iglesia2, cercania_iglesia3, cercania_iglesia4, cercania_iglesia5,
    recurrir_sacerdote, dedicacion_sacedrote, consejos_papa,
    desempeño_iglesia1, desempeño_iglesia2, desempeño_iglesia3, desempeño_iglesia4, desempeño_iglesia5,
    desempeño_sacerdote1, desempeño_sacerdote2, desempeño_sacerdote3, desempeño_sacerdote4, desempeño_sacerdote5,
    creencia_cat1, creencia_cat3, creencia_cat4, feliz_fe, creencia_personas_justas, creencia_fe_siniglesia,
    creencia_fe_sinritos, frecuencia_confesion, frecuencia_comulgar, uncion_muertos, importancia_sacramentos,
    sacramento_mas_importante, sacramento_menos_importante, creencia_cat5, creencia_cat6, creencia_cat7,
    creencia_cat8, op_iglesia1, op_iglesia2, op_iglesia3, op_iglesia4, op_iglesia5, muerte_piensa, muerte_conversa,
    temor_muerte_padres, temor_muerte_amigos, temor_muerte_hijos, temor_muerte_pareja, sepultado, mirar_muertos,
    asistir_cementerio, rezar_familiares, salvacion_muerte1, salvacion_muerte2, muerte_identificacion1,
    muerte_identificacion2, muerte_identificacion3, muerte_identificacion4, muerte_identificacion5,
    muerte_identificacion6, muerte_identificacion7, seguir_enseñanzas, consejo_sacerdote, tipo_igualdad,
    op_trabajo_mujer1, op_trabajo_mujer2, op_trabajo_mujer3, op_trabajo_mujer4, op_trabajo_mujer5,
    op_trabajo_mujer6, op_trabajo_mujer7, op_trabajo_mujer8, op_trabajo_mujer9, op_trabajo_mujer10,
    op_trabajo_mujer11, op_trabajo_mujer12, op_trabajo_mujer13, op_trabajo_mujer14, op_trabajo_mujer15,
    moral_econ1, moral_econ1_5, moral_econ2, moral_econ3, moral_econ4, moral_econ5, moral_econ6,
    causas_riqueza1, causas_riqueza2, causas_pobreza1, causas_pobreza2, prob_ascenso1, prob_ascenso2,
    prob_ascenso3, prob_ascenso4, prob_ascenso5, prob_ascenso6, op_rol_estado1, op_rol_estado2, op_rol_estado3,
    percep_mov, expectativa_mov, membersia1, membersia2, membersia3, membersia4, membersia5, membersia6,
    membersia7, membersia8, membersia9, membersia10, membersia11, membersia12, membersia13, membersia14,
    membersia15, membersia16, membersia17, membersia18, confianza2, confianza3, confianza4, confianza5,
    confianza6, confianza7, confianza_obispos, confianza_sacerdotes, conf_inst1, conf_inst2, conf_inst3,
    conf_inst4, conf_inst5, conf_inst6, conf_inst7, conf_inst8, conf_inst9, conf_inst10, conf_inst11,
    conf_inst12, conf_inst13, conf_inst14, conf_inst15, op_politica2, op_politica4, op_politica5, tolerancia1,
    tolerancia2, tolerancia3, tolerancia4, tolerancia5, op_chile1, op_chile3, op_chile5, op_chile6, orgullo_chileno,
    percep_clase, op_inmigrantes1, op_inmigrantes2, op_inmigrantes3, op_inmigrantes4, op_inmigrantes5,
    op_inmigrantes6, op_inmigrantes7, op_inmigrantes8, op_inmigrantes9, op_violencia1, pp_indispensables,
    salario_suficiente, justificacion_violencia1, justificacion_violencia2, justificacion_violencia3,
    justificacion_violencia4, justificacion_violencia5, justificacion_violencia6, justificacion_violencia7,
    justificacion_violencia8, justificacion_violencia9, democracia1, op_trabajo_mujer16, op_mujeres1, op_mujeres2,
    papa_francisco1, papa_francisco2, papa_francisco3, papa_francisco4, papa_francisco5, cambios_papa1,
    cambios_papa2, cambios_papa3, cambios_papa4, cambios_papa5, cambios_papa6, cambios_papa7, cambios_papa8,
    op_iglesia, tiene_hijos, perdida_influencia, op_iglesia6, op_iglesia7, op_iglesia8, op_iglesia9, op_iglesia10,
    op_iglesia11, marihuana, confianza_inter1, confianza_inter2, confianza_inter3, confianza_inter4,
    confianza_inter5, confianza_inter6, membersia19, confianza_parroquia, afectados_abusos_sacerdotes,
    abusos_comunes_iglesia, causa_abusos1, causa_abusos2, causa_abusos3, causa_abusos4, iglesia_parroquia,
    grupo_catolico, mov_catolico, confianza_sacerdotes_parroquia, dios_presente1, dios_presente2, dios_presente3,
    dios_presente4, abusos_sexuales1, cambio_practica, describir_cambio_practica, evento_cambio1, evento_cambio2,
    evento_cambio3, abuso_sexual2, iden_pol, iden_pol1, iden_pol2, define_como1, encomendar_familiares,
    invocar_familiares, frecuencia_rezar1, frecuencia_rezar2
  )

# Guardar múltiples objetos en un solo archivo .RData
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario")
save(data_filtrada, file = "base_pt1.RData")

load("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Bicentenario/base_pt1.RData")


#Están: zona, sexo, edad, tipo comuna, nse, tramo edad, ponderador
#Ciudad: existe hasta 2013, 2015, 2018
#Comuna: ver que var comuna y id_comuna coincidan
#Región: falta 2014
#Ver que hago con actividad, ver base a base
#Stgo_reg: 2015, 2016, 2017 (consolidada), 2014,

#comuna, pareja, reli, gse, reli2, dfolio, id, rr10, trabajo, comuna_aux, id_comuna
#membersías preguntadas de otra manera 2016: v1292 - v1297
#voy a tener que revisar op_migrantes desde 2011 en adelante

