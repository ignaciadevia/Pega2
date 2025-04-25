##CREENCIA EN DIOS

datos <- datos %>%
  mutate(estado_civil2 = case_when(
    estado_civil %in% c("Conviviente sin acuerdo", "Conviviente con acuerdo") ~ "Conviviente",
    TRUE ~ estado_civil
  ))
table(datos$estado_civil2)

datos2 <- datos %>%
  filter(religion_encuestado == "Ninguna religión")

datos2 <- datos2 %>%
  filter(creencia_dios != "A momentos sí, en otros no") %>%
  mutate(creencia_dios = factor(creencia_dios))  # Reiniciar los niveles del factor

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios)) %>%  # Excluir NA en la variable de interés
  group_by(creencia_dios) %>%
  summarise(Porcentaje = sum(ponderador, na.rm = TRUE) / sum(datos2$ponderador, na.rm = TRUE) * 100) %>%
  ungroup()
colnames(tabla_df) <- c("", "Porcentaje")
kable(tabla_df, digits = 1, caption = "Creencia en Dios", 
      format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_dios))

# Creencia en Dios según NSE
tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(nse1)) %>%
  group_by(nse1, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(nse1, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = nse1, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_dios, datos2$nse1))

# Creencia en Dios según nivel educacional
tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(educ_enc)) %>%
  group_by(educ_enc, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(educ_enc, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = educ_enc, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según nivel educacional", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_dios, datos2$educ_enc))

# Creencia en Dios según sexo
tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(sexo)) %>%
  group_by(sexo, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(sexo, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = sexo, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según sexo", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_dios, datos2$sexo))

# Creencia en Dios según tramo de edad
tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(tramoedad)) %>%
  group_by(tramoedad, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(tramoedad, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = tramoedad, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según tramo de edad", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_dios, datos2$tramoedad))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(op_matr7)) %>%  
  group_by(op_matr7, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(op_matr7, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Opinión sobre matrimonio homosexual según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$op_matr7, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(op_aborto)) %>%  
  group_by(op_aborto, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(op_aborto, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Opinión sobre el aborto según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$op_aborto, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(conf_inst1)) %>%  
  group_by(conf_inst1, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(conf_inst1, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Confianza en la Iglesia Católica según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$conf_inst1, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_cat2)) %>%  
  group_by(creencia_cat2, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_cat2, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en que los sacerdotes pueden perdonar los pecados en el nombre de Dios según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_cat2, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_milagros)) %>%  
  group_by(creencia_milagros, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_milagros, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en que los milagros según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_milagros, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(frecuencia_misa_enc)) %>%  
  group_by(frecuencia_misa_enc, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(frecuencia_misa_enc, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Actividad religiosa (asistencia a misa) según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$frecuencia_misa_enc, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(frecuencia_rezar2)) %>%  
  group_by(frecuencia_rezar2, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(frecuencia_rezar2, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$frecuencia_rezar2, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(culto_virgen1)) %>%  
  group_by(culto_virgen1, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(culto_virgen1, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene la costumbre de detenerse y rezar a la Virgen en un oratorio, gruta o cualquier imagen pública expuesta según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$culto_virgen1, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(culto_virgen2)) %>%  
  group_by(culto_virgen2, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(culto_virgen2, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene la costumbre de hacer una manda a la Virgen y después paga con oraciones, homenajes o peregrinación según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$culto_virgen2, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(culto_virgen6)) %>%  
  group_by(culto_virgen6, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(culto_virgen6, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene costumbre de visitar el cementerio el día de los muertos a personas cercanas a usted o que han muerto según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$culto_virgen6, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(encomendarse_santo)) %>%  
  group_by(encomendarse_santo, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(encomendarse_santo, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene la costumbre de encomendarse a algún santo según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$encomendarse_santo, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_jesucristo_hijo)) %>%  
  group_by(creencia_jesucristo_hijo, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_jesucristo_hijo, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Actividad religiosa (asistencia a misa) según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_jesucristo_hijo, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_milagros_virgen)) %>%  
  group_by(creencia_milagros_virgen, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_milagros_virgen, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_milagros_virgen, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creeencia_resurreccion)) %>%  
  group_by(creeencia_resurreccion, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creeencia_resurreccion, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creeencia_resurreccion, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_reencarnacion)) %>%  
  group_by(creencia_reencarnacion, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_reencarnacion, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_reencarnacion, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_astrologia)) %>%  
  group_by(creencia_astrologia, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_astrologia, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_astrologia, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_yoga)) %>%  
  group_by(creencia_yoga, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_yoga, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_yoga, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_karma)) %>%  
  group_by(creencia_karma, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_karma, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_karma, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_meditacion)) %>%  
  group_by(creencia_meditacion, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_meditacion, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_meditacion, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_finmundo)) %>%  
  group_by(creencia_finmundo, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_finmundo, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_finmundo, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_energia)) %>%  
  group_by(creencia_energia, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_energia, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_energia, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_naturaleza)) %>%  
  group_by(creencia_naturaleza, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_naturaleza, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_naturaleza, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(creencia_animitas)) %>%  
  group_by(creencia_animitas, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_animitas, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_animitas, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(encomendar_familiares)) %>%  
  group_by(encomendar_familiares, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(encomendar_familiares, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = creencia_dios, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$encomendar_familiares, datos2$creencia_dios))

tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(estado_civil2)) %>%
  group_by(estado_civil2, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(estado_civil2, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = estado_civil2, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según estado civil", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_dios, datos2$estado_civil2))

# Creencia en Dios según iden_pol
tabla_df <- datos2 %>%
  filter(!is.na(creencia_dios), !is.na(iden_pol)) %>%
  group_by(iden_pol, creencia_dios) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(creencia_dios) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(iden_pol, creencia_dios, Porcentaje) %>%
  pivot_wider(names_from = iden_pol, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_dios, datos2$iden_pol))

# Función para calcular el N total de cada tabla de contingencia
calcular_N_total <- function(variable) {
  N_total <- datos2 %>%
    filter(!is.na(creencia_dios), !is.na(.data[[variable]])) %>%
    summarise(N = n()) %>%
    pull(N)
  
  cat("N total de", variable, "según creencia en Dios:", N_total, "\n")
}

# Variables a analizar
variables <- c("cree_dios", "nse1", "sexo", "educ_enc", "tramoedad", "op_matr7", "op_aborto", "conf_inst1", 
               "creencia_cat2", "creencia_milagros", "frecuencia_misa_enc", 
               "frecuencia_rezar2", "culto_virgen1", "culto_virgen2", "culto_virgen6", "creencia_jesucristo_hijo", "creeencia_resurreccion", 
               "creencia_milagros_virgen", "creencia_reencarnacion", 
               "creencia_astrologia", "creencia_karma", "creencia_yoga", 
               "creencia_meditacion", "creencia_finmundo", "creencia_energia", 
               "creencia_naturaleza", "creencia_animitas", "encomendar_familiares", "encomendarse_santo", "estado_civil2", "iden_pol")

# Calcular N total para cada variable
for (var in variables) {
  calcular_N_total(var)
}


# Función para ver en qué años se preguntó cada variable
ver_anos_preguntados <- function(variable) {
  anos_disponibles <- datos2 %>%
    filter(!is.na(.data[[variable]])) %>%
    distinct(ano) %>%
    pull(ano)
  
  cat("La variable", variable, "fue preguntada en los años:", paste(anos_disponibles, collapse = ", "), "\n")
}

# Aplicar la función a todas las variables del vector
for (var in variables) {
  ver_anos_preguntados(var)
}

##TRONCO RELIGIOSO

datos2 <- datos %>%
  filter(religion_encuestado == "Ninguna religión")

datos2 <- datos2 %>%
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

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(nse1)) %>%
  group_by(nse1, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(nse1, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = nse1, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tronco religioso según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$tronco_religioso, datos2$nse1))

# Tronco religioso según nivel educacional
tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(educ_enc)) %>%
  group_by(educ_enc, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(educ_enc, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = educ_enc, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tronco religioso según nivel educacional", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$tronco_religioso, datos2$educ_enc))

# Tronco religioso según sexo
tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(sexo)) %>%
  group_by(sexo, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(sexo, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = sexo, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tronco religioso según sexo", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$tronco_religioso, datos2$sexo))

# Tronco religioso según tramo de edad
tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(tramoedad)) %>%
  group_by(tramoedad, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(tramoedad, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tramoedad, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tronco religioso según tramo de edad", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$tronco_religioso, datos2$tramoedad))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(cree_dios)) %>%  
  group_by(cree_dios, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(cree_dios, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))
chisq.test(table(datos2$cree_dios, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(op_matr7)) %>%  
  group_by(op_matr7, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(op_matr7, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Opinión sobre matrimonio homosexual según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$op_matr7, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(op_aborto)) %>%  
  group_by(op_aborto, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(op_aborto, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Opinión sobre el aborto según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$op_aborto, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(conf_inst1)) %>%  
  group_by(conf_inst1, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(conf_inst1, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Confianza en la Iglesia Católica según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$conf_inst1, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_cat2)) %>%  
  group_by(creencia_cat2, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_cat2, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en que los sacerdotes pueden perdonar los pecados en el nombre de Dios según tronco religoso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_cat2, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_milagros)) %>%  
  group_by(creencia_milagros, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_milagros, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en que los milagros según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_milagros, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(frecuencia_misa_enc)) %>%  
  group_by(frecuencia_misa_enc, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(frecuencia_misa_enc, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Actividad religiosa (asistencia a misa) según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$frecuencia_misa_enc, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(frecuencia_rezar2)) %>%  
  group_by(frecuencia_rezar2, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(frecuencia_rezar2, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$frecuencia_rezar2, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(culto_virgen1)) %>%  
  group_by(culto_virgen1, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(culto_virgen1, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene la costumbre de detenerse y rezar a la Virgen en un oratorio, gruta o cualquier imagen pública expuesta según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$culto_virgen1, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(culto_virgen2)) %>%  
  group_by(culto_virgen2, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(culto_virgen2, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene la costumbre de hacer una manda a la Virgen y después paga con oraciones, homenajes o peregrinación según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$culto_virgen2, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(culto_virgen6)) %>%  
  group_by(culto_virgen6, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(culto_virgen6, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene costumbre de visitar el cementerio el día de los muertos a personas cercanas a usted o que han muerto según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$culto_virgen6, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(encomendarse_santo)) %>%  
  group_by(encomendarse_santo, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(encomendarse_santo, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Tiene la costumbre de encomendarse a algún santo según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$encomendarse_santo, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_jesucristo_hijo)) %>%  
  group_by(creencia_jesucristo_hijo, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_jesucristo_hijo, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Actividad religiosa (asistencia a misa) según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_jesucristo_hijo, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_milagros_virgen)) %>%  
  group_by(creencia_milagros_virgen, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_milagros_virgen, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_milagros_virgen, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creeencia_resurreccion)) %>%  
  group_by(creeencia_resurreccion, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creeencia_resurreccion, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según tronco religioso", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creeencia_resurreccion, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_reencarnacion)) %>%  
  group_by(creencia_reencarnacion, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_reencarnacion, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_reencarnacion, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_astrologia)) %>%  
  group_by(creencia_astrologia, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_astrologia, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_astrologia, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_yoga)) %>%  
  group_by(creencia_yoga, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_yoga, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_yoga, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_karma)) %>%  
  group_by(creencia_karma, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_karma, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_karma, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_meditacion)) %>%  
  group_by(creencia_meditacion, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_meditacion, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_meditacion, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_finmundo)) %>%  
  group_by(creencia_finmundo, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_finmundo, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_finmundo, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_energia)) %>%  
  group_by(creencia_energia, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_energia, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_energia, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_naturaleza)) %>%  
  group_by(creencia_naturaleza, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_naturaleza, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_naturaleza, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(creencia_animitas)) %>%  
  group_by(creencia_animitas, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(creencia_animitas, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$creencia_animitas, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(encomendar_familiares)) %>%  
  group_by(encomendar_familiares, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  
  ungroup()
tabla_df %>%
  select(encomendar_familiares, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = tronco_religioso, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Habito de rezar según creencia en Dios", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$encomendar_familiares, datos2$tronco_religioso))

tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(estado_civil2)) %>%
  group_by(estado_civil2, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(estado_civil2, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = estado_civil2, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$tronco_religioso, datos2$estado_civil2))

# Creencia en Dios según NSE
tabla_df <- datos2 %>%
  filter(!is.na(tronco_religioso), !is.na(iden_pol)) %>%
  group_by(iden_pol, tronco_religioso) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tronco_religioso) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(iden_pol, tronco_religioso, Porcentaje) %>%
  pivot_wider(names_from = iden_pol, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Creencia en Dios según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$tronco_religioso, datos2$iden_pol))

# Función para calcular el N total de cada tabla de contingencia
calcular_N_total <- function(variable) {
  N_total <- datos2 %>%
    filter(!is.na(tronco_religioso), !is.na(.data[[variable]])) %>%
    summarise(N = n()) %>%
    pull(N)
  
  cat("N total de", variable, "según tronco religioso:", N_total, "\n")
}

# Variables a analizar
variables <- c("cree_dios", "nse1", "sexo", "educ_enc", "tramoedad", "op_matr7", "op_aborto", "conf_inst1", 
               "creencia_cat2", "creencia_milagros", "frecuencia_misa_enc", 
               "frecuencia_rezar2", "culto_virgen1", "culto_virgen2", "culto_virgen6", "creencia_jesucristo_hijo", "creeencia_resurreccion", 
               "creencia_milagros_virgen", "creencia_reencarnacion", 
               "creencia_astrologia", "creencia_karma", "creencia_yoga", 
               "creencia_meditacion", "creencia_finmundo", "creencia_energia", 
               "creencia_naturaleza", "creencia_animitas", "encomendar_familiares", "encomendarse_santo", "estado_civil2", "iden_pol")

# Calcular N total para cada variable
for (var in variables) {
  calcular_N_total(var)
}


# Función para ver en qué años se preguntó cada variable
ver_anos_preguntados <- function(variable) {
  anos_disponibles <- datos2 %>%
    filter(!is.na(.data[[variable]])) %>%
    distinct(ano) %>%
    pull(ano)
  
  cat("La variable", variable, "fue preguntada en los años:", paste(anos_disponibles, collapse = ", "), "\n")
}

# Aplicar la función a todas las variables del vector
for (var in variables) {
  ver_anos_preguntados(var)
}

###TABLAS TEMPLO - SANTUARIO

#TEMPLO
tabla_df <- datos %>%
  filter(!is.na(misa_encuestado)) %>%  # Excluir NA en la variable de interés
  group_by(misa_encuestado) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  # Calcular el porcentaje
  ungroup()

# Mostrar la tabla en formato LaTeX
tabla_df %>%
  kable(digits = 1, caption = "Asistencia a servicios religiosos", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$misa_encuestado))

# Servicios religiosos según NSE, calculando porcentaje por columna
tabla_df <- datos %>%
  filter(!is.na(misa_encuestado), !is.na(nse1)) %>%
  group_by(nse1, misa_encuestado) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(nse1) %>%  # Cambiar a agrupar por nse1 (columna) para calcular el porcentaje por columna
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(nse1, misa_encuestado, Porcentaje) %>%
  pivot_wider(names_from = nse1, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$misa_encuestado, datos$nse1))


# Ejemplo para sexo
tabla_df <- datos %>%
  filter(!is.na(misa_encuestado), !is.na(sexo)) %>%
  group_by(sexo, misa_encuestado) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(sexo) %>%  # Cambiar a agrupar por sexo (columna)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(sexo, misa_encuestado, Porcentaje) %>%
  pivot_wider(names_from = sexo, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según sexo", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$misa_encuestado, datos$sexo))

# Ejemplo para tramoedad
tabla_df <- datos %>%
  filter(!is.na(misa_encuestado), !is.na(tramoedad)) %>%
  group_by(tramoedad, misa_encuestado) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tramoedad) %>%  # Cambiar a agrupar por tramoedad (columna)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(tramoedad, misa_encuestado, Porcentaje) %>%
  pivot_wider(names_from = tramoedad, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según tramo de edad", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$misa_encuestado, datos$tramoedad))

# Ejemplo para religion_encuestado2
tabla_df <- datos %>%
  filter(!is.na(misa_encuestado), !is.na(religion_encuestado2)) %>%
  group_by(religion_encuestado2, misa_encuestado) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(religion_encuestado2) %>%  # Cambiar a agrupar por religion_encuestado2 (columna)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(religion_encuestado2, misa_encuestado, Porcentaje) %>%
  pivot_wider(names_from = religion_encuestado2, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según religion", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$misa_encuestado, datos$religion_encuestado2))

tabla_df <- datos %>%
  filter(!is.na(misa_encuestado), !is.na(educ_enc)) %>%
  group_by(educ_enc, misa_encuestado) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(educ_enc) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(educ_enc, misa_encuestado, Porcentaje) %>%
  pivot_wider(names_from = educ_enc, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según Educacion", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$misa_encuestado, datos$educ_enc))

tabla_df <- datos %>%
  filter(!is.na(misa_encuestado), !is.na(estado_civil2)) %>%
  group_by(estado_civil2, misa_encuestado) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(estado_civil2) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(estado_civil2, misa_encuestado, Porcentaje) %>%
  pivot_wider(names_from = estado_civil2, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según Educacion", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$misa_encuestado, datos$estado_civil2))


# Función para calcular el N total de cada tabla de contingencia
calcular_N_total <- function(variable) {
  N_total <- datos %>%
    filter(!is.na(misa_encuestado), !is.na(.data[[variable]])) %>%
    summarise(N = n()) %>%
    pull(N)
  
  cat("N total de", variable, "según creencia en Dios:", N_total, "\n")
}

# Variables a analizar
variables <- c("misa_encuestado", "nse1", "sexo", "educ_enc", "tramoedad", "religion_encuestado", "estado_civil2")

# Calcular N total para cada variable
for (var in variables) {
  calcular_N_total(var)
}


# Función para ver en qué años se preguntó cada variable
ver_anos_preguntados <- function(variable) {
  anos_disponibles <- datos %>%
    filter(!is.na(misa_encuestado), !is.na(.data[[variable]])) %>%
    distinct(ano) %>%
    pull(ano)
  
  cat("La variable", variable, "fue preguntada en los años:", paste(anos_disponibles, collapse = ", "), "\n")
}

# Aplicar la función a todas las variables del vector
for (var in variables) {
  ver_anos_preguntados(var)
}

#SANTUARIO

tabla_df <- datos %>%
  filter(!is.na(asistencia_virgen)) %>%  # Excluir NA en la variable de interés
  group_by(asistencia_virgen) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  # Calcular el porcentaje
  ungroup()

# Mostrar la tabla en formato LaTeX
tabla_df %>%
  kable(digits = 1, caption = "Asistencia a servicios religiosos", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$asistencia_virgen))

# Asistencia a santuarios según NSE
tabla_df <- datos %>%
  filter(!is.na(asistencia_virgen), !is.na(nse1)) %>%
  group_by(nse1, asistencia_virgen) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(nse1) %>%  # Cambiar el grupo por columna (nse1) en lugar de por fila (asistencia_virgen)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(nse1, asistencia_virgen, Porcentaje) %>%
  pivot_wider(names_from = nse1, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Asistencia a santuarios según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$asistencia_virgen, datos$nse1))

# Asistencia a santuarios según nivel educacional
tabla_df <- datos %>%
  filter(!is.na(asistencia_virgen), !is.na(educ_enc)) %>%
  group_by(educ_enc, asistencia_virgen) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(educ_enc) %>%  # Cambiar el grupo por columna (educ_enc)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(educ_enc, asistencia_virgen, Porcentaje) %>%
  pivot_wider(names_from = educ_enc, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Asistencia a santuarios según nivel educacional", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$asistencia_virgen, datos$educ_enc))

# Asistencia a santuarios según sexo
tabla_df <- datos %>%
  filter(!is.na(asistencia_virgen), !is.na(sexo)) %>%
  group_by(sexo, asistencia_virgen) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(sexo) %>%  # Cambiar el grupo por columna (sexo)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(sexo, asistencia_virgen, Porcentaje) %>%
  pivot_wider(names_from = sexo, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Asistencia a santuarios según sexo", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$asistencia_virgen, datos$sexo))

# Asistencia a santuarios según tramo de edad
tabla_df <- datos %>%
  filter(!is.na(asistencia_virgen), !is.na(tramoedad)) %>%
  group_by(tramoedad, asistencia_virgen) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tramoedad) %>%  # Cambiar el grupo por columna (tramoedad)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(tramoedad, asistencia_virgen, Porcentaje) %>%
  pivot_wider(names_from = tramoedad, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Asistencia a santuarios según tramo de edad", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$asistencia_virgen, datos$tramoedad))

# Asistencia a santuarios según religión
tabla_df <- datos %>%
  filter(!is.na(asistencia_virgen), !is.na(religion_encuestado2)) %>%
  group_by(religion_encuestado2, asistencia_virgen) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(religion_encuestado2) %>%  # Cambiar el grupo por columna (religion_encuestado2)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(religion_encuestado2, asistencia_virgen, Porcentaje) %>%
  pivot_wider(names_from = religion_encuestado2, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según religión", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$asistencia_virgen, datos$religion_encuestado2))

tabla_df <- datos %>%
  filter(!is.na(asistencia_virgen), !is.na(estado_civil2)) %>%
  group_by(estado_civil2, asistencia_virgen) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(estado_civil2) %>%  # Cambiar el grupo por columna (estado_civil2)
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>% 
  ungroup()

tabla_df %>%
  select(estado_civil2, asistencia_virgen, Porcentaje) %>%
  pivot_wider(names_from = estado_civil2, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Servicios religiosos según estado civil", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos$asistencia_virgen, datos$estado_civil2))


# Función para calcular el N total de cada tabla de contingencia
calcular_N_total <- function(variable) {
  N_total <- datos %>%
    filter(!is.na(asistencia_virgen), !is.na(.data[[variable]])) %>%
    summarise(N = n()) %>%
    pull(N)
  
  cat("N total de", variable, "según creencia en Dios:", N_total, "\n")
}

# Variables a analizar
variables <- c("nse1", "sexo", "educ_enc", "tramoedad", "asistencia_virgen", "religion_encuestado2", "estado_civil2")

# Calcular N total para cada variable
for (var in variables) {
  calcular_N_total(var)
}


# Función para ver en qué años se preguntó cada variable
ver_anos_preguntados <- function(variable) {
  anos_disponibles <- datos %>%
    filter(!is.na(.data[[variable]])) %>%
    distinct(ano) %>%
    pull(ano)
  
  cat("La variable", variable, "fue preguntada en los años:", paste(anos_disponibles, collapse = ", "), "\n")
}

# Aplicar la función a todas las variables del vector
for (var in variables) {
  ver_anos_preguntados(var)
}

#TEMPLO-SANTUARIO SOLO EN CATÓLICOS
datos2 <- datos %>%
  filter(reli_encuestado == "Católica") %>%  # Filtrar solo católicos
  mutate(
    templo_santuario = case_when(
      misa_encuestado == "Activo" & asistencia_virgen == "Si"  ~ "Ambos",
      misa_encuestado == "Activo" & asistencia_virgen == "No"  ~ "Templo",
      misa_encuestado == "Inactivo" & asistencia_virgen == "Si" ~ "Santuario",
      misa_encuestado == "Inactivo" & asistencia_virgen == "No" ~ "Ninguno",
      TRUE ~ NA_character_  # Mantener NA si hay valores faltantes
    )
  )
table(datos2$templo_santuario)

tabla_df <- datos2 %>%
  filter(!is.na(templo_santuario)) %>%  # Excluir NA en la variable de interés
  group_by(templo_santuario) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%  # Calcular el porcentaje
  ungroup()

# Mostrar la tabla en formato LaTeX
tabla_df %>%
  kable(digits = 1, caption = "Asistencia a servicios religiosos", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$templo_santuario))

# Templo vs Santuario según NSE
tabla_df <- datos2 %>%
  filter(!is.na(templo_santuario), !is.na(nse1)) %>%
  group_by(nse1, templo_santuario) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(nse1) %>%  # CAMBIAMOS a agrupar por NSE
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(nse1, templo_santuario, Porcentaje) %>%
  pivot_wider(names_from = templo_santuario, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Distribución de templo vs santuario según NSE", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$templo_santuario, datos2$nse1))

# Templo vs Santuario según nivel educacional
tabla_df <- datos2 %>%
  filter(!is.na(templo_santuario), !is.na(educ_enc)) %>%
  group_by(educ_enc, templo_santuario) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(educ_enc) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(educ_enc, templo_santuario, Porcentaje) %>%
  pivot_wider(names_from = templo_santuario, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Distribución de templo vs santuario según nivel educacional", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$templo_santuario, datos2$educ_enc))

# Templo vs Santuario según sexo
tabla_df <- datos2 %>%
  filter(!is.na(templo_santuario), !is.na(sexo)) %>%
  group_by(sexo, templo_santuario) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(sexo, templo_santuario, Porcentaje) %>%
  pivot_wider(names_from = templo_santuario, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Distribución de templo vs santuario según sexo", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$templo_santuario, datos2$sexo))

# Templo vs Santuario según tramo de edad
tabla_df <- datos2 %>%
  filter(!is.na(templo_santuario), !is.na(tramoedad)) %>%
  group_by(tramoedad, templo_santuario) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(tramoedad) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(tramoedad, templo_santuario, Porcentaje) %>%
  pivot_wider(names_from = templo_santuario, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Distribución de templo vs santuario según tramo de edad", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$templo_santuario, datos2$tramoedad))

# Templo vs Santuario según religión
tabla_df <- datos2 %>%
  filter(!is.na(templo_santuario), !is.na(religion_encuestado2)) %>%
  group_by(religion_encuestado2, templo_santuario) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(religion_encuestado2) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(religion_encuestado2, templo_santuario, Porcentaje) %>%
  pivot_wider(names_from = templo_santuario, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Distribución de templo vs santuario según religión", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$templo_santuario, datos2$religion_encuestado2))

tabla_df <- datos2 %>%
  filter(!is.na(templo_santuario), !is.na(estado_civil2)) %>%
  group_by(estado_civil2, templo_santuario) %>%
  summarise(Frecuencia = sum(ponderador, na.rm = TRUE), .groups = "drop") %>%
  group_by(estado_civil2) %>%
  mutate(Porcentaje = (Frecuencia / sum(Frecuencia)) * 100) %>%
  ungroup()

tabla_df %>%
  select(estado_civil2, templo_santuario, Porcentaje) %>%
  pivot_wider(names_from = templo_santuario, values_from = Porcentaje) %>%
  kable(digits = 1, caption = "Distribución de templo vs santuario según religión", 
        format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

chisq.test(table(datos2$templo_santuario, datos2$estado_civil2))

# Función para calcular el N total de cada tabla de contingencia
calcular_N_total <- function(variable) {
  N_total <- datos2 %>%
    filter(!is.na(templo_santuario), !is.na(.data[[variable]])) %>%
    summarise(N = n()) %>%
    pull(N)
  
  cat("N total de", variable, "según creencia en Dios:", N_total, "\n")
}

# Variables a analizar
variables <- c("nse1", "sexo", "educ_enc", "tramoedad", "templo_santuario", "religion_encuestado", "estado_civil2")

# Calcular N total para cada variable
for (var in variables) {
  calcular_N_total(var)
}


# Función para ver en qué años se preguntó cada variable
ver_anos_preguntados <- function(variable) {
  anos_disponibles <- datos2 %>%
    filter(!is.na(templo_santuario), !is.na(.data[[variable]])) %>%
    distinct(ano) %>%
    pull(ano)
  
  cat("La variable", variable, "fue preguntada en los años:", paste(anos_disponibles, collapse = ", "), "\n")
}

# Aplicar la función a todas las variables del vector
for (var in variables) {
  ver_anos_preguntados(var)
}
