library(tidyverse)



base_persona <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_base.txt",
                           header =  TRUE,
                           sep ="|",
)


str(base_persona)
ncol(base_persona)
dim(base_persona)
names(base_persona)




#Hogar monomarental con y sin demanda de cuidado
monomarental_demanda <- base_persona %>%
                      filter(TIPO_HOGAR_NUCLEAR == 4) %>%
                      select(TIPO_HOGAR_NUCLEAR, BHDC_SEL,WHOG) %>% 
                      mutate(BHDC_SEL = ifelse(BHDC_SEL == 0, "Sin demanda de cuidados", "Con demanda de cuidados")) %>% 
                      group_by(BHDC_SEL) %>% 
                      summarise(
                        hogares = sum(WHOG))
                        spread(., key = BHDC_SEL,value = hogares)

#  CUIDADO_FAMILIA, CUIDADO_FAMILIA_EXTERNA, CUIDADO_HOGAR, CUIDADO_COMUNIDAD


#Hogares monomarentales con demanda que usan sistema de cuidado Estatal 

monomarental_estatal <- base_persona %>%
  filter(TIPO_HOGAR_NUCLEAR == 4) %>%
  select(TIPO_HOGAR_NUCLEAR, CUIDADO_ESTADO, WHOG) %>%
  mutate(CUIDADO_ESTADO = ifelse(CUIDADO_ESTADO == 0, "Con demanda de cuidados sin cuidado del Estado", "Con demanda de cuidados con cuidado del Estado")) %>% 
  group_by(CUIDADO_ESTADO) %>% 
  summarise(
  hogares_monomarentales = sum(WHOG)
  ) #%>% spread(., key = CUIDADO_ESTADO,value =  hogares_monomarentales)


#Hogares monomarentales con demanda que usan sistema de cuidado comunitario

monomarental_comunidad <- base_persona %>%
  filter(TIPO_HOGAR_NUCLEAR == 4) %>%
  select(TIPO_HOGAR_NUCLEAR, CUIDADO_COMUNIDAD, WHOG) %>%
  mutate(CUIDADO_COMUNIDAD = ifelse(CUIDADO_COMUNIDAD == 0, "Con demanda de cuidados sin cuidado del de la comunidad", "Con demanda de cuidados con cuidado de la comunidad")) %>% 
  group_by(CUIDADO_COMUNIDAD) %>% 
  summarise(
    hogares_monomarentales = sum(WHOG)
  )
                  
#Hogares monomarentales con demanda que usan sistema de cuidado del mercado
monomarental_mercado <- base_persona %>%
  filter(TIPO_HOGAR_NUCLEAR == 4) %>%
  select(TIPO_HOGAR_NUCLEAR, CUIDADO_MERCADO, WHOG) %>%
  mutate(CUIDADO_MERCADO = ifelse(CUIDADO_MERCADO == 0, "Con demanda de cuidados sin cuidado del mercado", "Con demanda de cuidados con cuidado del mercado")) %>% 
  group_by(CUIDADO_MERCADO) %>% 
  summarise(
    hogares_monomarentales = sum(WHOG)
  )



#Hogares con miembro seleccionado INACTIVO+DESOCUPADO (ocupado/desocupado/inactivo) con demandante de cuidado hasta 13 aÃ±os 
inactivo_con_demanda_13 <- base_persona %>%
  #filter(CONDICION_ACTIVIDAD_AGRUPADA == 2) %>% #No ocupado
  #filter(CONDICION_ACTIVIDAD_AGRUPADA == 1) %>% #ocupado
  filter(TIPO_HOGAR_DCPOREDAD == 1) %>%
  select(CONDICION_ACTIVIDAD_AGRUPADA, TIPO_HOGAR_DCPOREDAD, WHOG) %>%
  mutate(CONDICION_ACTIVIDAD_AGRUPADA = ifelse(CONDICION_ACTIVIDAD_AGRUPADA == 1, "ocupado", "no ocupado")) %>% 
  group_by(CONDICION_ACTIVIDAD_AGRUPADA) %>% 
  summarise(
    hogares_con_demanda_inactivo_13 = sum(WHOG)
  )

#----------------------------------------
#Horas promedio dedicadas a las tareas domésticas y de cuidado no remunerada en hogares monomarentales con presencia de niñxs de hasta 13 años

monomarental_horas <- base_persona %>%
  filter(TIPO_HOGAR_NUCLEAR == 4, CANT_PERSONASHASTA13 >= 1) %>%
  select(TCS_GRUPO_CUIDADO,TCS_GRUPO_DOMESTICO, WHOG) %>%
  group_by(TCS_GRUPO_DOMESTICO) %>% 
  summarise(
    hogares = sum(WHOG))
    mean(monomarental_horas)
    
#promedio entre columnas
colMeans (monomarental_horas)
#--------------------------------------------------------------------
#Hogares con personas mayores que pagan por una cuidadora 
hogar_mayores_cuidadora <- base_persona %>%
  filter(BHDC01_06_SEL == 1) %>%
  select(CANT_PERSONAS65YMAS, BHDC01_06_SEL,WHOG) %>% 
  mutate(BHDC01_06_SEL = ifelse(BHDC01_06_SEL == 0, "No cuida una persona a quien se paga", "Lo cuida una persona a quien se paga")) %>% 
  group_by(BHDC01_06_SEL) %>% 
  summarise(
    hogares = sum(WHOG))
#spread(., key = BHDC_SEL,value = hogares)


#--------------------------------------------------------------------
#Hogares que tienen personas mayores viviendo en residencias 

hogar_mayores_residencia <- base_persona %>%
  filter(BHAH01 == 1, BHAH02_01== 1) %>%
  select(BHAH02_01, BHAH01,WHOG) %>% 
  mutate(BHAH01 = ifelse(BHAH01 == 0, "Hogares que no  personas mayores viviendo en residencias", "Hogares que tiene personas mayores viviendo en residencias")) %>% 
  group_by(BHAH01) %>% 
  summarise(
    hogares = sum(WHOG))






#monomarental_horas$promedio <- rowMeans (monomarental_horas [, c (1,2)], na.rm = TRUE )
    

#¿si quiero calcular este indicador para hogares con ninxs de hasta 5 años, tengo que joinear con base miembros o no se puede directamente?
#¿o a este cálculo lo tengo que sacar con la base de diario?
#¿pasar minutos a horas?
