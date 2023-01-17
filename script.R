library(tidyverse)



base_persona <- read.table("enut2021_base.txt",
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
                       #%>% spread(., key = BHDC_SEL,value = hogares) %>% 

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



#Hogares con miembro seleccionado INACTIVO+DESOCUPADO (ocupado/desocupado/inactivo) con demandante de cuidado hasta 13 años 
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




