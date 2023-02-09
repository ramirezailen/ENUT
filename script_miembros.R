library(tidyverse)



base_miembros <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_miembros.txt",
                           header =  TRUE,
                           sep ="|",
)

base_usuario <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_base.txt",
                           header =  TRUE,
                           sep ="|",
)

#Hogares con presencia de niñxs de hasta 2  años que reciben asistencia en cuidados  ( sea mediante gestion estatal,privada o comunitaria )

menores_2_años <-base_miembros %>%
  filter( BHCH03 <= 2 ) %>% #edad
  group_by (ID) %>%
  summarise (
    dos_años_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    dos_años_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &  BHDC01_04 != 1 & BHDC01_06 != 1]),
    dos_años_privado = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 != 1 & BHDC01_06 == 1]),
    dos_años_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 == 1 & BHDC01_06 != 1]),
    dos_años_total = length(N_MIEMBRO)
  )



#ponderador

persona_miembro <- menores_2_años %>%
  left_join( ., base_usuario, by ='ID')%>%
  select(dos_años_no_excluyente,dos_años_comunitario, dos_años_privado,dos_años_estatal, dos_años_total, WHOG)




#porcentajes


porcentaje_2_años <- persona_miembro %>%
  summarise(dos_años_no_excluyente = sum(WHOG[dos_años_no_excluyente >= 1]),
            dos_años_comunitario = sum(WHOG[dos_años_comunitario >= 1]),
            dos_años_privado = sum(WHOG[dos_años_privado >= 1]),
            dos_años_estatal = sum(WHOG[dos_años_estatal >= 1]),
            dos_años_total = sum(WHOG),

            
            porcentaje_no_exc = dos_años_no_excluyente/dos_años_total * 100,
            porcentaje_comu = dos_años_comunitario/dos_años_total * 100,
            porcentaje_priv = dos_años_privado/dos_años_total * 100,
            porcentaje_esta = dos_años_estatal/dos_años_total * 100,
            porcentaje_total = dos_años_total/dos_años_total * 100
            
            )



#-------------------------------

#Hogares con presencia de niñxs 3  años que reciben asistencia en cuidados  ( sea mediante gestion estatal,privada o comunitaria )

niños_3_años <- base_miembros %>%
  filter( BHCH03 == 3 ) %>% #edad
  group_by (ID) %>%
  summarise (
    tres_años_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    tres_años_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &  BHDC01_04 != 1 & BHDC01_06 != 1]),
    tres_años_privado = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 != 1 & BHDC01_06 == 1]),
    tres_años_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 == 1 & BHDC01_06 != 1]),
    tres_años_total = length(N_MIEMBRO)
  )



#ponderador

pondera_3 <- niños_3_años %>%
  left_join( ., base_usuario, by ='ID')%>%
  select( tres_años_no_excluyente, tres_años_comunitario,  tres_años_privado, tres_años_estatal,  tres_años_total, WHOG)




#porcentajes
porcentajes_3 <- pondera_3 %>%
  summarise(tres_años_no_excluyente = sum(WHOG[tres_años_no_excluyente >= 1]),
            tres_años_comunitario = sum(WHOG[tres_años_comunitario >= 1]),
            tres_años_privado = sum(WHOG[tres_años_privado >= 1]),
            tres_años_estatal = sum(WHOG[tres_años_estatal >= 1]),
            tres_años_total = sum(WHOG), 
            
            porcentaje = tres_años_no_excluyente/tres_años_total * 100,
            porcentaje2 = tres_años_comunitario/tres_años_total * 100,
            porcentaje3 = tres_años_privado/tres_años_total * 100,
            porcentaje4 = tres_años_estatal/tres_años_total * 100,
            porcentaje5 = tres_años_total/tres_años_total * 100
            )


#-------------------------------
#Hogares con presencia de niñxs de 4 a 5 años que reciben asistencia en cuidados ( sea mediante gestion estatal,privada o comunitaria )


niños_4_5_años <- base_miembros %>%
  filter( BHCH03 == 4&5 ) %>% #edad
  group_by (ID) %>%
  summarise (
    cuatrocinco_años_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    cuatrocinco_años_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &  BHDC01_04 != 1 & BHDC01_06 != 1]),
    cuatrocinco_años_privado = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 != 1 & BHDC01_06 == 1]),
    cuatrocinco_años_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 == 1 & BHDC01_06 != 1]),
    cuatrocinco_años_total = length(N_MIEMBRO)
  )



#ponderador

pondera_4_5 <- niños_4_5_años %>%
  left_join( ., base_usuario, by ='ID')%>%
  select( cuatrocinco_años_no_excluyente, cuatrocinco_años_comunitario,  cuatrocinco_años_privado, cuatrocinco_años_estatal,  cuatrocinco_años_total, WHOG)




#porcentajes
porcentajes_4_5 <- pondera_4_5 %>%
  summarise(cuatrocinco_años_no_excluyente = sum(WHOG[cuatrocinco_años_no_excluyente >= 1]),
            cuatrocinco_años_comunitario = sum(WHOG[cuatrocinco_años_comunitario >= 1]),
            cuatrocinco_años_privado = sum(WHOG[cuatrocinco_años_privado >= 1]),
            cuatrocinco_años_estatal = sum(WHOG[cuatrocinco_años_estatal >= 1]),
            cuatrocinco_años_total = sum(WHOG),
            
            porcentaje = cuatrocinco_años_no_excluyente/cuatrocinco_años_total * 100,
            porcentaje2 = cuatrocinco_años_comunitario/cuatrocinco_años_total * 100,
            porcentaje3 = cuatrocinco_años_privado/cuatrocinco_años_total * 100,
            porcentaje4 = cuatrocinco_años_estatal/cuatrocinco_años_total * 100,
            porcentaje5 = cuatrocinco_años_total/cuatrocincos_años_total * 100
            )


#---------------------------------------------


#Porcentaje de hogares que contrata servicios de cuidado para niñxs


hogares_cuidados <- base_miembros %>% 
  #filter(BHDC01_06 == 1) %>% #BHDC01_06 también se puede considerar BHDC02_01
  group_by (BHDC01_06) %>%
  summarise (
    hogares_cuidados_contrata = sum(N_MIEMBRO[BHDC01_06== 1]),
    hogares_cuidados_total = sum(N_MIEMBRO[BHDC01_06])
  )


#chequear si está bien para seguir haciendo el %
#hay que agrupar edades?


monomarental_demanda <- base_persona %>%
  filter(TIPO_HOGAR_NUCLEAR == 4) %>%
  select(TIPO_HOGAR_NUCLEAR, BHDC_SEL,WHOG) %>% 
  mutate(BHDC_SEL = ifelse(BHDC_SEL == 0, "Sin demanda de cuidados", "Con demanda de cuidados")) %>% 
  group_by(BHDC_SEL) %>% 
  summarise(
    hogares = sum(WHOG))
spread(., key = BHDC_SEL,value = hogares)

#ponderador

pondera_4_5 <- niños_4_5_años %>%
  left_join( ., base_usuario, by ='ID')%>%
  select( cuatrocinco_años_no_excluyente, cuatrocinco_años_comunitario,  cuatrocinco_años_privado, cuatrocinco_años_estatal,  cuatrocinco_años_total, WHOG)




#porcentajes
porcentajes_4_5 <- pondera_4_5 %>%
  summarise(cuatrocinco_años_no_excluyente = sum(WHOG[cuatrocinco_años_no_excluyente >= 1]),
            cuatrocinco_años_comunitario = sum(WHOG[cuatrocinco_años_comunitario >= 1]),
            cuatrocinco_años_privado = sum(WHOG[cuatrocinco_años_privado >= 1]),
            cuatrocinco_años_estatal = sum(WHOG[cuatrocinco_años_estatal >= 1]),
            cuatrocinco_años_total = sum(WHOG),
            
            porcentaje = cuatrocinco_años_no_excluyente/cuatrocinco_años_total * 100,
            porcentaje2 = cuatrocinco_años_comunitario/cuatrocinco_años_total * 100,
            porcentaje3 = cuatrocinco_años_privado/cuatrocinco_años_total * 100,
            porcentaje4 = cuatrocinco_años_estatal/cuatrocinco_años_total * 100,
            porcentaje5 = cuatrocinco_años_total/cuatrocincos_años_total * 100
  )





#-------------------------------
#Este no va
#Hogares con presencia de niñxs de hasta 5 años  que asisten al jardín y que además reciben asistencia en cuidados (gestión estatal, privada o comunitaria)

menores_5_años <-base_miembros %>% 
  filter( BHCH03 >= 5 &  BHCH08 == 1) %>% 
  group_by (ID) %>%
  summarise (
    cinco_años_asistencia_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    cinco_años_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &  BHDC01_04 != 1 & BHDC01_06 != 1]),
    cinco_años_privado = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 != 1 & BHDC01_06 == 1]),
    cinco_años_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &  BHDC01_04 == 1 & BHDC01_06 != 1]),
    cinco_años_total = length(N_MIEMBRO)
  )


#ponderador

persona_miembro <- menores_5_años%>%
  left_join( ., base_usuario, by ='ID')%>%
  select(cinco_años_asistencia_no_excluyente,cinco_años_comunitario, cinco_años_privado,cinco_años_estatal, cinco_años_total, WHOG) 


#porcentajes
porcentajes <-  persona_miembro %>% 
  summarise(cinco_años_asistencia_no_excluyente = sum(WHOG[cinco_años_asistencia_no_excluyente >= 1]),
            cinco_años_total = sum(WHOG), 
            porcentaje = cinco_años_asistencia_no_excluyente/cinco_años_total * 100 )


