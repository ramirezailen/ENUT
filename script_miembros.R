library(tidyverse)



base_miembros <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_miembros.txt",
                           header =  TRUE,
                           sep ="|",
)

base_usuario <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_base.txt",
                           header =  TRUE,
                           sep ="|",
)

#Hogares con presencia de ni?xs de hasta 2  a?os que reciben asistencia en cuidados  ( sea mediante gestion estatal,privada o comunitaria )

menores_2_a?os <-base_miembros %>%
  filter( BHCH03 <= 2 ) %>% #edad
  group_by (ID) %>%
  summarise (
    dos_a?os_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    dos_a?os_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &? BHDC01_04 != 1 & BHDC01_06 != 1]),
    dos_a?os_privado = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 != 1 & BHDC01_06 == 1]),
    dos_a?os_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 == 1 & BHDC01_06 != 1]),
    dos_a?os_total = length(N_MIEMBRO)
  )



#ponderador

persona_miembro <- menores_2_a?os %>%
  left_join( ., base_usuario, by ='ID')%>%
  select(dos_a?os_no_excluyente,dos_a?os_comunitario, dos_a?os_privado,dos_a?os_estatal, dos_a?os_total, WHOG)




#porcentajes


porcentaje_2_a?os <- persona_miembro %>%
  summarise(dos_a?os_no_excluyente = sum(WHOG[dos_a?os_no_excluyente >= 1]),
            dos_a?os_comunitario = sum(WHOG[dos_a?os_comunitario >= 1]),
            dos_a?os_privado = sum(WHOG[dos_a?os_privado >= 1]),
            dos_a?os_estatal = sum(WHOG[dos_a?os_estatal >= 1]),
            dos_a?os_total = sum(WHOG),

            
            porcentaje_no_exc = dos_a?os_no_excluyente/dos_a?os_total * 100,
            porcentaje_comu = dos_a?os_comunitario/dos_a?os_total * 100,
            porcentaje_priv = dos_a?os_privado/dos_a?os_total * 100,
            porcentaje_esta = dos_a?os_estatal/dos_a?os_total * 100,
            porcentaje_total = dos_a?os_total/dos_a?os_total * 100
            
            )



#-------------------------------

#Hogares con presencia de ni?xs 3  a?os que reciben asistencia en cuidados  ( sea mediante gestion estatal,privada o comunitaria )

ni?os_3_a?os <- base_miembros %>%
  filter( BHCH03 == 3 ) %>% #edad
  group_by (ID) %>%
  summarise (
    tres_a?os_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    tres_a?os_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &? BHDC01_04 != 1 & BHDC01_06 != 1]),
    tres_a?os_privado = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 != 1 & BHDC01_06 == 1]),
    tres_a?os_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 == 1 & BHDC01_06 != 1]),
    tres_a?os_total = length(N_MIEMBRO)
  )



#ponderador

pondera_3 <- ni?os_3_a?os %>%
  left_join( ., base_usuario, by ='ID')%>%
  select( tres_a?os_no_excluyente, tres_a?os_comunitario,  tres_a?os_privado, tres_a?os_estatal,  tres_a?os_total, WHOG)




#porcentajes
porcentajes_3 <- pondera_3 %>%
  summarise(tres_a?os_no_excluyente = sum(WHOG[tres_a?os_no_excluyente >= 1]),
            tres_a?os_comunitario = sum(WHOG[tres_a?os_comunitario >= 1]),
            tres_a?os_privado = sum(WHOG[tres_a?os_privado >= 1]),
            tres_a?os_estatal = sum(WHOG[tres_a?os_estatal >= 1]),
            tres_a?os_total = sum(WHOG), 
            
            porcentaje = tres_a?os_no_excluyente/tres_a?os_total * 100,
            porcentaje2 = tres_a?os_comunitario/tres_a?os_total * 100,
            porcentaje3 = tres_a?os_privado/tres_a?os_total * 100,
            porcentaje4 = tres_a?os_estatal/tres_a?os_total * 100,
            porcentaje5 = tres_a?os_total/tres_a?os_total * 100
            )


#-------------------------------
#Hogares con presencia de ni?xs de 4 a 5 a?os que reciben asistencia en cuidados ( sea mediante gestion estatal,privada o comunitaria )


ni?os_4_5_a?os <- base_miembros %>%
  filter( BHCH03 == 4&5 ) %>% #edad
  group_by (ID) %>%
  summarise (
    cuatrocinco_a?os_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    cuatrocinco_a?os_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &? BHDC01_04 != 1 & BHDC01_06 != 1]),
    cuatrocinco_a?os_privado = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 != 1 & BHDC01_06 == 1]),
    cuatrocinco_a?os_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 == 1 & BHDC01_06 != 1]),
    cuatrocinco_a?os_total = length(N_MIEMBRO)
  )



#ponderador

pondera_4_5 <- ni?os_4_5_a?os %>%
  left_join( ., base_usuario, by ='ID')%>%
  select( cuatrocinco_a?os_no_excluyente, cuatrocinco_a?os_comunitario,  cuatrocinco_a?os_privado, cuatrocinco_a?os_estatal,  cuatrocinco_a?os_total, WHOG)




#porcentajes
porcentajes_4_5 <- pondera_4_5 %>%
  summarise(cuatrocinco_a?os_no_excluyente = sum(WHOG[cuatrocinco_a?os_no_excluyente >= 1]),
            cuatrocinco_a?os_comunitario = sum(WHOG[cuatrocinco_a?os_comunitario >= 1]),
            cuatrocinco_a?os_privado = sum(WHOG[cuatrocinco_a?os_privado >= 1]),
            cuatrocinco_a?os_estatal = sum(WHOG[cuatrocinco_a?os_estatal >= 1]),
            cuatrocinco_a?os_total = sum(WHOG),
            
            porcentaje = cuatrocinco_a?os_no_excluyente/cuatrocinco_a?os_total * 100,
            porcentaje2 = cuatrocinco_a?os_comunitario/cuatrocinco_a?os_total * 100,
            porcentaje3 = cuatrocinco_a?os_privado/cuatrocinco_a?os_total * 100,
            porcentaje4 = cuatrocinco_a?os_estatal/cuatrocinco_a?os_total * 100,
            porcentaje5 = cuatrocinco_a?os_total/cuatrocincos_a?os_total * 100
            )


#---------------------------------------------


#Porcentaje de hogares que contrata servicios de cuidado para ni?xs


hogares_cuidados <- base_miembros %>% 
  #filter(BHDC01_06 == 1) %>% #BHDC01_06 tambi?n se puede considerar BHDC02_01
  group_by (BHDC01_06) %>%
  summarise (
    hogares_cuidados_contrata = sum(N_MIEMBRO[BHDC01_06== 1]),
    hogares_cuidados_total = sum(N_MIEMBRO[BHDC01_06])
  )


#chequear si est? bien para seguir haciendo el %
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

pondera_4_5 <- ni?os_4_5_a?os %>%
  left_join( ., base_usuario, by ='ID')%>%
  select( cuatrocinco_a?os_no_excluyente, cuatrocinco_a?os_comunitario,  cuatrocinco_a?os_privado, cuatrocinco_a?os_estatal,  cuatrocinco_a?os_total, WHOG)




#porcentajes
porcentajes_4_5 <- pondera_4_5 %>%
  summarise(cuatrocinco_a?os_no_excluyente = sum(WHOG[cuatrocinco_a?os_no_excluyente >= 1]),
            cuatrocinco_a?os_comunitario = sum(WHOG[cuatrocinco_a?os_comunitario >= 1]),
            cuatrocinco_a?os_privado = sum(WHOG[cuatrocinco_a?os_privado >= 1]),
            cuatrocinco_a?os_estatal = sum(WHOG[cuatrocinco_a?os_estatal >= 1]),
            cuatrocinco_a?os_total = sum(WHOG),
            
            porcentaje = cuatrocinco_a?os_no_excluyente/cuatrocinco_a?os_total * 100,
            porcentaje2 = cuatrocinco_a?os_comunitario/cuatrocinco_a?os_total * 100,
            porcentaje3 = cuatrocinco_a?os_privado/cuatrocinco_a?os_total * 100,
            porcentaje4 = cuatrocinco_a?os_estatal/cuatrocinco_a?os_total * 100,
            porcentaje5 = cuatrocinco_a?os_total/cuatrocincos_a?os_total * 100
  )





#-------------------------------
#Este no va
#Hogares con presencia de ni?xs de hasta 5 a?os  que asisten al jard?n y que adem?s reciben asistencia en cuidados (gesti?n estatal, privada o comunitaria)

menores_5_a?os <-base_miembros %>%?
  filter( BHCH03 >= 5 &? BHCH08 == 1) %>%?
  group_by (ID) %>%
  summarise (
    cinco_a?os_asistencia_no_excluyente = length(N_MIEMBRO[BHDC01_03 == 1 | BHDC01_04 == 1 | BHDC01_06 == 1]),
    cinco_a?os_comunitario = length(N_MIEMBRO[BHDC01_03 == 1 &? BHDC01_04 != 1 & BHDC01_06 != 1]),
    cinco_a?os_privado = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 != 1 & BHDC01_06 == 1]),
    cinco_a?os_estatal = length(N_MIEMBRO[BHDC01_03 != 1 &? BHDC01_04 == 1 & BHDC01_06 != 1]),
    cinco_a?os_total = length(N_MIEMBRO)
  )


#ponderador

persona_miembro <- menores_5_a?os%>%
  left_join( ., base_usuario, by ='ID')%>%
  select(cinco_a?os_asistencia_no_excluyente,cinco_a?os_comunitario, cinco_a?os_privado,cinco_a?os_estatal, cinco_a?os_total, WHOG)?


#porcentajes
porcentajes <-? persona_miembro %>%?
  summarise(cinco_a?os_asistencia_no_excluyente = sum(WHOG[cinco_a?os_asistencia_no_excluyente >= 1]),
            cinco_a?os_total = sum(WHOG),?
            porcentaje = cinco_a?os_asistencia_no_excluyente/cinco_a?os_total * 100 )


