library(tidyverse)



base_miembro <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_miembros.txt",
                           header =  TRUE,
                           sep ="|",
)

base_persona <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_base.txt",
                           header =  TRUE,
                           sep ="|",
)


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