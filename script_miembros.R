library(tidyverse)



base_miembro <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_miembros.txt",
                           header =  TRUE,
                           sep ="|",
)

base_persona <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_base.txt",
                           header =  TRUE,
                           sep ="|",
)


#Hogares con presencia de ni�xs de hasta 2 a�os  que asisten al jard�n y que adem�s reciben asistencia en cuidados (gesti�n estatal, privada o comunitaria)

hogares_dos_a�os_jardin <- base_miembro %>%
  group_by (ID) %>%
  summarise (
    n = length (N_MIEMBRO [BHCH09 == 1 & BHCH11 >= 2 & BHCH08 == 1])
    )


 # merge ambas tablas    
persona_miembro <- merge(
  x = base_miembro,
  y = base_persona,
  by = "ID"
)

 #ponderar n 
  
hogares_ponderado_2_a�os <- hogares_dos_a�os_jardin %>%
  select(ID, n, WHOG) %>% 
  mutate(n = ifelse(n == 0, "Sin presencia de ninxs", "Con presencia de ninxs que asisten y reciben asistencia estatal")) %>% 
  group_by(n) %>% 
  summarise(
    hogares = sum(WHOG)
    ) 


#Hogares con presencia de ni�xs de hasta 3 a�os  que asisten al jard�n y que adem�s reciben asistencia en cuidados (gesti�n estatal, privada o comunitaria)

 


# Hogares con presencia de ni�xs de hasta 5 a�os  que asisten al jard�n y que adem�s reciben asistencia en cuidados (gesti�n estatal, privada o comunitaria)

1 o m�s ni�os que vayan a estblecimiento y sea gesti�n estatal, privada o comunitaria

id hogar agrupar x id (group by) y contar la cantidad de personas

cuenta cantidad de filas de n miembros {condici�n edad mayor a dos, demadnante de cuidados(1 demandante de cuidado-ver), y tipo de establecimiento} tabla ID hogar y variable N unidad an�lisis hogar. 


JOIN con base  usuario por id y hacer ponderar n y hacer porcentaje y absoluto. 