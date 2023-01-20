library(tidyverse)



base_miembro <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_miembros.txt",
                           header =  TRUE,
                           sep ="|",
)

base_persona <- read.table("C:/Users/ailen/Documents/GitHub/ENUT/enut2021_base.txt",
                           header =  TRUE,
                           sep ="|",
)


#Hogares con presencia de niñxs de hasta 2 años  que asisten al jardín y que además reciben asistencia en cuidados (gestión estatal, privada o comunitaria)

hogares_dos_años_jardin <- base_miembro %>%
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
  
hogares_ponderado_2_años <- hogares_dos_años_jardin %>%
  select(ID, n, WHOG) %>% 
  mutate(n = ifelse(n == 0, "Sin presencia de ninxs", "Con presencia de ninxs que asisten y reciben asistencia estatal")) %>% 
  group_by(n) %>% 
  summarise(
    hogares = sum(WHOG)
    ) 


#Hogares con presencia de niñxs de hasta 3 años  que asisten al jardín y que además reciben asistencia en cuidados (gestión estatal, privada o comunitaria)

 


# Hogares con presencia de niñxs de hasta 5 años  que asisten al jardín y que además reciben asistencia en cuidados (gestión estatal, privada o comunitaria)

1 o más niños que vayan a estblecimiento y sea gestión estatal, privada o comunitaria

id hogar agrupar x id (group by) y contar la cantidad de personas

cuenta cantidad de filas de n miembros {condición edad mayor a dos, demadnante de cuidados(1 demandante de cuidado-ver), y tipo de establecimiento} tabla ID hogar y variable N unidad análisis hogar. 


JOIN con base  usuario por id y hacer ponderar n y hacer porcentaje y absoluto. 