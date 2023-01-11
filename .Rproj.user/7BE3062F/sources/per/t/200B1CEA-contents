library(tidyverse)



base_persona <- read.table("enut2021_base.txt",
                           header =  TRUE,
                           sep ="|",
)


str(base_persona)
ncol(base_persona)
dim(base_persona)
names(base_persona)

#Hogares monomarentales que usan sistema de cuidado Estatal


<- base_persona %>% 
             filter(TIPO_HOGAR_DCTOTAL == 1 

filter(base_persona, TIPO_HOGAR_NUCLEAR%in% c("4"))




tipo_hogar_demanda <- base_persona %>%
                      filter(TIPO_HOGAR_NUCLEAR == 4) %>%
                      select(TIPO_HOGAR_NUCLEAR, BHDC_SEL,WHOG) %>% 
                      mutate(BHDC_SEL = ifelse(BHDC_SEL == 0, "Sin demanda de cuidados", "Con demanda de cuidados")) %>% 
                      group_by(BHDC_SEL) %>% 
                      summarise(
                        hogares = sum(WHOG)
                      ) #%>% 
                       #spread(., key = BHDC_SEL,value =  hogares) %>% 
           
                     
                  

