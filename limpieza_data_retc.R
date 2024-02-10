options(scipen=999)
library(janitor)
library(tidyverse)


#Bases de datos RETC-MUNICIPALES----
retc17<-read.csv2("2017.csv", stringsAsFactors = FALSE)
retc18<-read.csv2("2018.csv", stringsAsFactors = FALSE)  
retc19<-read.csv2("2019.csv", stringsAsFactors = FALSE)
retc20<-read.csv2("2020.csv", stringsAsFactors = FALSE)
retc21<-read.csv2("2021.csv", stringsAsFactors = FALSE)# nuevo 01/2024
retc22<-read.csv2("2022.csv", stringsAsFactors = FALSE)# nuevo 01/2024
# EXPLORACION DE DATOS DIRECTA

#RBIND para unir data frames. Importante el orden de los data frames.
#base_madre2<-rbind.fill(retc19,retc20,retc18,retc17)

names(retc17)
names(retc18)
names(retc19)
names(retc20)
names(retc21)
names(retc22)
#Limpieza de datos, Nombre de las columnas



retc17_uno<-retc17 %>% 
  clean_names()

retc18_uno<-retc18 %>% 
  clean_names() 

retc19_uno<-retc19 %>% 
  clean_names()

retc20_uno<-retc20 %>% 
  clean_names()

retc21_uno<-retc21 %>% 
  clean_names()

retc22_uno<-retc22 %>% 
  clean_names()

# RETC2017------

names(retc17_uno)
glimpse(retc17_uno)
levels(retc17_uno$razon_social)

retc17_uno<-retc17_uno%>% 
  mutate(valorizacion_eliminacion=as_factor(valorizacion_eliminacion),
                  razon_social=as_factor(razon_social),
         tratamiento=as_factor(tratamiento),
         declaracion_estimacion=as_factor(declaracion_estimacion),
         nombre_establecimiento=as_factor(nombre_establecimiento),
         nombre_ler=as_factor(nombre_ler),
         capitulo_ler=as.character(capitulo_ler), #ajustar largo del nombre y usar como factor
         subcapitulo_ler=as.character(subcapitulo_ler),
         region=as_factor(region),
         tipo_o_rol_declarante=as_factor(tipo_o_rol_declarante))#ajustar largo del nombre y usar como factor


write.csv2(retc17_uno,file="retc17_uno.csv", row.names = FALSE)

# RETC2018--------
levels(retc18_uno$rubro_retc)

retc18_uno<-retc18_uno %>% 
  select(-ciiu4)

glimpse(retc18_uno)

retc18_uno<-retc18_uno %>% 
  mutate(valorizacion_eliminacion=as_factor(valorizacion_eliminacion),
         rubro_retc=as_factor(rubro_retc),
         #ciiu4=as_factor(ciiu4),
         tratamiento=as_factor(tratamiento),
         declaracion_estimacion=as_factor(declaracion_estimacion),
         nombre_establecimiento=as_factor(nombre_establecimiento),
         nombre_ler=as_factor(nombre_ler),
         capitulo_ler=as.character(capitulo_ler), #ajustar largo del nombre y usar como factor
         subcapitulo_ler=as.character(subcapitulo_ler),
         region=as_factor(region),
         tipo_o_rol_declarante=as_factor(tipo_o_rol_declarante))


write.csv2(retc18_uno,file="retc18_uno.csv", row.names = FALSE)




# RETC2019--------

glimpse(retc19_uno)

retc19_uno<-retc19_uno %>% 
  mutate(valorizacion_eliminacion=as_factor(valorizacion_eliminacion),
         tratamiento=as_factor(tratamiento),
         declaracion_estimacion=as_factor(declaracion_estimacion),
         nombre_establecimiento=as_factor(nombre_establecimiento),
         nombre_ler=as_factor(nombre_ler),
         capitulo_ler=as.character(capitulo_ler), #ajustar largo del nombre y usar como factor
         subcapitulo_ler=as.character(subcapitulo_ler),
         region=as_factor(region),
         tipo_o_rol_declarante=as_factor(tipo_o_rol_declarante))


retc19_uno<-retc19_uno %>% 
  select(-c(numero_capitulo, numero_subcapitulo, numero_ler)) %>% 
  relocate(razon_social, .before=nombre_establecimiento)


write.csv2(retc19_uno,file="retc19_uno.csv", row.names = FALSE)

glimpse(retc20_uno)

#RETC2020--------------------------------
#select. rename, mutate

retc20_uno<-as_tibble(retc20_uno)

retc20_cuatro<-retc20_uno %>% 
 select(-c(numero_capitulo_ler, numero_subcapitulo_ler, numero_ler)) %>% 
  rename(coordenada_norte=utmn,
         coordenada_este=utme,
         nombre_ler=ler,
         id_establecimiento_vu=codigo_vu_establecimiento,
         declaracion_estimacion=estimacion_declaracion,
         region_destinatario  = region_trazabilidad,
         comuna_destinatario  = comuna_trazabilidad,
         razon_social_destinatario= razon_social_trazabilidad,
         nombre_de_establecimiento_destinatario = nombre_trazabilidad,
         id_establecimiento_vu_destinatario=retc_code_trazabilidad,
         tipo_o_rol_declarante=rol_declarante) %>% 
  mutate(capitulo_ler= str_replace(capitulo_ler,"\\d\\d\\s\\|\\s",""), 
         subcapitulo_ler= str_replace(subcapitulo_ler,"\\d\\d\\s\\|\\s",""),
        nombre_ler=str_replace(nombre_ler, "\\d\\d\\s\\d\\d\\s\\d\\d\\s\\|\\s",""))

# Reorden de columnas
retc20_cinco <- retc20_cuatro %>% 
  relocate("ano", .before=id_establecimiento_vu_destinatario)

retc20_cinco <- retc20_cinco %>%          
  relocate("declaracion_estimacion", .before=ano )

retc20_cinco <- retc20_cinco %>%          
   relocate("razon_social", .after=tipo_o_rol_declarante)



retc20_cinco <- retc20_cinco %>%          
  relocate("nombre_establecimiento", .after=razon_social)


retc20_cinco <- retc20_cinco %>%          
  relocate("ciiu6", .before=ciiu4)

retc20_cinco <- retc20_cinco %>%          
  relocate("ciiu4", .after=id_establecimiento_vu )

retc20_cinco <- retc20_cinco %>%          
  relocate("region", .after=ciiu4 )

retc20_cinco <- retc20_cinco %>%          
    relocate("comuna", .after=region )


retc20_cinco <- retc20_cinco %>%          
  relocate("coordenada_este", .after=huso )


retc20_cinco <- retc20_cinco %>%          
  relocate("tratamiento", .after=nombre_ler)

retc20_cinco <- retc20_cinco %>%          
  relocate("cantidad_toneladas", .before=declaracion_estimacion)

glimpse(retc20_cinco)


write.csv2(retc20_cinco,file="retc20_uno.csv", row.names = FALSE)

rm(retc17,retc18,retc19,retc20)

#RETC2022--------------------------------
#select. rename, mutate

glimpse(retc22_uno)

retc22_uno<-as_tibble(retc22_uno)

retc22_cuatro<-retc22_uno %>% 
  select(-c(rut_razon_social,ciiu6,ler_codigo, entrada_salida,id_vu_trazabilidad, x,id_tratamiento_nivel_3,
            id_tratamiento_nivel_1,tratamiento_nivel_3,tratamiento_nivel_1)) %>% 
  rename(coordenada_norte=latitud,
         coordenada_este=longitud,
         tipo_o_rol_declarante=rol_establecimiento,
         id_establecimiento_vu=id_vu) %>% 
  mutate(capitulo_ler= str_replace(capitulo_ler,"\\d\\d\\s\\|\\s",""), 
         subcapitulo_ler= str_replace(subcapitulo_ler,"\\d\\d\\s\\|\\s",""),
         nombre_ler=str_replace(nombre_ler, "\\d\\d\\s\\d\\d\\s\\d\\d\\s\\|\\s",""))

glimpse(retc22_cinco)
glimpse(retc18_uno)
# Reorden de columnas
retc22_cinco <- retc22_cuatro %>% 
  relocate("ano", .before=capitulo_ler)

retc22_seis <- retc22_cinco %>% 
  relocate("id_establecimiento", .before=coordenada_norte)

retc22_cinco <- retc202_cinco %>%          
  relocate("coordenada_norte", .before=id_comuna )

retc22_cinco <- retc20_cinco %>%          
  relocate("razon_social", .after=tipo_o_rol_declarante)



retc20_cinco <- retc20_cinco %>%          
  relocate("nombre_establecimiento", .after=razon_social)


retc20_cinco <- retc20_cinco %>%          
  relocate("ciiu6", .before=ciiu4)

retc20_cinco <- retc20_cinco %>%          
  relocate("ciiu4", .after=id_establecimiento_vu )

retc20_cinco <- retc20_cinco %>%          
  relocate("region", .after=ciiu4 )

retc20_cinco <- retc20_cinco %>%          
  relocate("comuna", .after=region )


retc20_cinco <- retc20_cinco %>%          
  relocate("coordenada_este", .after=huso )


retc20_cinco <- retc20_cinco %>%          
  relocate("tratamiento", .after=nombre_ler)

retc20_cinco <- retc20_cinco %>%          
  relocate("cantidad_toneladas", .before=declaracion_estimacion)

glimpse(retc20_cinco)


write.csv2(retc20_cinco,file="retc20_uno.csv", row.names = FALSE)

rm(retc17,retc18,retc19,retc20)
#BASE DE DATOS RETC INDUSTRIALES

#INDUSTRIALES RETC 2017-----

industriales17<-read.csv2("industriales_2017_v3.csv", stringsAsFactors = FALSE) #w15/03/2021
industriales18<-read.csv2("industriales_2018_v3.csv", stringsAsFactors = FALSE)#15/03/2021
industriales19<-read.csv2("industriales_2019_v2.csv", stringsAsFactors = FALSE)#15/03/2021
industriales20<-read.csv2("industriales_2020_v5.csv", stringsAsFactors = FALSE)#25/04/2022
industriales21 <-read.csv2("generadores21.csv", stringsAsFactors = FALSE)#12/2023
industriales22 <-read.csv2("generadores22.csv", stringsAsFactors = FALSE)#12/2023



#Limpieza INDUSTRIALES año 2017

generadores17<- industriales17 %>% 
  clean_names()


distinct(generadores17, tratamiento) %>% 
  view()

write.csv2(generadores17,file="generadores17.csv", row.names = FALSE)


#INDUSTRIALES RETC2018----


generadores18<- industriales18 %>% 
  clean_names() 

generadores18<-generadores18%>% 
  rename(tipo_o_rol_declarante = i_tipo_o_rol_declarante,
         razon_social = raz_a3n_social,
         region = regi_a3n ,
         capitulo_ler = cap_a_tulo_ler,
         subcapitulo_ler = subcap_a_tulo_ler, 
         valorizacion_eliminacion = valorizaci_a3n_eliminaci_a3n,
         ano = aa_o,
         razon_social_destinatarios = raz_a3n_social_destinatario ,
         region_destinatario = regi_a3n_destinatario,
         id_establecimiento_destinatario = id_establecimiento_vu_destinatario) %>% 
  relocate("razon_social", .after=tipo_o_rol_declarante)  
 
write.csv2(generadores18,file="generadores18.csv", row.names = FALSE)

distinct(generadores18, comuna_destinatario)
summary(generadores18)

generadores18$comuna_destinatario<-as_factor(generadores18$comuna_destinatario)
generadores18$region_destinatario<-as_factor(generadores18$region_destinatario)


#INDUSTRIALES RETC2019----

generadores19<- industriales19 %>% 
  clean_names()

generadores19<-generadores19 %>% 
  select(-c(numero_capitulo, numero_subcapitulo, numero_ler)) 

generadores19<-generadores19%>% 
  rename(id_establecimiento_destinatario = id_establecimiento_vu_destinatario) %>% 
  relocate("razon_social", .after=tipo_o_rol_declarante)

write.csv2(generadores19,file="generadores19.csv", row.names = FALSE)

#INDUSTRIALES RETC2020----

generadores20<- industriales20 %>% 
  clean_names()%>% 
  select(-c(numero_ler, numero_subcapitulo_ler, numero_capitulo_ler))

  generadores20<-generadores20 %>% 
  rename( coordenada_norte = utmn,
          coordenada_este = utme,
          nombre_ler = ler,
          id_establecimiento_destinatario=retc_code_trazabilidad,
          nombre_establecimiento_destinatario = nombre_trazabilidad,
           comuna_destino= comuna_trazabilidad ,
          region_destino = region_trazabilidad,
          id_establecimiento_vu= codigo_vu_establecimiento,
          razon_social_destinatario = razon_social_trazabilidad) 

generadores20<-generadores20%>% 
  relocate( "rol_declarante", .before=ano) %>% 
  relocate("razon_social", .before=ano) %>% 
  relocate("nombre_establecimiento", .before=ano) %>% 
  relocate("id_establecimiento_vu", .before=ano) %>% 
  relocate("ano", .after=cantidad_toneladas) %>% 
  relocate("ciiu6", .before=ciiu4) %>% 
  relocate("region", .after=ciiu4) %>% 
  relocate("comuna", .after = region) %>% 
  relocate("coordenada_este", .before= coordenada_norte) %>% 
  relocate("capitulo_ler", .after= coordenada_norte) %>% 
  relocate("subcapitulo_ler", .after= capitulo_ler) %>% 
  relocate("nombre_ler", .after= subcapitulo_ler) %>% 
  relocate("tratamiento", .after=nombre_ler) %>% 
  relocate("outlier", .after=tratamiento) 

generadores20<-generadores20 %>% 
  mutate(capitulo_ler= str_replace(capitulo_ler,"\\d\\d\\s\\|\\s",""), 
         subcapitulo_ler= str_replace(subcapitulo_ler,"\\d\\d\\s\\|\\s",""),
         nombre_ler=str_replace(nombre_ler, "\\d\\d\\s\\d\\d\\s\\d\\d\\s\\|\\s",""))


generadores20$capitulo_ler<-as.factor(generadores20$capitulo_ler)
generadores20$nombre_ler<-as.factor(generadores20$nombre_ler)
generadores20$subcapitulo_ler<-as.factor(generadores20$subcapitulo_ler)


glimpse(generadores20)
distinct(generadores20,nombre_ler)
  
write.csv2(generadores20,file="generadores20.csv", row.names = FALSE) 


