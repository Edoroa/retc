options(scipen=999)
library(extrafont)
library(showtext)
library(extrafontdb)
library(pagedown)
library(forecast)
library(ggrepel)
library(viridis)
library(tidyverse)
library(sf)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(RColorBrewer)
library(ggsci)
library(scales)
library(plotly)
library(Cairo)
library(janitor)
library(wesanderson) #names(wes_palettes)
library(hrbrthemes)


#Base de datos consolidada, obtenida del docuento "limpieza_data_retc"


retc<-read.csv2 ("base_retc_v3.csv", stringsAsFactors = TRUE, na.strings=c("","NA"))
as_tibble(retc)
summary(retc)
glimpse(retc)



font_add_google(family = "League Spartan", "league Spartan")
font_add_google(family="Lato", "Lato")
showtext_auto()


#DATA WRANGLING----
#Limpieza bases de datos de retc
#1.-Seleccion y ajuste de variables
retc %>% 
    group_by(razon_social,nombre_establecimiento, tratamiento,ano) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
 filter(ano== "2019") %>% 
  view()
  

summary(retc)

retc1 <- retc %>% 
    relocate("ano", .before=tipo_o_rol_declarante ) %>%
     rename(tipo = tipo_o_rol_declarante,
         id_esta_vu = id_establecimiento_vu,#id_establecimiento_vu
         id_esta_vu_desti = id_establecimiento_vu_destinatario,
         establecimiento_destinatario = nombre_de_establecimiento_destinatario
         ) %>% 
  mutate(ano=as_factor(ano)) %>% 
  mutate(ciiu4 = str_replace(ciiu4,"0",""),
         tratamiento=str_replace(tratamiento,"null","No definido"),
         rubro_retc = str_replace(rubro_retc,"Gestor de residuos", "Gestores de residuos"),
         razon_social = str_replace(razon_social,"I\\s", ""),
         razon_social = str_replace(razon_social,"\\ILUSTRE\\s", ""),
         nombre_establecimiento = str_replace(nombre_establecimiento,"I\\s", ""),
         nombre_establecimiento = str_replace(nombre_establecimiento,"\\ILUSTRE\\s", ""),
         nombre_establecimiento = str_replace(nombre_establecimiento,"EDIFICIO CONSISTORIAL MUNICIPALIDAD CURACAUTIN",
                                              "MUNICIPALIDAD DE CURACAUTÍN"),
         nombre_establecimiento = str_replace(nombre_establecimiento,
                                              "I. MUNICIPALIDAD DE LA REINA","MUNICIPALIDAD DE LA REINA"),
         nombre_establecimiento = str_replace(nombre_establecimiento,
                                              "I. Municipalidad de Lolol","MUNICIPALIDAD DE LOLOL")) %>% 
  mutate(cantidad_toneladas = as.numeric(cantidad_toneladas),
         razon_social= as_factor(razon_social))

retc1 %>% 
  filter(tipo=="Municipio") %>% 
  group_by(tipo,nombre_establecimiento, razon_social) %>% 
  summarise( total=sum(cantidad_toneladas)) %>% 
  view()

levels(retc1$tipo)

retc1 %>% 
  group_by(tipo, region, razon_social, nombre_establecimiento) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
  ungroup() %>% 
  mutate(pcje= sum(total)/total) %>% 
  view()


retc2<-retc1 %>% 
  select(-c(huso, coordenada_este, coordenada_norte,  id_esta_vu_desti,
            id_esta_vu,provincia,region_destinatario,
              razon_social_destinatario,ciiu4,ciiu6))%>% 
  mutate(tipo = str_replace(tipo,"Generador$","Generador Industrial"),
         tipo = str_replace(tipo, "Generador de Residuos Industriales no Peligrosos",
                            "Generador Industrial"),
         valorizacion_eliminacion = str_replace(valorizacion_eliminacion,"EliminaciÃ³n",
                                                "Eliminación"),
         valorizacion_eliminacion = str_replace(valorizacion_eliminacion,"ValorizaciÃ³n",
                                                "Valorización" ),
         tratamiento = str_replace (tratamiento, "Residuos Voluminosos", 
                                    "Residuos voluminosos"),
         tratamiento = str_trim (tratamiento, "right"),
         region = str_trim (region, "right"),
         region = str_replace (region, "La Araucanía", "Araucanía"),
         region = str_replace (region,"Metropolitana$", "Metropolitana de Santiago" ),
         region = str_replace (region, "Libertador Gral. Bernardo O'higgins",
                               "Libertador Gral. Bernardo O'Higgins"))%>% 
  mutate(tipo = as_factor(tipo),
         rubro_retc = as_factor(rubro_retc),
         valorizacion_eliminacion = as_factor(valorizacion_eliminacion),
         razon_social = as_factor(razon_social),
         nombre_establecimiento = as_factor(nombre_establecimiento),
         region = as_factor(region),
         tratamiento = as.character(tratamiento),
         ) %>% 
   mutate(nombre_ler= str_replace(nombre_ler,"PlÃ¡stico","Plástico"),
         nombre_ler = str_replace(nombre_ler, "Plástico PVC (policloruro de viilo)", "Plástico PVC (policloruro de vinilo)"),
         nombre_ler = str_replace(nombre_ler,"Residuos de plÃ¡stico","Residuos de plástico"),
         nombre_ler = str_replace(nombre_ler, "Envases de plÃ¡stico","Envases de plástico"),
         nombre_ler = str_replace(nombre_ler, "Cenizas volantes de carbÃ³n", "Cenizas volantes de carbón"),
         nombre_ler = str_replace(nombre_ler, "Materiales inadecuados para el consumo o la elaboraciÃ³n",
                                  "Materiales inadecuados para el consumo o la elaboración"),
         nombre_ler = str_replace(nombre_ler, "Residuos de la extracciÃ³n de minerales metÃ¡licos",
                                  "Residuos de la extracción de minerales metálicos"),
         nombre_ler = str_replace( nombre_ler, "Residuos de procesos quÃmicos orgÃ¡nicos",
                                   "Residuos de procesos químicos orgánicos"),
         nombre_ler = str_replace(nombre_ler, "Materiales inadecuados para el consumo o la elaboraciÃ³n",
                                  "Materiales inadecuados para el consumo o la elaboración"),
         nombre_ler = str_replace(nombre_ler, "Papel y cartÃ³n", "Papel y cartón"),
         nombre_ler = str_replace(nombre_ler, "NeumÃ¡ticos fuera de uso", "Neumáticos fuera de uso"),
         nombre_ler = str_replace(nombre_ler, "Cascarilla de laminaciÃ³n", "Cascarilla de laminación"),
         nombre_ler = str_replace(nombre_ler, "Residuos procedentes de la clasificaciÃ³n de papel y cartÃ³n destinados al reciclado",
                                  "Residuos procedentes de la clasificación de papel y cartón destinados al reciclado"),
         nombre_ler = str_replace(nombre_ler, "Envases de papel y cartÃ³n", "Envases de papel y cartón"),
         nombre_ler = str_replace(nombre_ler, "Residuos cÃ¡lcicos de reacciÃ³n, en forma sÃ³lida, procedentes de la desulfuraciÃ³n de gases de combustiÃ³n",
                                  "Residuos cálcicos de reacción, en forma sólida, procedentes de la desulfuración de gases de combustión"),
         nombre_ler = str_replace(nombre_ler, "SerrÃn, virutas, recortes, madera, tableros de partÃculas y chapas distintos de los mencionados en el cÃ³digo 03 01 04",
                                  "Serrín, virutas, recortes, madera, tableros de partículas y chapas distintos de los mencionados en el código 03 01 04"),
         nombre_ler = str_replace(nombre_ler, "Absorbentes, materiales de filtraciÃ³n, trapos de limpieza y ropas protectoras distintos de los especificados en el cÃ³digo 15 02 02", 
         "Absorbentes, materiales de filtración, trapos de limpieza y ropas protectoras distintos de los especificados en el código 15 02 02"),
         nombre_ler = str_replace(nombre_ler, "BaterÃas y acumuladores distintos de los especificados en el cÃ³digo 20 01 33",
                                  "Baterías y acumuladores distintos de los especificados en el código 20 01 33"))

retc2$tratamiento[which(is.na(retc2$tratamiento))]<- "No identificado"



#Cruces con datos reporte del medio ambiente[REMATAR]-----

retc2 %>% 
  group_by(tipo ) %>% 
  mutate(tratamiento=as_factor(tratamiento)) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
  mutate(pcje = total/sum(total))



#cambiamos los NA's de la COL "Tipo" por "Municipio"
#Se idenfitico un total de 78 datos sin registro en la columna "tipo", mirando en detalle
#los datos son todos correspondientes a Municipalidades, estos identificados por medio dela columna "Razon social"
retc2$tipo[which(is.na(retc2$tipo))]<- "Municipio"

summary(retc2)
#BASE DE DATOS PARA UTILIZAR. 16 columnas de información. LA OFICIAL SIGUE siendo
#write.csv2(retc2, file ="retc_madre_26092022.csv", row.names = FALSE)


# BD INCINERACION-----
incineracion<-retc2 %>% 
  filter(tratamiento %in% c("Co-procesamiento", 
                            "Co-incineración",
                            "Incineración con recuperación de energía",
                            "Incineración sin recuperación de energía"))

summary(incineracion)
# "Relleno sanitario"))

incineracion %>% 
  filter(tipo == "Generador Industrial") %>% 
  group_by(nombre_establecimiento,razon_social,cantidad_toneladas)
  summarise(total=sum(cantidad_toneladas))



sum(retc2$cantidad_toneladas)
sum(incineracion$cantidad_toneladas)

#dato interesante #1-----
#los registros muestran la reducción constante en el tiempo 
#¿mejor gestión? realizar análisis a los datos para identificar patrones
#2da. parte ;)

incineracion %>% 
  group_by(ano) %>% 
  summarise(n()) %>% 
  view()

#Dato Interesante #2-----
# de acá sacar el top10 de gestión de residuos a nivel país: Co-procesamiento n°10.

tratamiento<-retc2 %>% 
  group_by(tratamiento) %>% 
  summarize(Total= sum(cantidad_toneladas)) %>% 
  arrange(-Total) %>% 
  mutate(pcje= paste0(round(Total/sum(Total)*100),"%")) %>% 
  arrange(-Total) %>% 
  top_n(10)


#Recordar a que corresponden los tratamientos no identificados y no definidos!!!!
retc2 %>% 
  group_by(tratamiento) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
  view()

sum(tratamiento$Total)

tratamiento
#Ajustamos columna tratamiento, NA's. Por "No identificados"  
tratamiento$tratamiento[which(is.na(tratamiento$tratamiento))]<- "No identificado"
  

  # generación por TIPO TIPO DE FUENTE----
fuentes<- incineracion %>% 
    group_by(tipo,ano) %>% 
    summarise(total=sum(cantidad_toneladas))
  
sum(fuentes$total)
  fuentes

#TABLA Reordenamos para generar tabla de inineración por año y tipo de fuente-----
fuentes2<-spread(fuentes,tipo, total)


write.csv2(fuentes2,file="082022_cantidad_tipo_ano.csv", row.names = FALSE) 

  fuentes
  
  #Municipio              46366.
  # Generador Industrial 2449656.
  
  
sum(fuentes$total)
#DATOS INCINERACION-----

  #INCINERACION POR TIPO y AÑO
#Tipos de incineración con sus cantidades anuales y % de participación anual
  #grafíca de lienas con tendencias
burn_tipo<-incineracion%>% 
  mutate(tratamiento = as_factor(tratamiento))%>% 
  filter(tratamiento %in% c("Co-procesamiento", 
                         "Co-incineración",
                         "Incineración con recuperación de energía",
                         "Incineración sin recuperación de energía")) %>% 
  group_by(tratamiento) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
  #mutate(sumcum=cumsum(total)) %>% 
  mutate(pct_acum= prop.table(total) * 100) %>% 
  mutate(tratamiento = fct_reorder(tratamiento, desc(total)))


burn_tipo
sum(incineracion$total)
write.csv2(burn_tipo,file="totales_tratamientos.csv", row.names = FALSE)        


levels(burn_tipo$tratamiento)
#PIE- tratamientos incineracion-----
ggplot(data=burn_tipo, aes(x="",y= total, fill=tratamiento))+
  geom_bar(stat='identity', color="black")+
  coord_polar(theta="y")+
  scale_y_continuous(
    trans = "sqrt")+
  scale_fill_manual(
    values=c("#a32020","#e0301e", "#eb8c00","#dc6900"),
    label = c("<strong><span style='color:#a32020'>Co-procesamiento</span>", 
              "<strong><span style='color:#e0301e'>Incineración con recuperación de energía</span>",
              "<strong><span style='color:#eb8c00'>Co-incineración</span> ",
              "<strong><span style='color:#dc6900'>Incineración sin recuperación de energía</span>"))+
 theme(
    text= element_text(family="Lato", size=30),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text= element_blank(),
    axis.ticks= element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 60,color = "#006600", family = "League Spartan", 
                              lineheight = 0.3, face ="bold" ),
    plot.background = element_rect(fill = "#e06666", color = "#e06666",linewidth = 1 ),
    plot.subtitle = element_text(size = 40,lineheight = 0.3, color = "#333333"),
    plot.caption = element_text(hjust=1, size = 35),
    plot.caption.position = "plot",
                                       # margin=margin(t=10)),
   legend.text = element_markdown(size = 30, lineheight = 0.3, hjust =0),
   legend.title = element_blank(),
   legend.justification = c("right", "top"),#element_text(hjust =0.5),
  #legend.position = c(1,0.8),
   legend.key = element_blank(),
    legend.key.size = unit(10,"pt"),
    legend.key.width = unit(0.5 , "cm"),
    legend.background = element_rect(fill = "#cccccc", color = "#cccccc",linewidth = 1 ))+
  labs(
    #fill="Tratamiento",
    title = "Métodos y participación de la incineración de residuos en Chile",
    caption = "Fuente | Bases de datos RETC, Alianza basura cero Chile. 2024 \n entre los años 2017 - 2020",
    subtitle = "Participación de la incineración de residuos por método entre ",
   # tag = "Grafica 1.-"
  )+
  xlab("Tratamientos")
#guides(color = guide_legend(
 # override.aes=list(shape = 12)))+
  #guides(fill=guide_legend(ncol=2))



ggsave("tratamiento_torta2.png",device = "png", type= "cairo", width = 8, height = 5)

summary(incineracion)


#PIE INCINERACION POR TRATAMIENTO y AÑO-----
#grafico  wrap

wte<-c("Co-procesamiento",
       "Co-incineración",
       "Incineración con recuperación de energía",
       "Incineración sin recuperación de energía")
#Residuos a relleno   # "Relleno sanitario",
#"preparación para reutilización")

    pieburn_tipo<-incineracion%>% 
      mutate(tratamiento = as_factor(tratamiento))%>% 
      filter(tratamiento %in% wte) %>% 
      group_by(ano, tratamiento) %>% 
      summarise(total=sum(cantidad_toneladas)) %>% 
      #mutate(sumcum=cumsum(total)) %>% 
     mutate(pct_acum= round(prop.table(total) * 100,2)) %>% 
     mutate(tratamiento = fct_reorder(tratamiento, desc(total)))
    
#[NUEVO]Tabla % variacion anual por metodo de incineinracion-----    
    pieburn_tipo2<-pieburn_tipo %>% 
      select(ano, pct_acum, tratamiento) %>% 
      spread(ano, pct_acum)

write.csv2(pieburn_tipo2,file="tendencia_Tratamientos.csv", row.names = FALSE)      
      
      
          
#grafica    
    ggplot(data=pieburn_tipo, aes(x="",y= total, fill=tratamiento))+
      geom_bar(stat='identity', color="black", position = position_fill())+
      # geom_text(aes(label = Cnt), position = position_fill(vjust = 0.5))+ #PARA LABEL!!!!!
      coord_polar(theta="y")+
      scale_y_continuous(
        trans = "sqrt")+
      scale_fill_manual(
        values=c("#a32020","#e0301e", "#eb8c00","#dc6900"),
        label = c("Co-procesamiento ", 
                  "Incineración con recuperación de energía",
                  "Co-incineración ",
                  "Incineración sin recuperación de energía"))+
      theme(
        text= element_text(family="Lato", size=30),
        panel.background = element_blank(),
        strip.background = element_rect(fill ="white"),
        strip.text = element_text(size = 30),
        axis.title = element_blank(),
        axis.text= element_blank(),
        axis.ticks= element_blank(),
        axis.line = element_blank(),
        plot.title = element_markdown(size = 60,color = "#006600", family = "League Spartan", 
                                  lineheight = 0.3, face ="bold" ),
        plot.subtitle = element_text(size = 40,lineheight = 0.3),
        plot.caption = element_text(lineheight =0.4, size = 35),
        plot.caption.position = "panel",
        # margin=margin(t=10)),
        legend.text = element_text(size = 30, lineheight = 0.3, hjust =0),
        legend.title = element_blank(), #element_text(hjust =0.5),
        legend.position = "top",
        legend.key = element_rect(colour ="white"),
        legend.key.size = unit(10,"pt"),
        legend.key.width = unit(0.7 , "cm"),
        legend.background = element_blank())+
      labs(
        title = "Total de residuos incinerados por año",
        subtitle = "Incineración de residuos anuales según método datos anuales entre los años 2017 - 2020",
        caption = "Fuente bases de datos RETC | Alianza basura cero Chile")+
      xlab("Tratamientos")+
      facet_grid(~ano)+
      guides(fill=guide_legend(ncol=2))
    
    
  
    ggsave("pie_tratamientos por año.png",device = "png", type= "cairo", width = 8, height = 4)
    
#Dato sobre promedios
pieburn_tipo %>% 
  group_by(tratamiento) %>% 
  summarise(total=sum(total))

  
    
#DATOS INCINERACION en CHILE----

#TIPOS DE MATERIALES

incineracion<-incineracion %>% 
  filter(tratamiento %in% c("Co-procesamiento", 
                           "Co-incineración",
                           "Incineración con recuperación de energía",
                           "Incineración sin recuperación de energía")) %>% 
  mutate(nombre_ler = str_replace(nombre_ler,"PlÃ¡stico","Plásticos"),
         nombre_ler = str_replace(nombre_ler,"Residuos de plÃ¡stico","Residuos de plástico"),
         nombre_ler = str_replace(nombre_ler, "Envases de plÃ¡stico","Envases de plástico"),
         nombre_ler = str_replace(nombre_ler, "Cenizas volantes de carbÃ³n", "Cenizas volantes de carbón"),
         nombre_ler = str_replace(nombre_ler, "Materiales inadecuados para el consumo o la elaboraciÃ³n",
                                  "Materiales inadecuados para el consumo o la elaboración"),
         nombre_ler = str_replace(nombre_ler, "Residuos de la extracciÃ³n de minerales metÃ¡licos",
                                  "Residuos de la extracción de minerales metálicos"),
         nombre_ler = str_replace( nombre_ler, "Residuos de procesos quÃmicos orgÃ¡nicos",
                                   "Residuos de procesos químicos orgánicos"),
         nombre_ler = str_replace(nombre_ler, "Materiales inadecuados para el consumo o la elaboraciÃ³n",
                                  "Materiales inadecuados para el consumo o la elaboración"),
         nombre_ler = str_replace(nombre_ler, "Papel y cartÃ³n", "Papel y cartón"),
         nombre_ler = str_replace(nombre_ler, "NeumÃ¡ticos fuera de uso", "Neumáticos fuera de uso"),
         nombre_ler = str_replace(nombre_ler, "Cascarilla de laminaciÃ³n", "Cascarilla de laminación"),
         nombre_ler = str_replace(nombre_ler, "Residuos procedentes de la clasificaciÃ³n de papel y cartÃ³n destinados al reciclado",
                                  "Residuos procedentes de la clasificación de papel y cartón destinados al reciclado"),
         nombre_ler = str_replace(nombre_ler, "Envases de papel y cartÃ³n", "Envases de papel y cartón"),
         nombre_ler = str_replace(nombre_ler, "Residuos cÃ¡lcicos de reacciÃ³n, en forma sÃ³lida, procedentes de la desulfuraciÃ³n de gases de combustiÃ³n",
                                  "Residuos cálcicos de reacción, en forma sólida, procedentes de la desulfuración de gases de combustión"),
         nombre_ler = str_replace(nombre_ler, "SerrÃn, virutas, recortes, madera, tableros de partÃculas y chapas distintos de los mencionados en el cÃ³digo 03 01 04",
                                  "Serrín, virutas, recortes, madera, tableros de partículas y chapas distintos de los mencionados en el código 03 01 04"),
         nombre_ler = str_replace(nombre_ler, "Absorbentes, materiales de filtraciÃ³n, trapos de limpieza y ropas protectoras distintos de los especificados en el cÃ³digo 15 02 02",
                                  "Absorbentes, materiales de filtración, trapos de limpieza y ropas protectoras distintos de los especificados en el código 15 02 02"),
         nombre_ler = str_replace (nombre_ler, "BaterÃ­as y acumuladores distintos de los especificados en el cÃ³digo 20 01 33",
                                   "Baterías y acumuladores distintos de los especificados en el código 20 01 33"),
         nombre_ler = str_replace (nombre_ler, "Equipos elÃ©ctricos y electrÃ³nicos desechados distintos de los especificados en los cÃ³digos 20 01 21, 20 01 23 y 20 01 35",
                                  "Equipos eléctricos y electrónicos desechados distintos de los especificados en los códigos 20 01 21, 20 01 23 y 20 01 35"),
         nombre_ler = str_replace (nombre_ler, "Lodos procedentes del tratamiento biolÃ³gico de aguas residuales industriales distintos de los especificados en el cÃ³digo 19 08 11",
                                  "Lodos procedentes del tratamiento biológico de aguas residuales industriales distintos de los especificados en el código 19 08 11"),
         nombre_ler = str_replace (nombre_ler, "SerrÃ­n, virutas, recortes, madera, tableros de partÃ­culas y chapas distintos de los mencionados en el cÃ³digo 03 01 04",
                                  "Serrín, virutas, recortes, madera, tableros de partículas y chapas distintos de los mencionados en el código 03 01 04"),
         nombre_ler = str_replace (nombre_ler, "Restos anatÃ³micos y Ã³rganos, incluidos bolsas y bancos de sangre (excepto el cÃ³digo 18 01 03)",
                                  "Restos anatómicos y órganos, incluidos bolsas y bancos de sangre (excepto el código 18 01 03)"),
         nombre_ler = str_replace (nombre_ler,"Residuos lÃ­quidos acuosos distintos de los especificados en el cÃ³digo 16 10 01",
                                   "Residuos líquidos acuosos distintos de los especificados en el código 16 10 01"),
         nombre_ler = str_replace (nombre_ler,"Lodos de fosas sÃ©pticas", "Lodos de fosas sépticas"),
         nombre_ler = str_replace(nombre_ler, "Lodos de lavado, limpieza", "centrifugado y separación"),
         nombre_ler = str_replace(nombre_ler, "centrifugado y separación, pelado, centrifugado y separaciÃ³n",
                                  "centrifugado y separación, pelado, centrifugado y separación"),
         nombre_ler = str_replace(nombre_ler, "Lodos de tratamientos fÃ­sicoquÃ­micos, distintos de los especificados en el cÃ³digo 19 02 05",
                                  "Lodos de tratamientos físicoquímicos, distintos de los especificados en el código 19 02 05"),
         nombre_ler = str_replace(nombre_ler, "Componentes no especificados en otra categorÃ­a",
                                  "Componentes no especificados en otra categoría"),
         nombre_ler = str_replace(nombre_ler, "Limaduras y virutas de metales férreos",
                                  "Limaduras y virutas de metales ferrosos"),
         nombre_ler = str_replace(nombre_ler, "Madera distinta de la especificada en el cÃ³digo 20 01 37",
                                  "Madera distinta de la especificada en el código 19 12 06"),
         nombre_ler = str_replace(nombre_ler, "Medicamentos distintos de los especificados en el cÃ³digo 18 01 08",
                                   "Medicamentos distintos de los especificados en el código 18 01 08"),
         nombre_ler = str_replace(nombre_ler, "Medicamentos distintos de los especificados en el cÃ³digo 20 01 31",
                                  "Medicamentos distintos de los especificados en el código 18 01 08"),
         nombre_ler = str_replace(nombre_ler,"Objetos cortantes y punzantes (excepto el cÃ³digo 18 01 03)",
                                  "Objetos cortantes y punzantes (excepto el código 18 01 03)"),
         nombre_ler = str_replace(nombre_ler, "Otras fracciones no especificadas en otra categorÃ­a",
                                  "Otras fracciones no especificadas en otra categoría"),
         nombre_ler = str_replace(nombre_ler, "Otros lodos y tortas de filtraciÃ³n",
                                  "Otros lodos y tortas de filtraciÃ³n"),
         nombre_ler = str_replace(nombre_ler, "Otros residuos (incluidas mezclas de materiales) procedentes del tratamiento mecÃ¡nico de residuos, distintos de los especificados en el cÃ³digo 19 12 11",
                                  "Otros residuos (incluidas mezclas de materiales) procedentes del tratamiento mecánico de residuos, distintos de los especificados en el código 19 12 11"),
         nombre_ler = str_replace(nombre_ler, "Pinturas, tintas, adhesivos y resinas distintos de los especificados en el cÃ³digo 20 01 27",
                                  "Pinturas, tintas, adhesivos y resinas distintos de los especificados en el código 20 01 27"),
         nombre_ler = str_replace(nombre_ler, "Residuos agroquÃ­micos distintos de los mencionados en el cÃ³digo 02 01 08",
                                 "Residuos agroquímicos distintos de los mencionados en el código 02 01 08"),
         nombre_ler = str_replace(nombre_ler, "Residuos inorgÃ¡nicos distintos de los especificados en el cÃ³digo 16 03 03",
                                  "Residuos inorgánicos distintos de los especificados en el código 16 03 03"),
         nombre_ler = str_replace(nombre_ler, "Residuos municipales no especificados en otra categorÃ­a",
                                  "Residuos municipales no especificados en otra categoría"),
         nombre_ler = str_replace(nombre_ler, "Restos anatÃ³micos y Ã³rganos, incluidos bolsas y bancos de sangre (excepto el cÃ³digo 18 01 03)",
                                  "Restos anatómicos y órganos, incluidos bolsas y bancos de sangre (excepto el código 18 01 03)"),
         nombre_ler = str_replace(nombre_ler, "Residuos no especificados en otra categorÃ­a",
                                  "Residuos no especificados en otra categoría"),
         nombre_ler = str_replace(nombre_ler, "Lodos del tratamiento in situ de efluentes, distintos de los especificados en el cÃ³digo 03 03 10",
                                  "Lodos del tratamiento in situ de efluentes, distintos de los especificados en el código 03 03 10"),
         nombre_ler =str_replace(nombre_ler, "Lodos procedentes de otros tratamientos de aguas residuales industriales, distintos de los especificados en el cÃ³digo 19 08 13",
                                 "Lodos procedentes de otros tratamientos de aguas residuales industriales, distintos de los especificados en el código 19 08 13"),
         nombre_ler = str_replace(nombre_ler, "Limaduras y virutas de metales fÃ©rreos",
                                  "Limaduras y virutas de metales ferrosos"),
         nombre_ler = str_replace(nombre_ler, "Heces de animales, orina y estiÃ©rcol (incluida paja podrida) y efluentes recogidos selectivamente y tratados fuera del lugar donde se generan",
                                  "Heces de animales, orina y estiércol (incluida paja podrida) y efluentes recogidos selectivamente y tratados fuera del lugar donde se generan"),
         nombre_ler = str_replace(nombre_ler, "Otros lodos y tortas de filtraciÃ³n", "Otros lodos y tortas de filtración"),
         nombre_ler = str_replace(nombre_ler, "Objetos cortantes y punzantes (excepto el cÃ³digo 18 01 03)",
                                  "Objetos cortantes y punzantes (excepto el código 18 01 03)"),
         nombre_ler = str_replace(nombre_ler, "Otros residuos (incluidas mezclas de materiales) procedentes del tratamiento mecÃ¡nico de residuos, distintos de los especificados en el cÃ³digo 19 12 11",
                                  "Otros residuos (incluidas mezclas de materiales) procedentes del tratamiento mecánico de residuos, distintos de los especificados en el código 19 12 11"),
         nombre_ler = str_replace(nombre_ler, "Residuos orgÃ¡nicos distintos de los especificados en el cÃ³digo 16 03 05",
                                  "Residuos orgánicos distintos de los especificados en el código 16 03 05"),
         nombre_ler = str_replace(nombre_ler, "Restos anatÃ³micos y Ã³rganos, incluidos bolsas y bancos de sangre (excepto el cÃ³digo 18 01 03)",
                                 "Restos anatómicos y órganos, incluidos bolsas y bancos de sangre (excepto el código 18 01 03)"),
         nombre_ler = str_replace(nombre_ler, "Plástico PVC (policloruro de vinilo)","Plástico PVC (policloruro de vinilo)"))                                 
    


#CARACTERIZACION DE RESIDUOS INCINERADOS [EN PROCESO]-----
#Limpieza de nombres leer

incineracion %>% 
 group_by(nombre_ler) %>% 
  summarise(total=round(sum(cantidad_toneladas),2)) %>% 
  arrange(-total) %>% 
   view()


#CO-PROCESAMIENTO
#Cantidad de residuos plásticos INCINERADOS


plasticos<- c("Plásticos",
            "Envases de plástico", 
            "Residuos de plástico",
            "Plástico y caucho",
            "Residuos de plásticos (excepto embalajes)",
            "Plástico HDPE (polietileno de alta densidad)",
            "Plástico PPR (polipropileno R)",
            "Plástico PVC (policloruro de viilo)",
            "Plástico PVC (policloruro de vinilo)")

maderas<- c("Envases de madera",
            "Madera",
            "Madera distinta de la especificada en el código 19 12 06",
            "Madera distinta de la especificada en el código 20 01 37",
            "Madera libre de impregnación o pinturas",
            "Residuos de corteza y madera",
            "Serrín, virutas, recortes, madera, tableros de partículas y chapas distintos de los mencionados en el código 03 01 04"
            )
vidrio<- c("Residuos de vidrio distintos de los especificados en el código 10 11 11",
           "Envases de vidrio",
           "Vidrio")

cenizas<- c("Cenizas volantes de carbón",
            "Cenizas del hogar, escorias y polvo de caldera (excepto el polvo de caldera especificado en el código 10 01 04",
            "Cenizas volantes procedentes de la co-incineración distintas de las especificadas en el código 10 01 16"
            )

neumaticos<-c("Neumáticos fuera de uso")

lodos<- c("Lodos del tratamiento in situ de efluentes, distintos de los especificados en el código 03 03 10",
          "Lodos del tratamiento in situ de efluentes",
          "Lodos de fosas sépticas",
          "Otros lodos y tortas de filtración",
          "Lodos del tratamiento de aguas residuales urbanas",
          "Lodos de lavado y limpieza",
          "Lodos de la clarificación del agua",
          "Lodos de tratamientos físicoquímicos, distintos de los especificados en el código 19 02 05",
          "Lodos procedentes del tratamiento biológico de aguas residuales industriales distintos de los especificados en el código 19 08 11",
          "Lodos acuosos que contienen adhesivos o sellantes, distintos de los especificados en el código 08 04 13",
          "Lodos procedentes de otros tratamientos de aguas residuales industriales, distintos de los especificados en el código 19 08 13")

#FILTRO DE residuos plasticos incinerados--
#Con esta grafica se puede hacer la caracterización de residuos incinerados
# por ahora, solo rabajaremos plasticos hay que seguir clasificando....
burn_clase<-incineracion %>% 
  group_by(nombre_ler, ano) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
  mutate(  clase = case_when(nombre_ler %in% plasticos ~ "plásticos",
                             nombre_ler %in% maderas ~ "maderas",
                             nombre_ler %in% vidrio ~ "vidrios",
                             nombre_ler %in% lodos ~ "lodos",
                             nombre_ler %in% cenizas~ "cenizas",
                             nombre_ler %in% neumaticos ~"neumáticos fuera de uso",
                                   TRUE ~ "otros residuos" )) %>% 
  ungroup() %>% 
  group_by(clase) %>% 
  summarise(total=sum(total)
    
  )



#data para grafica de plasticos incinerados por años

burn_plastic<-incineracion %>% 
  group_by(nombre_ler, ano) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
  mutate(  clase = case_when(nombre_ler %in% plasticos ~ "plásticos",
                             TRUE ~ "otros residuos" )) %>% 
  ungroup() %>% 
  group_by(clase, ano) %>% 
  summarise(total=sum(total))

sum(burn_plastic$total)


#Grafica de residuos plasticos incineracodos x año-----

ggplot(data=burn_plastic, aes(x="",y= total, fill=clase))+
  geom_bar(stat='identity', color="black", position = position_fill())+
  # geom_text(aes(label = Cnt), position = position_fill(vjust = 0.5))+ #PARA LABEL!!!!!
  coord_polar(theta="y")+
  scale_y_continuous(
    trans = "sqrt")+
  scale_fill_manual(
    values=c("#a32020", "#244a9c"),
    label = c("Otros residuos", 
              "Plásticos"))+
  theme(
    text= element_text(family="Lato", size=30),
    panel.background = element_blank(),
    strip.background = element_rect(fill ="white"),
    strip.text = element_text(size = 30),
    axis.title = element_blank(),
    axis.text= element_blank(),
    axis.ticks= element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 60,color = "#006600", family = "League Spartan", 
                              lineheight = 0.3, face ="bold" ),
    plot.subtitle = element_text(size = 40,lineheight = 0.3),
    plot.caption = element_text( size = 35),
    #plot.caption.position = "panel",
    # margin=margin(t=10)),
    legend.text = element_text(size = 30, vjust = 1, hjust =0),
    legend.title = element_blank(), #element_text(hjust =0.5),
    legend.position = "top",
    legend.key = element_rect(colour ="white"),
    legend.key.size = unit(10,"pt"),
    legend.key.width = unit(0.7 , "cm"),
    legend.background = element_blank())+
  labs(
    title = "Total de residuos plásticos incinerados por año",
    subtitle = "Incineración anual de residuos plásticos",
    caption = "Fuente bases de datos RETC | Alianza basura cero Chile")+
  xlab("Tratamientos")+
  facet_grid(~ano)+
  guides(fill=guide_legend(ncol=2))

ggsave("plastico_incinerado.png",device = "png", type= "cairo", width = 8, height = 4)



#[GRAFICA] forma de incineración de residuos plásticos ------

 
tratamiento_plasticos<-incineracion %>% 
  group_by(tratamiento, nombre_ler, ano) %>% 
  summarise(total=sum(cantidad_toneladas)) %>% 
  mutate(  carac = case_when(nombre_ler %in% plasticos ~ "plásticos",
                            # nombre_ler %in% maderas ~ "maderas",
                            # nombre_ler %in% vidrio ~ "vidrios",
                            # nombre_ler %in% lodos ~ "lodos",
                            # nombre_ler %in% cenizas~ "cenizas",
                             TRUE ~ "otros residuos" )) %>%
  ungroup() %>% 
  group_by(tratamiento,carac, ano) %>% 
  summarise(total = sum(total)) %>% 
  filter(carac=="plásticos")

ggplot(data=tratamiento_plasticos, aes(x=ano,y= total, fill=tratamiento))+
  geom_bar(stat='identity', color="black",width = 0.9,position = "dodge")+
  #scale_y_continuous(
    #trans = "sqrt")+
  scale_fill_manual(
    values=c("#a32020","#e0301e", "#eb8c00","#dc6900"),
    label = c("Co-procesamiento ", 
              "Incineración con recuperación de energía",
              "Co-incineración ",
              "Incineración sin recuperación de energía"))+
  theme(
    panel.grid.major.y = element_line(colour="black", linewidth =0.5, linetype = "dashed"),
    text= element_text(family="Lato", size=30),
    panel.background = element_blank(),
    strip.background = element_rect(fill ="white"),
    axis.title = element_blank(),
    axis.text = element_text (color ="black", family = "Lato"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(),
    plot.title = element_markdown(size = 60,color = "#006600", family = "League Spartan", 
                                  lineheight = 0.3, face ="bold" ),
    plot.subtitle = element_markdown(size = 40,lineheight = 0.3, face= "bold"),
    plot.caption = element_markdown(lineheight =0.4, size = 35),
    plot.caption.position = "panel",
    legend.position = "top",
    legend.text = element_text(size = 28),

       )+
  scale_y_sqrt(labels=comma_format(
    big.mark = ".",
    decimal.mark = ",",
    suffix = " Ton."),
    sec.axis = dup_axis(),
    limits=c(0,1500),
    expand = c(0,0),
    breaks = c(0,50,100,250,500,1000,1500))+
  labs(
    fill="Tratamientos",
    title = "Destino plásticos incinerados en Chile ",
    subtitle="<span style='color:#244a9c'>Plásticos</span> gestionados por medio de incineración entre los años 2017 - 2020",
    caption= "Fuente | Base de datos RETC \n<span style='color:#006600'>Alianza Basura Cero Chile</span>"
  )+
  guides(fill=guide_legend(ncol=2))
  

ggsave("tratamientoanual_plasticos.png",device = "png", type= "cairo", width = 8, height = 5)

incineración












#EMPRESAS GENERADORAS DE PLASTICO INCINERADO------
empresas_incineracion<-incineracion %>% 
  group_by(tipo, razon_social,) %>% 
  summarise( toneladas = round(sum(total),2)) %>%
  arrange(-toneladas) %>% 
  mutate(pct_acum= prop.table(toneladas) * 100) %>% 
  mutate( razon_social =str_replace(razon_social,"MUNICIPALIDAD.*", "MUNICIPALIDAD")) %>% 
  view() # agrupamos los municipios en un solo grupo

sum(empresas_incineracion$toneladas)

write.csv2(empresas_incineracion,file="012023_incineracion_plastico.csv", row.names = FALSE) 






#GENERADORES DE RESIDUOS PLASTICOS DESTINADOS A METODOS DE GESTIÖN EN BASE A INCINERACION-----

ggplot(data=empresas_incineracion,
       aes(
         x=toneladas , y=razon_social, fill =tipo))+
  geom_bar(
    position=position_dodge(),
    width = 0.7, 
    stat = "Identity")+
  theme_classic()+
  theme(
    legend.text = element_markdown(),
    panel.grid.major.x  = element_line(colour = "grey60", linetype = "dashed"),
    legend.key.size = unit(10,"pt"),
    legend.position = c(0.7,0.3))+
  scale_x_sqrt(labels=comma_format(
    big.mark = ".",
    decimal.mark = ","),
    limits=c(0,1000),
    expand= expansion(mult = c(0,0.1)))+
  labs(
    y=NULL,
    x="Residuos plásticos exportados (Kilogramos)",
    fill="Subpartida 3915",
    title = "Total exportaciones de Desechos, desperdicios y recortes.",
    subtitle="Desde Chile a Asia Pacífico entre 2015-2020.",
    caption= "Fuente: Base de datos, Aduanas Chile."
  )+scale_fill_jama()

ggsave("empresas incineradoras.png",device = "png", type= "cairo", width = 10, height = 10)  


#GENERADORES----

#CARTOGRAFIA DE RESIDUOS


chile<-st_read("Regional.shp")

class(chile)
head(chile)

ggplot(data= chile)+
  geom_sf()
