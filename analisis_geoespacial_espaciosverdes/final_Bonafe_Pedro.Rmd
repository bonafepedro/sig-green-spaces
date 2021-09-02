---
title: "Importancia del río en Villa Allende"
author: "Pedro Bonafé"
date: "18/8/2021"
output: html_document
---

La OMS ha dispuesto que para considerarse "saludable" una ciudad debe poseer al menos 15m2 de espacios verdes por habitante, sorprendente y lamentablemente Villa Allende "la entrada de las sierras", ciudad históricamente reconocida por sus bellos paisajes y su naturaleza, está lejos de ese número.

Por ello el motivo de este proyecto es poder visualizar la relación entre cantidad de habitantes y espacios verdes, buscando determinar cuáles zonas son las más desfaborecidas en esta relación que hace a la calidad de vida. Además compararemos dicha disponibilidad de espacios verdes con otras características socioeconómicas para ver si existe o no relación. 

Finalmente, como yapa, incorporaremos las orillas del río actualmente en un estado de lamentable abandono, a fin de observar cuanto impactaría en estos indicadores su recuperación para el uso de los vecinos como espacio verde. 

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```
## Consideraciones iniciales

Para el desarrollo de nuestro proyecto utilizaremos las siguientes librerías

```{r}
library(sf)
library(tidyverse)
library(osmdata)
library(units)
library(leaflet)
library(mapview)
library(ggmap)
library(kableExtra)
library(dplyr)
```

Para asegurarnos de arrancar con la memoria limpia de datasets viejos utilizamos la librería rm.
```{r}
rm(list = ls())
```

## Obtención de datos de nuestra ciudad

### Datos geoespaciales
Para poder comenzar con nuestro proyecto, primero seleccionamos el bbox correspondiente a la localidad de Villa Allende.

```{r}
bb_villa <- getbb("Villa Allende, Córdoba")

```
La definimos como polígono
```{r}
allende_poly <- getbb("Municipio de Villa Allende, Córdoba", format_out = "sf_polygon")
class(allende_poly)
```
Además extraeremos un mapa de Villa Allende que usaremos como "Capa Base". Para ello usaremos los mapas de Stamen Maps.

```{r}
vabase <- get_stamenmap(bbox = bb_villa, 
                      maptype = "terrain", 
                      zoom=12)
```
Lo observamos

```{r}
ggmap(vabase)
```


La extracción de los polígonos correspondientes a parques y espacios verdes la haremos desde el GeoServicio de OpenStreetmap, previamente realizamos un chequeo e incorporamos parques faltantes pudiendo observar que ciertos espacios como el polideportivo municipal cuyo uso es relevante para este proyecto se encontraban definidos como Sport Centre por lo que decidimos incorporar dicha categoría. 
```{r}
allende <- opq(bb_villa) %>% 
    add_osm_feature(key = "leisure", value = c("park", "sports_centre"))
allende
```

Posteriormente a la extracción desde el geoservicio guardamos la información como sf en la variable allende
```{r}
allende <- allende %>% 
    osmdata_sf()
allende
```
De la información extraida seleccionamos solamente los polígonos y los guardamos en la variable allende_plazas
```{r}
allende_plazas <- allende$osm_polygons
head(allende_plazas) 
```

Listo! Ahora podemos hacer un mapa, para tener una primera aproximación.



```{r}
allende_plazas <- st_intersection(allende_plazas, allende_poly)
ggmap(vabase) +
  geom_sf(data=allende_poly, inherit.aes = FALSE, fill = NA) +
  geom_sf(data = allende_plazas, inherit.aes = FALSE, color = "green")
```

El mapa refleja lo planteado anteriormente, la proporción de territorio dedicado a espacios verdes en la ciudad es muy baja. 

Para mayor información calcularemos las áreas de dichos espacios verdes en m2 y los incorporaremos como una columna más dentro del dataset allende_plazas.



```{r}
allende_plazas$area <-as.numeric(set_units(st_area(allende_plazas), m^2))
```

### Datos Demográficos

La información vinculada a las características poblacionales de la ciudad puede ser obtenida de la página poblaciones.org realizada por Conicet, la cual sintetiza geográficamente la información proveniente de los últimos censos realizados. 

```{r}
densidad <- st_read("vadensidad1/dataset11601hwr14979.shp")
head(densidad)
```
Nos interesan solo los polígonos, el RADIOCODE y P_TOTAL así que filtramos
```{r}
densidad <- select(densidad, REDCODE, P_TOTAL, geometry)
```

## Primeros Cálculos

Rápidamente podemos calcular el área verde por habitante total. 
```{r}
areaverde <- sum(allende_plazas$area)
p_total <- sum(densidad$P_TOTAL)
verde_porhabitante <- areaverde / p_total

# Creamos un data.frame con esos 3 objetos
tabla = data.frame(areaverde, p_total, verde_porhabitante)

# Cambiamos el nombre de las columnas

tabla = rename(tabla, "Area verde" = areaverde, "Poblacion total" = p_total, "Espacio verde por hab." = verde_porhabitante)

tabla %>%
  kbl(caption = "Datos de Espacios verdes Ciudad de Villa Allende en m2" ) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 16)


```

Hay un total de 285.957 m2 de espacio verde lo que corresponde a un total de 10m2 por habitante. Si bien es considerablemente menor a los 15m2 recomendados por la oms, posee mejores valores que muchas otras ciudades. Sin embargo recordemos que estamos tomando datos poblacionales correspondientes al censo del 2010, hoy la población estimada duplica ese número por lo que la relación espacio verde por habitante disminuyó drásticamente. 

Ahora tenemos la información de la densidad poblacional, haremos un coropleth.

```{r}
ggplot(data = densidad)+
  geom_sf(data = allende_poly) +
  geom_sf(aes(fill= P_TOTAL))+
  scale_fill_viridis_c() +
  labs(title = "Población total por radio censal año 2010",
       subtitle = "Ciudad de Villa Allende",
       caption = "Elaboración propia en base a datos de OpenStreetmap y Poblaciones.org \nlas zonas en blanco no poseían población registrada hasta 2010")+
  theme_void()

```

El dataset presenta ciertas ventajas y ciertas desventajas que vale la pena aclarar. Como ventaja está el hecho de que los radios censales coinciden aproximadamente con los límites de los barrios, digo aproximadamente porque hay excepciones leves, esto podemos saberlo porque por ser vecinos de la ciudad conocemos los límites y formas de los barrios. La desventaja está en el hecho de que por ser datos correspondientes al censo del 2010 existen zonas no delimitadas como pobladas que hoy en día se encuentran no solo pobladas sino que su población es importante, ya que coincide con el proceso de expanción y desarrollo inmobiliario de la ciudad. Dichas zonas son las que figuran en blanco


Ahora observaremos como se ve el mapa de los radios censales de la ciudad y los espacios verdes
```{r}
ggplot()+
  geom_sf(data = allende_poly) +
  geom_sf(data = densidad$geometry) +
  geom_sf(data = allende_plazas, fill = "green") +
  labs(title = "Espacios verdes por radio censal año 2010",
       subtitle = "Ciudad de Villa Allende",
       caption = "Elaboración propia en base a datos de OpenStreetmap y Poblaciones.org")+
  theme_void()
```

La escala no ayuda así que utilizaremos un tipo de mapa interactivo 
```{r}
mapview(allende_plazas) +
  mapview(densidad$geometry, fill = NA)
```

Ahora cruzaremos dicho mapa con el mapa de cantidad de población
```{r}
ggplot() +
  geom_sf(data = allende_poly) +
  geom_sf(data = densidad, aes(fill = P_TOTAL)) +
  geom_sf(data = allende_plazas, fill = "green") +
  labs(title = "Población total y espacios verdes por radio censal año 2010",
       subtitle = "Ciudad de Villa Allende", 
       caption = "Elaboración propia en base a datos de OpenStreetmap y Poblaciones.org \nlas zonas en blanco no poseían población registrada hasta 2010")+
  theme_void()
```
```{r}
bin0 <- c(0,250, 500, 750, 1000, 1500, Inf)
binpal <- colorBin("RdYlGn", densidad$P_TOTAL, bins = bin0, pretty = FALSE, reverse = TRUE)
leaflet() %>% 
  setView(lng = -64.29207, lat = -31.29080, zoom = 13) %>% 
  addTiles() %>% 
  addPolygons(data=densidad, stroke=FALSE, smoothFactor = 0.2, fillOpacity = .8,
    color = ~binpal(P_TOTAL)) %>%
  addPolygons(data = allende_plazas, fill = "green") %>%
  addLegend(values = densidad$P_TOTAL, pal = binpal, title = "Población total por radio censal")
```


Muy interesante el mapa nos permite ver rápidamente sin avanzar en muchos cálculos que dos de los radios más poblados no poseen ninguna plaza!!

## Afinando el Lápiz 

Hasta el momento pudimos hacer una primera aproximación en este proceso de tratar de entender cómo se presenta la disponibilidad de espacios verdes en la ciudad de Villa Allende.
Ahora bien, a fin de profundizar el análisis y continuando con el criterio planteado por la OMS sobre la disponibilidad de espacios verdes por cantidad de habitantes, veremos como se da esta relación en cada uno de los radios censales. Para ello haremos un dataset filtrando las columnas que nos interesan de densidad e incorporando en el mismo una columna donde registre la cantidad de plazas que caen en su interior. Finalmente incorporaremos una última columna donde sume los m2 de cada plaza correspondientes a cada radio censal. 

Para quienes no conocen la ciudad de Villa Allende podrán rápidamente detectar que hay un gran polígono verde en el centro norte de la ciudad no delimitado, pero el mismo corresponde al Golf Club el cual no es público y por lo tanto no puede ser utilizado sino por socios, distinto es el caso del gran espacio verde de la zona sureste el cual se encuentra dentro del Country San Isidro por lo que no es accesible al total de la población pero siguiendo el criterio de usabilidad por parte de los vecinos del mismo barrio, el mismo está disponible para todos los vecinos que viven allí por lo que fue considerado pertinente. 


```{r}
a <- st_join(allende_plazas, densidad, join = st_within)
```
```{r}
a <- a %>%
  group_by(REDCODE) %>%
  summarise(cantidad = n(), area_verde = sum(area, na.rm = TRUE)) %>%
  st_set_geometry(NULL)
```

Hacemos un left join para incorporar la información en densidad

```{r}
densidad <- left_join(densidad, a)
```

Ahora graficamos! 

```{r}
ggplot() +
  geom_sf(data = allende_poly) +
  geom_sf(data=densidad, aes(fill= cantidad)) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Cantidad de espacios verdes por radio censal",
       subtitle = "Ciudad de Villa Allende",
       fill = "Plazas por radio censal",
       caption = "Elaboración propia en base a datos de OpenStreetmap y Poblaciones.org \nlas zonas en blanco no poseían población registrada hasta 2010")
```

Se puede observar que hay muchos barrios sin plazas, 7 según los resultados visibles en el dataset generado.

Ahora para calcular el "area verde" por habitante creamos una nueva columna que divida area_verde sobre cantidad de habitantes del radio censal
```{r}
densidad$verdepor_habitante <- densidad$area_verde / densidad$P_TOTAL
```


Ahora veamos como se ve el gráfico en funcion del "área verde" por habitante
```{r}
ggplot() +
  geom_sf(data = allende_poly) +
  geom_sf(data=densidad, aes(fill= verdepor_habitante)) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Cantidad de m2 verdes por habitante por cada radio censal",
       subtitle = "Ciudad de Villa Allende",
       fill = "M2 verdes por habitante",
       caption = "Elaboración propia en base a datos de OpenStreetmap y Poblaciones.org \nlas zonas en blanco no poseían población registrada hasta 2010")
```

Lo mismo pero en interactivo!
```{r}
bin <- c(0, 1, 5, 10, 15, 20, 40, Inf)
paleta <- colorBin("RdYlGn", domain = densidad$verdepor_habitante, bins = bin, pretty = FALSE, reverse = FALSE)
leaflet() %>% 
  setView(lng = -64.29207, lat = -31.29080, zoom = 13) %>% 
  addTiles() %>% 
  addPolygons(data=densidad, stroke=FALSE, smoothFactor = 0.2, fillOpacity = .8,
    color = ~paleta(verdepor_habitante)) %>%
  addPolygons(data = allende_plazas, fill = "green") %>%
  addLegend(values = densidad$verdepor_habitante, pal = paleta, title = "Espacio verde por habitante en m2")

```

Bien ya observamos que hay radios que tienen varios espacios verdes, otros que tienen pocos y directamente algunos que no tienen. 

Ahora tratando de ampliar un poco el criterio consideraremos la disancia al espacio verde más cercano. 
Para ello calcularemos el centroide de cada radio censal y de cada plaza, para luego calcular la distancia con st_nearest_feature

```{r}
plazas_c <- st_point_on_surface(allende_plazas)
radios_c <- st_point_on_surface(densidad)
```

```{r}
densidad <- densidad %>% #Tomamos de base el df densidad que contiene los poligonos de radios censales
  mutate(distancia=st_distance(radios_c, plazas_c[st_nearest_feature(radios_c, plazas_c),], by_element = TRUE)) %>% #Agregamos columna nueva con los valores del calculo de distancia entre centroidoes ;)
  mutate(distancia=as.numeric(distancia)) #Convertimos a número
```
Ahora graficamos 

```{r}
ggplot() + 
  geom_sf(data = allende_poly) +
  geom_sf(data = densidad, aes(fill = distancia), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Distancia a plaza más cercana",
       subtitle = "Ciudad Villa Allende",
       fill = "Distancia a plaza más cercana en m",
       caption = "Elaboración propia en base a datos de OpenStreetmap y Poblaciones.org \nlas zonas en blanco no poseían población registrada hasta 2010")+
  theme_void()
```

En interactivo.

```{r}
bins <- c(0,50,100,250,500,1000,1500, Inf)
palet <- colorBin("RdYlGn", densidad$distancia, bins = bins, pretty = FALSE, reverse = TRUE)
leaflet() %>% 
  setView(lng = -64.29207, lat = -31.29080, zoom = 13) %>% 
  addTiles() %>% 
  addPolygons(data=densidad, stroke=FALSE, smoothFactor = 0.2, fillOpacity = 0.85,
    color = ~palet(distancia)) %>%
  addLegend(values = densidad$distancia, pal = palet, title = "Distancia a la plaza más cercana en m")
```

Este gráfico es sumamente interesante, puesto que nos permite observar que las zonas céntricas, si bien concentran la mayor cantidad de población son quienes poseen los espacios verdes más cerca, bastante similar a muchas otras ciudades. 

### ¿Y la economía?

A quienes venimos de las ciencias sociales nos encanta preguntarnos cómo influyen los factores socioeconómicos en casi todas las demás variables a analizar, pero salvando el vicio profesional es súmamente interesante ver si la desigualdad en el acceso a espacios verdes (considerándolos como dijimos factores de la salud pública y la calidad de vida de la población) reproduce otro tipo de desigualdades. Para ello observaremos si existe o no relación entre la disponibilidad de espacios verdes, Necesidades Básicas y Estrato socioeconómico, ambos dataset obtenidos también de poblaciones.org

```{r}
va_nbi <- st_read("vanbi/dataset8301hwr14979.shp")
va_socioec <- st_read("villaallende_socioec/dataset19901hwr14979.shp")
```
```{r}
ggplot() + 
  geom_sf(data = allende_poly) +
  geom_sf(data = va_nbi, aes(fill = H_NBI/H_TOTAL *100), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Porcentaje de hogares con al menos una NBI",
       subtitle = "Ciudad Villa Allende",
       fill = "Hogares con al menos una NBI",
       caption = "Elaboración propia en base a datos de Poblaciones.org \nlas zonas en blanco no poseían población registrada hasta 2010")+
  theme_void()
```

Sumamente interesante la comparativa! podemos ver que existe muy poca relación ya que por ejemplo los radios de la zona norte son quienes poseen menor porcentaje de hogares con al menos una nbi (y corresponden a los barrios más adinerados de la ciudad) son al mismo tiempo quienes poseen más lejos la plaza más cercana. Pero tampoco podemos afirmar una correlación inversa ya que los radios del sur con un porcentaje medio de hogares con nbi les siguen en lejanía a las plazas más cercanas.

Veremos como se comporta con respecto al dataset de estrato socio económico

```{r}
ggplot() + 
  geom_sf(data = allende_poly) +
  geom_sf(data = va_socioec, aes(fill = SEGMENTO), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Estrato socioeconómico de cada radio censal",
       subtitle = "Ciudad Villa Allende",
       fill = "Estrato socioeconómico",
       caption = "Elaboración propia en base a datos de Poblaciones.org \nlas zonas en blanco no poseían población registrada hasta 2010")+
  theme_void()
```

De los metadatos del dataset sabemos que los valores de la columna Segmento socioeconómico del radio corresponden a 
  1:Clase alta
  2:Clase media alta
  3:Clase media
  4:Clase media baja
  5:Populares
  6:Muy pobres
  7:Indigente
  99:Disperso

```{r}
table(va_socioec$SEGMENTO)
va_socioec$estrato <- as.factor(va_socioec$SEGMENTO)
str(va_socioec$estrato)
```
```{r}
va_socioec$estrato <- factor(va_socioec$estrato,
                             levels = levels(va_socioec$estrato),
                             labels = c("Clase alta", "Clase media alta", "Clase media", "Clase media baja", "Populares", "Muy pobres", "Indigente"))

table(va_socioec$estrato)
```

Categorizaremos también la distancia a la plaza más cercana.
Para ello primero hacemos un join con el dataset densidad donde teníamos guardada la columna distancia
```{r}
va_socioec <- select(va_socioec, REDCODE, SEGMENTO, estrato)
va_socioec <- st_join(densidad, va_socioec)
```

Luego creamos una columna con los valores categorizados de la distancia usando cut

```{r}
va_socioec$dist_plaza <- cut(va_socioec$distancia,
                             breaks = c(0, 100, 500, 750, 1000, Inf),
                             labels = c("Muy Cerca", "Cerca", "Media", "Lejos", "Muy lejos"),
                             right = FALSE)
```



```{r}
lab <- c("Clase alta", "Clase media alta", "Clase media", "Clase media baja", "Populares", "Muy pobres", "Indigente")
bin1 <- c(1,2,3,4,5,6,7)
paletaa <- colorBin("viridis", va_socioec$SEGMENTO, bins = bin1, pretty = FALSE)
leaflet() %>% 
  setView(lng = -64.29207, lat = -31.29080, zoom = 13) %>% 
  addTiles() %>% 
  addPolygons(data=va_socioec, stroke=TRUE, smoothFactor = 0.2, fillOpacity = .8,
    color = ~paletaa(SEGMENTO),
    highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE),
    popup = ~paste0("Distancia plaza: ",va_socioec$dist_plaza, "<br/>", "Estrato social: ",va_socioec$estrato)) 
  
```

Como consideraciones respecto al gráfico anterior, hay radios censales en los que el dataset de poblaciones.org no muestra registros que corresponden a los dos barrios cerrados situados sobre la av. Padre Luchesse al sureste, sin embargo por las características socioeconómicas de los mismos corresponderían a los denominados clase alta. 

## Bueno ahora la yapa!

Vamos a calcular qué impacto tendría la recuperación de la cuenca del río y sus orillas como espacio verde para ser utilizado y aprovechado por los vecinos
En este sentido utilizando el mapa de riesgo generado por IDECOR donde clasifica las zonas de la cuenca del río por ser más o menos inundables, consideramos como criterio seleccionar la zona cuyo riesgo de inundación es alto, entendiendo que debería ser esta la que se transforme en espacio verde mejorando así los indicadores de m2 verde por habitante que venimos trabajando y disminuyendo además las potencialidad de consecuencias trágicas en una eventual inundación ya que el sector altamente inundable que pasaría a ser espacio verde, funcionaría también como "zona de absorción".

Para ello primero descargamos la información relativa a la cuenca del río 

```{r}
rio <- st_read("WFS:https://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/Villa_Allende_Riesgo_Inundacion/wfs?getcapabilities")
```

Lo transformamos a WGS 84

```{r}
rio <- st_transform(rio, crs = st_crs(3857))
rio
```
Ahora vamos a ver como se ve junto al mapa de Villa Allende
```{r}
ggplot() +
  geom_sf(data = allende_poly) +
  geom_sf(data = rio, aes(fill = descripción)) +
  geom_sf(data = allende_plazas, fill= "orange") +
  geom_sf(data = densidad, fill= NA) +
  theme_void()
```

Observamos que la cuenca del río cubre una gran superficie de la ciudad, ahora bien nos interesa seleccionar solo la zona de alto riesgo. 

```{r}
zonainund <- rio[rio$descripción == "Alto",]
ggplot() +
  geom_sf(data = allende_poly) +
  geom_sf(data = allende_plazas, fill= "green") +
  geom_sf(data = densidad, aes(fill = P_TOTAL)) +
  geom_sf(data = zonainund, colour = "green", fill = "lightgreen", fillOpacity = .001) +
  theme_void()

```

Listo! Podemos observar a simple vista cuánto espacio verde ganaría la ciudad si transformara la zona altamente inundable en parques! Además vemos que este sector del río abarca las zonas más pobladas de la ciudad.

Ahora vamos a calcularlo 
```{r}

rio$area_rio <- as.numeric(set_units(st_area(rio), m^2))
areainund <- as.numeric(set_units(st_area(zonainund), m^2))

# Creamos un data.frame con esos 3 objetos
tabla2 = data.frame(areainund, areaverde)

# Cambiamos el nombre de las columnas

tabla2 = rename(tabla2, "Area verde" = areaverde, "Area Inundable" = areainund)

tabla2 %>%
  kbl(caption = "Espacios verdes y zona inundable Ciudad de Villa Allende en m2" ) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 16)


```
Veamoslo en proporción y por habitante
```{r}
propor <- areainund/areaverde *100

area_total_porhabitante <- (areainund + areaverde) / p_total

tabla3 = data.frame(areaverde, areainund, propor, area_total_porhabitante)

tabla3 = rename(tabla3, "Area verde" = areaverde, "Area Inundable" = areainund, "Espacio verde total por hab." = area_total_porhabitante, "Proporción" = propor)

tabla3 %>%
  kbl(caption = " Espacios verdes y zona Inundable Ciudad de Villa Allende en m2" ) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 16)

```
Impactante, incorporar el área inundable implicaría un 578% más de espacios verdes y llevaría a tener 71m2 verdes por habitante, ¿un sueño no?

Queda pendiente ver cómo se comportaría por radio censal, he probado varias cuestiones pero no he podido resolver el problema vinculado a que el polígono del río aparece como desconocido. Si pudieran brindarme alguna sugerencia para poder hacer un st_join y cuantificar qué área de la costa del río cae en cada radio censal a fin de sumarla como espacio verde y ver como impactaría en la cantidad de espacios verdes de cada uno se los agradecería mucho!
```{r}
# #zonainund <-  st_transform(zonainund, crs = st_crs(4326))
# 
# radios <- select(densidad, REDCODE, geometry) %>%
#   st_transform(radios, crs = st_crs(3857))
# nvodf <- st_intersection(radios, zonainund)
```

Desde aquí muchísimas gracias por el curso la verdad que fué sumamente enriquecedor y me voy muy contento con lo aprendido y con ganas de seguir aprendiendo! Abrazo gigante!



