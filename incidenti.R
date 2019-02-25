# Load packages
library(tidyverse)
library("rgdal")
library("rgeos")
library(leaflet)
library(tmap)
library(sjPlot)
library(htmltools)
library(colorspace)
library(RColorBrewer)
library(scales)
library(flexdashboard)
library(ggmap)
library(anytime)
library(lubridate)
library(hexbin)
library(leaflet.extras)

# import data
paste0("incidenti_sap",2011:2015)
paste0("/incidenti_sap_",2011:2015)

shape_files_list <- data.frame("folder"=paste0("incidenti_sap",2011:2015,rep("/", times=4)), "file"=paste0("incidenti_sap_",2011:2015))

incidenti_2011 <- readOGR("incidenti_sap2011/", layer="incidenti_sap_2011")
incidenti_2012 <- readOGR("incidenti_sap2012/", layer="incidenti_sap_2012")
incidenti_2013 <- readOGR("incidenti_sap2013/", layer="incidenti_sap_2013")
incidenti_2014 <- readOGR("incidenti_sap2014/", layer="incidenti_sap_2014")
incidenti_2015 <- readOGR("incidenti_sap2015/", layer="incidenti_sap_2015")

incidenti <- rbind(incidenti_2011, incidenti_2012, incidenti_2013, incidenti_2014, incidenti_2015)

# convert to projection for plotting points with leaflet (WSG84)
latlong = "+init=EPSG:4326" # AKA: WSG84 
incidenti <-spTransform(incidenti, CRS(latlong))

# subset attached df
incidenti@data <- incidenti@data[,c(1:5,9,11:15)] # some duplicate columns
glimpse(incidenti@data) 
glimpse(incidenti_2012@data) 

incidenti@data$altri_veicoli <- ifelse(incidenti@data$TIPO_VA=="VELOCIPEDE", ifelse(is.na(as.character(incidenti@data$TIPO_VB)),"Nessuno",as.character(incidenti@data$TIPO_VB)), as.character(incidenti@data$TIPO_VA))

sjt.frq(factor(incidenti@data$altri_veicoli))
sjt.frq(factor(incidenti@data$altri_veicoli), file="veicoli_coinvolti.html")
# need to recode, and reduce to the following categories
# Autovetture, Autobus, Autocarri, Biciclette, Ciclomotori, Motocicli, Ignoti (fuggiti dopo sinistro), Altri (veicoli di soccorso o di polizia, tminicar, quadriciclo)

incidenti@data$altro_veicolo_recoded <- recode(incidenti@data$altri_veicoli, 
                                               `AUTO IN SOSTA` = "Autovettura",
                                               `AUTOVETTURA PRIVATA` = "Autovettura",
                                               `AUTOVETTURA PRIVATA CON RIMORCHIO` = "Autovettura",
                                               `AUTOVETTURA PUBBLICA` = "Autovettura",
                                               `AUTOBUS DI LINEA O NON IN EXTRAURBANA` = "Autobus",
                                               `AUTOBUS O FILOBUS IN SERVIZIO URBANO` = "Autobus",
                                               `TRAM` = "Autobus",
                                               `AUTOCARRO` = "Autocarro",
                                               `AUTOSNODATO O AUTOARTICOLATO` = "Autocarro",
                                               `AUTOTRENO CON RIMORCHIO` =  "Autocarro",
                                               `CICLOMOTORE` = "Ciclomotore",
                                               `MOTOCICLO A SOLO` = "Motociclo",
                                               `MOTOCICLO CON PASSEGGERO` = "Motociclo",
                                               `MOTOCICLO IN SOSTA` = "Motociclo",
                                               `VELOCIPEDE` = "Bicicletta",
                                               `IGNOTO` = "Ignoto",
                                               `Nessuno` = "Nessuno",
                                               .default = "Altro")

sjt.frq(factor(incidenti@data$altro_veicolo_recoded)) # OK 

# code whether the other veicle was moving or not
incidenti@data$altro_veicolo_movement <- recode(incidenti@data$altri_veicoli, 
                                                `AUTO IN SOSTA` = "In sosta",
                                                `MOTOCICLO IN SOSTA` = "In sosta",
                                                `Nessuno` = "Nessuno",
                                                `IGNOTO` = "Ignoto",
                                                .default = "In movimento")
sjt.frq(factor(incidenti@data$altro_veicolo_movement))

# convert data in proper date format in a new variable
incidenti@data$data_ok <- date(incidenti@data$DATA)
min(incidenti@data$data_ok) # 2 aprile 2011.  i dati partono dall'aprile 2011
max(incidenti@data$data_ok) # 31 marzo 2015.  i dati finiscono con il marzo 2015
incidenti@data$data_ok <- anytime(str_c(as.character(incidenti@data$DATA),str_replace(as.character(incidenti@data$ORA), "\\.",":"), sep= " "))


# recode anno bis, per avere statistiche esatte per periodi di un anno
incidenti@data$anno2 <- ifelse(incidenti@data$data_ok<date("2012-04-01"), "Aprile 2011 - Marzo 2012", ifelse(incidenti@data$data_ok<date("2013-04-01"), "Aprile 2012 - Marzo 2013", ifelse(incidenti@data$data_ok<date("2014-04-01"), "Aprile 2013 - Marzo 2014","Aprile 2014 - Marzo 2015")))

# reorder levels of MESE
mesi <- levels(incidenti@data$MESE)
mesi <- mesi[c(11,10,12,2,6,4:5,1,9,8,7,3)]
incidenti@data$MESE <- factor(incidenti@data$MESE, levels = mesi)
incidenti@data %>% group_by(anno2, MESE) %>% summarise(n=n()) 

# make a different variable for periods (first month is april)
incidenti@data$MESE2 <- factor(incidenti@data$MESE, levels = mesi[c(4:12,1:3)])

# giorno è gi codificato, reorder levels
giorni <- levels(incidenti@data$GIORNO)
giorni <- giorni[c(3:5,2,7,6,1)]
incidenti@data$GIORNO <- factor(incidenti@data$GIORNO, levels = giorni)
incidenti@data$ora2 <- as.numeric(str_sub(incidenti@data$ORA, 1,2))

# summary statistics
# totale 1264 incidenti in 5 anni
incidenti@data %>% group_by(ANNO) %>% summarise(n=n())# max 337 l'anno
incidenti@data %>% group_by(DATA) %>% summarise(n=n()) %>% summarise(sinistri_giorno=mean(n)) # 1.5 
incidenti@data %>% group_by(ANNO, MESE) %>% summarise(n=n()) %>% ungroup() %>% summarise(sinistri_mese=mean(n))  # 26 al mese in media nel periodo studiato

# recompute
incidenti@data %>% group_by(anno2) %>% summarise(n=n()) %>% ungroup() %>% summarise(media = mean(n), sd = sd(n))
# 316 per anno in media

1264/(4*365)

incidenti@data %>% 
  group_by(anno2, altro_veicolo_recoded) %>% 
  summarise(n = n()) %>% 
  mutate(totale_anno = sum(n)) %>% 
  ggplot(aes(x = anno2, y = n, colour = altro_veicolo_recoded, group = altro_veicolo_recoded)) +
  geom_line(stat = "identity") +
  ylab("Numero di incidenti") +
  xlab("") +
  labs(fill = "Altri veicoli\ncoinvolti") +
  geom_line(aes(x = anno2, y = tot, group = 1),
            data = incidenti@data %>%
              group_by(anno2) %>%
              summarise(tot = n()), inherit.aes = FALSE,
            colour = "black", size = 1.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# fare plot di incidenti nei mesi per tutto il periodo
ggplot(incidenti@data %>% group_by(anno2, MESE2) %>% summarise(n=n()), 
       aes(x=MESE2, y=n, group=1)) +
  geom_line(stat = "identity") +
  facet_wrap(~anno2, ncol = 1) +
  ylab("Frequenza (n. incidenti)") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=.5))
  

ggplot(incidenti@data %>% group_by(MESE2) %>% summarise(n=n()), 
       aes(x=MESE2, y=n, group=1)) +
  geom_line(stat = "identity") +
  ylab("Frequenza (n. incidenti)") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=.5))

# giorni della settimana
incidenti@data %>% 
  group_by(GIORNO) %>% 
  summarise(n=n()) %>% 
  mutate(percentuale=n/sum(n)*100)

# Heatmap di giorni/ore (8 righe x 7 colonne)
head(incidenti@data)
wday(incidenti@data$data_ok, label = TRUE)


ggplot(incidenti@data %>% group_by(GIORNO, ora2) %>% summarise(n=n()) %>% mutate(percentuale=n/sum(n)*100), 
      aes(ora2, GIORNO)) +
  geom_tile(aes(fill = n), colour = "white") + # or map fill to percentuale
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  scale_x_continuous(breaks = 0:23) + 
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(
    #legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()) + 
  coord_equal()

# heapmap giorni/mesi 
incidenti@data %>% group_by(MESE2, GIORNO) %>% summarise(n=n()) %>% mutate(percentuale=n/sum(n)*100) %>% 
  ggplot(aes(MESE2, GIORNO)) +
  geom_tile(aes(fill = n), colour = "white") + # or map fill to percentuale
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(
    #legend.position = "bottom", legend.key.width = unit(2, "cm"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
  coord_equal()

# fare mappa con density layer, per tutto il periodo e con small multiples uno per anno
# fare mappa con small multiples e density in base alle ore del giorno




make_labels <- function(data, anno=NULL) {
  if (!is.null(anno)){
    data <- filter(data, ANNO==anno)
  }
  labels <- sprintf(
    "<i>Data: </i>%s %s  %s<br/><br/><i>Ore:</i> %s<br/><br/><i>Luogo:</i> %s<br/><br/><i>Altri veicoli coinvolti: </i> %s",
    str_sub(data$DATA, start=9),
    data$MESE,
    data$ANNO,
    data$ORA,
    ifelse(is.na(data$CIVICO),str_c("Incrocio",data$STRADA, "con", data$STRADA_2, sep=" "), str_c(data$STRADA, ", ", data$CIVICO, sep="")),
    ifelse(data$TIPO_VA=="VELOCIPEDE", ifelse(is.na(as.character(data$TIPO_VB)),"Nessuno",as.character(data$TIPO_VB)), as.character(data$TIPO_VA))) %>% 
    lapply(htmltools::HTML)
  return(labels)
}

glimpse(ciclabili@data)

ciclabili@data %>% 
  select(TIPO, DESCRIZION) %>% 
  sjtab(fun = "frq")

make_labels_ciclabili <- function(data) {
  labels <- sprintf(
    "<i>Tipo: </i>%s <br/><i>Descrizione:</i> %s",
    data$TIPO,
    data$DESCRIZION
    ) %>% 
    lapply(htmltools::HTML)
  return(labels)
}

# make_labels(incidenti@data)

# mappa base con tutti gli incidenti
leaflet(incidenti) %>% 
  setView(lng = 11.25, lat = 43.783333, zoom = 14) %>% 
  addTiles() %>% 
  addMarkers(popup=~make_labels(incidenti@data))

# mappa con tutti gli incidenti divisi per anno, con overlay control
leaflet(subset(incidenti, ANNO=="2011")) %>% 
  setView(lng = 11.25, lat = 43.783333, zoom = 14) %>% 
  addTiles() %>% 
  addMarkers(popup=~make_labels(incidenti@data, anno="2011"), group="2011") %>% 
  addMarkers(data=subset(incidenti, ANNO=="2012"), 
             popup=~make_labels(incidenti@data, anno="2012"), group="2012") %>% 
  addMarkers(data=subset(incidenti, ANNO=="2012"), 
             popup=~make_labels(incidenti@data, anno="2012"), group="2012") %>% 
  addMarkers(data=subset(incidenti, ANNO=="2013"), 
             popup=~make_labels(incidenti@data, anno="2013"), group="2013") %>% 
  addMarkers(data=subset(incidenti, ANNO=="2014"), popup=~make_labels(incidenti@data, anno="2014"), group="2014") %>% 
  addMarkers(data=subset(incidenti, ANNO=="2015"), popup=~make_labels(incidenti@data, anno="2015"), group="2015") %>% 
  addLayersControl(overlayGroups = levels(incidenti@data$ANNO),
                   options = layersControlOptions(collapsed = TRUE),
                   position = "topleft"
  )

# color palette
levels(factor(incidenti@data$altri_veicoli)) #  21 levels!!!
sjt.frq(factor(incidenti@data$altri_veicoli))
sjt.frq(incidenti@data$ORA)


show_col(hue_pal()(9)) # default ggplot palette for factors to match other plots
factpal <- colorFactor(hue_pal()(9), incidenti@data$altro_veicolo_recoded)
# 

# circle maps all years and heatmap
#  make contour lines for incidents density in 2D
library("KernSmooth")
X=cbind(incidenti@coords)
kde2d <- bkde2D(X, bandwidth=c(bw.ucv(X[,1]),bw.ucv(X[,2])))
kde2d <- bkde2D(X, bandwidth=c(0.000798,0.000298))

x=kde2d$x1
y=kde2d$x2
z=kde2d$fhat
CL=contourLines(x , y , z)
class(CL)
str(CL)

map <- leaflet(subset(incidenti, ANNO=="2011")) %>% 
  setView(lng = 11.25, lat = 43.783333, zoom = 14) %>% 
  # addTiles() %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = ciclabili, color="darkgreen", group="Piste ciclabili")
# OK purrr :-) to add Outlines
seq_along(1:80) %>%
  purrr::walk( function(df) {
    map <<- map  %>%
      addPolygons(data=CL[[df]],
                  lng=~x, lat=~y,
                  group="heat",
                  fillColor = "red", stroke = FALSE)
  })
map %>% 
  addCircleMarkers(popup=~make_labels(incidenti@data, anno="2011"), group="2011", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2012"), 
             popup=~make_labels(incidenti@data, anno="2012"), group="2012", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2013"), 
             popup=~make_labels(incidenti@data, anno="2013"), group="2013", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2014"), 
                   popup=~make_labels(incidenti@data, anno="2014"), group="2014", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2015"), 
                   popup=~make_labels(incidenti@data, anno="2015"), group="2015", radius=2) %>% 
  addLayersControl(overlayGroups = c(levels(incidenti@data$ANNO),"heat","Piste ciclabili"),
                   options = layersControlOptions(collapsed = TRUE),
                   position = "topleft"
  ) 

# do not run
map %>% addLayersControl(overlayGroups = "heat",
                         options = layersControlOptions(collapsed = TRUE),
                         position = "topleft"
) 

# using leaflet extra
unique(incidenti@data$altro_veicolo_recoded)

map <- leaflet(incidenti) %>% 
  setView(lng = 11.25, lat = 43.783333, zoom = 14) %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addPolylines(data = ciclabili, color="darkgreen", group="Piste ciclabili") %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2011"),
                   popup=~make_labels(incidenti@data, anno2="2011"), group="2011", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2012"), 
                   popup=~make_labels(incidenti@data, anno="2012"), group="2012", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2013"), 
                   popup=~make_labels(incidenti@data, anno="2013"), group="2013", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2014"), 
                   popup=~make_labels(incidenti@data, anno="2014"), group="2014", radius=2) %>% 
  addCircleMarkers(data=subset(incidenti, ANNO=="2015"), 
                   popup=~make_labels(incidenti@data, anno="2015"), group="2015", radius=2) %>%
  addHeatmap(blur = 20, max = 0.05, radius = 15, group = "heatmap") %>% 
  addLayersControl(overlayGroups = c(levels(incidenti@data$ANNO),"heatmap","Piste ciclabili"),
                   options = layersControlOptions(collapsed = TRUE),
                   position = "topleft"
  ) 

# versione con icone per i veicoli coinvolti
# define a list of icons
icone_sinistri <- awesomeIconList(
  Autovettura = makeAwesomeIcon(icon = "car", library = "fa"),
  Autocarro = makeAwesomeIcon(icon = "truck", library = "fa"),
  Autobus = makeAwesomeIcon(icon = "bus-alt", library = "fa"),
  Bicicletta = makeAwesomeIcon(icon = "bicycle", library = "fa"),
  Motociclo = makeAwesomeIcon(icon = "motorcycle", library = "fa"),
  Ciclomotore = makeAwesomeIcon(icon = "motorcycle", library = "fa"),
  Ignoto = makeAwesomeIcon(icon = "question", library = "fa"),
  Altro = makeAwesomeIcon(icon = "ambulance", library = "fa"),
  Nessuno = makeAwesomeIcon(icon = "exclamation", library = "fa")
)

# prova awsome markers
leaflet(incidenti) %>% 
  setView(lng = 11.25, lat = 43.783333, zoom = 14) %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addPolylines(data = ciclabili, color="darkgreen", group="Piste ciclabili") %>% 
  addHeatmap(blur = 20, max = 0.05, radius = 15, group = "heatmap") %>% 
  addAwesomeMarkers(data = subset(incidenti, altro_veicolo_recoded=="Autovettura"), 
                    icon = ~ icone_sinistri["Autovettura"]) %>% 
  addLayersControl(overlayGroups = c("heatmap","Piste ciclabili"),
                   options = layersControlOptions(collapsed = TRUE),
                   position = "topleft"
  ) 


# layer diversi per tipi di veicoli coinvolti
incidenti_split <- split(incidenti, incidenti@data$altro_veicolo_recoded)
mappa <- leaflet(incidenti) %>% 
  setView(lng = 11.25, lat = 43.783333, zoom = 14) %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addPolylines(data = ciclabili, color="darkgreen", group="Piste ciclabili") %>% 
  addHeatmap(blur = 20, max = 0.05, radius = 15, group = "Densità")

names(incidenti_split) %>%
  purrr::walk( function(df) {
    mappa <<- mappa %>%
      addCircleMarkers(data=incidenti_split[[df]],
                       popup=~make_labels(incidenti_split[[df]]@data), 
                       group=df, radius=2) 
  })
mappa %>% 
  addLayersControl(
    overlayGroups = c(names(incidenti_split), "Densità","Piste ciclabili"),
    options = layersControlOptions(collapsed = TRUE),
    position = "topleft"
  ) 
# http://www.sir.toscana.it/index.php?IDS=2&IDSS=6
# prove bandwidth: this is the one used by ggplot 2d density
MASS::bandwidth.nrd(X[,1]) # 0.02836317  way larger than the one used in the leaflet map

# static map with ggmap ####
myLocation <- "Florence"
myMap <- get_map(location=myLocation, source="osm")
incidenti@bbox
geocode("Firenze")
myMap <- get_googlemap(center=c(lon = 11.25581, lat = 43.7695), zoom=13, maptype="roadmap")
#myMap <- get_openstreetmap(incidenti@bbox, urlonly = TRUE)

incidenti_df <- cbind(incidenti@data,incidenti@coords)
names(incidenti_df)[15:16] <- c("lon","lat")
ggmap(myMap)

glimpse(incidenti_df)
View(incidenti_df)

# basemap
mappeIncidenti <- ggmap(myMap,
                    base_layer = ggplot(aes(x = lon, y = lat),
                                        data = incidenti_df))

# version with stat_density (still need to be fixed)
mappeIncidenti +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level.., alpha = ..level..), # ,n=51, h=c(0.000801554, 0.000332227)
                 bins = 5, geom = "polygon", # bins = 5,
                 data = incidenti_df) +
  scale_fill_gradient(low = "black",
                      high= "red") +
  guides(alpha=FALSE) + 
  facet_wrap(~ ANNO) +
  xlab(NULL) + 
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

# versione 2: curve di livello
mappeIncidenti + 
  geom_point() +
  geom_density_2d(aes(alpha = ..level..)) + 
  # stat_density2d(aes(x = lon, y = lat,
  #                    fill = ..level.., alpha = ..level..), # ,n=51, h=c(0.000801554, 0.000332227)
  #                bins = 5, geom = "polygon", # bins = 5,
  #                data = incidenti_df) +
  # scale_fill_gradient(low = "black",
  #                     high= "red") +
  # guides(alpha=FALSE) + 
  facet_wrap(~ anno2) +
  xlab(NULL) + 
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

# versione 3: curve riempite
mappeIncidenti + 
  geom_point() +
  stat_density_2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), contour = TRUE, geom = "polygon") + 
  scale_fill_gradient(low = "black",
                      high= "red") +
  guides(alpha=FALSE) +
  facet_wrap(~ ANNO) +
  xlab(NULL) + 
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

# version with polygons computed separately: fix
mappeIncidenti +
  geom_polygon(aes(x=x, y=y, fill="red", alpha=(level)), data = contorni_heat) +
  facet_wrap(~ ANNO) +
  xlab(NULL) + 
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

# altre versioni,  cercando di levare i warnings x i 534 missinf values...
myMap2 <- get_googlemap(center=c(lon = 11.25581, lat = 43.7695), zoom=13, maptype="roadmap")
incidenti_df2 <- incidenti_df %>% select(anno2, lon, lat) # non deriva da questo però, forse dal livello di zoom che taglia fuori alcuni dati.  Si è così, i warnings dipendono dallo zoom
# 534 tagliati at zoom=14
# 86 tagliati at zoom = 13
# tutti at zoom = 12 o minore
mappeIncidenti2 <- ggmap(myMap2,
                         base_layer = ggplot(aes(x = lon, y = lat),
                                             data = incidenti_df2))
# removing the bandwith increase size, but what is level?
# OK
mappeIncidenti2 + geom_point() +
#  geom_density_2d(aes(alpha = ..level..)) + # solo contorni
  stat_density_2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon", h=c(0.01, 0.01)) +
  guides(alpha=FALSE) +
  facet_wrap(~ anno2) +
  xlab(NULL) + 
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

# using hexbin
mappeIncidenti2 + coord_equal() + geom_hex(aes(x = lon, y = lat))
# better version
mappeIncidenti2 + coord_fixed(ratio = 1.45) + 
#  geom_hex(aes(x = lon, y = lat, alpha=..count..), bins = 45) + # , alpha = 0.75
  geom_hex(aes(x = lon, y = lat), bins = 45, alpha = 0.75) + # 
#  guides(alpha=FALSE) +
#  scale_alpha(range = c(0.3,1)) +
  facet_wrap(~ anno2) +
  xlab(NULL) + 
  ylab(NULL) +  
  theme(axis.ticks = element_blank(), axis.text = element_blank())

# using other types of maps
# STAMEN terrain: very nice, bb can be reduced a bit, maybe increasing the zoom level (14 to 15) gives more datail
myMap3b <- get_stamenmap(bbox = c(11.20, 43.750, 11.30, 43.8), zoom=14, maptype="terrain")
# 137 dots removed (out of bb I guess)
mappeIncidenti3 <- ggmap(myMap3b,
                         base_layer = ggplot(aes(x = lon, y = lat),
                                             data = incidenti_df2))
mappeIncidenti3 + geom_point() +
  #  geom_density_2d(aes(alpha = ..level..)) + # solo contorni
  stat_density_2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon", h=c(0.01, 0.01)) +
  guides(alpha=FALSE, fill=FALSE) +
  facet_wrap(~ anno2) +
  xlab(NULL) + 
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

qmap("Florence",zoom=15, source="osm")
SpatialPolygons(CL)

contorni_heat <- bind_rows(lapply(CL,as.data.frame)) 
View(contorni_heat)
contorni_heat %>% distinct(level) %>% View()

# read cycle lanes shapefile
ciclabili <- readOGR("percorsi_ciclabili/", layer="percorsi_ciclabiliLine")
ciclabili@proj4string
ciclabili <- spTransform(ciclabili, CRS(latlong))



# Load the output area shapefiles (2015)
Output.Areas<- readOGR("incidenti_sap2015/", layer="incidenti_sap_2015")
str(Output.Areas)
class(Output.Areas)
Output.Areas@proj4string
Output.Areas@coords
plot(Output.Areas)

glimpse(Output.Areas@data)

View(Output.Areas@data)

bbox(Output.Areas)
# there's a bit of recoding to do.  All are factors, and 
str(Output.Areas@coords)

parse_guess(Output.Areas@data$DATA, na = c("", "NA"), locale = default_locale())

sjt.xtab(Output.Areas@data$TIPO_VA, Output.Areas@data$TIPO_VB)

# convert to projection for plotting points with leaflet (WSG84)
latlong = "+init=EPSG:4326" # AKA: WSG84 
SPtrans = spTransform(Output.Areas, CRS(latlong))

SPtrans@coords
glimpse(SPtrans@data)
# remove dupolicated columns
SPtrans@data <- SPtrans@data[,c(1:5,9,11:15)]

# labels
labels <- sprintf(
  "<strong>%s</strong><br/>%s, ore %s<br/><br/><i>Luogo:</i><br/>%s <br/><br/>Tra bici e %s",
  SPtrans@data$DATA,
  SPtrans@data$GIORNO,
  SPtrans@data$ORA,
  ifelse(is.na(SPtrans@data$CIVICO),str_c("Incrocio",SPtrans@data$STRADA, "con", SPtrans@data$STRADA_2, sep=" "), str_c(SPtrans@data$STRADA, ", ", SPtrans@data$CIVICO, sep="")),
  ifelse(SPtrans@data$TIPO_VA=="VELOCIPEDE", as.character(SPtrans@data$TIPO_VB), as.character(SPtrans@data$TIPO_VA))) %>% 
  lapply(htmltools::HTML)

leaflet(SPtrans) %>% 
  setView(lng = 11.25, lat = 43.783333, zoom = 14) %>% 
  addTiles() %>% 
  addMarkers(popup=~labels)

# prove colore
pie(rep(1,8), labels = c("00-03","03-06","06-09","09-12","12-15","15-18","18-21","21-24"), col=rainbow(8, start = 0.675, end = 0.5), clockwise = TRUE)
show_col(rainbow_hcl(8, start=0, c=70, l=50))
show_col(rainbow(8))
show_col(rainbow(4, start = 0.675, end = 0.175))

show_col(blue2yellow(4))
library(colorRamps)
display.brewer.pal(4, "YlGnBu")
brewer.pal.info["YlGnBu",]

# interactive using tmaps (still leaflet but differernt interface) ####
# set interactive mode on
tmap_mode("view")
# plot accidents location (no need to transform projection this time)
tm_shape(incidenti) + tm_dots() 
