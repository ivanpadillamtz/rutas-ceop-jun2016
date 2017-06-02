
library(rgdal)
library(plotKML)

agebs_edoMex_rural <- readOGR("agebs_mex", "mex_ageb_rural")
agebs_edoMex_urban <- readOGR("agebs_mex", "mex_ageb_urbana")
agebs_df_urban <- readOGR("agebs_df", "df_ageb_urbana")
agebs_df_rural <- readOGR("agebs_df", "df_ageb_rural")

poligonos_mex_rural <- polygons(agebs_edoMex_rural)
poligonos_mex_urban <- polygons(agebs_edoMex_urban)
poligonos_df_rural <- polygons(agebs_df_rural)
poligonos_df_urban <- polygons(agebs_df_urban)

muestra <- read.table("agebs_mexYdf_numEnc.txt", header = TRUE)
ageb_urban_mex_data <- agebs_edoMex_urban@data
ageb_rural_mex_data <- agebs_edoMex_rural@data
ageb_urban_df_data <- agebs_df_urban@data
ageb_rural_df_data <- agebs_df_rural@data

valoresMexUrb<-NULL
valoresMexRur<-NULL
valoresDFUrb<-NULL
valoresDFRur<-NULL

tablas<-NULL

kml_open("VC01JUN.kml")

for(j in 232:271){#Se cuenta hasta donde sean valores par atenco o el minucipio en cuestion#
  num_enc <- as.numeric(muestra[j,2])
  esta<-FALSE
  
  if(esta==FALSE){
    #Revisa en las AGEBS urbanas EDOMEX#
    lonag <- length(ageb_urban_mex_data$CVEGEO)
    for(i in 1:lonag){
      ageb<-as.character(ageb_urban_mex_data[i,1])
      
      if(muestra[j,1]==ageb){
        esta<-TRUE
        ag_id<-i
        valoresMexUrb<-c(valoresMexUrb, ag_id)
        break
      }
    }
    #Revisa en las AGEBS urbanas EDOMEX#
  }
  
  if(esta==FALSE){
    #Revisa en las AGEBS rurales EDOMEX#
    lonag <- length(ageb_rural_mex_data$CVEGEO)
    for(i in 1:lonag){
      ageb<-as.character(ageb_rural_mex_data[i,1])
      
      if(muestra[j,1]==ageb){
        esta<-TRUE
        ag_id<-i
        valoresMexRur<-c(valoresMexRur, ag_id)
        break
      }
    }
    #Revisa en las AGEBS rurales EDOMEX#
  }
  
  if(esta==FALSE){
    #Revisa en las AGEBS urbanas CDMX#
    lonag <- length(ageb_urban_df_data$CVEGEO)
    for(i in 1:lonag){
      ageb<-as.character(ageb_urban_df_data[i,1])
      
      if(muestra[j,1]==ageb){
        esta<-TRUE
        ag_id<-i
        valoresDFUrb<-c(valoresDFUrb, ag_id)
        break
      }
    }
    #Revisa en las AGEBS urbanas CDMX#
  }
  
  if(esta==FALSE){
    #Revisa en las AGEBS rurales CDMX#
    lonag <- length(ageb_rural_df_data$CVEGEO)
    for(i in 1:lonag){
      ageb<-as.character(ageb_rural_df_data[i,1])
      
      if(muestra[j,1]==ageb){
        esta<-TRUE
        ag_id<-i
        valoresDFRur<-c(valoresDFRur, ag_id)
        break
      }
    }
    #Revisa en las AGEBS rurales CDMX#
  }
  
  if(esta){
    print("Se encontro.")
    print(paste("AGEB: ",muestra[j,1]))
    print(paste("Con ID: ",ag_id))
    
    df<-data.frame(ageb, num_enc)
    tablas<-c(tablas,kml_description(df, caption = "NUM_D_ENC",asText = TRUE))
  }else{
    print("No se encontro.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    print(paste("AGEB: ",muestra[j,1]))
  }
}

if(is.null(valoresMexUrb)==FALSE){
  poligonosMexUrb<-poligonos_mex_urban[valoresMexUrb]  
  proj4string(poligonosMexUrb) <- NA_character_
  proj4string(poligonosMexUrb)<-CRS("+proj=longlat +datum=WGS84")
  kml_layer.SpatialPolygons(poligonosMexUrb, colour="blue", labels="",plot.labpt = TRUE, html.table = tablas)
}

if(is.null(valoresMexRur)==FALSE){
  poligonosMexRur<-poligonos_mex_rural[valoresMexRur]  
  proj4string(poligonosMexRur) <- NA_character_
  proj4string(poligonosMexRur)<-CRS("+proj=longlat +datum=WGS84")
  kml_layer.SpatialPolygons(poligonosMexRur, colour="blue", labels="",plot.labpt = TRUE, html.table = tablas)
}

if(is.null(valoresDFUrb)==FALSE){
  poligonosDFUrb<-poligonos_df_urban[valoresDFUrb]
  proj4string(poligonosDFUrb) <- NA_character_
  proj4string(poligonosDFUrb)<-CRS("+proj=longlat +datum=WGS84")
  kml_layer.SpatialPolygons(poligonosDFUrb, colour="blue", labels="",plot.labpt = TRUE, html.table = tablas)
}

if(is.null(valoresDFRur)==FALSE){
  poligonosDFRur<-poligonos_df_rural[valoresDFRur]
  proj4string(poligonosDFRur) <- NA_character_
  proj4string(poligonosDFRur)<-CRS("+proj=longlat +datum=WGS84")
  kml_layer.SpatialPolygons(poligonosDFRur, colour="blue", labels="",plot.labpt = TRUE, html.table = tablas)
}

kml_close("VC01JUN.kml")