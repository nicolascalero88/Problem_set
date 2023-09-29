#Nombre: Nicola Calero
#Código: 202122051
#Version: R 4.3.1
#instalacion de las librerias de apoyo para correr el modelo
{
  require("readxl"); require("dplyr");require("stringr")
}
#file.choose()
#punto 1
#Importacion de la base de Módulo sitio o ubicación
{
  location_1<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.1")
  location_2<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.2")
  location_3<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.3")
  location_4<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.4")
  location_5<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.5")
  location_6<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.6")
  location_7<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.7")
  location_8<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "D.8")
  #Unificacion de las bases de datos del Módulo sitio o ubicación
  location<-rbind(location_1,location_2,location_3,location_4,location_5,location_6,location_7,location_8)
}
#punto 2
#Importacion de la base de Módulo de identificacion
{
  identification_1<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "B.1")
  identification_2<-read_excel("C:/Users/Julia/Desktop/NicolasCalero/Anexo_total_nacional_2021.xlsx", sheet = "B.2")
  
  #Unificacion de las bases de datos del Módulo de identificacion
  identification<-rbind(identification_1,identification_2)
}
#Exportacion de la base de datos a formato RDS
{
  saveRDS(location, file = "C:/Users/Julia/Desktop/NicolasCalero/location.rds")
  saveRDS(identification, file = "C:/Users/Julia/Desktop/NicolasCalero/identification.rds")
}
#importacion de la base de datos modificada de formato RDS
{
  identification <- readRDS("C:/Users/Julia/Desktop/NicolasCalero/identification.rds")
  location <- readRDS("C:/Users/Julia/Desktop/NicolasCalero/location.rds")
}
#punto 3
# Creamos la función que asigna valores numéricos a las palabras clave a nuestra columna de "Encuesta de micronegocios"
{
  assign_business_type <- function(text) {
    if (grepl("Agricultura", text, ignore.case = TRUE)) {
      return(1)
    } else if (grepl("Industria manufacturera", text, ignore.case = TRUE)) {
      return(2)
    } else if (grepl("Comercio", text, ignore.case = TRUE)) {
      return(3)
    } else if (grepl("Servicios", text, ignore.case = TRUE)) {
      return(4)
    } else {
      return(NA)
    }
  }
}
# Aplica la función a cada fila de la primera columna de la base "identification y creamos la columna "business_type"
{
  identification$business_type <- apply(identification[, 1, drop = FALSE], 1, assign_business_type)
}

# Identificacion del codigo P3053 por medio de la identidicacion de D.6 y D.7 que corresponen a las respuestas de ser 6 o 7 
#creacion de la funcion para buscar los valore de D.6 y D.7
{
  local <- function(text) {
    if (grepl("D.6", text, ignore.case = TRUE)) {
      return(1)} 
    else if (grepl("D.7", text, ignore.case = TRUE)) {
      return(1)
    } else {
      return(0)
    }
  }
}
#mostrar los resultados de la busqueda en location y creacion de la nueva variable
{
  location$local <- apply(location[, 1, drop = FALSE], 1, local)
}
#punto 4
{
  #4.1-nueva base de datos con los valos de industria manufacturera de identification
  {
    identification_sub <- identification %>%
      filter(business_type == 2)
  }
  #4.2 tomar las variables de la base "location" c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA","P35","P241","P3032_1","P3032_2","P3032_3","P3033","P3034")
  location_sub <- location[c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA","P35","P241","P3032_1","P3032_2","P3032_3","P3033","P3034")]
  #4.3 tomar las variables de la base "identification_sub" c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA","P35","P241","P3032_1","P3032_2","P3032_3","P3033","P3034")
  identification_sub <- identification_sub[c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA","P35","P241","P3032_1","P3032_2","P3032_3","P3033","P3034")]
}
#ejemplo si existieran las variables 
{
  location_sub <- location[c(1,2)]
  identification_sub <- identification_sub[c(1,2)]
  nueva_base_estudio <- merge(location_sub, identification_sub,all = TRUE)
}

#5. unificacion de las bases de datos location_sub y identification_sub para crear la nueva base de estudio 
{
  nueva_base_estudio <- merge(location_sub, identification_sub, 
                              by.x = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"), 
                              by.y = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"), 
                              all = TRUE)
}


