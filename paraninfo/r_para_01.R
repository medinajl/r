#############################################################################
#
# fichero: r_para_1.r
#
# Análisis del nivel de ocupación de la planta +1 de la biblioteca del
# Paraninfo de la UC.
#
# autor: Mario Mañana
# fecha: 5/2/2017
# ultima revision: 5/2/2017
#

# Acceso al JDK. Necesario para acceder a ficheros excel
options(java.home="C:\\Program Files\\Java\\jdk1.8.0_92\\")
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_92\\")

# Paquetes necesarios para el análisis
library(XLConnect)
library(ggplot2)
library(extrafont)
library(pracma)
library(lubridate)
library(rJava)


# Ruta absoluta al fichero de registros
fichero <- "C:/mario/docencia/reproducible_research/r/paraninfo/para_01.xlsx"
# Título de la gráfica
titulo <- "sábado 28/5"
# Fila de inicio de datos
fila_inicio <- 7
# Fila de fin de datos
fila_fin <- 3807

# Hoja de análisis
hoja <- 1
datos <- readWorksheetFromFile( fichero, sheet=hoja, startRow=fila_inicio, startCol=1, endRow=fila_fin, endCol=18, header=TRUE)

Fecha<-datos$Fecha
Hora<-datos$Hora
entradasalida=datos$Col6
listadni=datos$Col17
evento=datos$Evento
N=length( entradasalida)
temporalesDNI <- rep( NA, N)
evo <- rep( NA, N)
x <- as.POSIXct(rep(NA, N))
x_h <- as.POSIXct(rep(NA, N))
entradas <- rep( NA, N)
salidas <- rep( NA, N)

entradas=0
salidas=0
neto=0
otros=0
indexDNI=0
for (h in 1:N){
  
  diah<-Fecha[h]
  horah<-Hora[h]
  fechatotal<-paste(diah,horah, sep=" ")
  horatotal<-paste("1/1/71",horah, sep=" ")
  x[h]<-as.POSIXct( fechatotal, format = "%d/%m/%y %H:%M:%S")
  x_h[h]<-as.POSIXct( horatotal, format = "%d/%m/%y %H:%M:%S")
  
  ES=entradasalida[h]
  EVE=evento[h]
  DNI=listadni[h]
  if( strcmp( ES, 'Entrada Derecha Planta -1')==TRUE ){  # entrada
    if( (DNI %in% temporalesDNI)==TRUE){
      print(DNI)
      f <- match( DNI, temporalesDNI)
      temporalesDNI[f] <- 'x'
      evo[h] <- neto
    }
    else{
      neto <- neto+1
      evo[h] <- neto
      entradas <- entradas+1    
    }
  }
  else if( strcmp( ES, 'Entrada Izquierda Planta -1')==TRUE ){ # entrada
    if( (DNI %in% temporalesDNI)==TRUE){
      print(DNI)
      f <- match( DNI, temporalesDNI)
      temporalesDNI[f] <- 'x'
      evo[h] <- neto
    }
    else{
      neto <- neto+1
      evo[h] <- neto
      entradas <- entradas+1    
    }
  }
  else if( strcmp( ES, 'Salida Derecha Planta -1')==TRUE ){ # salida
    if( strcmp( EVE, 'Salida Temporal')==TRUE){
      indexDNI<-indexDNI+1
      temporalesDNI[indexDNI]=DNI
      evo[h] <- neto
    }
    else{
      
      if( (DNI %in% temporalesDNI)==TRUE){
        print(DNI)
        f <- match( DNI, temporalesDNI)
        temporalesDNI[f] <- 'x'
      }

      neto <- neto-1
      evo[h] <- neto
      salidas <- salidas+1
    }
  }
  else if( strcmp( ES, 'Salida Izquierda Planta -1')==TRUE ){ # salida
     if( strcmp( EVE, 'Salida Temporal')==TRUE){
      indexDNI<-indexDNI+1
      temporalesDNI[indexDNI]=DNI
      evo[h] <- neto
    }
    else{

      if( (DNI %in% temporalesDNI)==TRUE){
        print(DNI)
        f <- match( DNI, temporalesDNI)
        temporalesDNI[f] <- 'x'
      }
      
      neto <- neto-1
      evo[h] <- neto
      salidas <- salidas+1
    }
  }
  else{
    evo[h] <- neto
    otros <- otros+1
  }
}


# Muestra el porcentaje horario de ocupación considerando 180 plazas
evo = evo*100.0/180.0
md <- data.frame( tiempo=x, tiempo_h=x_h, ocupacion=evo)
require(ggplot2)
require(scales)
theme_set(theme_bw()) # Define formato de la gráfica
ggplot(aes(x = tiempo, y = ocupacion), data = md) + 
  geom_line() + 
  xlab("hora") +
  ylab("Porcentaje de ocupación Planta -1 [%]") + 
  ggtitle(titulo)



