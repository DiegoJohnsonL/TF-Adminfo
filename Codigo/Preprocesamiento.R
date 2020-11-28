#### Preprocesamiento

source(file="Configuracion.R")
#Read
positivos_covid_data = read_xlsx("../../Data/positivos_covid.xlsx")
fallecidos_covid_data = read.csv2("../../Data/fallecidos_covid.csv", encoding = "Latin-1",header = TRUE, na.strings=c(""," ","NA"))
fallecidos_sinadef = read.csv2("../../Data/fallecidos_sinadef.csv", encoding = "Latin-1", header = TRUE, na.strings=c(""," ","NA"))
positivos<-positivos_covid_data
fallecidos<- fallecidos_sinadef
fallecidos_covid <-fallecidos_covid_data
#View
View(positivos_covid_data)
View(fallecidos_covid_data)
View(fallecidos_sinadef)

#Positivos covid
positivos_covid_data$FECHA_CORTE<-NULL
positivos_covid_data$UUID<-NULL
fecha_temp<-strptime(positivos_covid_data$FECHA_RESULTADO,"%Y%m%d")
positivos_covid_data$FECHA_RESULTADO<-fecha_temp
glimpse(positivos_covid_data)

#Fallecidos covid
fallecidos_covid_data$FECHA_CORTE<-NULL
fallecidos_covid_data$UUID<-NULL
fecha_temp<-strptime(fallecidos_covid_data$FECHA_FALLECIMIENTO,"%Y%m%d")
fallecidos_covid_data$FECHA_FALLECIMIENTO <-fecha_temp
fecha_temp<-strptime(fallecidos_covid_data$FECHA_NAC,"%Y%m%d")
fallecidos_covid_data$FECHA_NAC <-fecha_temp
glimpse(fallecidos_covid_data)

#Fallecidos sinadef
fallecidos_sinadef$X......<-NULL
fallecidos_sinadef$X.2<-NULL
fallecidos_sinadef$X.1<-NULL
fallecidos_sinadef$X<-NULL
fallecidos_sinadef$COD..UBIGEO.DOMICILIO<-NULL
fallecidos_sinadef$CAUSA.A..CIE.X.<-NULL
fallecidos_sinadef$CAUSA.B..CIE.X.<-NULL
fallecidos_sinadef$CAUSA.C..CIE.X.<-NULL
fallecidos_sinadef$CAUSA.D..CIE.X.<-NULL
fallecidos_sinadef$CAUSA.E..CIE.X.<-NULL
fallecidos_sinadef$CAUSA.F..CIE.X.<-NULL
fallecidos_sinadef$AÑO<-NULL
fallecidos_sinadef$MES<-NULL
fecha_temp<-strptime(fallecidos_sinadef$FECHA,"%Y-%m-%d")
fallecidos_sinadef$FECHA <-fecha_temp
glimpse(fallecidos_sinadef)

#eliminando datos de sexo
fallecidos_covid_data <- fallecidos_covid_data[] %>% filter(SEXO == "MASCULINO" | SEXO == "FEMENINO")