

##    DATOS 

#Tabla Departamento
DEPARTAMENTO = c(fallecidos_covid_data$DEPARTAMENTO, positivos_covid_data$DEPARTAMENTO, fallecidos_sinadef$DEPARTAMENTO.DOMICILIO)
DEPARTAMENTO <- unique(DEPARTAMENTO)
#View(DEPARTAMENTO)
Id<-(1:(NROW(DEPARTAMENTO)))
DEPARTAMENTO = data.frame(DEPARTAMENTO, Id)
colnames(DEPARTAMENTO)[1]  <- "nombre"
View(DEPARTAMENTO)

#Tabla Provincia
PROVINCIA = c(fallecidos_covid_data$PROVINCIA, positivos_covid_data$PROVINCIA, fallecidos_sinadef$PROVINCIA.DOMICILIO)
PROVINCIA <- unique(PROVINCIA)
#View(PROVINCIA)
Id<-(1:(NROW(PROVINCIA)))
PROVINCIA = data.frame(PROVINCIA, Id)
colnames(PROVINCIA)[1]  <- "nombre"
View(PROVINCIA)

#Tabla distrito
DISTRITO = c(fallecidos_covid_data$DISTRITO, positivos_covid_data$DISTRITO, fallecidos_sinadef$DISTRITO.DOMICILIO)
DISTRITO <- unique(DISTRITO)
#View(DISTRITO)
Id<-(1:(NROW(DISTRITO)))
DISTRITO = data.frame(DISTRITO, Id)
colnames(DISTRITO)[1]  <- "nombre"
View(DISTRITO)

#Tabla pais
PAIS = c(fallecidos_sinadef$`PAIS.DOMICILIO`)
PAIS <- unique(PAIS)
#View(PAIS)
Id<-(1:(NROW(PAIS)))
PAIS = data.frame(PAIS, Id)
colnames(PAIS)[1]  <- "nombre"
View(PAIS)


#Tabla Sexo
sexos <-fallecidos_sinadef %>% group_by(`SEXO`) %>%  summarise()
#View(sexos)
sexo = c("MASCULINO", "FEMENINO", "INDETERMINADO", "SIN REGISTRO")
id <- c(1:NROW(sexo))
SEXO = data.frame(id, sexo)
View(SEXO)

#Tabla Metodo
metodos <- positivos_covid_data %>% group_by(`METODODX`) %>%  summarise()
nombre = metodos[,1]
colnames(nombre)[1]  <- "nombre"
id<-c(1:NROW(metodos))
METODO = data.frame(id, nombre)
View(METODO)

#Tabla Institucion
instituciones <- fallecidos_sinadef %>% group_by(`INSTITUCION`) %>%  summarise()
colnames(instituciones)[1]  <- "nombre"
id<-c(1:NROW(instituciones))
INSTITUCION = data.frame(id, instituciones)
View(INSTITUCION)

#Tabla Edad
#edades <- fallecidos_sinadef %>% group_by(`EDAD`, `TIEMPO EDAD`) %>%  summarise()
#id<-c(1:NROW(edades))
#EDAD = data.frame(id, edades)
#View(EDAD)

#Tabla Seguro
seguros <- fallecidos_sinadef %>% group_by(`TIPO.SEGURO`) %>%  summarise()
id<-c(1:NROW(seguros))
colnames(seguros)[1]  <- "tipo"
SEGURO = data.frame(id, seguros)
View(SEGURO)

#Tipo Lugar
LUGAR <- fallecidos_sinadef %>% group_by(`TIPO.LUGAR`) %>%  summarise()
id<-c(1:NROW(LUGAR))
colnames(LUGAR)[1]  <- "nombre"
LUGAR = data.frame(id, LUGAR)
View(LUGAR)

#Nivel Instruccion
instruccion <- fallecidos_sinadef %>% group_by(`NIVEL.DE.INSTRUCCIÓN`) %>%  summarise()
id<-c(1:NROW(instruccion))
colnames(instruccion)[1]  <- "nivel"
NIVEL_INSTRUCCION = data.frame(id, instruccion)
View(NIVEL_INSTRUCCION)

#Causa
causas <- c(fallecidos_sinadef$DEBIDO.A..CAUSA.A., fallecidos_sinadef$DEBIDO.A..CAUSA.B., fallecidos_sinadef$DEBIDO.A..CAUSA.C.,
            fallecidos_sinadef$DEBIDO.A..CAUSA.D., fallecidos_sinadef$DEBIDO.A..CAUSA.E., fallecidos_sinadef$DEBIDO.A..CAUSA.F.)
causas <- unique(causas)
causas <- gsub("Ó", "O", causas)
causas <- gsub("Á", "A", causas)
causas <- gsub("É", "E", causas)
causas <- gsub("Í", "I", causas)
causas <- gsub("Ú", "U", causas)
causas <- unique(causas)
id<-c(1:NROW(causas))
CAUSAS = data.frame(id, causas)
colnames(CAUSAS)[2]  <- "nombre"
View(CAUSAS) #130047


#Tabla Fallecido causa
FALLECIDO_id<- c(1:(NROW(fallecidos_sinadef)*6))
CAUSA_id<-FALLECIDO_id
FALLECIDO_CAUSA = data.frame(FALLECIDO_id, CAUSA_id)

fallecidos_sinadef[17:22] <- data.frame(lapply(fallecidos_sinadef[17:22], function(x) { gsub("Á", "A", x) }))
fallecidos_sinadef[17:22] <- data.frame(lapply(fallecidos_sinadef[17:22], function(x) { gsub("É", "E", x) }))
fallecidos_sinadef[17:22] <- data.frame(lapply(fallecidos_sinadef[17:22], function(x) { gsub("Í", "I", x) }))
fallecidos_sinadef[17:22] <- data.frame(lapply(fallecidos_sinadef[17:22], function(x) { gsub("Ó", "O", x) }))
fallecidos_sinadef[17:22] <- data.frame(lapply(fallecidos_sinadef[17:22], function(x) { gsub("Ú", "U", x) }))

indicesA<-match(fallecidos_sinadef$DEBIDO.A..CAUSA.A., CAUSAS$nombre)
indicesB<-match(fallecidos_sinadef$DEBIDO.A..CAUSA.B., CAUSAS$nombre)
indicesC<-match(fallecidos_sinadef$DEBIDO.A..CAUSA.C., CAUSAS$nombre)
indicesD<-match(fallecidos_sinadef$DEBIDO.A..CAUSA.D., CAUSAS$nombre)
indicesE<-match(fallecidos_sinadef$DEBIDO.A..CAUSA.E., CAUSAS$nombre)
indicesF<-match(fallecidos_sinadef$DEBIDO.A..CAUSA.F., CAUSAS$nombre)
#head(fallecidos_sinadef$`DEBIDO A (CAUSA A)`, 20)
#head(CAUSAS, 20)
#head(indicesA)
indices_total = c(indicesA, indicesB, indicesC, indicesD, indicesE, indicesF)
FALLECIDO_id = c(1:NROW(fallecidos_sinadef),1:NROW(fallecidos_sinadef),1:NROW(fallecidos_sinadef),1:NROW(fallecidos_sinadef),1:NROW(fallecidos_sinadef),1:NROW(fallecidos_sinadef))
#View(indices_total)
FALLECIDO_CAUSA$FALLECIDO_id = FALLECIDO_id
FALLECIDO_CAUSA$CAUSA_id = indices_total
View(FALLECIDO_CAUSA)
#test <- FALLECIDO_CAUSA %>% filter(FALLECIDO_id == 1)
#View(test)


#########   Tabla Fallecido Covid

##agregando sexo forenkey
indices<-match(fallecidos_covid_data$SEXO, SEXO$sexo)
#View(indices)
fallecidos_covid_data$SEXO_id = indices
fallecidos_covid_data<-fallecidos_covid_data[-c(3)]

##Agregando distrito forenkey
indices<-match(fallecidos_covid_data$DISTRITO, DISTRITO$nombre)
fallecidos_covid_data$DISTRITO_id = indices
#View(indices)
fallecidos_covid_data<-fallecidos_covid_data[-c(6)]

##Agregando Provincia forenkey
indices<-match(fallecidos_covid_data$PROVINCIA, PROVINCIA$nombre)
#View(indices)
fallecidos_covid_data$PROVINCIA_id = indices
fallecidos_covid_data<-fallecidos_covid_data[-c(5)]


##Agregando Departamento forenkey
indices<-match(fallecidos_covid_data$DEPARTAMENTO, DEPARTAMENTO$nombre)
#View(indices)
fallecidos_covid_data$DEPARTAMENTO_id = indices
fallecidos_covid_data<-fallecidos_covid_data[-c(4)]

#AGREANDO PK
fallecidos_covid_data$id = c(1:(NROW(fallecidos_covid_data)))
colnames(fallecidos_covid_data)[3]  <- "fecha_nacimiento"
colnames(fallecidos_covid_data)[2]  <- "edad_declarada"
colnames(fallecidos_covid_data)[1]  <- "fecha_fallecimiento"


View(fallecidos_covid_data)


#######     Tabla Positivo Covid

#agregando sexo forenkey 
indices<-match(positivos_covid_data$SEXO, SEXO$sexo)
#View(indices)
positivos_covid_data$SEXO_id = indices
positivos_covid_data<-positivos_covid_data[-c(6)]

#Agregando distrito forenkey
indices<-match(positivos_covid_data$DISTRITO, DISTRITO$nombre)
positivos_covid_data$DISTRITO_id = indices
#View(indices)
positivos_covid_data<-positivos_covid_data[-c(3)]

#Agregando Provincia forenkey
indices<-match(positivos_covid_data$PROVINCIA, PROVINCIA$nombre)
#View(indices)
positivos_covid_data$PROVINCIA_id = indices
positivos_covid_data<-positivos_covid_data[-c(2)]

#Agregando Departamento forenkey
indices<-match(positivos_covid_data$DEPARTAMENTO, DEPARTAMENTO$nombre)
#View(indices)
positivos_covid_data$DEPARTAMENTO_id = indices
positivos_covid_data<-positivos_covid_data[-c(1)]

#Agregando Metodo forenkey
indices<-match(positivos_covid_data$METODODX, METODO$nombre)
#View(indices)
positivos_covid_data$METODO_id = indices
positivos_covid_data<-positivos_covid_data[-c(1)]

positivos_covid_data$id = c(1:(NROW(positivos_covid_data)))
colnames(positivos_covid_data)[1]  <- "edad"
colnames(positivos_covid_data)[2]  <- "fecha_resultado"


View(positivos_covid_data)

########### Tabla Fallecido Sinadef

#agregando sexo forenkey 
indices<-match(fallecidos_sinadef$SEXO, SEXO$sexo)
#View(indices)
fallecidos_sinadef$SEXO_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(3)]

#Agregando distrito forenkey
indices<-match(fallecidos_sinadef$`DISTRITO.DOMICILIO`, DISTRITO$nombre)
#View(indices)
fallecidos_sinadef$DISTRITO_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(10)]

#Agregando Provincia forenkey
indices<-match(fallecidos_sinadef$`PROVINCIA.DOMICILIO`, PROVINCIA$nombre)
#View(indices)
fallecidos_sinadef$PROVINCIA_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(9)]


#Agregando Departamento forenkey
indices<-match(fallecidos_sinadef$DEPARTAMENTO.DOMICILIO, DEPARTAMENTO$nombre)
#View(indices)
fallecidos_sinadef$DEPARTAMENTO_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(8)]

#Agregando SEGURO forenkey
indices<-match(fallecidos_sinadef$`TIPO.SEGURO`, SEGURO$tipo)
#View(indices)
fallecidos_sinadef$SEGURO_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(2)]

#Agregando NIVEL INSTRUCCION forenkey
indices<-match(fallecidos_sinadef$NIVEL.DE.INSTRUCCIÓN, NIVEL_INSTRUCCION$nivel)
#View(indices)
fallecidos_sinadef$NIVEL_INSTRUCCION_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(5)]

#Agregando PAIS forenkey
indices<-match(fallecidos_sinadef$`PAIS.DOMICILIO`, PAIS$nombre)
#View(indices)
fallecidos_sinadef$PAIS_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(5)]

#Agregando LUGAR forenkey
indices<-match(fallecidos_sinadef$`TIPO.LUGAR`, LUGAR$nombre)
#View(indices)
fallecidos_sinadef$LUGAR_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(6)]

#Agregando NIVEL INSTITUCION forenkey
indices<-match(fallecidos_sinadef$INSTITUCION, INSTITUCION$nombre)
#View(indices)
fallecidos_sinadef$INSTITUCION_id = indices
fallecidos_sinadef<-fallecidos_sinadef[-c(6)]

##Eliminando ultimas columnas en fallecidos
fallecidos_sinadef <- fallecidos_sinadef[-c(8:13)]
colnames(fallecidos_sinadef)[1]  <- "id"
colnames(fallecidos_sinadef)[2]  <- "edad"
colnames(fallecidos_sinadef)[3]  <- "tiempo_edad"
colnames(fallecidos_sinadef)[4]  <- "estado_civil"
colnames(fallecidos_sinadef)[5]  <- "fecha"
colnames(fallecidos_sinadef)[6]  <- "muerte_violenta"
colnames(fallecidos_sinadef)[7]  <- "necropsia"

View(fallecidos_sinadef)

#parametros de la conexion
driver=MySQL()
host = "35.174.139.24"
port = 3306
user="admin"
password="admin2020"
dbname="COVID_DB"

#2.	Crear conexion a la base de datos
dbCanConnect(drv=driver,host=host,port=port,username=user,password=password,dbname=dbname)
conexion<-dbConnect(drv=driver,host=host,port=port,username=user,password=password,dbname=dbname)
dbIsValid(conexion)
DBI::dbSendQuery(conexion, "SET SESSION WAIT_TIMEOUT=9999999")
DBI::dbSendQuery(conexion, "SET SESSION MAX_EXECUTION_TIME=9999999")
#dbDisconnect(conexion)
#dbSendQuery(conexion, "SET GLOBAL local_infile = true")


####        Creando tablas en nuestro servidor

#fallecidos_sinadef = as.data.frame(fallecidos_sinadef)
#class(fallecidos_sinadef)
dbWriteTable(conexion,"PAIS", PAIS)
dbWriteTable(conexion,"DISTRITO",DISTRITO)
dbWriteTable(conexion,"PROVINCIA",PROVINCIA)
dbWriteTable(conexion,"DEPARTAMENTO",DEPARTAMENTO)
dbWriteTable(conexion,"FALLECIDO_COVID", fallecidos_covid_data)
dbWriteTable(conexion,"POSITIVO_COVID", positivos_covid_data)
dbWriteTable(conexion,"METODO", METODO)
dbWriteTable(conexion,"SEXO", SEXO)
dbWriteTable(conexion,"INSTITUCION", INSTITUCION)
dbWriteTable(conexion,"FALLECIDO_SINADEF", fallecidos_sinadef)
dbWriteTable(conexion,"SEGURO", SEGURO)
dbWriteTable(conexion,"LUGAR", LUGAR)
dbWriteTable(conexion,"NIVEL_INSTRUCCION", NIVEL_INSTRUCCION)
dbWriteTable(conexion,"FALLECIDO_CAUSA", FALLECIDO_CAUSA)
#dbRemoveTable(conexion,"FALLECIDO_SINADEF")
dbWriteTable(conexion,"CAUSA", CAUSAS)



###           Serialización

DB<-list()

#Nombres de las tablas
tablas<-dbListTables(conexion)
tablas

#guardar cada tabla en una lista en memoria
for(i in tables){
  cat(i)
  BD[[i]]<- dbReadTable(conexion, i)
}

names(BD[[1]])
#Back de la lista de data.sets que descargamos de nuestra base de datos
save(DB, file = "Backup.RData")

#Cargar los archivos en el backup
load(file = "Backup.RData")




