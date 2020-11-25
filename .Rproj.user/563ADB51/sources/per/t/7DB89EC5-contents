source(file="Codigo/Configuracion.R")

#Read
positivos_covid_data = read_xlsx("Data/positivos_covid.xlsx")
fallecidos_covid_data = read.csv2("Data/fallecidos_covid.csv", encoding = "Latin-1",header = TRUE, na.strings=c(""," ","NA"))
fallecidos_sinadef = read.csv2("Data/fallecidos_sinadef.csv", encoding = "Latin-1", header = TRUE, na.strings=c(""," ","NA"))
#View
View(positivos_covid_data)
View(fallecidos_covid_data)
View(fallecidos_sinadef)

#### Preprocesamiento

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


####             QUERIES 


dbListTables(conexion)
dbListFields(conexion, "FALLECIDO_SINADEF")
dbListFields(conexion, "CAUSA")

#                Query 1 - Cantidad de muertes por covid en Lima Departamento segun la informacion de FALLECIDOS_COVID

q1<-dbGetQuery(conexion, "select DEPARTAMENTO.nombre, FALLECIDO_COVID.id from FALLECIDO_COVID
               INNER JOIN DEPARTAMENTO ON FALLECIDO_COVID.DEPARTAMENTO_id=DEPARTAMENTO.id
               where DEPARTAMENTO.nombre LIKE 'LIMA'")
NROW(q1)
View(q1)

#              Query 2 - Cantidad de muertes por cancer por Departamento segun la informacion / TABLA FALLECIDOS SINADEF
dbListFields(conexion, "DEPARTAMENTO")
q2 = dbGetQuery(conexion, "select FALLECIDO_SINADEF.DEPARTAMENTO_id as id, COUNT(FALLECIDO_CAUSA.CAUSA_ID) as cantidad_fallecidos from CAUSA
                 JOIN FALLECIDO_CAUSA on FALLECIDO_CAUSA.CAUSA_ID = CAUSA.id
                 JOIN FALLECIDO_SINADEF on FALLECIDO_SINADEF.id = FALLECIDO_CAUSA.FALLECIDO_id
                 where CAUSA.nombre like '%CANCER%' 
                 group by FALLECIDO_SINADEF.DEPARTAMENTO_id")
q2_2 = dbGetQuery(conexion, "select id, nombre from DEPARTAMENTO")
q2 <- q2 %>% inner_join(q2_2)
View(q2)

#                Query 3 - Muertos por cancer del departamento de Lima / TABLA FALLECIDOS SINADEF
dbListFields(conexion, "DEPARTAMENTO")
q3 = q2 %>% filter(nombre == 'LIMA')
View(q3) 


#                Query 4 - CANTIDAD DE FALLECIDOS POR COVID SEGUN CADA DEPARTAMENTO/ TABLA FALLECIDOS_COVID
dbListFields(conexion, "DEPARTAMENTO")
q4 = dbGetQuery(conexion, "select DEPARTAMENTO.nombre, COUNT(FALLECIDO_COVID.id) from FALLECIDO_COVID 
                JOIN DEPARTAMENTO ON FALLECIDO_COVID.DEPARTAMENTO_id = DEPARTAMENTO.id
                GROUP BY DEPARTAMENTO.nombre")
View(q4) 

#                Query 5 - CANTIDAD DE FALLECIDOS POR COVID SEGUN CADA ESTADO CIVIL/ TABLA FALLECIDOS_SINADEF
dbListFields(conexion, "FALLECIDO_SINADEF")
q5 = dbGetQuery(conexion, "select estado_civil, COUNT(id) as cantidad_fallecidos from FALLECIDO_SINADEF 
                GROUP BY estado_civil")
View(q5) 

#                Query 6 - CANTIDAD DE FALLECIDOS POR COVID SEGUN CADA SEXO/ TABLA FALLECIDOS_SINADEF
dbListFields(conexion, "FALLECIDO_SINADEF")
q6 = dbGetQuery(conexion, "select SEXO.sexo, COUNT(FALLECIDO_SINADEF.id) as cantidad_fallecidos from FALLECIDO_SINADEF
                INNER JOIN SEXO ON SEXO.id = FALLECIDO_SINADEF.SEXO_id GROUP BY SEXO.sexo")
View(q6) 

#                Query 7 - CANTIDAD DE FALLECIDOS DEL 2017 AL 2020 / TABLA FALLECIDOS_SINADEF
dbListFields(conexion, "FALLECIDO_SINADEF")
q7 = dbGetQuery(conexion, "select YEAR(FALLECIDO_SINADEF.fecha) as fecha, COUNT(id) as cantidad_fallecidos from FALLECIDO_SINADEF 
                GROUP BY YEAR(FALLECIDO_SINADEF.fecha)")
View(q7) 

#                Query 8 - CANTIDAD DE FALLECIDOS POR MES / TABLA FALLECIDOS_COVID
dbListFields(conexion, "FALLECIDO_COVID")
q8 = dbGetQuery(conexion, "select MONTH(FALLECIDO_COVID.fecha_fallecimiento) as Mes ,COUNT(id) as cantidad_fallecidos from FALLECIDO_COVID 
                GROUP BY MONTH(FALLECIDO_COVID.fecha_fallecimiento)")
View(q8) 

#                Query 9 - CANTIDAD DE RESULTADOS POSITIVOS DE COVID SEGUN SEXO / TABLA POSITIVO_COVID
dbListFields(conexion, "POSITIVO_COVID")
q9 = dbGetQuery(conexion, "select SEXO.sexo, COUNT(POSITIVO_COVID.id) as cantidad_positivos from POSITIVO_COVID
                JOIN SEXO on SEXO.id = POSITIVO_COVID.SEXO_id
                group by SEXO.sexo")
View(q9) 

#                Query 10 - CANTIDAD DE FALLECIDOS DEL MES DE JUNIO LA PROVINCIA DE LLIMA / TABLA FALLECIDOS_COVID
dbListFields(conexion, "FALLECIDO_COVID")
lima_provincia_id = dbGetQuery(conexion, "select id from PROVINCIA WHERE nombre = 'Lima'")
lima_provincia_id
q10 = dbGetQuery(conexion, "SELECT SEXO.sexo, FALLECIDO_COVID.id, FALLECIDO_COVID.edad_declarada from FALLECIDO_COVID
                 JOIN SEXO ON SEXO.id = FALLECIDO_COVID.SEXO_id
                 JOIN PROVINCIA ON PROVINCIA.id = FALLECIDO_COVID.PROVINCIA_id
                 WHERE PROVINCIA.id = 1 AND MONTH(FALLECIDO_COVID.fecha_fallecimiento) = '6'")
View(q10) 

#                Query 11 - CANTIDAD DE FALLECIDOS POR COVID QUE NO FUERON EN EL DEPARTAMENTO DE LIMA, EL MES DE AGOSTO/ TABLA FALLECIDOS_COVID
dbListFields(conexion, "FALLECIDO_COVID")
q11 = dbGetQuery(conexion, "select DEPARTAMENTO.nombre, COUNT(FALLECIDO_COVID.id) from FALLECIDO_COVID
                join DEPARTAMENTO on FALLECIDO_COVID.DEPARTAMENTO_id = DEPARTAMENTO.id
                where DEPARTAMENTO.nombre != 'LIMA' AND YEAR(FALLECIDO_COVID.fecha_fallecimiento) = '2020'
                AND MONTH(FALLECIDO_COVID.fecha_fallecimiento) = '8'
                group by DEPARTAMENTO.nombre")
View(q11) 

#                Query 12 - CANTIDAD DE FALLECIDOS EN GENERAL QUE NO FUERON EN EL DEPARTAMENTO DE LIMA / TABLA FALLLECIDO_SINADEF
dbListFields(conexion, "FALLECIDO_SINADEF")
q12 = dbGetQuery(conexion, "select DEPARTAMENTO.nombre, COUNT(FALLECIDO_SINADEF.id) from FALLECIDO_SINADEF
                join DEPARTAMENTO on FALLECIDO_SINADEF.DEPARTAMENTO_id = DEPARTAMENTO.id
                where DEPARTAMENTO.nombre != 'LIMA' AND YEAR(FALLECIDO_SINADEF.fecha) = '2020'
                AND MONTH(FALLECIDO_SINADEF.fecha) = '8'
                group by DEPARTAMENTO.nombre")
View(q12) 

#                Query 13 - FALLECIDOS POR COVID DEL  DEPARTAMENTO DE LIMA EN EL MEZ DE JULIO 
dbListFields(conexion, "FALLECIDO_COVID")
q13 = dbGetQuery(conexion, "select DISTRITO.nombre, COUNT(FALLECIDO_COVID.id) from FALLECIDO_COVID
                JOIN DEPARTAMENTO on FALLECIDO_COVID.DEPARTAMENTO_id = DEPARTAMENTO.id
                JOIN DISTRITO ON FALLECIDO_COVID.DISTRITO_id = DISTRITO.id
                where DEPARTAMENTO.nombre = 'LIMA' AND YEAR(FALLECIDO_COVID.fecha_fallecimiento) = '2020'
                AND MONTH(FALLECIDO_COVID.fecha_fallecimiento) = '8'
                group by DISTRITO.nombre")
View(q13) 

#                Query 14 - POSITIVOS COVID DEL DEPARTAMENTO DE LIMA EN EL MEZ DE JULIO 
dbListFields(conexion, "POSITIVO_COVID")
q14 = dbGetQuery(conexion, "select DISTRITO.nombre, COUNT(POSITIVO_COVID.id) from POSITIVO_COVID
                JOIN DEPARTAMENTO on POSITIVO_COVID.DEPARTAMENTO_id = DEPARTAMENTO.id
                JOIN DISTRITO ON POSITIVO_COVID.DISTRITO_id = DISTRITO.id
                where DEPARTAMENTO.nombre = 'LIMA' AND YEAR(POSITIVO_COVID.fecha_resultado) = '2020'
                AND MONTH(POSITIVO_COVID.fecha_resultado) = '8'
                group by DISTRITO.nombre")
View(q14) 

#                Query 15 Tabla de cantidad de resultados positivod x cantidad muertos segun distritos en Lima

q15<-q13 %>% inner_join(q14)
View(q15)

#                Query 16 - CANTIDAD DE FALLECIDOS EN EL 2020 POR CANCER 
dbListFields(conexion, "CAUSA")
q16 = dbGetQuery(conexion, "select CAUSA.nombre, COUNT(FALLECIDO_CAUSA.FALLECIDO_id) as cantidad_fallecidos from CAUSA
                 JOIN FALLECIDO_CAUSA on FALLECIDO_CAUSA.CAUSA_ID = CAUSA.id
                 where CAUSA.nombre like '%CANCER%' 
                 group by CAUSA.nombre")
View(q16) 

#                Query 17 - CANTIDAD DE FALLECIDOS POR CANCER / TABLA FALLECIDO SINADEF 
dbListFields(conexion, "FALLECIDO_CAUSA")
q17 = dbGetQuery(conexion, "select CAUSA.nombre, COUNT(FALLECIDO_CAUSA.FALLECIDO_id) FROM CAUSA 
                 JOIN FALLECIDO_CAUSA ON FALLECIDO_CAUSA.CAUSA_ID = CAUSA.id
                 WHERE CAUSA.nombre LIKE '%CANCER%'
                 GROUP BY CAUSA.nombre")
View(q17) 

#                Query 18 - CANTIDAD DE FALLECIDOS POR COVID / TABLA FALLECIDO SINADEF 
dbListFields(conexion, "FALLECIDO_CAUSA")
q18 = dbGetQuery(conexion, "select CAUSA.nombre, COUNT(FALLECIDO_CAUSA.FALLECIDO_id) FROM CAUSA 
                 JOIN FALLECIDO_CAUSA ON FALLECIDO_CAUSA.CAUSA_ID = CAUSA.id
                 WHERE CAUSA.nombre LIKE '%COVID%'
                 GROUP BY CAUSA.nombre")
View(q18) 

#                Query 19 -  Numero de necropsias realizadas y no realizadas a fallecidos por covid/ TABLA FALLECIDO SINADEF 

dbListFields(conexion, "FALLECIDO_SINADEF")
q19 = dbGetQuery(conexion, "select FALLECIDO_SINADEF.necropsia as id, COUNT(FALLECIDO_CAUSA.CAUSA_ID) as cantidad_fallecidos from CAUSA
                 JOIN FALLECIDO_CAUSA on FALLECIDO_CAUSA.CAUSA_ID = CAUSA.id
                 JOIN FALLECIDO_SINADEF on FALLECIDO_SINADEF.id = FALLECIDO_CAUSA.FALLECIDO_id
                 where CAUSA.nombre like '%COVID%' 
                 group by FALLECIDO_SINADEF.necropsia")
View(q19) 

#                Query 20 - Diferencia entre fallecidos registros del anio 2019 contra el 2020 en el mes de
#                           Junio (pico mas alto)

dbListFields(conexion, "FALLECIDO_SINADEF")
q20_2019 = dbGetQuery(conexion, "select YEAR(FALLECIDO_SINADEF.fecha) as year, MONTH(FALLECIDO_SINADEF.fecha) as month, COUNT(FALLECIDO_SINADEF.id) as cantidad_fallecidos  from FALLECIDO_SINADEF
                 WHERE YEAR(FALLECIDO_SINADEF.fecha) = '2019' AND
                 MONTH(FALLECIDO_SINADEF.fecha) = '6'
                 group by YEAR(FALLECIDO_SINADEF.fecha), MONTH(FALLECIDO_SINADEF.fecha)")

q20_2020 = dbGetQuery(conexion, "select YEAR(FALLECIDO_SINADEF.fecha) as year, MONTH(FALLECIDO_SINADEF.fecha) as month, COUNT(FALLECIDO_SINADEF.id) as cantidad_fallecidos from FALLECIDO_SINADEF
             WHERE YEAR(FALLECIDO_SINADEF.fecha) = '2020' AND
                 MONTH(FALLECIDO_SINADEF.fecha) = '6'
                 group by YEAR(FALLECIDO_SINADEF.fecha), MONTH(FALLECIDO_SINADEF.fecha)")
q20_2019
q20_2020

#                Query 21 - Diferencia entre fallecidos registros por cada institucion de salud del anio 2019 contra el 2020 en el mes de
#                           Junio (pico mas alto)

dbListFields(conexion, "FALLECIDO_SINADEF")
q21_2019 = dbGetQuery(conexion, "select YEAR(FALLECIDO_SINADEF.fecha) as year, MONTH(FALLECIDO_SINADEF.fecha) as month, INSTITUCION.nombre, COUNT(FALLECIDO_SINADEF.id) as cantidad_fallecidos  from INSTITUCION
                 JOIN FALLECIDO_SINADEF ON FALLECIDO_SINADEF.INSTITUCION_id = INSTITUCION.id
                 WHERE YEAR(FALLECIDO_SINADEF.fecha) = '2019' AND
                 MONTH(FALLECIDO_SINADEF.fecha) = '6'
                 group by YEAR(FALLECIDO_SINADEF.fecha), MONTH(FALLECIDO_SINADEF.fecha), INSTITUCION.nombre")

q21_2020 = dbGetQuery(conexion, "select YEAR(FALLECIDO_SINADEF.fecha) as year, MONTH(FALLECIDO_SINADEF.fecha) as month, INSTITUCION.nombre, COUNT(FALLECIDO_SINADEF.id) as cantidad_fallecidos  from INSTITUCION
                 JOIN FALLECIDO_SINADEF ON FALLECIDO_SINADEF.INSTITUCION_id = INSTITUCION.id
                 WHERE YEAR(FALLECIDO_SINADEF.fecha) = '2020' AND
                 MONTH(FALLECIDO_SINADEF.fecha) = '6'
                 group by YEAR(FALLECIDO_SINADEF.fecha), MONTH(FALLECIDO_SINADEF.fecha), INSTITUCION.nombre ")
q21_2019
q21_2020 


#                Query 22 - Diferencia entre positivos a covidid entre sexos menores de edad

dbListFields(conexion, "POSITIVO_COVID")
q22 = dbGetQuery(conexion, "select SEXO.sexo, COUNT(POSITIVO_COVID.id) from POSITIVO_COVID
                 JOIN SEXO on SEXO.id = POSITIVO_COVID.SEXO_id
                 where POSITIVO_COVID.edad < 19
                 group by SEXO.sexo")
View(q22)
q22

#                Query 23 - Diferencia entre positivos a covidid entre sexos mayores de edad

dbListFields(conexion, "POSITIVO_COVID")
q23 = dbGetQuery(conexion, "select SEXO.sexo, COUNT(POSITIVO_COVID.id) from POSITIVO_COVID
                 JOIN SEXO on SEXO.id = POSITIVO_COVID.SEXO_id
                 where POSITIVO_COVID.edad > 19
                 group by SEXO.sexo")
View(q23)
q23

 
#                Query 24 - Diferencia entre positivos sacados con pruebas rapidas y con pruebas moleculares

dbListFields(conexion, "METODO")
q24 = dbGetQuery(conexion, "select METODO.nombre, COUNT(POSITIVO_COVID.id) from METODO
                 JOIN POSITIVO_COVID on METODO.id = POSITIVO_COVID.METODO_id
                 GROUP BY METODO.nombre")
View(q24)
q24

#                Query 25 - CANTIDAD DE FALLECIDOS POR TIPO DE SEGURO EN EL 2020 

dbListFields(conexion, "SEGURO")
q25 = dbGetQuery(conexion, "select YEAR(FALLECIDO_SINADEF.fecha) as year, SEGURO.tipo, COUNT(FALLECIDO_SINADEF.id) as cantidad_fallecidos from SEGURO
                 JOIN FALLECIDO_SINADEF ON FALLECIDO_SINADEF.SEGURO_id = SEGURO.id
                 WHERE YEAR(FALLECIDO_SINADEF.fecha) = '2020'
                 group by YEAR(FALLECIDO_SINADEF.fecha), SEGURO.tipo DESC" )
View(q25)
q25

#                Query 26 - CANTIDAD DE FALLECIDOS POR TIPO DE SEGURO EN EL 2020 

dbListFields(conexion, "FALLECIDO_SINADEF")
q26 = dbGetQuery(conexion, "select CAUSA.nombre, FALLECIDO_SINADEF.muerte_violenta as id, COUNT(FALLECIDO_CAUSA.CAUSA_ID) as cantidad_fallecidos from CAUSA
                 JOIN FALLECIDO_CAUSA on FALLECIDO_CAUSA.CAUSA_ID = CAUSA.id
                 JOIN FALLECIDO_SINADEF on FALLECIDO_SINADEF.id = FALLECIDO_CAUSA.FALLECIDO_id
                 group by CAUSA.nombre, FALLECIDO_SINADEF.muerte_violenta " )
View(q26)
q26

#                Query 27 - CANTIDAD DE FALLECIDOS POR TIPO DE NIVEL_INSTRUCCION EN EL 2020 

dbListFields(conexion, "FALLECIDO_SINADEF")
q27 = dbGetQuery(conexion, "select NIVEL_INSTRUCCION.nivel, COUNT(FALLECIDO_SINADEF.id) from NIVEL_INSTRUCCION
                 JOIN FALLECIDO_SINADEF ON NIVEL_INSTRUCCION.id = FALLECIDO_SINADEF.NIVEL_INSTRUCCION_id
                 WHERE YEAR(FALLECIDO_SINADEF.fecha) = '2020'
                 GROUP BY NIVEL_INSTRUCCION.NIVEL" )
View(q27)
q27

#                Query 28 - PERSONAS QUE VIVEN EN EL EXTRANJERO QUE FALLECIDOS EN PERU EL 2020 

dbListFields(conexion, "PAIS")
q28 = dbGetQuery(conexion, "select PAIS.NOMBRE, COUNT(FALLECIDO_SINADEF.id) from PAIS
                 JOIN FALLECIDO_SINADEF ON PAIS.Id = FALLECIDO_SINADEF.PAIS_id
                 WHERE YEAR(FALLECIDO_SINADEF.fecha) = '2020'
                 GROUP BY PAIS.NOMBRE" )
View(q28)
q28


#                Query 29 - Persona mas joven y mas longeva en salir positivo para covid-19

dbListFields(conexion, "POSITIVO_COVID")
q29_menor = dbGetQuery(conexion, "SELECT id, edad from POSITIVO_COVID
                 WHERE  edad != 'NA'
                 ORDER BY edad asc
                 LIMIT 1")
q29
q29_mayor = dbGetQuery(conexion, "SELECT id, edad from POSITIVO_COVID
                 WHERE  edad != 'NA'
                 ORDER BY edad DESC
                 LIMIT 1")
q29_mayor


#                Query 30 - Cantidad de positivos a covid por mes 

dbListFields(conexion, "")
q30 = dbGetQuery(conexion, "select MONTH(fecha_resultado) as mes, COUNT(id) as cantidad_positivos FROM POSITIVO_COVID 
                 GROUP BY MONTH(fecha_resultado)" )
View(q30)
q30



##              GRAFICOS


##              MODELO

#1. Direccion
driver=MySQL()
host = "35.174.139.24"
port = 3306
user = "admin"
password = "admin2020"
dbname = "COVID_DB"

#2. Crear conexion a la base de datos
dbCanConnect(drv=driver,host=host,port=port,username=user,password=password,dbname=dbname)
conexion<-dbConnect(drv=driver,host=host,port=port,username=user,password=password,dbname=dbname)
dbIsValid(conexion)

#3. Leer base de datos
covid_19 = read_xlsx("Covid-19 Daily Financial Report.xlsx")

#4. Eliminación de valores anómalos (0's en su mayoría)
covid_19 = covid_19[-c(1:8,16,18,20,22:24,29:31),]



### MODELOS

## REGRESION LINEAL

x = covid_19$`Total Non P Card Expense`
#x = covid_19$`Total Encumbrances`
#x = covid_19$`Total P Card`
y = covid_19$`Est. Personnel Exp`

# Funcion regresion
regresion<-function(x,y, nx=NA, ny=NA){
  resultado<-list()
  prediccion<-NA
  n<-NROW(x)
  promX<-mean(x)
  promY<-mean(y)
  (dx<-x-promX)
  (dy<-y-promY)
  (xy<-dx*dy)
  #calculo de la covarianza
  cov<-sum(xy)/(n-1) #cov(x,y)
  dx2<-dx^2
  dy2<-dy^2
  #calculo de las desviaciones estandar
  sdX<-sqrt(sum(dx2)/(n-1))#  sd(x)
  sdY<-sqrt(sum(dy2)/(n-1))# sd(y)
  # calculo del coeficiente correlacional
  (r<-cov/(sdX*sdY))
  #creando la prediccion regresional x~y
  if(is.na(ny))
    prediccion<-promY+cov*(nx-promX)/(sdX^2)
  #creando la prediccion regresional y~x
  if(is.na(nx))
    prediccion<-promX+cov*(ny-promY)/(sdY^2)
  
  resultado[[1]]<-data.frame(x,y,dx,dy,xy,dx2,dy2) #Calculo
  resultado[[2]]<-cov #covarianza
  resultado[[3]]<-r #coeficiente correlacional de pearson
  resultado[[4]]<-prediccion #prediccion
  return (resultado)
}
regresion(x,y,10)
plot(x,y)



## REGRESION MULTIVARIABLE (lineal multiple)

covid_19 = read_xlsx("Covid-19 Daily Financial Report.xlsx")
covid_19 = covid_19[-c(1:8,16,18,20,22:24,29:31),]

#obtener la regresion lineal de mpg en funcion de Total P Card, Est. Personnel Exp y Total Hours

f=lm(`Total Non P Card Expense`~`Total Encumbrances`+`Total P Card`+`Est. Personnel Exp`+`Total Hours`,data = covid_19)
f

#Predecir en funcion de F
#Tecnica para predecir (entrenamiento, pruebas, validacion)
#Entrenamiento--->70%=70
#Pruebas--->30%=30

ids<-sample(1:NROW(covid_19),NROW(covid_19)*0.7)

entrenamiento<-covid_19[ids,c(2:6)]

probar<-covid_19[-ids,c(2:6)]
probar

#Generar un modelo para entrenamiento (`Total Non P Card Expense`~`Total Encumbrances`+`Total P Card`+`Est. Personnel Exp`+`Total Hours)
ft=lm(`Total Non P Card Expense`~`Total Encumbrances`+`Total P Card`+`Est. Personnel Exp`+`Total Hours`,data = covid_19)
ft

#Predecir
predict(ft,probar)
probar$prediccion<-predict(ft,probar)
probar

#Determinar la precision del modelo entrenado
(probar$prediccion-probar$`Total Non P Card Expense`)/probar$prediccion
100*(probar$prediccion-probar$`Total Non P Card Expense`)/probar$prediccion
mean(abs(100*(probar$prediccion-probar$`Total Non P Card Expense`)/probar$prediccion))
#Accuracy==> 100% - mean = 15%

#Generar un modelo para entrenamiento (Total Encumbrances`~`Est. Personnel Exp`+`Total Hours`)
ft1=lm(`Total Non P Card Expense`~`Total Encumbrances`+`Total P Card`+`Est. Personnel Exp`+`Total Hours`,data = covid_19)
ft1

#Predecir
probar$prediccion<-predict(ft1,probar)
probar

#Determinar la precision del modelo entrenado
mean(abs(100*(probar$prediccion-probar$`Total Encumbrances`)/probar$prediccion))
#Accuraccy=83




## SVM

covid_19 = read_xlsx("Covid-19 Daily Financial Report.xlsx")
covid_19 = covid_19[-c(1:8,16,18,20,22:24,29:31),]

#convertir chr a factorial
covid_19$Department = as.factor(covid_19$Department)


plot(covid_19)
#View(covid_19)
glimpse(covid_19)

plot(covid_19$`Total P Card`, covid_19$`Est. Personnel Exp`, col=covid_19$Department)

s<-sample(150, 100)
col<-c("Total P Card","Est. Personnel Exp", "Department")

covid_19_train<-covid_19[s,col]
covid_19_test<-covid_19[-s,col]

svmfit <- svm(Department ~., data = covid_19_train, kernel = "linear", cost = .1, scale = FALSE)
print(svmfit)
plot(svmfit,covid_19_train[,col])

tuned <- tune(svm, Department ~., data = covid_19_train, kernel = "linear", ranges = list(cost=c(0.001,0.01,.1,1,10,100)))

# Muestr el parÃ¡metro de costo Ã³ptimo 
summary(tuned)

p <- predict(svmfit, covid_19_test[,col], type="class")
plot(p)

table(p, covid_19_test[,3])
mean(p== covid_19_test[,3])


#--------------------------------------------------------------------------
#plot(iris)
#View(iris)
#glimpse(iris)

#plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
#plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

#s<-sample(150, 100)

#col<-c("Petal.Length", "Petal.Width", "Species")

#iris_train<-iris[s,col]
#iris_test<-iris[-s,col]

#svmfit <- svm(Species ~., data = iris_train, kernel = "linear", cost = .1, scale = FALSE)
#print(svmfit)
#plot(svmfit, iris_train[,col])

#tuned <- tune(svm, Species ~., data = iris_train, kernel = "linear", ranges = list(cost=c(0.001,0.01,.1,1,10,100)))

# Will show the optimal cost parameter
#summary(tuned)

#p <- predict(svmfit, iris_test[,col], type="class")
#plot(p)

#table(p, iris_test[,3])
#mean(p== iris_test[,3])




## PCA

covid_19 = read_xlsx("Covid-19 Daily Financial Report.xlsx")
covid_19 = covid_19[-c(1:8,16,18,20,22:24,29:31),]

#Quitando la columna que no tiene valores numéricos
covid_19 = select(covid_19,-Department)

View(covid_19)
boxplot(covid_19)#datos anomalos o espureos
summary(covid_19)
corrplot(cor(covid_19)) #diagrama de calor


#normalizacion de los datos: estandarizacion(variables- promedio)/desv
datosEsc<-scale(covid_19)

pca<-prcomp(datosEsc)
pca #desviación estandar
str(pca) #estructura
summary(pca) #resultados acumulada


pca[[1]]#desviaciones
pca[[2]]#rotaciones
pca[[5]]#individuos


componentes<-cbind(pca[[2]][,1], pca[[2]][,2]) #llega el valor acumulado al 87% 
componentes
individuos<-pca[[5]][,c(1:5)]
individuos


#analisis cluster del componente c1 y c2
s.corcircle(componentes[,c(1,2)])

#los valores mas relacionados son Total Encumbrances y Total Non P Card Expense
#y por otro lado Total P Card, Total Hours y Est. Personnel Exp

#cuales son los registros que indican  el grado de participacion
s.label(individuos[,c(1,2)],label = row.names(covid_19))

levels(covid_19$`Total Encumbrances`)
covid_19$`Total Non P Card Expense`

#no se puede realizar clusters de los componentes c2 y c3 porque se va fuera de los límites






## KNN

covid_19 = read_xlsx("Covid-19 Daily Financial Report.xlsx")
covid_19 = covid_19[-c(1:8,16,18,20,22:24,29:31),]

#View
#plot(covid_19)
#View(covid_19)

x=covid_19$`Est. Personnel Exp`
y=covid_19$`Total Hours`

df<-data.frame(x,y)
plot(df)

###Etiquetar (categorizar)
etiquetar<-function(df){
  categorias<-c()
  for(i in 1:NROW(df)){
    if(df$x[i]>=0 & df$x[i]<20000)
      categorias<-c(categorias,'A') 
    else if(df$x[i]>=20000& df$x[i]<40000)
      categorias<-c(categorias,'B')
    else categorias<-c(categorias,'C')
  }
  df<-cbind(df,categorias)  
  return (df)
}
df=etiquetar(df)
df


#######Visualizando el df
library(ggplot2)
ggplot(data = df,aes(x=df$x,y=df$y,color=df$categorias))+
  geom_point()+xlab("X")+ylab("Y")+ggtitle("Clasificador KNN")


#datos para entrenamiento
ids=sample(1:nrow(df),0.80*nrow(df)) #muestreo aleatorio
dfEnt<-df[ids,]
nrow(dfEnt)
dfTest<-df[-ids,]
nrow(dfTest)
ggplot(data = dfEnt ,aes(x=x,y=y,color=categorias))+
  geom_point()+xlab("Est. Personnel Exp")+ylab("Total Hours")+ggtitle("Clasificador KNN")

dFTemp=df
knn<-function(dFTemp,newX,newY,k, method){
  if(method==1){
    d<-(abs(newX-dFTemp$x)+abs(newY-dFTemp$y))    
  }else{
    d<-sqrt((newX-dFTemp$x)^2+(newY-dFTemp$y)^2)  
  }
  dFTemp<-cbind(dFTemp,d)
  dFTemp  
  vOrden<-sort(dFTemp$d)
  vecinos<-dFTemp[dFTemp$d %in% vOrden[1:k],3]
  return (vecinos[1:k] )
}

v<-knn(df,20000,700,5,2)
v
porc<-function(vector,value){
  return (sum(as.integer(vector==value)))
}
a<-porc(v,"A")
b<-porc(v,"B")
c<-porc(v,"C")
total<-(a+b+c)
a*100/total
b*100/total
c*100/total




## Kmeans

#Read
covid_19 = read_xlsx("Data/Covid-19 Daily Financial Report.xlsx")
covid_19 = covid_19[-c(1:8,16,18,20,22:24,29:31),]

#glimpse(covid_19)
#View(covid_19)

x = covid_19$`Total P Card`
y = covid_19$`Est. Personnel Exp`
df = data.frame(x,y)
#View(df)

#como estan agrupados los datos

distancias = dist(df)
cluster = hclust(distancias)
plot(cluster)

##obtener los "k" puntos iniciales
k = 5

#forma 1
obtenerKpuntosAleatorios<-function(df, k){
  x1=sample(min(df$x):max(df$x),size = k)
  y1=sample(min(df$y):max(df$y),size = k)
  return (data.frame(x1,y1))  
}
obtenerKpuntosAleatorios(df,k)

#forma 2
obtenerKpuntos<-function(df, k){
  ids<-sample(x = 1:NROW(df),k)
  return (df[ids,])
}
puntos<-obtenerKpuntos(df,k)
class(puntos)
puntos


#############################################################
#Cálculo de las distancias eucledianas

euclidiana<-function(pA,pB) {
  return (sqrt((pA$x-pB$x)^2+(pA$y-pB$y)^2))
}
calcularDistancias<-function(df,puntos){
  dtemp<-df
  for(i in 1:NROW(puntos))
    dtemp[,i+NCOL(df)]<-euclidiana(df,puntos[i,])
  return (dtemp) 
}
calcularDistancias(df,puntos)


calcDistancias<-function(df,puntos){
  m<-matrix(nrow = NROW(df),ncol = NROW(puntos))  
  for(i in 1:NROW(puntos))
    m[,i]<-euclidiana(df,puntos[i,])
  return (m) 
}
m<-calcDistancias(df,puntos)
View(m)


obtenerGrupos<-function(m){
  matriz<-apply(m,1,min)==m
  grupos<-rep(-1,NROW(m))
  for(i in 1:NCOL(matriz))
    grupos[matriz[,i]]=i
  return (grupos)
}
grupo<-obtenerGrupos(m)

df<-cbind(df,grupo)
View(df)


############################################################
calcularCentroide<-function(df, puntos){
  # solamente para el grupo1
  px<-c()
  py<-c()
  for(i in 1:NROW(puntos)){
    px<-c(px,mean(df[df$grupo==1,]$x))
    py<-c(py,mean(df[df$grupo==1,]$y))   
  }
  
  puntos<-cbind(px,py)
  return (puntos)
}
puntos<-calcularCentroide(df,puntos)
view(puntos)


dbWriteTable(conexion,"covid_19",covid_19)






