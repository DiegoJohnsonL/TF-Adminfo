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






