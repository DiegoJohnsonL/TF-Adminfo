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






