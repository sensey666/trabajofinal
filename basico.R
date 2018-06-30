# Cargar archivo ----

# NO OLVIDAR set working directory
filename="colombia.csv"
colb=read.csv(filename, stringsAsFactors = FALSE)

# que variables y tipo
str(colb)


# Exploracion Univariada --------------------------------------------------

## estadisticos

# nos interesa IDH, y poblacion cabecera y poblacion resto
# no se puede secar tabla de frecuencia, 
# solo estadisticos:
summary(colb)

## graficos
# el plot de cada uno seria el histograma:

hist(colb$IDH)
hist(colb$Población.Cabecera)
hist(colb$Población.Resto)

# dado el sesgo de las pobaciones, 
# podriamos transformarla para que se acerque a la 
# normalidad

colb$cabeLog=log(colb$Población.Cabecera)
colb$restoLog=log(colb$Población.Resto)

hist(colb$cabeLog)
hist(colb$restoLog)


# Exploracion Bivariada ---------------------------------------------------


# En este trabajo estamos interesados en el impacto de 
# la poblacion en el el IDH, veamos IDH con cada uno:

explanans=names(colb)[c(7:8)] # usando las logs
corrDem=cor(colb$IDH,colb[,explanans],
            use = "na.or.complete")
corrDem

    
    
# y la correlación entre las variables independientes:
    
corrTableX=round(cor(colb[,explanans],
                         use = "na.or.complete"),2)
corrTableX_copy=corrTableX
corrTableX[upper.tri(corrTableX)]<-""
#ver:
corrTableX

 
# visualmente:  

plot(colb[,explanans])


# Modelos de Regresión ----------------------------------------------------

# Veamos los modelos propuestos. 
# Primero sin poblacion resto, luego con esa:

LinRegA = lm(IDH ~ ., data = colb[,c(1,7)])
LinRegB = lm(IDH ~ ., data = colb[,c(1,7:8)])

#resultados
summary(LinRegA)
summary(LinRegB)



# Exploración Espacial ----------------------------------------------------

#Calculemos conglomerados de regiones,
#usando toda la información de las tres variables.
# usaremos la tecnica de k-means propuesta por MacQueen.


library(rgdal)
folder='COL_maps'
file='COL_adm1.shp'
mapaFile=file.path(folder,file)
mapCol <- rgdal::readOGR(mapaFile,stringsAsFactors=F) 

# lo tenemos:
plot(mapCol)

# veamos que variables hay:

head(mapCol@data)

# con esto hagamos el merge:
sub_colb=colb[,c(1:2,7:8)]
mapCol_idh=merge(mapCol,sub_colb, by.x='NAME_1', by.y='Departamento',all.x=F)

# cuantas regiones me quedaron luego del merge?
nrow(mapCol_idh)  # todas!!...


# preparacion para clusterizar:

# que tengo?:
names(mapCol_idh)
# nombre de la variables que usaré:
dimensions=c("NAME_1","IDH","cabeLog","restoLog")

# creo un nuevo data frame con esas:
dataCluster=mapCol_idh@data[,c(dimensions)]

# como la data es numerica la normalizo (menos la column 1):
dataCluster[,-1]=scale(dataCluster[,-1])

## APLICANDO TECNICA KMEANS

# calculo 3 clusters

resultado=kmeans(dataCluster[,-1],3)

#creo data frame con los clusters:
clusters=as.data.frame(resultado$cluster)

# añado columna con nombre de regiones
clusters$NAME_1=dataCluster$NAME_1
names(clusters)=c('cluster','NAME_1')
#hago el merge hacia el mapa:
mapCol_idh=merge(mapCol_idh,clusters, by='NAME_1',all.x=F)

# lo tengo?
names(mapCol_idh)

## a pintar:

library(RColorBrewer)
library(classInt)

#variable a colorear
varToPLot=mapCol_idh$cluster

# decidir color:
unique(varToPLot)
aggregate(mapCol_idh@data[,c(10,11,12)],
          by=list(mapCol_idh@data$cluster),FUN=mean)

#preparo colores
numberOfClasses = length(unique(varToPLot)) 
colorForScale='Set2'
paleta = brewer.pal(numberOfClasses, colorForScale)

# grafico mapa basico
plot(mapCol,col='grey',border=0)

# grafico mapa cluster
plot(mapCol_idh, col = paleta[varToPLot],border=F,add=T)
legend('left', legend = c("LOW","UP","MEDIUM"),
       fill = paleta, 
       cex = 0.6, 
       bty = "n",
       title="conglomerado")