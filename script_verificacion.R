# -----------------------Busqueda de errores en la topologia de modelo MODFLOW-------------------------------

# Seleccionar a la carpeta donde guardo los archivos exportados desde MODFLOW

setwd("C:/Users/GONZALO/Dropbox/00 CAR RIO BOGOTA 2015/INAR_Sabana_Bogota/Verificacion_topologica")

# Abrir los paquetes "reshape2" y "dplyr"

library(reshape2)

library(dplyr)

# Abrir los datos como "data frames" (data table no funciona con dplyr)

conductividad <- read.table("./conductividades.TXT", header = TRUE, 
                            colClasses = c("numeric","numeric","factor", "numeric"))

espesores <- read.table("./espesores.TXT", header = TRUE, 
                        colClasses = c("numeric","numeric","factor", "numeric"))

# Colocar un ID común en ambas entidades

conductividad <- mutate(conductividad, rowcollay = paste0(Row, "-", Col, "-", Lay), formacion = "",rowcol = paste0(Row, "-", Col))

espesores <- mutate(espesores, rowcollay = paste0(Row, "-", Col, "-", Lay) )

# Funcion para generalizar las formaciones en donde:
  # 1,2,3,4,5,6,7,8 corresponden a cuaternarios
  # 10 es Fcn. Arenisca Dura 
  # 11,12,13 es Fcn. Bogotá 
  # 15 es Fcn. Chipaque 
  # 16 es Fcn. Guaduas
  # 17 es Fcn. Labor tierna
  # 18 es Formación plaeners
  # 21 son Fallas

formacion <- function(numero)
{
  quat <- seq(from=2, to=8)
  bta = seq(from=11, to=13)
  if (numero == 1) {ans = "NA"}
  else if (numero%in% quat) {ans  = "cuaternario"}
  else if (numero %in% bta) {ans="bogota"}
  else if (numero == 9) {ans = "regadera"}
  else if (numero == 10) {ans = "arenisca_dura"}
  else if (numero == 15) {ans = "chipaque"}
  else if (numero == 16) {ans = "guaduas"}
  else if (numero == 17) {ans = "labor_tierna"}
  else if (numero == 18) {ans = "plaeners"}
  else if (numero == 21) {ans = "fallas"}
  else {ans = "NA"}
  ans
}

# Evaluar cada formación con la funcion formacion

conductividad$formacion <- sapply(X = conductividad$Kx, FUN = formacion )

# Juntar la información, quitar la información innecesaria de espesor

espesores <- mutate(espesores, Row = NULL, Col = NULL, Lay = NULL )

completo <- merge(conductividad, espesores, by ="rowcollay" )

completo$rowcol <- as.factor(completo$rowcol )

# Crear un nuevo dataframe, donde cada fila es una celda
# cada celda contiene la formación asignada en cada layer y el espesor de cada layer
# el nombre de las propiedades es Lay_prop

orderedframe <- 

celdas <- as.factor(unique(completo$rowcol))
  
filas <- as.factor(unique(completo$Row))

columnas <- as.factor(unique(completo$Col))

# Tabla ordenada con cuantas celdas son asignadas en la vertical a una celda horizontal

a <- dcast(conductividad, rowcol ~ Lay, value.var = "formacion")

a <- a[,c(1,2,4,5,6,7,8,9,10,11,3)]

# Guardar la tabla que se obtiene como un documento adicional 

write.table(a, "./organizado.csv", sep="\t", row.names = FALSE)

# Limpiar la caché de R

rm(list = ls())

# -----------------------------Analisis de problemas de topologia------------------------------------------

analisis <- read.table("./organizado.csv", header = TRUE, colClasses = rep("character",11),
                       col.names = c("rowcol","L1","L2","L3","L4","L5","L6","L7","L8","L9","L10"),
                       na.strings = "NA")
                       
# Eliminación de las celdas inactivas

  # son todos los valores negativos
neg <- function(list)
{
  n <- all(is.na(list[2:11]))
  n
}

selec <- apply(analisis,1,  neg)

analisis <- analisis[!selec,]

  # El orden de las formaciones debe ser
  # Cuaternario o bogotá
  # Guaduas
  # Labor_tierna
  # Plaeners
  # arenisca_dura
  # chipaque
  # NO REVISO TILATA (se revisa manualmente)
  # las fallas unicamente pueden estar debajo de fallas

# Vector vacio donde se almacenarán posibles problemas

problemas <- data.frame()

  # Orden de las capas

orden = c("cuaternario", "guaduas", "labor_tierna", "plaeners", "arenisca_dura", "chipaque")

  # Condicional que evalua si las capas están dispuestas correctamente

for (var in 1:nrow(analisis))
{ 
  
  a <- as.vector(analisis[var,2:11])
  
  for (ele in 1:9)
  {
    corregir = FALSE
    
    ubicacion <- which(orden == as.character(a[ele]))
    
    if (orden[ubicacion + 1] != a[ele +1] || a[ele +1] == a[ele] )
    {
      corregir = FALSE
    } else {
      corregir = TRUE
    }
  }
}

