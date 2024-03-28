#-----------------------------------------------------------
#Equipo 
#    Andrea Abigail Hernandez Cordova
#    Maria de Guadalupe Hernández Gómez
#    Jesús Márquez García
#    José Rafael Cuevas Cabrera
#    Kevin soto hernandez
#------------------------------------------------------------
# ------------------- Menu para elegir el dataset -----------------------------------------------------

library(svDialogs)#libreria que nos permite enviar el cuadro de dialogo

nb <- dlgInput(message= " KNN con Distancia euclídea  
              \n Selecciona alguno de los siguientes datasets: 
              \n 1.-Iris
              \n 2.- glass
              \n 3.- Breast-Cancer
              \n 4.- Personas con autismo
              \n 5.- QSAR androgen receptor 
              \n 6.- Abalone
              \n 7.- conjunto de datos de choques de simulación de modelo climático 
              \n 8.- Clasificacion para cesaria
              \n 9.- clasificación de personas con Parkinson
              \n 10.- cáncer de cuello uterino (factores de riesgo)")$res
nb  <- as.integer(nb) 

if (nb==1){
  dbase=iris
  clasifi='class'
  f=20
 
}
if (nb==2){
  dbase=glass
  clasifi='class'
  f=20
}
if (nb==3){
  dbase=breastC
  clasifi='class'
  f=20
}

if (nb==4){
  dbase=autismo
  clasifi='class'
  f=33
}
if (nb==5){
  dbase=dataset
    clasifi='class'
  f=33
}
if (nb==6){
  dbase=abalone
    clasifi='class'
  f=73
}
if (nb==7){
  dbase=SimulationCrashes
    clasifi='class'
  f=20
}
if (nb==8){
  dbase=cesarian
    clasifi='class'
  f=21
}
if (nb==9){
  dbase=pd_speech_features
    clasifi='class'
  f=27
}
if (nb==10){
  dbase=risk_factors_cervical_cancer
    clasifi='class'
  f=20
}
#---------------------------------------------------------------------------------------------------------
  
distancia_eu = function(a, b){
  # Comprobamos que tienen la misma cantidad de observaciones
  if(length(a) == length(b)){
    sqrt(sum((a-b)^2))  
  } else{
    stop('Los vectores deben de tener el mismo tamaño')
  }
}


vec_cercano = function(x,obs, k, FUN, p = NULL){
  
  # Comprobar que el número de observaciones es el mismo
  if(ncol(x) != ncol(obs)){
    stop('Los datos deben tener el mismo número de variables.')
  }
  
  #La distancia de Minkowski es un tipo de distancia que generaliza las distancias Euclídea y de Manhattan
  if(is.null(p)){
    dist = apply(x,1, FUN,obs)  
  }else{
    dist = apply(x,1, FUN,obs,p)
  }
  
  # Encuentra vecinos más cercanos
  distancias = sort(dist)[1:k]
  indice_vec = which(dist %in% sort(dist)[1:k])
  
  if(length(indice_vec)!= k){
    warning(
      paste('Varias variables con igual distancia. k usado:',length(indice_vec))
    )
  }
  
  ret = list(indice_vec, distancias)
  return(ret)
}

x = dbase[1:(nrow(dbase)-1),]
obs = dbase[nrow(dbase),]

ind = vec_cercano(x[,1:7], obs[,1:7],7, distancia_eu)[[1]]
as.matrix(x[ind,1:7])

obs[,1:7]

prediccion = function(x,y){
  
  grupos = table(x[,y])
  pred = grupos[grupos == max(grupos)]
  return(pred)
  
}

prediccion(x[ind,], clasifi)

obs[,clasifi]


prediccion = function(x,y, pond = NULL){
  
  x = as.matrix(x)
  
  if(is.factor(x[,y]) | is.character(x[,y])){
    grupos = table(x[,y])
    pred = names(grupos[grupos == max(grupos)])
  } 
  
  if(is.numeric(x[,y])){
    
    # Calcular predicción ponderada
    if(!is.null(pond)){
      w = 1/pond/ sum(pond)
      pred = weighted.mean(x[,y], w)
      
      # Calcular predicción estándar  
    }else{
      pred = mean(x[,y])
    }
    
  }
  
  # Si no hay pred, entonces la clase no es correcta
  if(try(clasifi(x[,y])) == 'try-error'){
    stop('Y debe ser factor o numérico.')
  }
  
  return(pred)
  
}

knn = function(x_fit, x_pred, y, k, 
               func = distancia_eu,weighted_pred = F, p = NULL){
  
  # Inicilizamos las predicciones
  prediccion = c()
  
  y_ind = which(colnames(x_pred) == y)
  
  # Para cada observaciones, obtenemos la prediccion
  for(i in 1:nrow(x_pred)){
    
    neighbors = vec_cercano(x_fit[,-y_ind], 
                            x_pred[i,-y_ind],k,FUN = func)
    
    if(weighted_pred){
      pred = knn_prediction(x_fit[neighbors[[1]], ],y, neighbors[[2]])
    } else{
      pred = knn_prediction(x_fit[neighbors[[1]], ],y)
    }
    
    #Si hay más de 1 predicción, haz la predicción con 1 k más
    if(length(pred)>1){
      pred = knn(x_fit, x_pred[i,],y, k = k+1, 
                 func = func, weighted_pred = weighted_pred, p == p)
    }
    
    prediccion[i] = pred
    
  }
  return(prediccion)
  
}
set.seed(1234)

n_fit = f
ind_Entr = sample(1:nrow(dbase),n_fit)

x_fit = dbase[-ind_Entr,]
x_pred = dbase[ind_Entr,]

vark <- dlgInput(message= "Seleciona el numero de vecinos: \n 3.- K=3
               \n 5.- K=5
               \n 7.- K=7
               \n 9 K=9")$res
vark  <- as.integer(k) # convert character into integer

prediccion = knn(x_fit, x_pred, clasifi, k=vark)
prediccion

library(caret)


confusionMatrix(as.factor(prediccion),as.factor(x_pred$class))$overall[1]