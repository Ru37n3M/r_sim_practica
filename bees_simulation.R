

Sn_t <- function() {
  
  spam <- replicate(n = 100, expr = sample (0:1, 100, replace = T), 
                    simplify = T)#spam produce valores de 0 y 1, de forma aleatoria, en 100 tiempos, 
                                #es decir, produce 1 valor entre 0 y 1 por cada una de las filas en 100 columnas

  
  abejas_buscando <- sum(rbind(spam))#Se suman los valores por cada fila en spam
  
  
  nidos_encontrados <- sample(c(0, "K1","K2","K3","K4","K5"), abejas_buscando,
                              replace = T)#En nidos_encontrados se producen los caracteres definidos de forma aleatoria 
                                          #por el numero definido en abejas buscando
  
  resultado <- matrix(NA, nrow = 100, ncol = 100)#Creamos una matriz que nos permita contener la informacion producida por spam
                                                #nrow debe ser igual a la cantidad de valores por columna en spam,
                                                #ncol debe ser igual a la cantidad de columnas en spam
  
  resultado[which(spam[] == 1)] <- nidos_encontrados#Por cada valor igual a 1 en spam, se obtiene de forma aleatoria
                                                    #uno de los caracteres definidos en nidos_encontrados
                                                    #y son reemplazados en la matriz de resultados
  
  
  return(resultado) #IMPORTANTE, muestra la matriz resultado como output en la consola, si no se coloca return
                    #no podremos ver los datos de la matriz
}

Sn_t() #Llamamos a la función, cada vez que la llamamos, el contenido de la funcion corre nuevamente
                             #Al llamarla, solo va a mostrar lo que determinamos como return dentro de la misma


table(Sn_t()) #Tabla de frecuencia de los valores de la funcion, al no estar guardada como una variable, van a cambiar
              #cada vez que llamemos a la función

hist(replicate(100, table(Sn_t())))#observamos un histograma que muestra la distrubución de los resultados de la 
                                  #función si la hubieramos llamado 100 veces.

guardado_Sn_t <- Sn_t() #Guardamos la función para mantener los resultados obtenidos
table(guardado_Sn_t) #Tabla de frecuencias 


no_busqueda <- function (x){
  
  return(sum(is.na(x)))
}


apply(X = guardado_Sn_t, MARGIN = 2, FUN = no_busqueda ) #apply es un for loop optimizado: 
                                                        #aplica una función determinada previamente 
                                                        #en cada elemento de la dimensión que hayamos seleccionado. 
                                                        #En este caso MARGIN = 2 refiere a la segunda dimensión
                                                        # de la matriz, es decir, a las columnas. Entonces,
                                                        #aplica la función en cada valor por columna, que cumpla 
                                                        #los criterios especificados en la función.
                                                        #MARGIN = 1 refiere a las filas.


library(dplyr)

sn_t_df<- tbl_df(as.data.frame(guardado_Sn_t)) #Conversión de matriz a data_frame 


slice(sn_t_df, c(1:4)) #pido que me muestre las filas seleccionadas

sn_t_datatable<- data.table::as.data.table(sn_t_df)  #Data_table permite la modificación de var existentes y agregado
                                                    #de nuevas variables en el data_frame

sn_t_datatable$confianza <- runif(100, 0, 1) #agregamos la variable confianza en el data_frame,
#a partir de una serie de numeros, entre 0 y 1, elegidos al azar.

sn_t_datatable$confianza #ahora cada n posee un grado de confianza asociado





sn_t_datatable

sn_t_datatable[i = 2] #permite ver fila/s seleccionada

sn_t_datatable[j = V3] #Permite ver columna/s seleccionada

sn_t_datatable[i = c(2,3), j = confianza]

sn_t_datatable$confianza <- NULL #Si quiero borrar la nueva variable.

sn_t_datatable[which(confianza[] >= 0.5)]


#########################################
sn_t_datatable$J <- sample(c("J1","J2","J3","J4","J5"), 100,
                           replace = T)

sn_t_datatable$confianza <- runif(100, 0, 1)

#FUNCIÓN INICIAL
j_sorting <- function ()
  
{

  n_en_j<- table( sn_t_datatable$J )#Vector que cuyos valores representan la cantidad total de J
  
  sn_t_datatable [ which ( confianza[] >= 0.7 ) ]$J <- sample ( c ( "J1","J2","J3","J4","J5" ), 
                                                        length ( sn_t_datatable [ which ( confianza[] >= 0.7 ) ]$J ), 
                                                        prob = c ( 0.2,0.2,0.2,0.2,0.7 ),
                                                        replace = T ) #Se samplea J nuevamente con una prob mayor para J5 p/c/n cuyo valor en la var.confianza sea mayor a 0.7
 
  promedio_conf_J5<- mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J5" ) ]$confianza )#Promedio de los valores de la var confianza p/c/j
  
  promedio_conf_J4<- mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J4" ) ]$confianza )
  
  promedio_conf_J3<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J3" ) ]$confianza )
  
  promedio_conf_J2<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J2" ) ]$confianza )
  
  promedio_conf_J1<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J1" ) ]$confianza )
  
  #Si la cantidad de n y el promedio de conf. es mayor en ej.J5, los valores dentro de ej.J4 serán sampleados con una prob. de 0.8 para J5, y viceversa.
  #Si la cantidad de n y el promedio de conf no favorecen a un J por sobre otro, se samplean los valores en las diadas seleccionadas con prob aleatoria
  
  
  #J4 <-> J5
  
 
    if ( promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] >= n_en_j[4] ) {
      sn_t_datatable[ which( J[] == "J4" ) ]$J <- sample ( c ( "J4","J5" ),length ( sn_t_datatable[which( J[] == "J4" )]$J),
                                                     prob = c ( 0.2,0.8 ),
                                                     replace =T ) # 1) n y promedio favorecen a J5, por lo tanto, valores de J4  tienen mayor prob de ser sampleados como J5
      
    }else if ( promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] <= n_en_j[4] ) {
      sn_t_datatable[ which( J[] == "J4" ) ]$J <- sample ( c ( "J4","J5" ),length ( sn_t_datatable [ which ( J[] == "J4" ) ]$J ),
                                                     replace = T ) # 2) promedio favorece a J5, pero n favorece J4, los valores de J4 tienen prob aleatoria de ser sampleados como J4 o J5
      
    }else if ( promedio_conf_J5 < promedio_conf_J4 && n_en_j[5] <= n_en_j[4] ) {
      sn_t_datatable [ which ( J[] == "J5" ) ]$J <- sample ( c ( "J4","J5" ),length ( sn_t_datatable [ which ( J[] == "J5" ) ] $J ),
                                                     prob = c( 0.8,0.2 ), # 3) Idem 1) pero a la inversa 
                                                     replace = T )
    }else {
      sn_t_datatable[ which( J[] == "J5" ) ]$J <- sample( c ( "J4","J5" ),length ( sn_t_datatable[ which( J[] == "J5" ) ]$J ),
                                                     replace = T ) # 4) Idem 2) pero a la inversa
    }
  
  #Seguir lógica de las Anotaciones 1), 2), 3) y 4) p/c diada de J
  #J3 <-> J4
  
  if ( promedio_conf_J4 > promedio_conf_J3 && n_en_j[4] >= n_en_j[3] ){
    sn_t_datatable[ which ( J[] == "J3" ) ]$J <- sample ( c ( "J3","J4" ),length ( sn_t_datatable [ which( J[] == "J4" ) ]$J ),
                                                   prob = c ( 0.2,0.8 ),
                                                   replace = T )
    
  }else if ( promedio_conf_J4 > promedio_conf_J3 && n_en_j[4] <= n_en_j[3] ){
    sn_t_datatable[ which ( J[] == "J3" ) ]$J <- sample ( c ( "J3","J4" ),length ( sn_t_datatable [ which( J[] == "J3" ) ]$J ),
                                                   replace = T )
    
  }else if ( promedio_conf_J4 < promedio_conf_J3 && n_en_j[4] <= n_en_j[3] ){
    sn_t_datatable[which(J[] == "J4")]$J <- sample ( c ("J3","J4" ),length ( sn_t_datatable [ which ( J[] == "J4" )]$J),
                                                   prob = c ( 0.8,0.2 ),
                                                   replace = T )
  }else {
    sn_t_datatable[which(J[] == "J4")]$J <- sample ( c ( "J3","J4" ),length ( sn_t_datatable [ which ( J[] == "J4" ) ]$J ),
                                                   replace = T )
  }
  
  #J2 <-> J3
  if ( promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] >= n_en_j[2] ){
    sn_t_datatable [ which ( J[] == "J2" ) ]$J <- sample ( c ( "J2","J3" ),length ( sn_t_datatable [ which ( J[] == "J2" ) ]$J ),
                                                   prob = c(0.2,0.8),
                                                   replace = T)
    
  }else if ( promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] <= n_en_j[2] ) {
    sn_t_datatable [ which ( J[] == "J2" ) ]$J <- sample ( c ( "J2","J3" ),length ( sn_t_datatable [ which ( J[] == "J2" ) ]$J ),
                                                   replace = T)
    
  }else if ( promedio_conf_J3 < promedio_conf_J2 && n_en_j[3] <= n_en_j[2] ) {
    sn_t_datatable [ which ( J[] == "J3" ) ]$J <- sample ( c ( "J2","J3" ),length ( sn_t_datatable [ which ( J[] == "J3" ) ]$J ),
                                                   prob = c ( 0.8,0.2 ),
                                                   replace = T )
  }else {
    sn_t_datatable [ which ( J[] == "J3" ) ]$J <- sample ( c ( "J2","J3" ),length ( sn_t_datatable[which(J[] == "J3" ) ]$J ),
                                                   replace = T )
  }
  
  #J1 <-> J2
  if ( promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] >= n_en_j[1] ) {
    sn_t_datatable [ which ( J[] == "J1" ) ]$J <- sample ( c ( "J1","J2" ),length ( sn_t_datatable [ which ( J[] == "J1" ) ]$J ),
                                                   prob = c ( 0.2,0.8 ),
                                                   replace = T )
    
  }else if ( promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] <= n_en_j[1] ) {
    sn_t_datatable [ which ( J[] == "J1" ) ]$J <- sample ( c ( "J1","J2" ),length ( sn_t_datatable [ which ( J[] == "J1" ) ]$J ),
                                                   replace = T )
    
  }else if ( promedio_conf_J2 < promedio_conf_J1 && n_en_j[2] <= n_en_j[1] ) {
    sn_t_datatable [ which (J[] == "J2" ) ]$J <- sample ( c ( "J1","J2" ),length ( sn_t_datatable[which(J[] == "J2" ) ]$J ),
                                                   prob = c ( 0.8,0.2 ),
                                                   replace = T )
  }else {
    sn_t_datatable [ which ( J[] == "J2" ) ]$J <- sample ( c ( "J1","J2" ),length ( sn_t_datatable [ which ( J[] == "J2" ) ]$J ),
                                                   replace = T )
  }
  

  return( sn_t_datatable$J )

}


j_sorting()

barplot((replicate(5,(table(j_sorting())),simplify = "array")[[1]]) ) #replicate devuelve una lista, porque los vectores tienen diferentes valorees,table de j_Sorting es un integro, son objetos distintos


#Como paso el replicate, que es una lista, a un vector para barplot #SOLUCIONADO
#abrir los parentesis con enter, r lee un vector de 5 números, barplot espera un vector

i <- replicate( 100,table(j_sorting() ) )

i

i2<- unlist( i )

names( i2 )

sum_rep_j1<- sum( i2[ which ( names( i2 ) == "J1" ) ] )

sum_rep_j2<- sum( i2[ which( names ( i2 ) == "J2" ) ] )

sum_rep_j3<- sum( i2 [ which ( names( i2 ) == "J3" ) ] )

sum_rep_j4<- sum( i2 [ which ( names ( i2 ) == "J4" ) ] )

sum_rep_j5<- sum( i2 [ which ( names( i2 ) == "J5" ) ] )

sum_tot <- c( sum_rep_j1,sum_rep_j2,sum_rep_j3,sum_rep_j4,sum_rep_j5 )

barplot( sum_tot )

sum_tot

#NEcesito que esa lista se transforme en una matriz que transforme cada interacion 
#generar una matrix a partir de la lista, reemplazando NA con 0 #LISTO
#Ajustar valores según nombres
#se puede crear una matriz con nombres #LISTO

#Ver la mejor manera de resolver este problema 
#Realizar diferentes soluciones
#Solución más sencilla, resolverlo en función, colocar table dentro de la función
#El desafío es pedirle a la máquina que complete los valores a pesar de una diferente longuitud en los elementos del vector, con datos perdidos

#Usar de base la solución más ineficiente

#1)Para la proxima pasar de lista a vector, ver las distintas maneras de hacerlo, #LISTO
#2)Fijarme como optimizar el código dentro de la función de j sorting, #LISTO
#3)Fijarse que sea escalable, es decir, que pueda contener 100 j. #LISTO
#4)Documentar la parte del código de la función #LISTO (EN J_SORTING Y EN J_SORTING_OPT_3)

system.time(j_sorting())
system.time(table(j_sorting()))
system.time(replicate(30,j_sorting()))


resultado_rep <- matrix( NA , nrow = 5, ncol = 10, dimnames = list( c ( "J1","J2","J3","J4","J5" ), NULL ) )

resultado_rep [] <- c(n_rep_j2)

resultado_rep

names(i2)
length(i2)


########################################

#PRIMERA OPTIMIZACION IFELSE, TARDA CASI LO MISMO QUE LA ORIGINAL, NO ESCALABLE
j_sorting_opt_1 <- function ()
  
{

  
  n_en_j<- table(sn_t_datatable$J) #Vector que cuyos valores representan la cantidad total de J
  
  sn_t_datatable[which(confianza[] >= 0.7)]$J <- sample(c("J1","J2","J3","J4","J5"), 
                                                        length(sn_t_datatable[which(confianza[] >= 0.7)]$J), 
                                                        prob = c(0.2,0.2,0.2,0.2,0.7),
                                                        replace = T) #Se samplea J nuevamente con una prob mayor para J5 p/c/n cuyo valor en la var.confianza sea mayor a 0.7
  
  promedio_conf_J5 <- mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J5" ) ]$confianza ) #Promedio de los valores de la var confianza p/c/j
  
  promedio_conf_J4 <- mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J4" ) ]$confianza )
  
  promedio_conf_J3 <- mean ( sn_t_datatable [ which(sn_t_datatable$J[] == "J3" ) ]$confianza )
  
  promedio_conf_J2 <- mean ( sn_t_datatable [ which(sn_t_datatable$J[] == "J2" ) ]$confianza )
  
  promedio_conf_J1<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J1" ) ]$confianza )
  
  
  #J4 <-> J5
  
  ifelse( test = promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] >= n_en_j[4],
          
          
          yes = sn_t_datatable[ which( J[] == "J4" ) ]$J <- sample( c( "J4","J5" ),
                                                                    length( sn_t_datatable[which( J[] == "J4" )]$J),
                                                                    prob = c( 0.2,0.8 ),
                                                                    replace =T ),
          
          no = ifelse(test = promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] <= n_en_j[4], 
                      
                      yes = sn_t_datatable[ which( J[] == "J4" ) ]$J <- sample(c("J4","J5"),
                                                                               length(sn_t_datatable[which(J[] == "J4")]$J),
                                                                               replace = T ), 
                      no = ifelse( test = promedio_conf_J5 < promedio_conf_J4 && n_en_j[5] <= n_en_j[4], 
                                   
                                   yes = sn_t_datatable[which(J[] == "J5") ]$J <- sample( c( "J4","J5" ),
                                                                                          length( sn_t_datatable [ which ( J[] == "J5" ) ] $J ),
                                                                                          prob = c( 0.8,0.2 ),
                                                                                          replace = T ),
                                   
                                   no = sn_t_datatable[ which( J[] == "J5" ) ]$J <- sample( c( "J4","J5" ),
                                                                                            length( sn_t_datatable[ which( J[] == "J5" ) ]$J ),
                                                                                            replace = T ) ) ) )
  
  
  
  
  #J3 <-> J4
  
  ifelse( test = promedio_conf_J4 > promedio_conf_J3 && n_en_j[4] >= n_en_j[3],
          
          
          yes = sn_t_datatable[ which( J[] == "J3" ) ]$J <- sample( c( "J3","J4" ),
                                                                    length( sn_t_datatable[which( J[] == "J4" )]$J),
                                                                    prob = c( 0.2,0.8 ),
                                                                    replace =T ),
          
          no = ifelse(test = promedio_conf_J4 > promedio_conf_J3 && n_en_j[4] <= n_en_j[3], 
                      
                      yes = sn_t_datatable[ which( J[] == "J3" ) ]$J <- sample(c("J3","J4"),
                                                                               length(sn_t_datatable[which(J[] == "J3")]$J),
                                                                               replace = T ), 
                      no = ifelse( test = promedio_conf_J4 < promedio_conf_J3 && n_en_j[4] <= n_en_j[3], 
                                   
                                   yes = sn_t_datatable[which(J[] == "J4") ]$J <- sample( c( "J3","J4" ),
                                                                                          length( sn_t_datatable [ which ( J[] == "J4" ) ] $J ),
                                                                                          prob = c( 0.8,0.2 ),
                                                                                          replace = T ),
                                   
                                   no = sn_t_datatable[ which( J[] == "J4" ) ]$J <- sample( c( "J3","J4" ),
                                                                                            length( sn_t_datatable[ which( J[] == "J4" ) ]$J ),
                                                                                            replace = T ) ) ) )
  
  
  
  #J2 <-> J3
  
  ifelse( test = promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] >= n_en_j[2],
          
          
          yes = sn_t_datatable[ which( J[] == "J2" ) ]$J <- sample( c( "J2","J3" ),
                                                                    length( sn_t_datatable[which( J[] == "J2" )]$J),
                                                                    prob = c( 0.2,0.8 ),
                                                                    replace =T ),
          
          no = ifelse(test = promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] <= n_en_j[2], 
                      
                      yes = sn_t_datatable[ which( J[] == "J2" ) ]$J <- sample(c("J2","J3"),
                                                                               length(sn_t_datatable[which(J[] == "J2")]$J),
                                                                               replace = T ), 
                      no = ifelse( test = promedio_conf_J3 < promedio_conf_J2 && n_en_j[3] <= n_en_j[2], 
                                   
                                   yes = sn_t_datatable[which(J[] == "J3") ]$J <- sample( c( "J2","J3" ),
                                                                                          length( sn_t_datatable [ which ( J[] == "J3" ) ] $J ),
                                                                                          prob = c( 0.8,0.2 ),
                                                                                          replace = T ),
                                   
                                   no = sn_t_datatable[ which( J[] == "J3" ) ]$J <- sample( c( "J2","J3" ),
                                                                                            length( sn_t_datatable[ which( J[] == "J3" ) ]$J ),
                                                                                            replace = T ) ) ) )
  #J1 <-> J2
  
  ifelse( test = promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] >= n_en_j[1],
          
          
          yes = sn_t_datatable[ which( J[] == "J1" ) ]$J <- sample( c( "J1","J2" ),
                                                                    length( sn_t_datatable[which( J[] == "J1" )]$J),
                                                                    prob = c( 0.2,0.8 ),
                                                                    replace =T ),
          
          no = ifelse(test = promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] <= n_en_j[1], 
                      
                      yes = sn_t_datatable[ which( J[] == "J1" ) ]$J <- sample(c("J1","J2"),
                                                                               length(sn_t_datatable[which(J[] == "J1")]$J),
                                                                               replace = T ), 
                      
                      no = ifelse( test = promedio_conf_J2 < promedio_conf_J1 && n_en_j[2] <= n_en_j[1], 
                                   
                                   yes = sn_t_datatable[which(J[] == "J2") ]$J <- sample( c( "J1","J2" ),
                                                                                          length( sn_t_datatable [ which ( J[] == "J2" ) ] $J ),
                                                                                          prob = c( 0.8,0.2 ),
                                                                                          replace = T ),
                                   
                                   no = sn_t_datatable[ which( J[] == "J2" ) ]$J <- sample( c( "J1","J2" ),
                                                                                            length( sn_t_datatable[ which( J[] == "J2" ) ]$J ),
                                                                                            replace = T ) ) ) )

  
  
  
  



  return(table(sn_t_datatable$J))
  
}

j_sorting_opt_1()

replicate(10,j_sorting_opt_1())


system.time(j_sorting_opt_1())
system.time(table(j_sorting_opt_1()))


system.time(replicate(150,j_sorting()))
system.time(replicate(150,j_sorting_opt_1())) #Tardan casi lo mismo

#######################################

#Segunda optimización Dplyr #FALLA (DESASTRE TOTAL), NO LLEGA AL RESULTADO DESEADO, FALLOS UBICADOS


j_sorting_opt_2 <- function ()
  
{

  
  n_en_j<- table(sn_t_datatable$J)
  
  sn_t_datatable[which(confianza[] >= 0.7)]$J <- sample(c("J1","J2","J3","J4","J5"), 
                                                        length(sn_t_datatable[which(confianza[] >= 0.7)]$J), 
                                                        prob = c(0.2,0.2,0.2,0.2,0.7),
                                                        replace = T)
  
  promedio_conf_J5<- mean(sn_t_datatable[which(sn_t_datatable$J[] == "J5")]$confianza)
  
  promedio_conf_J4<- mean(sn_t_datatable[which(sn_t_datatable$J[] == "J4")]$confianza)
  
  promedio_conf_J3<-mean(sn_t_datatable[which(sn_t_datatable$J[] == "J3")]$confianza)
  
  promedio_conf_J2<-mean(sn_t_datatable[which(sn_t_datatable$J[] == "J2")]$confianza)
  
  promedio_conf_J1<-mean(sn_t_datatable[which(sn_t_datatable$J[] == "J1")]$confianza)
  
  
  #J1 <-> J2
  
  if_else( promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] >= n_en_j[1],
           
           
           sn_t_datatable[ which( J[] == "J1" ) ]$J <- sample( c( "J1","J2" ),
                                                               length( sn_t_datatable[which( J[] == "J1" )]$J),
                                                               prob = c( 0.2,0.8 ),
                                                               replace =T )
           
           
           %>% length(),
           
           
           if_else(promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] <= n_en_j[1],
                   
                   sn_t_datatable[ which( J[] == "J1" ) ]$J <- sample( c( "J1","J2" ),
                                                                       length( sn_t_datatable[which( J[] == "J1" )]$J),
                                                                       prob = c( 0.2,0.8 ),
                                                                       replace =T )
                   
                   
                   %>% length(), 
                  
                    if_else(promedio_conf_J2 < promedio_conf_J1 && n_en_j[2] <= n_en_j[1],
                           
                           sn_t_datatable[which(J[] == "J2") ]$J <- sample( c( "J1","J2" ),
                                                                            length( sn_t_datatable [ which ( J[] == "J2" ) ] $J ),
                                                                            prob = c( 0.8,0.2 ),
                                                                            replace = T )
                           %>% length(),            
                           
                           sn_t_datatable[ which( J[] == "J2" ) ]$J <- sample( c( "J1","J2" ),
                                                                               length( sn_t_datatable[ which( J[] == "J2" ) ]$J ),
                                                                               replace = T )
                           %>% length()))
           
  
  )
  
  #J2 <-> J3
  
  if_else( promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] >= n_en_j[2],
           
           
           sn_t_datatable[ which( J[] == "J2" ) ]$J <- sample( c( "J2","J3" ),
                                                               length( sn_t_datatable[which( J[] == "J2" )]$J),
                                                               prob = c( 0.2,0.8 ),
                                                               replace =T )
           
           
           %>% length(),
           
           
           if_else(promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] <= n_en_j[2],
                   
                   sn_t_datatable[ which( J[] == "J2" ) ]$J <- sample( c( "J2","J3" ),
                                                                       length( sn_t_datatable[which( J[] == "J2" )]$J),
                                                                       prob = c( 0.2,0.8 ),
                                                                       replace =T )
                   
                   
                   %>% length(), 
                   
                   if_else(promedio_conf_J3 < promedio_conf_J2 && n_en_j[3] <= n_en_j[2],
                           
                           sn_t_datatable[which(J[] == "J3") ]$J <- sample( c( "J2","J3" ),
                                                                            length( sn_t_datatable [ which ( J[] == "J3" ) ] $J ),
                                                                            prob = c( 0.8,0.2 ),
                                                                            replace = T )
                           %>% length(),
                           
                           sn_t_datatable[ which( J[] == "J3" ) ]$J <- sample( c( "J2","J3" ),
                                                                               length( sn_t_datatable[ which( J[] == "J3" ) ]$J ),
                                                                               replace = T )
                           %>% length())))
           

                   
  
  #J4 <-> J5
  
  if_else( promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] >= n_en_j[4],
           
           
           sn_t_datatable[ which( J[] == "J4" ) ]$J <- sample( c( "J4","J5" ),
                                                               length( sn_t_datatable[which( J[] == "J4" )]$J),
                                                               prob = c( 0.2,0.8 ),
                                                               replace =T )
           
           
           %>% length(),
           
           
           if_else(promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] <= n_en_j[4],
                   
                   sn_t_datatable[ which( J[] == "J4" ) ]$J <- sample( c( "J4","J5" ),
                                                                       length( sn_t_datatable[which( J[] == "J4" )]$J),
                                                                       prob = c( 0.2,0.8 ),
                                                                       replace =T )
                   
                   
                   %>% length(),
                   
                   if_else(promedio_conf_J5 < promedio_conf_J4 && n_en_j[5] <= n_en_j[4],
                                       
                                       sn_t_datatable[which(J[] == "J5") ]$J <- sample( c( "J4","J5" ),
                                                                                        length( sn_t_datatable [ which ( J[] == "J5" ) ] $J ),
                                                                                        prob = c( 0.8,0.2 ),
                                                                                        replace = T )
                                       %>% length(),
                           sn_t_datatable[ which( J[] == "J5" ) ]$J <- sample( c( "J4","J5" ),
                                                                               length( sn_t_datatable[ which( J[] == "J5" ) ]$J ),
                                                                               replace = T )
                           %>% length() ) ) )
  
  
  
  
  
  return(table(sn_t_datatable$J))
  
}

j_sorting_opt_2()


system.time(replicate(50,j_sorting()))
system.time(replicate(50,j_sorting_opt_1())) #Tardan casi lo mismo
system.time(replicate(50,j_sorting_opt_2()))

install.packages("Rcpp")


######################################
#INTENTO DE SALVAR EL FALLO ANTERIOR, TARDA MUCHO, NO ESCALABLE, ESTÁ INCOMPLETO
j_sorting_opt_2 <- function ()
  
{
  
  
  n_en_j<- table(sn_t_datatable$J)
  
  sn_t_datatable[which(confianza[] >= 0.7)]$J <- sample(c("J1","J2","J3","J4","J5"), 
                                                        length(sn_t_datatable[which(confianza[] >= 0.7)]$J), 
                                                        prob = c(0.2,0.2,0.2,0.2,0.7),
                                                        replace = T)
  
  promedio_conf_J5<- mean(sn_t_datatable[which(sn_t_datatable$J[] == "J5")]$confianza)
  
  promedio_conf_J4<- mean(sn_t_datatable[which(sn_t_datatable$J[] == "J4")]$confianza)
  
  promedio_conf_J3<-mean(sn_t_datatable[which(sn_t_datatable$J[] == "J3")]$confianza)
  
  promedio_conf_J2<-mean(sn_t_datatable[which(sn_t_datatable$J[] == "J2")]$confianza)
  
  promedio_conf_J1<-mean(sn_t_datatable[which(sn_t_datatable$J[] == "J1")]$confianza)
  
  resultado_opt_2 <- matrix(0, nrow = 5, ncol = 1)
  
  #J1 <-> J2
  
  if_else( promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] >= n_en_j[1],
           
           
         resultado_opt_2[2,] <- length(sn_t_datatable[ which( J[] == "J2" ) ]$J) + length ( which (sample( c( "J1","J2" ),
                                                               length( sn_t_datatable[which( J[] == "J1" )]$J),
                                                               prob = c( 0.2,0.8 ),
                                                               replace =T )  == "J2" ) ) 
           ,
           
           
           if_else(promedio_conf_J2 > promedio_conf_J1 && n_en_j[2] <= n_en_j[1],
                   
                   resultado_opt_2[2,] <- length (sn_t_datatable[ which( J[] == "J2" ) ]$J) + length( which( sample ( c( "J1","J2" ),
                                                                       length( sn_t_datatable[which( J[] == "J1" )]$J),
                                                                       replace =T ) == "J2" ) )
                   
                   
                  , 
                   
                   if_else(promedio_conf_J2 < promedio_conf_J1 && n_en_j[2] <= n_en_j[1],
                           
                           resultado_opt_2[1,] <- length(sn_t_datatable[which(J[] == "J1") ]$J) + length(which(sample( c( "J1","J2" ),
                                                                            length( sn_t_datatable [ which ( J[] == "J2" ) ] $J ),
                                                                            prob = c( 0.8,0.2 ),
                                                                            replace = T ) == "J1" ) )
                           ,            
                           
                           resultado_opt_2[1,] <- length(sn_t_datatable[ which( J[] == "J1" ) ]$J) + length(which(sample( c( "J1","J2" ),
                                                                                              length( sn_t_datatable [ which ( J[] == "J2" ) ] $J ),
                                                                                              prob = c( 0.8,0.2 ),
                                                                                              replace = T ) == "J1" ) )
                          ) )
           
           
  )
  
  #J2 <-> J3
  
  if_else( promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] >= n_en_j[2],
           
           
           resultado_opt_2[3,] <- length(sn_t_datatable[ which( J[] == "J3" ) ]$J) + length(which(sample( c( "J2","J3" ),
                                                               length( sn_t_datatable[which( J[] == "J2" )]$J),
                                                               prob = c( 0.2,0.8 ),
                                                               replace =T ) == "J3") )
           
           
          ,
           
           
           if_else(promedio_conf_J3 > promedio_conf_J2 && n_en_j[3] <= n_en_j[2],
                   
                   resultado_opt_2[3,] <- length(sn_t_datatable[ which( J[] == "J3" ) ]$J) + length(which(sample( c( "J2","J3" ),
                                                                       length( sn_t_datatable[which( J[] == "J2" )]$J),
                                                                       replace =T ) == "J3" ) )
                   , 
                   
                   if_else(promedio_conf_J3 < promedio_conf_J2 && n_en_j[3] <= n_en_j[2],
                           
                           resultado_opt_2[2,] <- length(sn_t_datatable[which(J[] == "J2") ]$J) + length(which(sample( c( "J2","J3" ),
                                                                            length( sn_t_datatable [ which ( J[] == "J3" ) ] $J ),
                                                                            prob = c( 0.8,0.2 ),
                                                                            replace = T ) == "J2" ) )
                           ,
                           
                           resultado_opt_2[2,] <- length(sn_t_datatable[ which( J[] == "J2" ) ]$J) + length(which(sample( c( "J2","J3" ),
                                                                               length( sn_t_datatable[ which( J[] == "J3" ) ]$J ),
                                                                               replace = T ) == "J2" ) )
                           ) ) )
  
  
  
  
  #J4 <-> J5
  
  if_else( promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] >= n_en_j[4],
           
           
           resultado_opt_2[5,] <- length(sn_t_datatable[ which( J[] == "J5" ) ]$J) + length(which(sample( c( "J4","J5" ),
                                                               length( sn_t_datatable[which( J[] == "J4" )]$J),
                                                               prob = c( 0.2,0.8 ),
                                                               replace =T ) == "J5" ) )
           
           
           ,
           
           
           if_else(promedio_conf_J5 > promedio_conf_J4 && n_en_j[5] <= n_en_j[4],
                   
                   resultado_opt_2[5,] <- length(sn_t_datatable[ which( J[] == "J5" ) ]$J) + length(which(sample( c( "J4","J5" ),
                                                                                           length( sn_t_datatable[which( J[] == "J4" )]$J),
                                                                                           replace =T ) == "J5") )
                   
                   
                   ,
                   
                   if_else(promedio_conf_J5 < promedio_conf_J4 && n_en_j[5] <= n_en_j[4],
                           
                           resultado_opt_2[4,] <- length(sn_t_datatable[which(J[] == "J4") ]$J) + length(which(sample( c( "J4","J5" ),
                                                                            length( sn_t_datatable [ which ( J[] == "J5" ) ] $J ),
                                                                            prob = c( 0.8,0.2 ),
                                                                            replace = T ) == "J4" ) )
                          ,
                           resultado_opt_2[4,] <-length(sn_t_datatable[which(J[] == "J4") ]$J) + length(which(sample( c( "J4","J5" ),
                                                                                                                      length( sn_t_datatable [ which ( J[] == "J5" ) ] $J ),
                                                                                                                      prob = c( 0.8,0.2 ),
                                                                                                                      replace = T ) == "J4" ) )
                            ) ) )
  
  
  
  
  
  return(resultado_opt_2)
  
}


j_sorting_opt_2()

library(dplyr)


system.time(replicate(50,j_sorting()))
system.time(replicate(50,j_sorting_opt_1())) #Tardan casi lo mismo
system.time(replicate(50,j_sorting_opt_2()))

#######################################

#TERCERA OPTIMIZACIÓN, ALIAS LA QUE SE ME OCURRIÓ ANOCHE
#VENTAJAS -> ESCALABLE (POTENCIALMENTE, HAY QUE ESCRIBIR DE FORMA MANUAL UNA PARTE DE CÓDIGO NUEVA P/C/VAR),RÁPIDA,
#MATRIZ CON NOMBRE Y CON LOS VALORES DE CADA ITERACIÓN, TIENE EN CUENTA N_EN_J Y SE LE AGREGA VALOR DE INTERDEPENDENCIA, 
#PARA PROBABILIDAD USA FORMULA DEL MODELO DE LAS ABEJAS
#DESVENTAJAS -> NO TIENE EN CUENTA LA DISCREPANCIA DE OPCIÓN
#(J1 PUEDE POTENCIALMENTE SER SAMPLEADO NUEVAMENTE EN J5)



j_sorting_opt_3 <- function (){
  
  sn_t_datatable$J <- sample ( c( "J1","J2","J3","J4","J5","J6","J7","J8"), 100, #Sampleo aleatorio para crear var J en el datatable
                             replace = T )
  
  sn_t_datatable$confianza <- runif( 100, 0, 1 ) #runif aleatorio para crear var confianza en el datatable
  
  sn_t_datatable[ which ( confianza[] >= 0.7 ) ]$J <- sample ( c( "J1","J2","J3","J4","J5","J6","J7","J8" ), 
                                                        length ( sn_t_datatable[ which( confianza[] >= 0.7 ) ]$J ), 
                                                        prob = c( 0.2,0.2,0.2,0.2,0.7,0.2,0.2,0.2 ),
                                                        replace = T ) #Sampleo con prob modificadas según el nivel de confianza d/c/n, si tienen confianza mayor a 0.7, tienen mayor prob de ser sampleados c/j5

  n_en_j<- table ( sn_t_datatable$J ) #Vector que contiene la cantidad de n sampleados p/j
  
  promedio_conf_J8<- mean( sn_t_datatable [ which ( sn_t_datatable$J[] == "J8" ) ]$confianza )
  
  promedio_conf_J7<- mean( sn_t_datatable [ which ( sn_t_datatable$J[] == "J7" ) ]$confianza )
  
  promedio_conf_J6<- mean( sn_t_datatable [ which ( sn_t_datatable$J[] == "J6" ) ]$confianza )

  promedio_conf_J5<- mean( sn_t_datatable [ which ( sn_t_datatable$J[] == "J5" ) ]$confianza ) #Promedio de la variable 
                                                                                               #confianza d/c/n en c/j
  promedio_conf_J4<- mean ( sn_t_datatable [ which( sn_t_datatable$J[] == "J4" ) ]$confianza )

  promedio_conf_J3<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J3" ) ]$confianza )

  promedio_conf_J2<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J2" ) ]$confianza )

  promedio_conf_J1<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J1" ) ]$confianza )

  promedio_tot_conf <- c( promedio_conf_J1,promedio_conf_J2,promedio_conf_J3,promedio_conf_J4,promedio_conf_J5,
                          promedio_conf_J6,promedio_conf_J7,promedio_conf_J8) 
  
  prop_n_en_j<- table ( sn_t_datatable$J )/100 #Proporción de n en c/j
  
  interdep <- runif ( 1,0,1 ) #Variable interdepencia, si es = 0, c/j tiene una prob de ser sampleado a igual al promedio de confianza d/c/j
                             # si = 1, c/j tiene una prob igual a la proporción de n en j de ser sampleado
  
  prob_j <- ( 1-interdep )*promedio_tot_conf + interdep*prop_n_en_j #Formula extraída del paper de List,2009 (paper abejas)
  
  distancia <- c(10,20,20,20,10,10,5,5)/100 #Intento de agregar dicrepancia por distancia de opinion, cuanto más alto es el valor, aumenta la prob. de ser sampleado. 
                              #Un valor alto implica menor distancia y mayor prob. de ser sampleado
  
  prob_j_mod_por_distancia <- prob_j + distancia

  sn_t_datatable[]$J <- sample ( c( "J1","J2","J3","J4","J5","J6","J7","J8" ), 100, prob = prob_j_mod_por_distancia, replace = T ) #Sampleo de J c/prob ajustadas según formula

return ( table ( sn_t_datatable$J ) )

}

j_sorting_opt_3()

i3 <- replicate ( 100,j_sorting_opt_3() )

i3

sum_tot<- c ( sum ( i3[1,] ),sum ( i3[2,] ),sum ( i3[3,] ),sum ( i3[4,] ),sum ( i3[5,] ),
              sum ( i3[6,] ),sum ( i3[7,] ),sum ( i3[8,] ) ) 

barplot ( sum_tot )

system.time ( replicate(50,j_sorting() ) )       #Funcion original
system.time ( replicate(50,j_sorting_opt_1() ) ) #Tardan casi lo mismo
system.time ( replicate(50,j_sorting_opt_2() ) ) #Tarda más 
system.time ( replicate(50,j_sorting_opt_3() ) ) #Tarda menos (la mitad del tiempo de la original)

####PRUEBA DE PARTE DE LA FUNCIÓN FUERA DE LA MISMA
n_en_j<- table ( sn_t_datatable$J ) #Vector que contiene la cantidad de n sampleados p/j

promedio_conf_J5<- mean( sn_t_datatable [ which ( sn_t_datatable$J[] == "J5" ) ]$confianza ) #Promedio de la variable 
                                                                                             #confianza d/c/n en c/j
promedio_conf_J4<- mean ( sn_t_datatable [ which( sn_t_datatable$J[] == "J4" ) ]$confianza )

promedio_conf_J3<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J3" ) ]$confianza )

promedio_conf_J2<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J2" ) ]$confianza )

promedio_conf_J1<-mean ( sn_t_datatable [ which ( sn_t_datatable$J[] == "J1" ) ]$confianza )

promedio_tot_conf <- c( promedio_conf_J1,promedio_conf_J2,promedio_conf_J3,promedio_conf_J4,promedio_conf_J5 ) 

prop_n_en_j<- table ( sn_t_datatable$J )/100 #Proporción de n en c/j

interdep <- runif ( 1,0,1 ) #Variable interdepencia, si es = 0, c/j tiene una prob de ser sampleado a igual al promedio de confianza d/c/j
# si = 1, c/j tiene una prob igual a la proporción de n en j de ser sampleado

prob_j <- ( 1-interdep )*promedio_tot_conf + interdep*prop_n_en_j

distancia <- c(10,30,40,30,10)/100

prob_j

distancia

prob_j + distancia
 





                        