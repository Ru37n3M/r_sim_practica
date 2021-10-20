#TERCERA OPTIMIZACIÓN, ALIAS LA QUE SE ME OCURRIÓ ANOCHE
#VENTAJAS -> ESCALABLE (POTENCIALMENTE, HAY QUE ESCRIBIR DE FORMA MANUAL UNA PARTE DE CÓDIGO NUEVA P/C/VAR),RÁPIDA,
#MATRIZ CON NOMBRE Y CON LOS VALORES DE CADA ITERACIÓN, TIENE EN CUENTA N_EN_J Y SE LE AGREGA VALOR DE INTERDEPENDENCIA, 
#PARA PROBABILIDAD USA FORMULA DEL MODELO DE LAS ABEJAS
#DESVENTAJAS -> NO TIENE EN CUENTA LA DISCREPANCIA DE OPCIÓN
#(J1 PUEDE POTENCIALMENTE SER SAMPLEADO NUEVAMENTE EN J5)

Sn_t <- function() {
  
  spam <- replicate(n = 1000, expr = sample (0:1, 1000, replace = T), 
                    simplify = T)#spam produce valores de 0 y 1, de forma aleatoria, en 100 tiempos, 
  #es decir, produce 1 valor entre 0 y 1 por cada una de las filas en 100 columnas
  
  
  abejas_buscando <- sum(rbind(spam))#Se suman los valores por cada fila en spam
  
  
  nidos_encontrados <- sample(c(0, "K1","K2","K3","K4","K5"), abejas_buscando,
                              replace = T)#En nidos_encontrados se producen los caracteres definidos de forma aleatoria 
  #por el numero definido en abejas buscando
  
  resultado <- matrix(NA, nrow = 1000, ncol = 1000)#Creamos una matriz que nos permita contener la informacion producida por spam
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

guardado_Sn_t <- Sn_t() #Guardamos la función para mantener los resultados obtenidos

library(dplyr)

sn_t_df<- tbl_df(as.data.frame(guardado_Sn_t)) #Conversión de matriz a data_frame 


sn_t_datatable<- data.table::as.data.table(sn_t_df)  #Data_table permite la modificación de var existentes y agregado
#de nuevas variables en el data_frame


j_sorting_opt_3 <- function (){
  
  sn_t_datatable$J <- sample ( c( "J1","J2","J3","J4","J5","J6","J7","J8"), 1000, #Sampleo aleatorio para crear var J en el datatable
                               replace = T )
  
  sn_t_datatable$confianza <- runif( 1000, 0, 1 ) #runif aleatorio para crear var confianza en el datatable
  
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
  
  sn_t_datatable[]$J <- sample ( c( "J1","J2","J3","J4","J5","J6","J7","J8" ), 1000, prob = prob_j_mod_por_distancia, replace = T ) #Sampleo de J c/prob ajustadas según formula
  
  return ( table ( sn_t_datatable$J ) )
  
}

j_sorting_opt_3()

i3 <- replicate ( 100,j_sorting_opt_3() )

i3

sum_tot<- c ( sum ( i3[1,] ),sum ( i3[2,] ),sum ( i3[3,] ),sum ( i3[4,] ),sum ( i3[5,] ),
              sum ( i3[6,] ),sum ( i3[7,] ),sum ( i3[8,] ) ) 

df_sumrep_tot<- data.frame(J=c("J1","J2","J3","J4","J5","J6","J7","J8"), n=sum_tot)

ggplot(data=df_sumrep_tot, aes(x=J, y=n)) + 
  geom_bar(stat = "identity", color="black", fill="steelblue") + 
  geom_text(aes(label=n), nudge_x = -0.5, nudge_y = -3000, vjust=-1, size=3.5)+
  xlab("Cantidad de J") + 
  ylab("Total de n en J")+
  theme_minimal()+
  coord_flip()


system.time ( replicate(50,j_sorting_opt_3() ) ) 

  
  
