#' Formulación de Alimentos con el Metódo Taguchi
#'
#' formula una propuesta de alimento con los factores(ingredientes o residuos orgánicos) que se le asignen, siempre y cuando se cuenten con los datos solicitados
#'
#' @name Taguchi
#' @param Porcentajes contiene los porcentajes de cada factor (ingrediente) de acuerdo a el nivel 1 o 2
#' @param Arrreglo_Ortogonal tabla que contiene las pruebas y en que nivel se aplica cada factor, tabla de 7 filas por 1 +n repeticiones
#' @param DatosT Es una tabla de 1+n filas (n de acuedo al numero de pruebas) y 13 columnas (No Exp),(C_inicio),(C_final) peso de la muestras para ceniza, (H_inicial)	(H_final) peso para humedad, 	(GC_inicial)	(GC_final) peso grasa,	(P_inicial) (vHCl)	(N_HCl)	(J_inicial)	(PPI)	(PFC)
#' @return devuelve un data.frame que contiene el nombre de los factores y al nivel que deben de usarse el la formulacion del alimento, el porcentaje de los niveles los determina el usuario
#' @export
#' #'
#' @examples
#' \dontrun{
#' # 1.- Si cuentas con los datos en archivos scv., solo faltaria dar la ruta
#' library(FAMTaguchi) ###Llamamos a la BIblioteca FAMTaguchi
#' rm(list = ls())
#' Porcentajes<- "ruta/nombre_del_archivo.csv"
#' datos1<- read.csv(Porcentajes)
#' Arreglo_Ortogonal<- ""ruta/nombre_del_archivo.csv"
#' datos2<- read.csv(Arreglo_Ortogonal)
#' DatosT<- ""ruta/nombre_del_archivo.csv"
#' datos3<- read.csv(DatosT)
#' Taguchi<- function(A= data.frame(Porcentajes),B= data.frame(Arreglo_Ortogonal),C=data.frame(DatosT)) ##Llamamos a la funcion
#' ###### NO correra los archivos estan vacios #######
#' }
#' 
Taguchi<- function(A= data.frame(),B= data.frame(),C=data.frame()){

  No.Niveles<- length(A[1,-1])
  No.Experimentos<- (C[ ,1])
  No.Factores<- length(A[ ,1])
  ### CALCULO DE PORCENTAJES DE CADA FACTOR
  a<- C[ ,4]
  b<- C[ ,5]
  porcentaje_humedad<- function(H_inicial, H_final){
    H<- ((H_inicial-H_final)/H_inicial)*100
    return(H)
  }
  PH<- porcentaje_humedad(H_inicial = a, H_final = b)

  c<- C[ ,2]
  d<- C[ ,3]
  porcentaje_ceniza<- function(pms, pmi){
    c<- ((pms-pmi)/pms)*100
    return(c)
  }
  PC<- porcentaje_ceniza(pms = c, pmi = d)

  e<- C[ ,6]
  f<- C[ ,7]
  grasa_cruda<- function(pms2, pmsd){
    z<- ((pms2-pmsd)/pms2)*100
    return(z)
  }
  Z<- grasa_cruda(pms2 = e, pmsd = f)

  g<- C[ ,8]
  h<- C[ ,9]
  i<- C[ ,10]
  proteina_cruda<- function(vhcl, nhcl, pm){
    p<- ((vhcl*nhcl)/pm)*(0.014)*(100)*(6.25)
    return(p)
  }
  P<- proteina_cruda(vhcl = h, nhcl = i, pm = g)

  j<- C[ ,12]
  k<- C[ ,13]
  fibra_cruda<- function(ppi, ppfc, pm){
    j<- ((ppi-ppfc)*100)/pm
    return(j)
  }
  J<- fibra_cruda(ppi = j, ppfc = k, pm = 2)

  Carbohidratos<- function(PH, PC, Z, P, J){
    w<- (100-PH-PC-Z-P-J)
    return(w)
  }
  W<- Carbohidratos(PH=PH, PC=PC, Z=Z, P=P, J=J)

  #### DATA.FRAME PARA ACOMODAR LOS RESULTADOS

  PDDRF<- data.frame(Experimentos= c(No.Experimentos),
                     Porc_Ceniza= c(PC),
                     Porc_Humedad= c(PH),
                     Porc_GrasaC= c(Z),
                     Porc_Proteina= c(P),
                     Porc_Fibra= c(J),
                     Carbohidratos= c(W))

  #### EVALUACION DE FACTORES DE ACUERDO AL NIVEL
  B[ ,"Factores_No.Exp"]<- list(NULL) # eliminamos primera columna para igualar longitudes

  ### Totales para Proteina ###
  ### Cascarilla de Arroz
  ab<- which(B[1,]==1)
  CAL1<- sum(PDDRF[ab,5])
  ab2<- which(B[1, ]==2)
  CAL2<- sum(PDDRF[ab2,5])
  ### Cascara de Platano
  ab3<- which(B[2, ]==1)
  CDPL1<- sum(PDDRF[ab3,5])
  ab4<- which(B[2, ]==2)
  CDPL2<- sum(PDDRF[ab4,5])
  ###Cascara de Huevo
  ab5<- which(B[3, ]==1)
  CDHL1<- sum(PDDRF[ab5,5])
  ab6<- which(B[3, ]==2)
  CDHL2<- sum(PDDRF[ab6,5])
  ### Grasa de Vaca
  ab7<- which(B[4, ]==1)
  GDVL1<- sum(PDDRF[ab7,5])
  ab8<- which(B[4, ]==2)
  GDVL2<- sum(PDDRF[ab8,5])
  ### Frijol
  ab9<- which(B[5, ]==1)
  FL1<- sum(PDDRF[ab9,5])
  ab10<- which(B[5, ]==2)
  FL2<- sum(PDDRF[ab10,5])
  ### MiEm1
  ab11<- which(B[6, ]==1)
  MiEm1L1<- sum(PDDRF[ab11,5])
  ab12<- which(B[6, ]==2)
  MiEm1L2<- sum(PDDRF[ab12,5])
  ### Errores
  ab13<- which(B[7, ]==1)
  el1<- sum(PDDRF[ab13, 5])
  ab14<- which(B[7, ]==2)
  el2<- sum(PDDRF[ab14, 5])
  ab15<- which(B[8, ]==1)
  e2l1<- sum(PDDRF[ab15, 5])
  ab16<- which(B[8, ]==2)
  e2l2<- sum(PDDRF[ab16, 5])
  #### T Proteina
  TOTALES_PROTEINA<-data.frame(Proteina= c("N1", "N2", "Suma"),
                               C.Arroz= c(CAL1, CAL2, (CAL1+CAL2)),
                               C.Platano= c(CDPL1, CDPL2, (CDPL1+CDPL2)),
                               C.Huevo= c(CDHL1, CDHL2, (CDHL1+CDHL2)),
                               G.Vaca= c(GDVL1, GDVL2, (GDVL1+GDVL2)),
                               Frijol= c(FL1, FL2, (FL1+FL2)),
                               MiEm1= c(MiEm1L1, MiEm1L2, (MiEm1L1+MiEm1L2)),
                               Error1= c(el1, el2, (el1+el2)),
                               Error2= c(e2l1, e2l2, (e2l1+e2l2)))

  ### Totales para Ceniza ###
  ### Cascarilla de Arroz
  ab<- which(B[1,]==1)
  CAL11<- sum(PDDRF[ab,2])
  ab2<- which(B[1, ]==2)
  CAL21<- sum(PDDRF[ab2,2])
  ### Cascara de Platano
  ab3<- which(B[2, ]==1)
  CDPL11<- sum(PDDRF[ab3,2])
  ab4<- which(B[2, ]==2)
  CDPL21<- sum(PDDRF[ab4,2])
  ###Cascara de Huevo
  ab5<- which(B[3, ]==1)
  CDHL11<- sum(PDDRF[ab5,2])
  ab6<- which(B[3, ]==2)
  CDHL21<- sum(PDDRF[ab6,2])
  ### Grasa de Vaca
  ab7<- which(B[4, ]==1)
  GDVL11<- sum(PDDRF[ab7,2])
  ab8<- which(B[4, ]==2)
  GDVL21<- sum(PDDRF[ab8,2])
  ### Frijol
  ab9<- which(B[5, ]==1)
  FL11<- sum(PDDRF[ab9,2])
  ab10<- which(B[5, ]==2)
  FL21<- sum(PDDRF[ab10,2])
  ### MiEm1
  ab11<- which(B[6, ]==1)
  MiEm1L11<- sum(PDDRF[ab11,2])
  ab12<- which(B[6, ]==2)
  MiEm1L21<- sum(PDDRF[ab12,2])
  ### Errores
  ab13<- which(B[7, ]==1)
  el1<- sum(PDDRF[ab13, 2])
  ab14<- which(B[7, ]==2)
  el2<- sum(PDDRF[ab14, 2])
  ab15<- which(B[8, ]==1)
  e2l1<- sum(PDDRF[ab15, 2])
  ab16<- which(B[8, ]==2)
  e2l2<- sum(PDDRF[ab16, 2])

  #### T Ceniza
  TOTALES_CENIZA<-data.frame(Ceniza = c("N1", "N2", "Suma"),
                             C.Arroz= c(CAL11, CAL21, (CAL11+CAL21)),
                             C.Platano= c(CDPL11, CDPL21, (CDPL11+CDPL21)),
                             C.Huevo= c(CDHL11, CDHL21, (CDHL11+CDHL21)),
                             G.Vaca= c(GDVL11, GDVL21, (GDVL11+GDVL21)),
                             Frijol= c(FL11, FL21, (FL11+FL21)),
                             MiEm1= c(MiEm1L11, MiEm1L21, (MiEm1L11+MiEm1L21)),
                             Error1= c(el1, el2, (el1+el2)),
                             Error2= c(e2l1, e2l2, (e2l1+e2l2)))

  #### Totales para Fibra ###
  ### Cascarilla de Arroz
  ab<- which(B[1,]==1)
  CAL12<- sum(PDDRF[ab,6])
  ab2<- which(B[1, ]==2)
  CAL22<- sum(PDDRF[ab2,6])
  ### Cascara de Platano
  ab3<- which(B[2, ]==1)
  CDPL12<- sum(PDDRF[ab3,6])
  ab4<- which(B[2, ]==2)
  CDPL22<- sum(PDDRF[ab4,6])
  ###Cascara de Huevo
  ab5<- which(B[3, ]==1)
  CDHL12<- sum(PDDRF[ab5,6])
  ab6<- which(B[3, ]==2)
  CDHL22<- sum(PDDRF[ab6,6])
  ### Grasa de Vaca
  ab7<- which(B[4, ]==1)
  GDVL12<- sum(PDDRF[ab7,6])
  ab8<- which(B[4, ]==2)
  GDVL22<- sum(PDDRF[ab8,6])
  ### Frijol
  ab9<- which(B[5, ]==1)
  FL12<- sum(PDDRF[ab9,6])
  ab10<- which(B[5, ]==2)
  FL22<- sum(PDDRF[ab10,6])
  ### MiEm1
  ab112<- which(B[6, ]==1)
  MiEm1L12<- sum(PDDRF[ab11,6])
  ab122<- which(B[6, ]==2)
  MiEm1L22<- sum(PDDRF[ab12,6])
  ### Errores
  ab13<- which(B[7, ]==1)
  el1<- sum(PDDRF[ab13, 6])
  ab14<- which(B[7, ]==2)
  el2<- sum(PDDRF[ab14, 6])
  ab15<- which(B[8, ]==1)
  e2l1<- sum(PDDRF[ab15, 6])
  ab16<- which(B[8, ]==2)
  e2l2<- sum(PDDRF[ab16, 6])

  #### T Fibra
  TOTALES_FIBRA<-data.frame(Fibra = c("N1", "N2", "Suma"),
                            C.Arroz= c(CAL12, CAL22, (CAL12+CAL22)),
                            C.Platano= c(CDPL12, CDPL22, (CDPL12+CDPL22)),
                            C.Huevo= c(CDHL12, CDHL22, (CDHL12+CDHL22)),
                            G.Vaca= c(GDVL12, GDVL22, (GDVL12+GDVL22)),
                            Frijol= c(FL12, FL22, (FL12+FL22)),
                            MiEm1= c(MiEm1L12, MiEm1L22, (MiEm1L12+MiEm1L22)),
                            Error1= c(el1, el2, (el1+el2)),
                            Error2= c(e2l1, e2l2, (e2l1+e2l2)))

  ### Totales para Grasa cruda ###
  ### Cascarilla de Arroz
  ab<- which(B[1,]==1)
  CAL13<- sum(PDDRF[ab,4])
  ab2<- which(B[1, ]==2)
  CAL23<- sum(PDDRF[ab2,4])
  ### Cascara de Platano
  ab3<- which(B[2, ]==1)
  CDPL13<- sum(PDDRF[ab3,4])
  ab4<- which(B[2, ]==2)
  CDPL23<- sum(PDDRF[ab4,4])
  ###Cascara de Huevo
  ab5<- which(B[3, ]==1)
  CDHL13<- sum(PDDRF[ab5,4])
  ab6<- which(B[3, ]==2)
  CDHL23<- sum(PDDRF[ab6,4])
  ### Grasa de Vaca
  ab7<- which(B[4, ]==1)
  GDVL13<- sum(PDDRF[ab7,4])
  ab8<- which(B[4, ]==2)
  GDVL23<- sum(PDDRF[ab8,4])
  ### Frijol
  ab9<- which(B[5, ]==1)
  FL13<- sum(PDDRF[ab9,4])
  ab10<- which(B[5, ]==2)
  FL23<- sum(PDDRF[ab10,4])
  ### MiEm1
  ab11<- which(B[6, ]==1)
  MiEm1L13<- sum(PDDRF[ab11,4])
  ab12<- which(B[6, ]==2)
  MiEm1L23<- sum(PDDRF[ab12,4])
  ### Errores
  ab13<- which(B[7, ]==1)
  el1<- sum(PDDRF[ab13, 4])
  ab14<- which(B[7, ]==2)
  el2<- sum(PDDRF[ab14, 4])
  ab15<- which(B[8, ]==1)
  e2l1<- sum(PDDRF[ab15, 4])
  ab16<- which(B[8, ]==2)
  e2l2<- sum(PDDRF[ab16, 4])

  #### T Grasa
  TOTALES_GRASA<-data.frame(Grasa= c("N1", "N2", "Suma"),
                            C.Arroz= c(CAL13, CAL23, (CAL13+CAL23)),
                            C.Platano= c(CDPL13, CDPL23, (CDPL13+CDPL23)),
                            C.Huevo= c(CDHL13, CDHL23, (CDHL13+CDHL23)),
                            G.Vaca= c(GDVL13, GDVL23, (GDVL13+GDVL23)),
                            Frijol= c(FL13, FL23, (FL13+FL23)),
                            MiEm1= c(MiEm1L13, MiEm1L23, (MiEm1L13+MiEm1L23)),
                            Error1= c(el1, el2, (el1+el2)),
                            Error2= c(e2l1, e2l2, (e2l1+e2l2)))

  ### Totales para Carbohidratos ###
  ### Cascarilla de Arroz
  ab<- which(B[1,]==1)
  CAL14<- sum(PDDRF[ab,7])
  ab2<- which(B[1, ]==2)
  CAL24<- sum(PDDRF[ab2,7])
  ### Cascara de Platano
  ab3<- which(B[2, ]==1)
  CDPL14<- sum(PDDRF[ab3,7])
  ab4<- which(B[2, ]==2)
  CDPL24<- sum(PDDRF[ab4,7])
  ###Cascara de Huevo
  ab5<- which(B[3, ]==1)
  CDHL14<- sum(PDDRF[ab5,7])
  ab6<- which(B[3, ]==2)
  CDHL24<- sum(PDDRF[ab6,7])
  ### Grasa de Vaca
  ab7<- which(B[4, ]==1)
  GDVL14<- sum(PDDRF[ab7,7])
  ab8<- which(B[4, ]==2)
  GDVL24<- sum(PDDRF[ab8,7])
  ### Frijol
  ab9<- which(B[5, ]==1)
  FL14<- sum(PDDRF[ab9,7])
  ab10<- which(B[5, ]==2)
  FL24<- sum(PDDRF[ab10,7])
  ### MiEm1
  ab11<- which(B[6, ]==1)
  MiEm1L14<- sum(PDDRF[ab11,7])
  ab12<- which(B[6, ]==2)
  MiEm1L24<- sum(PDDRF[ab12,7])
  ### Errores
  ab13<- which(B[7, ]==1)
  el1<- sum(PDDRF[ab13, 7])
  ab14<- which(B[7, ]==2)
  el2<- sum(PDDRF[ab14, 7])
  ab15<- which(B[8, ]==1)
  e2l1<- sum(PDDRF[ab15, 7])
  ab16<- which(B[8, ]==2)
  e2l2<- sum(PDDRF[ab16, 7])

  #### T Carbohidratos
  TOTALES_CARBOHIDRATOS<-data.frame(Carbohidratos= c("N1", "N2", "Suma"),
                                    C.Arroz= c(CAL14, CAL24, (CAL14+CAL24)),
                                    C.Platano= c(CDPL14, CDPL24, (CDPL14+CDPL24)),
                                    C.Huevo= c(CDHL14, CDHL24, (CDHL14+CDHL24)),
                                    G.Vaca= c(GDVL14, GDVL24, (GDVL14+GDVL24)),
                                    Frijol= c(FL14, FL24, (FL14+FL24)),
                                    MiEm1= c(MiEm1L14, MiEm1L24, (MiEm1L14+MiEm1L24)),
                                    Error1= c(el1, el2, (el1+el2)),
                                    Error2= c(e2l1, e2l2, (e2l1+e2l2)))

  ### Totales para Humedad ###
  ### Cascarilla de Arroz
  ab<- which(B[1,]==1)
  CAL15<- sum(PDDRF[ab,3])
  ab2<- which(B[1, ]==2)
  CAL25<- sum(PDDRF[ab2,3])
  ### Cascara de Platano
  ab3<- which(B[2, ]==1)
  CDPL15<- sum(PDDRF[ab3,3])
  ab4<- which(B[2, ]==2)
  CDPL25<- sum(PDDRF[ab4,3])
  ###Cascara de Huevo
  ab5<- which(B[3, ]==1)
  CDHL15<- sum(PDDRF[ab5,3])
  ab6<- which(B[3, ]==2)
  CDHL25<- sum(PDDRF[ab6,3])
  ### Grasa de Vaca
  ab7<- which(B[4, ]==1)
  GDVL15<- sum(PDDRF[ab7,3])
  ab8<- which(B[4, ]==2)
  GDVL25<- sum(PDDRF[ab8,3])
  ### Frijol
  ab9<- which(B[5, ]==1)
  FL15<- sum(PDDRF[ab9,3])
  ab10<- which(B[5, ]==2)
  FL25<- sum(PDDRF[ab10,3])
  ### MiEm1
  ab11<- which(B[6, ]==1)
  MiEm1L15<- sum(PDDRF[ab11,3])
  ab12<- which(B[6, ]==2)
  MiEm1L25<- sum(PDDRF[ab12,3])
  ### Errores
  ab13<- which(B[7, ]==1)
  el1<- sum(PDDRF[ab13, 3])
  ab14<- which(B[7, ]==2)
  el2<- sum(PDDRF[ab14, 3])
  ab15<- which(B[8, ]==1)
  e2l1<- sum(PDDRF[ab15, 3])
  ab16<- which(B[8, ]==2)
  e2l2<- sum(PDDRF[ab16, 3])

  ####  T Humedad
  TOTALES_HUMEDAD<-data.frame(Humedad= c("N1", "N2", "Suma"),
                              C.Arroz= c(CAL15, CAL25, (CAL15+CAL25)),
                              C.Platano= c(CDPL15, CDPL25, (CDPL15+CDPL25)),
                              C.Huevo= c(CDHL15, CDHL25, (CDHL15+CDHL25)),
                              G.Vaca= c(GDVL15, GDVL25, (GDVL15+GDVL25)),
                              Frijol= c(FL15, FL25, (FL15+FL25)),
                              MiEm1= c(MiEm1L15, MiEm1L25, (MiEm1L15+MiEm1L25)),
                              Error1= c(el1, el2, (el1+el2)),
                              Error2= c(e2l1, e2l2, (e2l1+e2l2)))


  #### ANOVAS DE LAS VARIABLES RESPUESTA #####
  TOTALES_CENIZA[ ,"Ceniza"]<- list(NULL)
  sm<- (TOTALES_CENIZA[1,-8]-TOTALES_CENIZA[2,-8])^2/length(No.Experimentos)
  sm1<- data.frame(t(sm))
  sm2<- sm1[,1]
  gm<- c(rep(1,No.Factores),2)
  v<- sm2/gm
  v2<- v[7]*100
  AC<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e.", "Error" ),
                  Sum_Cuadrados= sm2,
                  G.L.= gm,
                  Valor_prob= v,
                  Fexp= sm2/v2)
  AC[7,5]<-NA

  TOTALES_PROTEINA[ ,"Proteina"]<- list(NULL)
  sm<- (TOTALES_PROTEINA[1,-8]-TOTALES_PROTEINA[2,-8])^2/length(No.Experimentos)
  sm1<- data.frame(t(sm))
  sm2<- sm1[,1]
  gm<- c(rep(1,No.Factores),2)
  v<- sm2/gm
  v2<- v[7]
  AP<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e.", "Error" ),
                  Sum_Cuadrados= sm2,
                  G.L.= gm,
                  Valor_prob= v,
                  Fexp= sm2/v2)
  AP[7,5]<-NA

  TOTALES_GRASA[ ,"Grasa"]<- list(NULL)
  sm<- (TOTALES_GRASA[1,-8]-TOTALES_GRASA[2,-8])^2/length(No.Experimentos)
  sm1<- data.frame(t(sm))
  sm2<- sm1[,1]
  gm<- c(rep(1,No.Factores),2)
  v<- sm2/gm
  v2<- v[7]
  AG<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e.", "Error" ),
                  Sum_Cuadrados= sm2,
                  G.L.= gm,
                  Valor_prob= v,
                  Fexp= sm2/v2)
  AG[7,5]<-NA

  TOTALES_FIBRA[ ,"Fibra"]<- list(NULL)
  sm<- (TOTALES_FIBRA[1,-8]-TOTALES_FIBRA[2,-8])^2/length(No.Experimentos)
  sm1<- data.frame(t(sm))
  sm2<- sm1[,1]
  gm<- c(rep(1,No.Factores),2)
  v<- sm2/gm
  v2<- v[7]*10
  AF<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e.", "Error" ),
                  Sum_Cuadrados= sm2,
                  G.L.= gm,
                  Valor_prob= v,
                  Fexp= sm2/v2)
  AF[7,5]<-NA
  AF[-c(4), ]

  TOTALES_CARBOHIDRATOS[ ,"Carbohidratos"]<- list(NULL)
  sm<- (TOTALES_CARBOHIDRATOS[1,-8]-TOTALES_CARBOHIDRATOS[2,-8])^2/length(No.Experimentos)
  sm1<- data.frame(t(sm))
  sm2<- sm1[,1]
  gm<- c(rep(1,No.Factores),2)
  v<- sm2/gm
  v2<- v[7]
  ACH<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e.", "Error" ),
                   Sum_Cuadrados= sm2,
                   G.L.= gm,
                   Valor_prob= v,
                   Fexp= sm2/v2)
  ACH[7,5]<-NA
  ACH[-c(3), ]

  TOTALES_HUMEDAD[ ,"Humedad"]<- list(NULL)
  sm<- (TOTALES_HUMEDAD[1,-8]-TOTALES_HUMEDAD[2,-8])^2/length(No.Experimentos)
  sm1<- data.frame(t(sm))
  sm2<- sm1[,1]
  gm<- c(rep(1,No.Factores),2)
  v<- sm2/gm
  v2<- v[7]
  AH<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e.", "Error" ),
                  Sum_Cuadrados= sm2,
                  G.L.= gm,
                  Valor_prob= v,
                  Fexp= sm2/v2)
  AH[7,5]<-NA
  AH[-c(3), ]

  ##### EFECTO PROMEDIO #####

  EPCeniza<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e."),
                        Nivel1= c(CAL11/4, CDPL11/4, CDHL11/4, GDVL11/4, FL11/4, MiEm1L11/4),
                        NIvel2= c(CAL21/4, CDPL21/4, CDHL21/4, GDVL21/4, FL21/4, MiEm1L21/4))
  EPProteina<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e."),
                          Nivel1= c(CAL1/4, CDPL1/4, CDHL1/4, GDVL1/4, FL1/4, MiEm1L1/4),
                          NIvel2= c(CAL2/4, CDPL2/4, CDHL2/4, GDVL2/4, FL2/4, MiEm1L2/4))
  EPFibra<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e."),
                       Nivel1= c(CAL12/4, CDPL12/4, CDHL12/4, GDVL12/4, FL12/4, MiEm1L12/4),
                       NIvel2= c(CAL22/4, CDPL22/4, CDHL22/4, GDVL22/4, FL22/4, MiEm1L22/4))
  EPGrasa<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e."),
                       Nivel1= c(CAL13/4, CDPL13/4, CDHL13/4, GDVL13/4, FL13/4, MiEm1L13/4),
                       NIvel2= c(CAL23/4, CDPL23/4, CDHL23/4, GDVL23/4, FL23/4, MiEm1L23/4))
  EPCarbohidratos<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e."),
                               Nivel1= c(CAL14/4, CDPL14/4, CDHL14/4, GDVL14/4, FL14/4, MiEm1L14/4),
                               NIvel2= c(CAL24/4, CDPL24/4, CDHL24/4, GDVL24/4, FL24/4, MiEm1L24/4))
  EPHumedad<- data.frame(Efecto= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e."),
                         Nivel1= c(CAL15/4, CDPL15/4, CDHL15/4, GDVL15/4, FL15/4, MiEm1L15/4),
                         NIvel2= c(CAL25/4, CDPL25/4, CDHL25/4, GDVL25/4, FL25/4, MiEm1L25/4))

  ##### EFECTOS SIGNIFICATIVOS #####
  AC1<-AC[-c(4), ]
  ES_Ceniza<- which(AC1[,5]>2)
  ES_Proteina<- which(AP[,5]>2)
  ES_Grasa<- which(AG[,5]>2)
  ACH1<-ACH[-c(1,3,4), ]
  ES_Carbohidratos<- which(ACH1[,5]>2)
  AH1<- AH[-c(3),]
  ES_Humedad<- which(AH1[,5]>2)
  AF1<- AF[-c(4,5),]
  ES_Fibra<- which(AF1[,5]>2)

  ##### FORMULACION ####

  Porcentaje_Frijol<- function(a,b){
    if (a > b){
      return ("Nivel 1")}
    else (a < b)
    {return("Nivel 2")
    }
  }

  fm<- Porcentaje_Frijol(a=EPProteina[5,2], b= EPProteina[5,3])

  P_G<- function(a=c(),b=c()){
    if (a > b){
      return ("Nivel 1")}
    else (a < b)
    {return("Nivel 2")
    }

  }

  fm1<- P_G(a=EPGrasa[4,2], b= EPGrasa[4,3])

  P_C<- function(a=c(),b=c()){
    if (a > b){
      return ("Nivel 1")}
    else (a < b)
    {return("Nivel 2")
    }

  }

  fm2<- P_C(a=EPCarbohidratos[1,2], b= EPCarbohidratos[1,3])
  fm3<- P_C(a=EPCarbohidratos[6,2], b= EPCarbohidratos[6,3])

  P_C<- function(a=c(),b=c()){
    if (a > b){
      return ("Nivel 1")}
    else (a < b)
    {return("Nivel 2")
    }

  }

  fm4<- P_C(a=EPCeniza[3,2], b= EPCeniza[3,3])

  P_H<- function(a=c(),b=c()){
    if (a > b){
      return ("Nivel 1")}
    else (a < b)
    {return("Nivel 2")
    }

  }

  fm5<- P_H(a=EPHumedad[2,2], b= EPHumedad[2,3])

  ###### Alimento Propuesta ######
  RESULTADO<- data.frame(Factor= c("C.Arroz", "C.platano", "C. Huevo", "Grasa", "Frijol", "M.e."),
                         Nivel= c(fm2, fm5, fm4, fm1, fm, fm3))
  return(list("Resultados_Analisis_Proximales"= PDDRF,"Efecto_Ceniza"= EPCeniza, "Efecto_Humedad"= EPHumedad, "Efecto_Proteina"= EPProteina, "Efecto_Grasa"= EPGrasa, "Efecto_Fibra"= EPFibra, "Efecto_Carbohidratos"= EPCarbohidratos,"Alimento_Propuesta"= RESULTADO))

}

