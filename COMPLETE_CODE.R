
#AQUI SE VA A HACER TODO EL CÓDIGO EL CUAL SE VA A PONER EN EL R MARKDOWN
#Importar matrices a objetos de database de R

todos<-read.csv("RAW DATA/B-Co-Occurrence All.csv")
View(todos)
row.names(todos)<-todos[,1]
todos<-todos[,-1]
colnames(todos)<-todos[1,]
todos<-todos[-1,]

#Puse nombres que podamos identificar de mejor manera ya que los archivos
#tambien estan clasificados, asi que siento que genera mucha confusion
#poner matriz_a-j (por ejemplo h_lean "healthy & lean")

h_lean<-read.csv("RAW DATA/C- Co-Occurrence Healthy & Lean.csv")
View(h_lean)
row.names(h_lean)<-h_lean[,1]
h_lean<-h_lean[,-1]
colnames(h_lean)<-h_lean[1,]
h_lean<-h_lean[-1,]

h_obese<-read.csv("RAW DATA/D-Co-Occurrence Healthy & Obese.csv")
View(h_obese)
row.names(h_obese)<-h_obese[,1]
h_obese<-h_obese[,-1]
colnames(h_obese)<-h_obese[1,]
h_obese<-h_obese[-1,]

i_lean<-read.csv("RAW DATA/E-Co-Occurrence IBD & Lean.csv")
View(i_lean)
row.names(i_lean)<-i_lean[,1]
i_lean<-i_lean[,-1]
colnames(i_lean)<-i_lean[1,]
i_lean<-i_lean[-1,]

i_obese<-read.csv("RAW DATA/F-Co-Occurrence IBD & Obese.csv")
View(i_obese)
row.names(i_obese)<-i_obese[,1]
i_obese<-i_obese[,-1]
colnames(i_obese)<-i_obese[1,]
i_obese<-i_obese[-1,]

danish<-read.csv("RAW DATA/G-Co-Occurrence Danish.csv")
View(danish)
row.names(danish)<-danish[,1]
danish<-danish[,-1]
colnames(danish)<-danish[1,]
danish<-danish[-1,]

spanish<-read.csv("RAW DATA/H-Co-Occurrence Spanish.csv")
View(spanish)
row.names(spanish)<-spanish[,1]
spanish<-spanish[,-1]
colnames(spanish)<-spanish[1,]
spanish<-spanish[-1,]

ET1<-read.csv("RAW DATA/I-Co-Occurrence ET1.csv")
View(ET1)
row.names(ET1)<-ET1[,1]
ET1<-ET1[,-1]
colnames(ET1)<-ET1[1,]
ET1<-ET1[-1,]

ET2<-read.csv("RAW DATA/K-Co-Occurrence ET2.csv")
View(ET2)
row.names(ET2)<-ET2[,1]
ET2<-ET2[,-1]
colnames(ET2)<-ET2[1,]
ET2<-ET2[-1,]

ET3<-read.csv("RAW DATA/L-Co-Occurrence ET3.csv")
View(ET3)
row.names(ET3)<-ET3[,1]
ET3<-ET3[,-1]
colnames(ET3)<-ET3[1,]
ET3<-ET3[-1,]

#hacer que R lea el contenido de la tabla como valores numericos
todos[] <- lapply(todos, as.numeric)
h_lean[] <- lapply(h_lean, as.numeric)
h_obese[] <- lapply(h_obese, as.numeric)
i_lean[] <- lapply(i_lean, as.numeric)
i_obese[] <- lapply(i_obese, as.numeric)
danish[] <- lapply(danish, as.numeric)
spanish[] <- lapply(spanish, as.numeric)
ET1[] <- lapply(ET1, as.numeric)
ET2[] <- lapply(ET2, as.numeric)
ET3[] <- lapply(ET3, as.numeric)

#volver todos los dataframes en matrices
m_todos<-as.matrix(todos)
mh_lean<-as.matrix(h_lean)
mh_obese<-as.matrix(h_obese)
mi_lean<-as.matrix(i_lean)
mi_obese<-as.matrix(i_obese)
m_danish<-as.matrix(danish)
m_spanish<-as.matrix(spanish)
m_ET1<-as.matrix(ET1)
m_ET2<-as.matrix(ET2)
m_ET3<-as.matrix(ET3)

#Verificar que sean valores numéricos en la matriz
class(m_todos)
mode(m_todos)

#observar cuantas NA's hay y cambiarlas por 0, ya que son matrices de similitud
sum(is.na(m_todos))
sum(is.na(mh_lean))
sum(is.na(mh_obese))
sum(is.na(mi_lean))
sum(is.na(mi_obese))
sum(is.na(m_danish))
sum(is.na(m_spanish))
sum(is.na(m_ET1))
sum(is.na(m_ET2))
sum(is.na(m_ET3))

m_todos[is.na(m_todos)] <- 0
mh_lean[is.na(mh_lean)] <- 0
mh_obese[is.na(mh_obese)] <- 0
mi_lean[is.na(mi_lean)] <- 0
mi_obese[is.na(mi_obese)] <- 0
m_danish[is.na(m_danish)] <- 0
m_spanish[is.na(m_spanish)] <- 0
m_ET1[is.na(m_ET1)] <- 0
m_ET2[is.na(m_ET2)] <- 0
m_ET3[is.na(m_ET3)] <- 0

#la matriz de todos tiene 2 filas extra vacias, esas las vamos a borrar
dim(m_todos)
rownames(m_todos)
colnames(m_todos)

m_todos <- m_todos[-c(155,156), ]

#empezar a hacer las redes usando el paquete "igraph"
library(igraph)

g_todos <- graph_from_adjacency_matrix(
  m_todos,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

gh_lean <- graph_from_adjacency_matrix(
  mh_lean,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

gh_obese<- graph_from_adjacency_matrix(
  mh_obese,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

gi_lean<- graph_from_adjacency_matrix(
  mi_lean,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

gi_obese<- graph_from_adjacency_matrix(
  mi_obese,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_danish<- graph_from_adjacency_matrix(
  m_danish,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_spanish<- graph_from_adjacency_matrix(
  m_spanish,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_ET1<- graph_from_adjacency_matrix(
  m_ET1,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_ET2<- graph_from_adjacency_matrix(
  m_ET2,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_ET3<- graph_from_adjacency_matrix(
  m_ET3,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

#THRESHOLD PARA QUEDAR CON CONEXIONES FUERTES Y ELIMINAR DEBILES

###--TODOS--###
vs_todos <- m_todos[upper.tri(m_todos)]
t_todos <- mean(vs_todos) + sd(vs_todos)
m_todos_thr <- m_todos 
m_todos_thr[m_todos_thr < t_todos] <- 0  #hacer que todos los valores que esten debajo del threshold sea igual a 0

###--HEALTHY & LEAN/OBESE--###
vs_hl<- mh_lean[upper.tri(mh_lean)]
th_lean<- mean(vs_hl) + sd(vs_hl)
mh_lean_thr<- mh_lean
mh_lean_thr[mh_lean_thr < th_lean] <- 0

vs_ho <- mh_obese[upper.tri(mh_obese)]
th_obese <- mean(vs_ho) + sd(vs_ho)
mh_obese_thr <- mh_obese
mh_obese_thr[mh_obese_thr < th_obese] <- 0

##--IBD & LEAN/OBESE--##
vs_il <- mi_lean[upper.tri(mi_lean)]
ti_lean<- mean(vs_il) + sd(vs_il)
mi_lean_thr<- mi_lean
mi_lean_thr[mi_lean_thr < ti_lean] <- 0

vs_io <- mi_obese[upper.tri(mi_obese)]
ti_obese <- mean(vs_io) + sd(vs_io)
mi_obese_thr <- mi_obese
mi_obese_thr[mi_obese_thr < ti_obese] <- 0

##--POBLACION: DANISH/SPANISH--##
vs_danish <- m_danish[upper.tri(m_danish)]
t_danish <- mean(vs_danish) + sd(vs_danish)
m_danish_thr <- m_danish
m_danish_thr[m_danish_thr < t_danish] <- 0

vs_spanish <- m_spanish[upper.tri(m_spanish)]
t_spanish <- mean(vs_spanish) + sd(vs_spanish)
m_spanish_thr <- m_spanish
m_spanish_thr[m_spanish_thr < t_spanish] <- 0

##--ET1/2/3--##
vs_ET1 <- m_ET1[upper.tri(m_ET1)]
t_ET1 <- mean(vs_ET1) + sd(vs_ET1)
m_ET1_thr <- m_ET1
m_ET1_thr[m_ET1_thr < t_ET1] <- 0

vs_ET2 <- m_ET2[upper.tri(m_ET2)]
t_ET2 <- mean(vs_ET2) + sd(vs_ET2)
m_ET2_thr <- m_ET2
m_ET2_thr[m_ET2_thr < t_ET2] <- 0

vs_ET3 <- m_ET3[upper.tri(m_ET3)]
t_ET3 <- mean(vs_ET3) + sd(vs_ET3)
m_ET3_thr <- m_ET3
m_ET3_thr[m_ET3_thr < t_ET3] <- 0

#Después volver a hacer los grafos de con igraph pero ahora con las matrices con threshold

g_todos_thr   <- graph_from_adjacency_matrix(m_todos_thr, mode = "undirected",
                                             weighted = TRUE)
gh_lean_thr   <- graph_from_adjacency_matrix(mh_lean_thr, mode = "undirected",
                                             weighted = TRUE)
gh_obese_thr  <- graph_from_adjacency_matrix(mh_obese_thr, mode = "undirected",
                                             weighted = TRUE)
gi_lean_thr   <- graph_from_adjacency_matrix(mi_lean_thr, mode = "undirected",
                                             weighted = TRUE)
gi_obese_thr  <- graph_from_adjacency_matrix(mi_obese_thr, mode = "undirected",
                                             weighted = TRUE)
g_danish_thr  <- graph_from_adjacency_matrix(m_danish_thr, mode = "undirected",
                                             weighted = TRUE)
g_spanish_thr <- graph_from_adjacency_matrix(m_spanish_thr, mode = "undirected",
                                             weighted = TRUE)
g_ET1_thr     <- graph_from_adjacency_matrix(m_ET1_thr, mode = "undirected",
                                             weighted = TRUE)
g_ET2_thr     <- graph_from_adjacency_matrix(m_ET2_thr, mode = "undirected",
                                             weighted = TRUE)
g_ET3_thr     <- graph_from_adjacency_matrix(m_ET3_thr, mode = "undirected",
                                             weighted = TRUE)

##¿COMO SE VE LA GRAFICA?, saque la visualizacion y otros atajos de esta pagina,
#la dejo aqui para que le den una leida, es muy buena y para que lo metamos en
#el documento de rmarkdown como cita

plot(gh_obese_thr,
     main = "HEALTHY & OBESE",
     vertex.size = 5,
     vertex.color = "purple",
     vertex.label.cex = 0.05,
     edge.color = "gray",
     edge.width = 0.6,
     layout = layout_with_fr)
#Csárdi G, Nepusz T (2006). “The igraph software package for complex network research.” InterJournal, Complex Systems, 1695. https://igraph.org.
#Ahi explican layout, tamaño de lineas, tamaño de texto, nodos, etc. 

#---------------------------------------------------------------------
#despues de eso continuar quitando los nodos aislados
#tambien lo saque de un tutorial, dejo la pagina a continuacion por si la quieren
#revisar, super funcional y para citar la pagina tambien.

#Eliminar nodos aislados (bacterias sin conexiones)
#Lo mas facil es hacer un ciclo que elimine los nodos y que vaya recorriendo cada objeto

# ELIMINAR NODOS AISLADOS

# lista de redes thresholded
redes_thr <- list(
  g_todos_thr,
  gh_lean_thr,
  gh_obese_thr,
  gi_lean_thr,
  gi_obese_thr,
  g_danish_thr,
  g_spanish_thr,
  g_ET1_thr,
  g_ET2_thr,
  g_ET3_thr
)

# nombres de las redes
nombres_redes <- c(
  "todos",
  "h_lean",
  "h_obese",
  "i_lean",
  "i_obese",
  "danish",
  "spanish",
  "ET1",
  "ET2",
  "ET3"
)

# lista vacía para guardar redes filtradas
redes_filtradas <- list()

# ciclo para eliminar nodos aislados
for(i in 1:10){
  g <- redes_thr[[i]]
  # eliminar nodos con degree = 0
  g_filtrado <- delete_vertices(
    g,
    V(g)[degree(g) == 0]
  )
   
  # guardar red filtrada
  redes_filtradas[[nombres_redes[i]]] <- g_filtrado
  
  # imprimir resumen
  cat("\n")
  cat("Red:", nombres_redes[i], "\n")
  cat("Nodos:", vcount(g_filtrado), "\n")
  cat("Edges:", ecount(g_filtrado), "\n")
}

#Al aplicar el filtrado, todas la redes conservaron los 154 nodos originales,
#o sea que despues del threshold ningina especie quedó aislada. 


##Nota: Continuar con metricas globales

library(igraph)

# CALCULAR METRICAS GLOBALES

# crear dataframe vacío para guardar resultados
metricas_globales <- data.frame()

# Ciclo para calcular metricas en todas las redes filtradas
for(i in 1:10){
  # seleccionar red
  g <- redes_filtradas[[nombres_redes[i]]]
   # detectar comunidades usando Louvain
  comunidades <- cluster_louvain(g)
  # guardar métricas
  temp <- data.frame(
    Red = nombres_redes[i],
    Nodos = vcount(g),
    Edges = ecount(g),
    Degree_promedio = mean(degree(g)),
    Densidad = edge_density(g),
    Clustering = transitivity(g, type = "global"),
    Path_length = mean_distance(g),
    Modularidad = modularity(comunidades)
  )
  
#agregar resultados al dataframe
  metricas_globales <- rbind(metricas_globales, temp)
}

# mostrar tabla final de todas las metricas paraa cada red filtrada 
metricas_globales



###Analisis biológico 
## Hay que determinar diferentes medidas de centralidad 

#Funcion para determinar medidad de centralidad 

calcular_centralidades <- function(g){
  data.frame(
   Bacteria= V(g)$name,
   Degree= degree(g),
   Strength= strength(g),
   Betweenness= betweenness(g),
   Eigen_vector = eigen_centrality(g)$vector
   
  )
}

#Probar con una red (verificar que funciona)

calcular_centralidades(redes_filtradas[["todos"]])

##Hacer un cilo for para calcular centralidad en todas las redes 

centralidades_redes <- list()

for(i in 1:10){
  nombre<- nombres_redes[i]
  g<- redes_filtradas[[nombre]]
  centralidades_redes[[nombre]]<- calcular_centralidades(g)
}

##Ver si si se guardaron 
names(centralidades_redes)
centralidades_redes

#Ver la tabla ordenada por degree

centralidades_todos_degree<- centralidades_redes[["todos"]]
centralidades_todos_degree<- centralidades_todos_degree[
  order(-centralidades_todos_degree$Degree),
  ]
View(centralidades_todos_degree)

##Podemos hacer lo mismo para las diferentes centralidades 



###se hizo un objeto por cada red filtrada en la cual abarque los las distintas metricas de diversidad
cen_todos<- calcular_centralidades(redes_filtradas[["todos"]])
cen_hl<-calcular_centralidades(redes_filtradas[["h_lean"]])
cen_ho<-calcular_centralidades(redes_filtradas[["h_obese"]])
cen_il<-calcular_centralidades(redes_filtradas[["i_lean"]])
cen_io<-calcular_centralidades(redes_filtradas[["i_obese"]])
cen_danish<-calcular_centralidades(redes_filtradas[["danish"]])
cen_spanish<-calcular_centralidades(redes_filtradas[["spanish"]])
cen_ET1<-calcular_centralidades(redes_filtradas[["ET1"]])
cen_ET2<-calcular_centralidades(redes_filtradas[["ET2"]])
cen_ET3<-calcular_centralidades(redes_filtradas[["ET3"]])


#https://colab.research.google.com/github/prbocca/na101_master/blob/master/homework_00_a_graphs/public_homework_00_a_graphs.ipynb


###obtener los 10 nodos hub dependiendo distintas metricas

###nodos hub por grado, fuerza, intermediación, valor propio
top_degree_todos<- cen_todos[order(-cen_todos$Degree), ]
hub_d_todos<-head(top_degree_todos, 10)
top_strength_todos<- cen_todos[order(-cen_todos$Strength), ]
hub_s_todos<-head(top_strength_todos, 10)
top_bet_todos<- cen_todos[order(-cen_todos$Betweennes), ]
hub_b_todos<-head(top_bet_todos, 10)
top_eigv_todos<- cen_todos[order(-cen_todos$Eigen_vector), ]
hub_e_todos<-head(top_eigv_todos, 10)

top_degree_hl<- cen_hl[order(-cen_hl$Degree), ]
hub_d_h_lean<-head(top_degree_hl, 10)
top_strength_hl<- cen_hl[order(-cen_hl$Strength), ]
hub_s_hl<-head(top_strength_hl, 10)
top_bet_hl <- cen_hl[order(-cen_hl$Betweennes), ]
hub_b_hl<-head(top_bet_hl, 10)
top_eigv_hl<- cen_hl[order(-cen_hl$Eigen_vector), ]
hub_e_hl<-head(top_eigv_hl, 10)

top_degree_ho<- cen_ho[order(-cen_ho$Degree), ]
hub_d_h_obese<-head(top_degree_ho, 10)
top_strength_ho<- cen_ho[order(-cen_ho$Strength), ]
hub_s_ho<-head(top_strength_ho, 10)
top_bet_ho <- cen_ho[order(-cen_ho$Betweennes), ]
hub_b_ho<-head(top_bet_ho, 10)
top_eigv_ho<- cen_ho[order(-cen_ho$Eigen_vector), ]
hub_e_ho<-head(top_eigv_ho, 10)

top_degree_il<- cen_il[order(-cen_il$Degree), ]
hub_d_i_lean<-head(top_degree_il, 10)
top_strength_il<- cen_il[order(-cen_il$Strength), ]
hub_s_il<-head(top_strength_il, 10)
top_bet_il <- cen_il[order(-cen_il$Betweennes), ]
hub_b_il<-head(top_bet_il, 10)
top_eigv_il<- cen_il[order(-cen_il$Eigen_vector), ]
hub_e_il<-head(top_eigv_il, 10)

top_degree_io<- cen_io[order(-cen_io$Degree), ]
hub_d_i_obese<-head(top_degree_io, 10)
top_strength_io<- cen_io[order(-cen_io$Strength), ]
hub_s_io<-head(top_strength_io, 10)
top_bet_io <- cen_io[order(-cen_io$Betweennes), ]
hub_b_io<-head(top_bet_io, 10)
top_eigv_io<- cen_io[order(-cen_io$Eigen_vector), ]
hub_e_io<-head(top_eigv_io, 10)

top_degree_danish<- cen_danish[order(-cen_danish$Degree), ]
hub_d_danish<-head(top_degree_danish, 10)
top_strength_danish<- cen_danish[order(-cen_danish$Strength), ]
hub_s_danish<-head(top_strength_danish, 10)
top_bet_danish <- cen_danish[order(-cen_danish$Betweennes), ]
hub_b_danish<-head(top_bet_danish, 10)
top_eigv_danish<- cen_danish[order(-cen_danish$Eigen_vector), ]
hub_e_danish<-head(top_eigv_danish, 10)

top_degree_spanish<- cen_spanish[order(-cen_spanish$Degree), ]
hub_d_spanish<-head(top_degree_spanish, 10)
top_strength_spanish<- cen_spanish[order(-cen_spanish$Strength), ]
hub_s_spanish<-head(top_strength_spanish, 10)
top_bet_spanish <- cen_spanish[order(-cen_spanish$Betweennes), ]
hub_b_spanish<-head(top_bet_spanish, 10)
top_eigv_spanish<- cen_spanish[order(-cen_spanish$Eigen_vector), ]
hub_e_spanish<-head(top_eigv_spanish, 10)

top_degree_ET1<- cen_ET1[order(-cen_ET1$Degree), ]
hub_d_ET1<-head(top_degree_ET1, 10)
top_strength_ET1<- cen_ET1[order(-cen_ET1$Strength), ]
hub_s_ET1<-head(top_strength_ET1, 10)
top_bet_ET1 <- cen_ET1[order(-cen_ET1$Betweennes), ]
hub_b_ET1<-head(top_bet_ET1, 10)
top_eigv_ET1<- cen_ET1[order(-cen_ET1$Eigen_vector), ]
hub_e_ET1<-head(top_eigv_ET1, 10)

top_degree_ET2<- cen_ET2[order(-cen_ET2$Degree), ]
hub_d_ET2<-head(top_degree_ET2, 10)
top_strength_ET2<- cen_ET2[order(-cen_ET2$Strength), ]
hub_s_ET2<-head(top_strength_ET2, 10)
top_bet_ET2 <- cen_ET2[order(-cen_ET2$Betweennes), ]
hub_b_ET2<-head(top_bet_ET2, 10)
top_eigv_ET2<- cen_ET2[order(-cen_ET2$Eigen_vector), ]
hub_e_ET2<-head(top_eigv_ET2, 10)

top_degree_ET3<- cen_ET3[order(-cen_ET3$Degree), ]
hub_d_ET3<-head(top_degree_ET3, 10)
top_strength_ET3<- cen_ET3[order(-cen_ET3$Strength), ]
hub_s_ET3<-head(top_strength_ET3, 10)
top_bet_ET3 <- cen_ET3[order(-cen_ET3$Betweennes), ]
hub_b_ET3<-head(top_bet_ET3, 10)
top_eigv_ET3<- cen_ET3[order(-cen_ET3$Eigen_vector), ]
hub_e_ET3<-head(top_eigv_ET3, 10)

#despues de sacar los genes hub tomando en cuenta las distintas medidas de centralidad hay que comparar entre redes las diferencias que se ven
#intentando encontrar hubs consevados, hubs perdidos y hubs nuevos (que dependiendo cuales sean las condiciones puede asociarse a inflamación)
#Comparar haciendo matrices o heatmaps

#Para poder trabajar con los hubs, trabajare solo los hubs por grado en
#las bacterias ya que vi que arriba ordenaron segun el valor del degree, asi que por
#eso quiero saber los nombres de esas bacterias que sirven para las graficas
#de representacion de datos, basicamente responder:

##¿las bacterias presentes en individuos sanos y delgados tambien estan presentes
##en pacientes enfermos y delgados?

#GRUPO 1_ HEALTH & LEAN, IDB & LEAN. 
grupo_lean_sanos<- hub_d_h_lean$Bacteria
grupo_lean_enfermos<- hub_d_i_lean$Bacteria

hub_d_h_lean

#GRUPO 1_conservados: conservados en sanos y enfermos
print(grupo_lean_sanos[grupo_lean_sanos %in% grupo_lean_enfermos])

#GRUPO 1_perdidos: No encontrados en sanos
print(grupo_lean_sanos[!grupo_lean_sanos %in% grupo_lean_enfermos])

#GRUPO 1_nuevos: Emergentes 
print(grupo_lean_enfermos[!grupo_lean_enfermos %in% grupo_lean_sanos])

-----------------------------------------
#GRUPO 2_ HEALTH & OBESE, IDB & OBESE. 
grupo_obese_sanos<- hub_d_h_obese$Bacteria
grupo_obese_enfermos<- hub_d_i_obese$Bacteria

#GRUPO 2_CONSERVADOS
print(grupo_obese_sanos[grupo_obese_sanos %in% grupo_obese_enfermos])
#[1] "Bacteroides_sp_9_1_42FAA"           "Dorea_formicigenerans_ATCC_27755"  
#[3] "Clostridiales_sp_SS3_4"             "Faecalibacterium_prausnitzii_SL3_3"

#GRUPO 2_PERDIDOS
print(grupo_obese_sanos[!grupo_obese_sanos %in% grupo_obese_enfermos])
#"Collinsella_aerofaciens_ATCC_25986" "Eubacterium_rectale_M104_1"        
#[3] "Bacteroides_sp_4_3_47FAA"           "Bacteroides_sp_D1"                 
#[5] "Bacteroides_vulgatus_ATCC_8482"     "Bacteroides_xylanisolvens_XB1A"  

#GRUPO 2_EMERGENTES
print(grupo_obese_enfermos[!grupo_obese_enfermos %in% grupo_obese_sanos])
#[1] "Clostridium_sp_SS2_1"                  "Bacteroides_thetaiotaomicron_VPI_5482"
#[3] "Ruminococcus_torques_L2_14"            "Bacteroides_sp_2_1_7"                 
#[5] "Roseburia_intestinalis_M50_1"          "Coprococcus_comes_ATCC_27758"

------------------------------------------
  
#GRUPO 3_POBLACIONES
grupo_danish<- hub_d_danish$Bacteria
grupo_spanish<- hub_d_spanish$Bacteria

#GRUPO 3_conservados: conservados en ambas poblaciones
print(grupo_danish[grupo_danish %in% grupo_spanish])
#[1] "Roseburia_intestinalis_M50_1"         "Faecalibacterium_prausnitzii_SL3_3"  
#[3] "Bacteroides_sp_9_1_42FAA"             "Bacteroides_vulgatus_ATCC_8482"      
#[5] "Bacteroides_sp_2_1_7"                 "Bacteroides_sp_4_3_47FAA"            
#[7] "Parabacteroides_distasonis_ATCC_8503"

#GRUPO 3_nuevos: Emergentes 
print(grupo_danish[!grupo_danish %in% grupo_spanish])
#[1] "Eubacterium_rectale_M104_1" "Ruminococcus_sp_SR1_5"   "Ruminococcus_torques_L2_14"

-------------------------------------------
#GRUPO 4_ ETNI
grupo_ET1<- hub_d_ET1$Bacteria
grupo_ET2<- hub_d_ET2$Bacteria
grupo_ET3<- hub_d_ET3$Bacteria

##HAGAN ESTE ULTIMO CON 3, YO CREO QUE ESTARIA BIEN CUALES ESTAN PRESENTES EN LOS 3
#Y CYUALES SON EXCLUSIVOS DE LA ETNIA

#HEATMAPS
bacterias<- unique(c(grupo_lean_sanos, grupo_lean_enfermos, grupo_obese_sanos, grupo_obese_enfermos))
colhl<- as.numeric(bacterias %in% grupo_lean_sanos)
colil<- as.numeric(bacterias %in% grupo_lean_enfermos)
colho<- as.numeric(bacterias %in% grupo_obese_sanos)
colio<- as.numeric(bacterias %in% grupo_obese_enfermos)

matriz_sanos_enfermos<- cbind(colhl,
                              colil,
                              colho,
                              colio)

rownames(matriz_sanos_enfermos)<- bacterias

colnames(matriz_sanos_enfermos)<- c("delgado-sano", "delgado-enfermo", 
                                    "obeso-sano", "obeso-enfermo")

##VISUALIZACION DE LA MATRIZ, como ven, hay valores 0-1 que puede leer el heatmap
matriz_sanos_enfermos

#Mejorar heatmap

install.packages("pheatmap")
library(pheatmap)

pheatmap(
  matriz_sanos_enfermos,
  color = c("green", "orange"),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize_row = 8,
  fontsize_col = 10,
  main = "Bacterias presentes en personas sanas y enfermos "
)



###Robustez 

#Componente gigante
library(igraph)

# Función auxiliar: tamaño relativo del componente gigante (la que nos dio el profe)
tamano_gigante <- function(g) {
  comp <- components(g)
  max(comp$csize)
}
cat("Componente gigante (original):", tamano_gigante(g), "\n")

#(puse tal cual la funcion del profe)
simular_fallos <- function(g, fraccion_eliminar = 0.5, semilla = 42) {
  set.seed(semilla)
  n_original <- vcount(g)
  n_eliminar <- floor(n_original * fraccion_eliminar)
  
  # Seleccionar nodos a eliminar en orden aleatorio
  orden_eliminacion <- sample(V(g)$name, n_eliminar)
  
  # Registrar métricas en cada paso
  resultados <- data.frame(
    eliminados = 0:n_eliminar,
    fraccion_eliminada = (0:n_eliminar) / n_original,
    comp_gigante = numeric(n_eliminar + 1),
    coeficiente_S = numeric(n_eliminar + 1),
    dist_media = numeric(n_eliminar + 1),
    diametro = numeric(n_eliminar + 1)
  )
  
  g_temp <- g
  
  for (i in 0:n_eliminar) {
    if (i > 0) {
      g_temp <- delete_vertices(g_temp, orden_eliminacion[i])
    }
    
    resultados$comp_gigante[i + 1] <- tamano_gigante(g_temp)
    resultados$coeficiente_S[i + 1] <- resultados$comp_gigante[i + 1] / vcount(g_temp)
    
    # Calcular distancia media solo en el componente gigante
    comp <- components(g_temp)
    giant_id <- which.max(comp$csize)
    giant_nodes <- which(comp$membership == giant_id)
    if (length(giant_nodes) > 1) {
      g_giant <- induced_subgraph(g_temp, giant_nodes)
      resultados$dist_media[i + 1] <- mean_distance(g_giant)
      resultados$diametro[i + 1] <- diameter(g_giant)
    } else {
      resultados$dist_media[i + 1] <- 0
      resultados$diametro[i + 1] <- 0
    }
  }
  
  resultados
}

#Usar la funcion para simular fallos aleatorios (en una sola red)

fallo_hlean <- simular_fallos(
  redes_filtradas[["h_lean"]],
  fraccion_eliminar = 2
  )
View(fallo_hlean)

#Josue, puedes hacer un ciclo for para aplicarlos a todas las redes 

ataque_aleatorios<- list() ##para ello genero una lista vacia en la cual se vayan a guardar los resultados de la simulación de fallos

for(i in 1:length(redes_filtradas)){#hacer que la iteración vaya por toda la longitud del objeto que contiene todas las redes filtradas
  nombre<- nombres_redes[i] #generar un objeto con los nombres de cada una de las redes (por cada ciclo va siendo un nombre diferente)
  g<- redes_filtradas[[nombre]]#otro objeto en el cual contenga la red especifica que indica el objeto antes creado (el de nombres)
  resultado<-simular_fallos(g, fraccion_eliminar = 0.5) #hacer fallos aleatorios para la red indicada
  ataque_aleatorios[[nombre]]<- resultado #los resultados se guardan en la lista
}#generar un ciclo for en el cual se use la función "simular_fallos" para cada red
View(ataque_aleatorios[["i_lean"]]) #observar como se ve una red con los resultados

#posteriormente para la intepretación se debe visualizar en una gráfica haciendo uso de ggplot y dplyr

library(dplyr)

df_robustez <- data.frame() #para eso debo generar un data frame vacío en donde pondre los valores de las columnas "fraccion_eliminadas" y "coeficiente_S", de cada red
for(i in 1:length(ataque_aleatorios)){
  df_tem<- ataque_aleatorios[[i]] #extraer de información de la red correspondiente de la lista de robustez
  df_tem<- df_tem[, c("fraccion_eliminada","coeficiente_S")]#quedarse solo con columnas importantes para hacer el plot
  df_tem$red <- names(ataque_aleatorios)[i] #agregar nombre de red usando los nombres correspondientes que viene en la lista que hicimos de robustez
  df_robustez <- rbind(df_robustez,df_tem)
}
View(df_robustez)


#ya podemos hacer la gráfica
library(ggplot2)
df_plot <- df_robustez[df_robustez$red %in% c("h_lean","h_obese","i_lean","i_obese"),]
ggplot(df_plot,aes(x = fraccion_eliminada, y = coeficiente_S, color = red)) +
  geom_line(linewidth = 1) +
  theme_minimal()

#ahora hacer ataques dirigidos porque esto puede desorganizar o descomponer más la redes biológicas
#modificar un poco la la función simulación de fallos, para que en vez de hacer ataque aleatorios se hagan ataque a los genes hub de cada red
simular_dirigidos <- function(g, hubs){
  n_original <- vcount(g)#numero original de nodos
  n_eliminar <- length(hubs)#cantidad de hubs a eliminar (usando el objeto de hubs por degree antes generado)
  #crear dataframe de resultados
  resultados <- data.frame(
    eliminados = 0:n_eliminar,
    fraccion_eliminada = (0:n_eliminar) / n_original,
    comp_gigante = numeric(n_eliminar + 1),
    coeficiente_S = numeric(n_eliminar + 1),
    dist_media = numeric(n_eliminar + 1),
    diametro = numeric(n_eliminar + 1)
  )
  
  g_temp <- g #copiar red temporal
  
  #ciclo for para eliminar uno por uno los hub
  for(i in 0:n_eliminar){
    if(i > 0){#eliminar hub correspondiente
      g_temp <- delete_vertices(g_temp,hubs[i])#que elimine cada hub correspondiente segun el ciclo en el cual este por ejemplo: si esta en el ciclo 1 quitaria el hub uno, si esta en el ciclo 2 quitaria el hub 1 y 2, y sigue el ciclo
    }
    #componente gigante
    resultados$comp_gigante[i + 1] <-tamano_gigante(g_temp)
    #coeficiente S
    resultados$coeficiente_S[i + 1] <-resultados$comp_gigante[i + 1]/ vcount(g_temp)
    #analizar componente gigante
    comp <- components(g_temp)
    giant_id <- which.max(comp$csize)
    giant_nodes <- which(comp$membership == giant_id)
    
    #calcular métricas
    if(length(giant_nodes) > 1){
      g_giant <- induced_subgraph(g_temp,giant_nodes)
      resultados$dist_media[i + 1] <-mean_distance(g_giant)
      resultados$diametro[i + 1] <-diameter(g_giant)
    } else {resultados$dist_media[i + 1] <- 0
    resultados$diametro[i + 1] <- 0
    }
  }
  resultados
}
#hacer un objeto con ataque dirigido a hubs para cada red
dirigido_todos<-simular_dirigidos(redes_filtradas[["todos"]],hub_d_todos$Bacteria)
dirigido_h_lean<-simular_dirigidos(redes_filtradas[["h_lean"]],hub_d_h_lean$Bacteria)
dirigido_h_obese<-simular_dirigidos(redes_filtradas[["h_obese"]],hub_d_h_obese$Bacteria)
dirigido_i_lean<-simular_dirigidos(redes_filtradas[["i_lean"]],hub_d_i_lean$Bacteria)
dirigido_i_obese<-simular_dirigidos(redes_filtradas[["i_obese"]],hub_d_i_obese$Bacteria)
dirigido_danish<-simular_dirigidos(redes_filtradas[["danish"]],hub_d_danish$Bacteria)
dirigido_spanish<-simular_dirigidos(redes_filtradas[["spanish"]],hub_d_spanish$Bacteria)
dirigido_ET1<-simular_dirigidos(redes_filtradas[["ET1"]],hub_d_ET1$Bacteria)
dirigido_ET2<-simular_dirigidos(redes_filtradas[["ET2"]],hub_d_ET2$Bacteria)
dirigido_ET3<-simular_dirigidos(redes_filtradas[["ET3"]],hub_d_ET3$Bacteria)

View(dirigido_i_lean)
##hacer un data.frame con los objetos antes creados para después gráficar
df_dirigidos <- rbind(
  cbind(dirigido_h_lean,red = "h_lean"),
  cbind(dirigido_h_obese,red = "h_obese"),
  cbind(dirigido_i_lean,red = "i_lean"),
  cbind(dirigido_i_obese,red = "i_obese")
)

#gráficar

ggplot(df_dirigidos,aes(x = fraccion_eliminada, y = coeficiente_S, color = red)) +
  geom_line(linewidth = 1) +
  theme_minimal()


