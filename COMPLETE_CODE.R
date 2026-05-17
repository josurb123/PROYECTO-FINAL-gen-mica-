
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

gh_obese<- graph_from_adjacency_matrix(
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

#después volver a hacer los grafos de con igraph pero ahora con las matrices con threshold

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

#CALCULAR METRICAS GLOBALES

#---------------------------------------------------------------------
# CALCULAR METRICAS GLOBALES

# crear dataframe vacío para guardar resultados
metricas_globales <- data.frame()

# Calcular metricas para TODAS las redes filtradas
for(i in 1:10){#---------------------------------------------------------------------
  # CALCULAR METRICAS GLOBALES
  
# crear dataframe vacío para guardar resultados
metricas_globales <- data.frame()
 
 # recorrer todas las redes filtradas
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
    
    # agregar resultados al dataframe
    metricas_globales <- rbind(metricas_globales, temp)
  }
  
  # mostrar tabla final
  metricas_globales
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
  
  # agregar resultados al dataframe
  metricas_globales <- rbind(metricas_globales, temp)
}

# mostrar tabla final
metricas_globales






#https://colab.research.google.com/github/prbocca/na101_master/blob/master/homework_00_a_graphs/public_homework_00_a_graphs.ipynb


#---------------------------------------------------------------------
#para despuescalcular las metricas globales (# nodos, # edges, degree, distancias, coeficiente de clustering, modularidad)
#después calcular centralidades











