#AQUI SE VA A HACER TODO EL CÓDIGO EL CUAL SE VA A PONER EN EL R MARKDOWN
#Importar matrices a objetos de database de R
matriz_todos<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/B-Co-Occurrence All.csv")
View(matriz_todos)
row.names(matriz_todos)<-matriz_todos[,1]
matriz_todos<-matriz_todos[,-1]
colnames(matriz_todos)<-matriz_todos[1,]
matriz_todos<-matriz_todos[-1,]

matriz_C<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/C- Co-Occurrence Healthy & Lean.csv")
View(matriz_C)
row.names(matriz_C)<-matriz_C[,1]
matriz_C<-matriz_C[,-1]
colnames(matriz_C)<-matriz_C[1,]
matriz_C<-matriz_C[-1,]

matriz_D<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/D-Co-Occurrence Healthy & Obese.csv")
View(matriz_D)
row.names(matriz_D)<-matriz_D[,1]
matriz_D<-matriz_D[,-1]
colnames(matriz_D)<-matriz_D[1,]
matriz_D<-matriz_D[-1,]

matriz_E<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/E-Co-Occurrence IBD & Lean.csv")
View(matriz_E)
row.names(matriz_E)<-matriz_E[,1]
matriz_E<-matriz_E[,-1]
colnames(matriz_E)<-matriz_E[1,]
matriz_E<-matriz_E[-1,]

matriz_F<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/F-Co-Occurrence IBD & Obese.csv")
View(matriz_F)
row.names(matriz_F)<-matriz_F[,1]
matriz_F<-matriz_F[,-1]
colnames(matriz_F)<-matriz_F[1,]
matriz_F<-matriz_F[-1,]

matriz_G<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/G-Co-Occurrence Danish.csv")
View(matriz_G)
row.names(matriz_G)<-matriz_G[,1]
matriz_G<-matriz_G[,-1]
colnames(matriz_G)<-matriz_G[1,]
matriz_G<-matriz_G[-1,]

matriz_H<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/H-Co-Occurrence Spanish.csv")
View(matriz_H)
row.names(matriz_H)<-matriz_H[,1]
matriz_H<-matriz_H[,-1]
colnames(matriz_H)<-matriz_H[1,]
matriz_H<-matriz_H[-1,]

matriz_I<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/I-Co-Occurrence ET1.csv")
View(matriz_I)
row.names(matriz_I)<-matriz_I[,1]
matriz_I<-matriz_I[,-1]
colnames(matriz_I)<-matriz_I[1,]
matriz_I<-matriz_I[-1,]

matriz_K<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/K-Co-Occurrence ET2.csv")
View(matriz_K)
row.names(matriz_K)<-matriz_K[,1]
matriz_K<-matriz_K[,-1]
colnames(matriz_K)<-matriz_K[1,]
matriz_K<-matriz_K[-1,]

matriz_L<-read.csv("C:/Users/us/Documents/GENOMICA/PROYECTO FINAL (genómica)/RAW DATA/L-Co-Occurrence ET3.csv")
View(matriz_L)
row.names(matriz_L)<-matriz_L[,1]
matriz_L<-matriz_L[,-1]
colnames(matriz_L)<-matriz_L[1,]
matriz_L<-matriz_L[-1,]

#hacer que R lea el contenido de la tabla como valores numericos
matriz_todos[] <- lapply(matriz_todos, as.numeric)
matriz_C[] <- lapply(matriz_C, as.numeric)
matriz_D[] <- lapply(matriz_D, as.numeric)
matriz_E[] <- lapply(matriz_E, as.numeric)
matriz_F[] <- lapply(matriz_F, as.numeric)
matriz_G[] <- lapply(matriz_G, as.numeric)
matriz_H[] <- lapply(matriz_H, as.numeric)
matriz_I[] <- lapply(matriz_I, as.numeric)
matriz_K[] <- lapply(matriz_K, as.numeric)
matriz_L[] <- lapply(matriz_L, as.numeric)

#volver todos los dataframes en matrices
mtodos<-as.matrix(matriz_todos)
mC<-as.matrix(matriz_C)
mD<-as.matrix(matriz_D)
mE<-as.matrix(matriz_E)
mF<-as.matrix(matriz_F)
mG<-as.matrix(matriz_G)
mH<-as.matrix(matriz_H)
mI<-as.matrix(matriz_I)
mK<-as.matrix(matriz_K)
mL<-as.matrix(matriz_L)

#Verificar que sean valores numéricos en la matriz
class(mtodos)
mode(mtodos)

#observar cuantas NA's hay y cambiarlas por 0, ya que son matrices de similitud
sum(is.na(mtodos))
sum(is.na(mC))
sum(is.na(mD))
sum(is.na(mE))
sum(is.na(mF))
sum(is.na(mG))
sum(is.na(mH))
sum(is.na(mI))
sum(is.na(mK))
sum(is.na(mL))

mtodos[is.na(mtodos)] <- 0
mtodos[is.na(mC)] <- 0
mtodos[is.na(mD)] <- 0
mtodos[is.na(mE)] <- 0
mtodos[is.na(mF)] <- 0
mtodos[is.na(mG)] <- 0
mtodos[is.na(mH)] <- 0
mtodos[is.na(mI)] <- 0
mtodos[is.na(mK)] <- 0
mtodos[is.na(mL)] <- 0

#la matriz de todos tiene 2 filas extra vacias, esas las vamos a borrar
dim(mtodos)
rownames(mtodos)
colnames(mtodos)

mtodos <- mtodos[-c(155,156), ]

#empezar a hacer las redes usando el paquete "igraph"
library(igraph)

g_todos <- graph_from_adjacency_matrix(
  mtodos,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_C <- graph_from_adjacency_matrix(
  mC,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_D<- graph_from_adjacency_matrix(
  mD,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_E<- graph_from_adjacency_matrix(
  mE,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_F<- graph_from_adjacency_matrix(
  mF,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_G<- graph_from_adjacency_matrix(
  mG,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_H<- graph_from_adjacency_matrix(
  mH,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_I<- graph_from_adjacency_matrix(
  mI,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_K<- graph_from_adjacency_matrix(
  mK,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g_L<- graph_from_adjacency_matrix(
  mL,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

#hacer threshold para quedarnos con las conexiones fuertes y eliminar las debiles

valstodos <- mtodos[upper.tri(mtodos)]#para usar solo los valores de la mitad superior de la tabla, ya que como la tabla es simetrica contiene la misma info cada una de las mitades, osea para no duplicar información
threshold <- mean(valstodos) + sd(valstodos)
threshold
mtodos_thr <- mtodos #hacer nuevo objeto con el threshold
mtodos_thr[mtodos_thr < threshold] <- 0 #hacer que todos los valores que esten debajo del threshold sea igual a 0

#hacer con el resto de matrices
#después volver a hacer los grafos de con igraph pero ahora con las matrices con threshold
#despues de eso continuar quitando los nodos aislados
#para despuescalcular las metricas globales (# nodos, # edges, degree, distancias, coeficiente de clustering, modularidad)
#después calcular centralidades