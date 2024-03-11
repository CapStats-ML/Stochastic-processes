################################################################################
###################### PARCIAL 1B- Computacional ###############################

# Directorio 
setwd("~/DOCUMENTOS PERSONAJES/CESAR/Procesos/Parcial1")

# Paquetes a utilizar en el desarrollo del documento

install.packages("markovchain")
install.packages("DiagrammeR")
install.packages("diagram")

library(markovchain)
library(diagram)
library(DiagrammeR)

### Extra

# El siguiente trozo de codigo fue tomado del reposirotio de GitHub donde esta subido el paquete
# markovchain, y modificado en alguno aspectos para que este pueda funcionar de manera correcta a la
# hora de graficar las cadenas descritas en los siguientes ejercicios. Para más ingresar en el 
# link: https://github.com/spedygiorgio/markovchain/blob/025b19efdcda54e329dba5013f8f2ff31059d517/R/supplementaryPlot.R#L76

# plot a diagram using DiagrammeR for a markovchain object
.plotDiagrammeR <- function(object, node_fill_color = "lightblue", ...) {
  if(is(object,"markovchain")){
    mat <- object@transitionMatrix
  } else if(is(object,"ctmc")){
    mat <- object@generator
  }
  names <- rownames(mat)
  
  # names of nodes
  nodes <- ''
  for(i in 1:nrow(mat)) {
    nodes <- paste0(nodes, names[i], ' [fillcolor = "', node_fill_color, '"]; ')
  }
  
  # store edges
  edges <- ''
  for(i in 1:nrow(mat)) {
    for(j in 1:ncol(mat)) {
      if (mat[i,j] != 0) {
        edges <- paste0(edges, names[i], "->", names[j], " [label = ", mat[i,j], "] ")
      }
    }
  }
  
  # extract extra parameter
  dots <- list(...)
  args <- ""
  for(name in names(dots)) {
    args <- paste0(args, name, "=\"", dots[[name]], "\" ")
  }
  
  # print(args)
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    res <- DiagrammeR::grViz(paste0("
  digraph circles {
            graph [overlap = true, fontsize = 10]

            node [shape = circle,
            fixedsize = true,
            width = 0.9] // sets as circles
            ", nodes, "
        
            ", edges, args,"

            // labelfontsize = 20 labelloc='t' label ='Weather transition matrix'
          }
  "))
    
    return (res)
  } else {
    print("Diagrammer unavailable")
  }
}

### PUNTO 1 ---------------------------- 

# Definir la matriz
P<- matrix(c(0, 0, 1, 0, 0, 0,
             0, 0, 0, 0, 0, 1,
             0, 0, 0, 0, 1, 0,
             1/3, 1/3, 1/3, 0, 0, 0,
             1, 0, 0, 0, 0, 0,
             0, 1/2, 0, 0, 0, 1/2), nrow = 6, byrow = TRUE);P

# Creamos la matriz de transicion
mc = new("markovchain",transitionMatrix=P,states=c("0","1","2","3","4","5"),
         name="Cadena 1") 

str(mc);show(mc) #Se explora la clase del objeto que se creo con la funcion anterior
summary(mc) # Se explora el contenido del objeto mc 

#Visualizacion de la cadena
set.seed(2)
.plotDiagrammeR(mc, node_fill_color = "lightblue", label = "Matrix de transición", labelloc = "t")


###### Punto 1a:  Clasificar los estados a las clases 

# Etiquetas de cadena para los estados
etiquetas <- c("Estado1", "Estado2", "Estado3", "Estado4", "Estado5", "Estado6")

# Crear un vector con las etiquetas de cadena para cada estado
cadenas_estados <- rep(etiquetas, each = nrow(P))

# Mostrar los estados y sus etiquetas de cadena
for (i in 1:length(cadenas_estados)) {
  cat("Estado", i, ":", cadenas_estados[i], "\n")
}






