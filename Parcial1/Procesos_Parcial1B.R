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


###### Punto 1a:  

# 1. Clasificar los estados en clases comunicantes
communicating_classes <- communicatingClasses(mc)
print("Clases de estados comunicantes:")
print(communicating_classes)

###### Punto 1b:  

# 2. Determinar la recurrencia de cada estado
recurrent_states <- recurrentStates(mc)
print("Estados recurrentes:")
print(recurrent_states)

###### Punto 1c:  

# 3. Calcular el período de cada clase recurrente
C1 <- new("markovchain", transitionMatrix = P[c(1,3,5), c(1,3,5)])
C2 <- new("markovchain", transitionMatrix = P[c(2,6), c(2,6)])
cbind(period(C1), period(C2))

###### Punto 1d:  


###### PUNTO 2 ------------------------

# Punto 2
 P <- matrix(c(1,0,0,0,0,
              0.2,0.1,0.7,0,0,
              0.12,0,0.08,0.8,0,
              0.04,0,0,0.06,0.9,
              0,0,0,0,1), nrow = 5, byrow = TRUE)

 mc <- new("markovchain", transitionMatrix = P, 
           states = c("B","1","2","3","G")) 
#Donde B y G significan Baja y Grado

 
CanoniMa <- matrix(c(1.00,0.00,0.00,0.00,0.00,
                    0.00,1.00,0.00,0.00,0.00,
                    0.20,0.00,0.10,0.70,0.00,
                    0.12,0.00,0.00,0.08,0.80,
                    0.04,0.90,0.00,0.00,0.06),
                    nrow = 5, byrow = TRUE,
                    dimnames = list(c("B","G","1","2","3"),
                                    c("B","G","1","2","3")))
CanoniMa

M <- solve(diag(1,3,3) - CanoniMa[c(3,4,5), c(3,4,5)]); M

#Probabilidades de Absorcion

M %*% CanoniMa[c(3,4,5), c(1,2)]
Pr <- absorptionProbabilities(mc);Pr

n <- ceiling(500/Pr[1,2]);n
Años <- ceiling(n/50/2);Años

#### PUNTO 3 ---------

P <- matrix(c(0,1/3,1/3,1/3,0,
              0.5,0,0.5,0,0,
              0,0.5,0,0.5,0,
              0,0,0.5,0,0.5,
              0,1/3,1/3,1/3,0), byrow = TRUE, ncol = 5)

#Visualizacion de la cadena
set.seed(2)
.plotDiagrammeR(mc, node_fill_color = "lightblue", label = "Matrix de transición", labelloc = "t")

mc <- new("markovchain", transitionMatrix = P, states = c("-2","-1","0","1","2"));mc

steadyStates(mc)


#### PUNTO 4 -----------------------
P <- matrix(c(1,0,0,0,0,0,0,0,
              0.6,0,0.4,0,0,0,0,0,
              0,0.6,0,0.4,0,0,0,0,
              0,0,0.6,0,0.4,0,0,0,
              0,0,0,0.6,0,0.4,0,0,
              0,0,0,0,0.6,0,0.4,0,
              0,0,0,0,0,0.6,0,0.4,
              0,0,0,0,0,0,0,1), nrow = 8, byrow = TRUE)

mc <- new("markovchain", transitionMatrix = P,
            states = c("0","1","2","3","4","5","6","7"))

#Visualizacion de la cadena
set.seed(2)
.plotDiagrammeR(mc, node_fill_color = "lightblue", label = "Matrix de transición", labelloc = "t")

summary(mc)

M <- absorptionProbabilities(mc); M












