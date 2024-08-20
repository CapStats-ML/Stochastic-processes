################################################################################
# Script desarrollo Parcial 2B: Procesos Estocasticos
# Autores: 
# - Cesar Augusto Prieto Sarmiento
#
# Fecha de Creación: 15/08/2024
# Ultima Fecha de Mod: 15/08/2024
# Descripción: Se plantea realizar el desarrollo del parcial practico de la asignatura
#           de Procesos Estocasticos, con el fin de demostrar los conocimientos adquiridos
#           en esta parte de la tematica del curso.
# Versión: 1
################################################################################

# Cargamos las librerias necesarias
setwd("~/REPOS GIT/Stochastic-processes/Parcial2")

## PUNTO 1 ---- 
# En una guardería hay N niños y sólo una persona encargada de cuidarlos. De vez en cuendo un bebé
# llora exigiendo la atencion de la niñera. Si la niñera está ocupada atendiendo a otro bebé, el nuevo
# bebé que llora debe esperar su turno. Si en el tiempo t un bebé está tranquilo entonces la probabilidad
# de que él empiece a llorar y exija ser atendido en el intertvalo de tiempo (t, t+h] es igual a
# λh + o(h). Si en el tiempo t un bebé está siendo atendido por la niñera entonces la porbabilidad de que 
# él se calme en el intervalo de tiempo (t, t+h] es igual a μh + o(h). 
# Supóngase que X_t :="número de bebés que están exigiendo ser atendidos en el tiempo t". 


### INCISO A) 
# Asumimos X_0 = 0, N = 5, los valores de λ = 0.5 y μ = 0.3 respectivamente ¿ Cual es la probabilidad
# de que a la larga haya exactamente 0 bebés exigiendo ser atendidos?


### INCISO B) 
# Asumimos X_0 = 0, N = 5, los valores de λ = 0.3 y μ = 0.5 respectivamente ¿ Cual es la probabilidad
# de que a la larga haya exactamente 0 bebés exigiendo ser atendidos?

### DESARROLLO

# Parámetros generales
N <- 5  # Número de bebés

# Función para calcular π0
calcular_pi0 <- function(lambda, mu, N) {
  # Calculamos la sumatoria
  sumatoria <- sum(sapply(0:N, function(i) {
    factorial(N) / factorial(N - i) * (lambda/mu)^i
  }))
  
  # Calculamos π0
  pi0 <- 1 / sumatoria
  return(pi0)
}

# Parámetros del Caso A
lambda_A <- 0.5
mu_A <- 0.3

# Cálculo de π0 para el Caso A
pi0_A <- calcular_pi0(lambda_A, mu_A, N)
cat("Probabilidad de que no haya bebés esperando ser atendidos (Caso A):", pi0_A, "\n")

# Parámetros del Caso B
lambda_B <- 0.3
mu_B <- 0.5

# Cálculo de π0 para el Caso B
pi0_B <- calcular_pi0(lambda_B, mu_B, N)
cat("Probabilidad de que no haya bebés esperando ser atendidos (Caso B):", pi0_B, "\n")

#------------------

# Definir un rango de valores para lambda y mu
lambda_values <- seq(0.1, 1, by = 0.1)
mu_values <- seq(0.1, 1, by = 0.1)

# Crear una matriz para almacenar los resultados de pi0
pi0_matrix <- matrix(0, nrow = length(lambda_values), ncol = length(mu_values),
                     dimnames = list(paste("λ =", lambda_values), paste("μ =", mu_values)))

# Calcular pi0 para cada combinación de lambda y mu
for (i in 1:length(lambda_values)) {
  for (j in 1:length(mu_values)) {
    pi0_matrix[i, j] <- calcular_pi0(lambda_values[i], mu_values[j], N)
  }
}

# Graficar como un mapa de calor
library(ggplot2)
library(reshape2)

# Convertir la matriz a formato largo para ggplot
pi0_df <- melt(pi0_matrix)

ggplot(pi0_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = expression(pi[0])) +
  labs(x = expression(mu), y = expression(lambda), title = "Sensibilidad de π0 con respecto a λ y μ") +
  theme_minimal()
#------------------

# Definir un rango de valores para lambda/mu
lambda_mu_ratio <- seq(0.1, 2, by = 0.1)
mu_fixed <- 0.5

# Calcular pi0 para cada relación
pi0_values <- sapply(lambda_mu_ratio, function(ratio) {
  calcular_pi0(lambda = ratio * mu_fixed, mu = mu_fixed, N = N)
})

# Crear un dataframe para graficar
df_ratio <- data.frame(Ratio = lambda_mu_ratio, pi0 = pi0_values)

# Graficar
ggplot(df_ratio, aes(x = Ratio, y = pi0)) +
  geom_line(color = "blue") +
  labs(x = expression(lambda/mu), y = expression(pi[0]), title = expression("π"[0] * " vs. λ/μ")) +
  theme_minimal()

## PUNTO 2 ---- 
# Escriba un programa de R o Python $u$ otras para simular un proceso de Poisson 
# no homogéneo con intensidad $\lambda_1(t)=\exp (-t / 5)+t / 5$, y 
# $\lambda_2(t)=\frac{1}{2} \sqrt{t / 2}$ en el intervalo $[0,5000]$

### DESARROLLO 

# Función para simular un proceso de Poisson no homogéneo
simular_poisson_no_homogeneo <- function(lambda, T) {
  t <- 0
  tiempos <- c()
  
  while (t < T) {
    # Usamos una intensidad máxima lambda_max para el rechazo
    lambda_max <- 1.5  # Escoge un valor mayor que la máxima intensidad de lambda(t)
    u1 <- runif(1)
    
    # Proponer un tiempo de llegada
    t <- t - log(u1) / lambda_max
    
    if (t > T) break
    
    # Aceptación o rechazo del tiempo propuesto
    u2 <- runif(1)
    
    if (u2 <= lambda(t) / lambda_max) {
      tiempos <- c(tiempos, t)
    }
  }
  
  return(tiempos)
}

# Definir las funciones de intensidad lambda1(t) y lambda2(t)

lambda1 <- function(t) {
  return(exp(-t / 5) + (t / 5))
}

lambda2 <- function(t) {
  return(0.5 * sqrt(t / 2))
}

# Simular un proceso de Poisson no homogéneo con intensidad lambda1(t)
set.seed(123)
tiempos1 <- simular_poisson_no_homogeneo(lambda1, 5000)
tiempos2 <- simular_poisson_no_homogeneo(lambda2, 5000)

# Ver los primeros tiempos simulados
head(tiempos1)
head(tiempos2)

# VALIDACION Y AJUSTE DEL MODELO

# Cargar librerías necesarias
library(ggplot2)

# Histograma de los tiempos simulados para lambda1
histograma_lambda1 <- ggplot(data.frame(tiempos = tiempos1), aes(x = tiempos)) +
  geom_histogram(binwidth = 100, fill = "royalblue", color = "blue", alpha = 0.6) +
  labs(title = "Histograma de Tiempos Simulados para lambda1(t)",
       x = "Tiempos", y = "Frecuencia") +
  theme_minimal()

plot(tiempos1, seq(1, length(tiempos1)), type = "l", xlim = c(0, 5000), ylim = c(0, length(tiempos1)+1),
     xlab = "Tiempo", ylab = "Número de eventos", main = "Proceso de Poisson no Homogéneo", lwd = 0.5)


plot(tiempos2, seq(1, length(tiempos2)), type = "s", xlim = c(0, 5000), ylim = c(0, length(tiempos2)+1),
     xlab = "Tiempo", ylab = "Número de eventos", main = "Proceso de Poisson no Homogéneo", lwd = 0.5)

# Histograma de los tiempos simulados para lambda2
histograma_lambda2 <- ggplot(data.frame(tiempos = tiempos2), aes(x = tiempos)) +
  geom_histogram(binwidth = 100, fill = "green", color = "limegreen", alpha = 0.6) +
  labs(title = "Histograma de Tiempos Simulados para lambda2(t)",
       x = "Tiempos", y = "Frecuencia") +
  theme_minimal()

# Mostrar los histogramas
print(histograma_lambda1)
print(histograma_lambda2)

# Validación de la tasa de llegadas: cálculo de la integral de lambda(t)
integrar_lambda <- function(lambda, T) {
  integrate(lambda, lower = 0, upper = T)$value
}

# Calcular el valor esperado de llegadas
esperado_lambda1 <- integrar_lambda(lambda1, 5000)
esperado_lambda2 <- integrar_lambda(lambda2, 5000)

# Mostrar los valores esperados
cat("Número esperado de eventos para lambda1(t):", esperado_lambda1, "\n")
cat("Número esperado de eventos para lambda2(t):", esperado_lambda2, "\n")

# Número real de eventos simulados
cat("Número de eventos simulados para lambda1(t):", length(tiempos1), "\n")
cat("Número de eventos simulados para lambda2(t):", length(tiempos2), "\n")

# Ajuste de lambda_max
lambda_max_ajustado <- max(sapply(seq(0, 5000, by = 1), lambda1))
cat("lambda_max ajustado para lambda1(t):", lambda_max_ajustado, "\n")


### PUUNTO 3 ----
# Haga un programa en R que simule al proceso de nacimiento puro $\left\{X_t, 
# t \geq 0\right\}$ con $X(0)=1$. Si caso 1: $\lambda=i \lambda$, para $\lambda>0$, 
# y caso 2 : $\lambda=i^2 \lambda$, para $\lambda>0$, comparar estos dos casos.

# Función para simular un proceso de nacimiento puro
simular_nacimiento_puro <- function(lambda_func, T) {
  t <- 0
  eventos <- c()
  
  while (t < T) {
    # Encontrar lambda_max en el intervalo [0, T]
    lambda_max <- max(lambda_func(seq(0, T, length.out = 1000)))
    
    # Generar el tiempo de llegada
    u1 <- runif(1)
    t <- t - log(u1) / lambda_max
    
    if (t > T) break
    
    # Generar el tiempo de espera y aceptar o rechazar el evento
    u2 <- runif(1)
    if (u2 <= lambda_func(t) / lambda_max) {
      eventos <- c(eventos, t)
    }
  }
  
  return(eventos)
}

# Definir las funciones de intensidad para ambos casos
lambda1 <- function(t) {
  return(1 * 5)  # Tasa constante para el caso 1
}

lambda2 <- function(t) {
  return(5 * 5)  # Tasa constante para el caso 2
}

# Simular el proceso de nacimiento puro para ambos casos
set.seed(123)
eventos1 <- simular_nacimiento_puro(lambda1, 50)
eventos2 <- simular_nacimiento_puro(lambda2, 50)

# Comparar los dos casos
summary(eventos1)
summary(eventos2)

# Graficar histogramas de los tiempos de llegada
par(mfrow = c(2, 1))
hist(eventos1, breaks = 50, main = "Histograma de Tiempos del Caso 1", xlab = "Tiempo", col = "lightblue", border = "black")
hist(eventos2, breaks = 50, main = "Histograma de Tiempos del Caso 2", xlab = "Tiempo", col = "lightcoral", border = "black")

# Graficar las funciones de densidad
par(mfrow = c(2, 1))
plot(density(eventos1), main = "Densidad de Eventos del Caso 1", xlab = "Tiempo", ylab = "Densidad", col = "blue", lwd = 2)
plot(density(eventos2), main = "Densidad de Eventos del Caso 2", xlab = "Tiempo", ylab = "Densidad", col = "red", lwd = 2)
