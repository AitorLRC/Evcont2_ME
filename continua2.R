
#################
#Primer ejercicio
#################

n <- 40 #Número de muestras

set.seed(1); x1 <- rnorm(n)
#Para generar la segunda variable con correlación de poisson >0.1 creo la función
corr.data<- function(x1, rho){
  set.seed(7);xr <- rnorm(length(x1)) #Genero una variable random con distribución normal
  
  xcorr<- rho*x1 + sqrt(1-rho^2)*xr #Genero la variable correlada
  
  return(xcorr)
}
x2 <- corr.data(x1, 0.2)
set.seed(2); x3 <- rnorm(n)
cor(x1,x2)

#Defino los coeficientes de la regresión
beta0 <- 10; beta1 <- 5; beta2 <- 23; beta3 <- 15

#Defino los residuales
sigma <- 4
residuales = rnorm(n, 0, sigma)

#Calculo la variable respuesta 
y <- beta0 + beta1*x1 + beta2*x2 + beta3*x3 + residuales

#Creo el modelo de regresión 
reg1 <- lm(y~x1+x2+x3)
summary(reg1) #Vemos que en todos los casos las betas son parecidas a las que hemos definidos y los p-valores las consideran significativas


#Si se añade un punto de influencia
#Al punto con el beta menor
x1.1 <- x1
x1.1[5] <- x1[5]+15
plot(x1.1, y)
#Hacemos la regresión para este caso
reg2 <- lm(y~x1.1+x2+x3)
summary(reg2)

#Al punto con el beta mayor
x2.1 <- x2
x2.1[5] <- x2[5]+20
plot(x2.1, y)
#Hacemos la regresión para este caso
reg3 <- lm(y~x1+x2.1+x3)
summary(reg3)



#Multicolinealidad: relación de dependencia lineal fuerte entre más de dos variables explicativas de una regresión múltiple
#Vamos a suponer multicolienalidad entre x1 y x2
set.seed(16); x2.2 <- 2*x1 + rnorm(n, mean = 0, sd = 1) #Defino x2 como x1 más una variable aleatoria
cor(x1, x2.2) #Veo que la correlación es alta, por lo tanto, hay multicolinealidad
#Hacemos la regresión para este caso
reg <- lm(y~x1+x2.2+x3)
summary(reg)


#Especificación inadecuada del modelo
#Una posible causa de la especificación inadecuada del modelo puede se devida a la omisión de regresores relevante
reg4 <- lm(y~x1+x2)  #Quitamos la tercera que tenía un p-valor muy pequeño
summary(reg4)  #Vemos que los p aumentan y el valor de beta varía

reg5 <- lm(y~x2+x3)
summary(reg5)

#Pruebo a quitar valores de x2
reg6 <- lm(y~x2+x3)
summary(reg6)






##################
#Segundo ejercicio
##################

n <- 30 #Número de muestras

#Por la forma que tiene la función logit sabemos que la probabilidad abarca el rango [0,1] cuando logit se encuentra en el intervalo [-5,5]
set.seed(10);logit <- rnorm(n, mean = 0, sd = 3)

#Defino los coeficientes de la regresión
beta0 <- 5; beta1 <- 3; beta2 <- 2; beta3 <- 3

#Defino la variable cualitativa con dos modalidades
set.seed(15); x1 <- sample(c("s", "n"), n, replace=TRUE)

#Para definir la variable cuantítativa hago uso del logit: logit = beta0 + beta*x1s + beta*x1n + beta3*x2
x2<- (logit - beta0 - beta1*as.numeric(x1=='s') - beta2*as.numeric(x1=='n'))/beta3

#Los residuales se comportan como una distribución normal con media cero y desviación típica sigma
sigma <- 0.8
residuales <- round(rnorm(n, 0, sigma), 1) #Los redondeo a un decimal

#Calculo el logit
logit_aleatorio <- beta0 + beta1*as.numeric(x1=='s') + beta2*as.numeric(x1=='n') + beta3*x2 + residuales

#Calulo la probabilidad
p <- 1/(1+exp(-logit_aleatorio))

#Simulo las variables respuesta según una distribución binomial dependiente de x
y <- rbinom(n, 1, prob = p )

#Estimo el modelo a partir de los parámetros simulados
modelo <- glm(y~x1+x2, family = binomial)
summary(modelo)
