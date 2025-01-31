
#################
#Primer ejercicio
#################

n <- 40 #Tama�o de muestra al menos de 3 observaciones

set.seed(1); x1 <- rnorm(n) #Genero la variable aleatoria x1
#Para generar la segunda variable con correlaci�n de poisson >0.1 creo la funci�n
corr.data<- function(x1, rho){
  set.seed(7);xr <- rnorm(length(x1)) #Genero una variable random con distribuci�n normal
  
  xcorr<- rho*x1 + sqrt(1-rho^2)*xr #Genero la variable correlada
  
  return(xcorr)
}
x2 <- corr.data(x1, 0.2) #Genero la variable aleatoria x2 con correlaci�n 0.1 con respecto a x1
set.seed(2); x3 <- rnorm(n) #Genero la variable aleatoria x3

cor(x1,x2)

#Defino los coeficientes de la regresi�n
beta0 <- 10; beta1 <- 5; beta2 <- 23; beta3 <- 15

#Defino los residuales
sigma <- 4
residuales = rnorm(n, 0, sigma)

#Calculo la variable respuesta 
y <- beta0 + beta1*x1 + beta2*x2 + beta3*x3 + residuales

#Creo el modelo de regresi�n 
reg1 <- lm(y~x1+x2+x3)
summary(reg1) #Vemos que en todos los casos las betas son parecidas a las que hemos definidos y los p-valores las consideran significativas


#Si se a�ade un punto de influencia
#Al punto con el beta menor
x1.1 <- x1
x1.1[5] <- x1[5]+20

plot(x1.1, y)
#Hacemos la regresi�n para este caso
reg2 <- lm(y.1~x1.1+x2+x3)
summary(reg2)

#Al punto con el beta mayor
x2.1 <- x2
x2.1[5] <- x2[5]+20

plot(x2.1, y)
#Hacemos la regresi�n para este caso
reg3 <- lm(y~x1+x2.1+x3)
summary(reg3)



#Multicolinealidad: relaci�n de dependencia lineal fuerte entre m�s de dos variables explicativas de una regresi�n m�ltiple
#Vamos a suponer multicolienalidad entre x1 y x2
set.seed(16); x2.2 <- 2*x1 + rnorm(n, mean = 0, sd = 1) #Defino x2 como x1 m�s una variable aleatoria
cor(x1, x2.2) #Veo que la correlaci�n es alta, por lo tanto, hay multicolinealidad
#Hacemos la regresi�n para este caso
reg4 <- lm(y~x1+x2.2+x3)
summary(reg4)


#Especificaci�n inadecuada del modelo
#Una posible causa de la especificaci�n inadecuada del modelo puede se devida a la omisi�n de regresores relevante
reg5 <- lm(y~x1+x2)  #Quitamos la tercera que ten�a un p-valor muy peque�o
summary(reg5)  #Vemos que los p aumentan y el valor de beta var�a

reg6 <- lm(y~x2+x3)
summary(reg6)

#Pruebo a quitar valores de x2
reg7 <- lm(y~x1+x3)
summary(reg7)






##################
#Segundo ejercicio
##################

n <- 30 #N�mero de muestras

#Por la forma que tiene la funci�n logit sabemos que la probabilidad abarca el rango [0,1] cuando logit se encuentra en el intervalo [-5,5]
set.seed(10);k <- rnorm(n, mean = 0, sd = 3)

#Defino los coeficientes de la regresi�n
beta0 <- 5; beta1 <- 3; beta2 <- 2; beta3 <- 3

#Defino la variable cualitativa con dos modalidades
set.seed(15); x1 <- sample(c("s", "n"), n, replace=TRUE)

#Para definir la variable cuant�tativa hago uso del logit: logit = beta0 + beta*x1s + beta*x1n + beta3*x2
x2<- (k - beta0 - beta1*as.numeric(x1=='s') - beta2*as.numeric(x1=='n'))/beta3

#Los residuales se comportan como una distribuci�n normal con media cero y desviaci�n t�pica sigma
sigma <- 0.8
residuales <- round(rnorm(n, 0, sigma), 1) #Los redondeo a un decimal

#Calculo el logit
k_aleatorio <- beta0 + beta1*as.numeric(x1=='s') + beta2*as.numeric(x1=='n') + beta3*x2 + residuales

#Calulo la probabilidad
p <- 1/(1+exp(-k_aleatorio))

#Simulo las variables respuesta seg�n una distribuci�n binomial dependiente de x
y <- rbinom(n, 1, prob = p )

#Estimo el modelo a partir de los par�metros simulados
modelo <- glm(y~x1+x2, family = binomial(logit))
summary(modelo)
exp(coef(modelo))
