library(readxl)

getwd()

df = read_excel("data/deforestacion_2022.xlsx")
save(df, file = "clase2.Rdata")
load("clase2.Rdata")

###########################################
# Operadores lógicos
# <, <=
# >, >=
# ==
# !=

# |
# &

1 > 3 | "a" == "b"
5 == 5 & "a" != "b"
5 %in% c(1,2,4,5,6)

if(10 %in% 20:40){
  print("Se encuentra dentro")
}else{
  print("No se encuentra dentro")
}

x <- 20
if (x < 0) {
  print("Numero negativo")
}else if (x > 0){
  print("Numero positivo")
} else {
  print("Cero")
}

temp <- c(15, 22, 30)
suelo <- c("seco", "humedo", "charcos")
valtemp <- temp[1]
valsuelo <- suelo[3]
if (valtemp %in% 12:22 & valsuelo =="seco") {
  print("\nCielo despejado")
} else if (valtemp == 22 | valsuelo == "humedo"){
  print("\nlluvia leve")
} else if (valtemp > 22 & valsuelo=="charcos"){
  print("\nlluvia severa")
} else {
  print("\nFaltan datos")
}


#################################################

anp_codi <- c("RC02", "PN09", "CC01")
for (codi in anp_codi) {
  print(codi)
}

numeros <- seq(1, 10, 2)
for (num in numeros) {
  potencia = 2^num
  print(potencia)
  if(potencia > 100){
    print("El valor es mayor que 100")
  }
}

# creando la expresión con valor 1
inicio <- 1
# Creando el loop
while (inicio <= 5){
  # Ver donde estamos
  cat('Este es el ciclo número ', inicio)
  # Agregar 1 a la variable inicio despues de cada iteracion
  inicio <- inicio + 1
  if( inicio == 3) next
  print(inicio)
}


suma <- function(a, b){
  c = a + b
  return(c)
}
suma(10, 20)

# 1. Utilizando i <- 1, escriba un bucle while() que imprima la variable i, 
#    (que se incrementa de 1 a 5), y se usa break para salir del bucle si i es igual 3.
# UTILIZAR break
inicio = 1
while (inicio <= 5){
  print(inicio)
  inicio = inicio + 1
  if(inicio == 3) break
}

# 2. Escriba un bucle for() que imprima todos los valores excepto 3 en la 
#    siguiente variable: i <- 1:5
# UTILIZAR next
i = c(1:5)
for(num in i){
  if(num == 3) next
  else print(num)
}

numero <- 1:5
for ( i in numero){ 
  if (i == 3) next
  else print(i)
}

# FUNCION
suma <- function(a, b, c=4){
  d = a + b + c
  return(d)
}
suma(10, 20)

getwd()
setwd("")

list.files("data")
length(anp_codi)

######################################################

library(tidyverse)
library(readxl)

defor_2022 <- read_excel("data/deforestacion_2022.xlsx")
defor_2022
View(defor_2022)

# DPLYR
## filter
# md_causa: 1 = Agropecuario
# md_causa: 2 = 
nrow(filter(defor_2022, anp_codi == "RC02"))
filter(defor_2022, anp_codi == "RC02" & md_causa != 1)

## select
select(defor_2022, anp_codi, md_causa, md_sup)
select(filter(defor_2022, anp_codi == "RC02" & md_causa != 1), anp_codi, md_causa, md_sup)

## PIPE
defor_2022 %>%
  filter(anp_codi == "RC02") %>% 
  select(md_zonif, md_causa, md_sup) %>% 
  arrange(md_causa) %>% 
  mutate(md_m2 = md_sup*10000)


defor_2022 %>% 
  filter(anp_codi == "RC02") %>% 
  select(md_zonif, md_causa, md_sup) %>% 
  group_by(md_zonif, md_causa) %>% 
  summarise(area = sum(md_sup))

defor_2022 %>%
  mutate(anp_cate = str_sub(anp_codi, 1, 2)) %>% 
  group_by (anp_cate, md_causa)%>%
  summarise (area = sum( md_sup))
  

defor_rc02 <- defor_2022 %>% 
  filter(anp_codi == "RC02") %>% 
  mutate(mes = months.Date(md_fecimg)) %>% 
  group_by(mes) %>% 
  summarise(area = sum(md_sup))

write_csv(defor_rc02, "data/defor_rc02.csv")


cate_causa <- defor_2022 %>%
  mutate(anp_cate = str_sub(anp_codi, 1, 2)) %>% 
  group_by (anp_cate, md_causa)%>%
  summarise (area = sum( md_sup))



plot(x = c(1,2,3,4),
     y = c(5,6,7,8), "l")

ggplot()

