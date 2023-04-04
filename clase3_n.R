library(tidyverse)
library(readxl)
library(xlsx)

# setwd("....")
# 1. Leer la data de deforestación del 2022
defor_2022 <- read_excel("data/deforestacion_2022.xlsx")
# 2. Filtrar un ANP y unas fechas en específico (RC02, Julio)
# 3. Agrupar por grillas de md_exa
# 4. Calcular la suma de deforestación (md_sup) por cada grilla afectada
# 5. Ordernar por area (descendente)
defor_2022 %>%
  filter(anp_codi == "RC02" & md_fecimg >= as.Date("2022-07-01") & md_fecimg <= as.Date("2022-07-31")) %>% 
  group_by(md_exa) %>% 
  summarise(area = sum(md_sup)) %>% 
  arrange(area)
    

historico <- read_excel("data/Historico_oficial_ANP_SIG.xls")

# CUANTAS ZONAS RESERVADAS HAY POR CADA AÑO
historico_ordenado <- historico %>% 
  mutate(CÓDIGO = gsub(" ", "", CÓDIGO)) %>% 
  pivot_longer(cols = -c(CÓDIGO, NOMBRES, CATEGORIAS, AÑO, `UBICACIÓN POLITICA`),
               names_to = "años",
               values_to = "valor")

acp_historico <- historico_ordenado %>% 
  filter(!is.na(valor) & CATEGORIAS == "ACP") %>% 
  group_by(años) %>% 
  summarise(cantidad = n())

acr_historico <- historico_ordenado %>% 
  filter(!is.na(valor) & CATEGORIAS == "ACR") %>% 
  group_by(años) %>% 
  summarise(cantidad = n())

  
plot(acp_historico, type="l", col="red", main="ACP Historico")

historico_ordenado %>%
  filter(CATEGORIAS == "ACP") %>%
  View()

  
names(historico)[1:5] <- c("codi", "nombres", "categ", "anno", "ubi")
names(historico)

###########################################################

head(iris, 10)

x <- iris$Sepal.Length
y <- iris$Petal.Length

plot(x, y, main="Titulo principal",
     xlab="Eje x", ylab="Eje y", pch=19)

x <- mtcars$wt
y <- mtcars$mpg

plot(x, y)
abline(lm(y ~ x, data = mtcars))

boxplot()
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")

install.packages("scatterplot3d")
library(scatterplot3d)

x <- iris$Sepal.Length
y <- iris$Sepal.Width
z <- iris$Petal.Length

scatterplot3d(x, y, z, pch=16)


#################################################
install.packages("gapminder")
library(gapminder)

ggplot(gapminder, aes(x = year, y = lifeExp, group = year)) +
  geom_boxplot() +
  facet_grid(.~continent) +
  theme_bw()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  facet_grid(.~continent) +
  theme_bw()



###########################################

df1 <- acp_historico %>% mutate(categoria = "acp")
df2 <- acr_historico %>% mutate(categoria = "acr")
df <- rbind(df1, df2)
# rbind: Juntar por fila
# cbind: Juntar por columna

ggplot(acp_historico, aes(x = años, y = cantidad)) +
  geom_col(fill="skyblue") +
  # theme_bw() +
  labs(title = "ACP Histórico",
       subtitle = "Taller de R") +
  xlab("") + ylab("Cantidad de ACP") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))


ggplot(df, aes(x = años, y = cantidad)) +
  geom_col(aes(fill=categoria), position = "stack", alpha=0.5) +
  # theme_bw() +
  labs(title = "Cantidad de Áreas Naturales Protegidas",
       subtitle = "Taller de R") +
  xlab("") + ylab("Cantidad") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))


# Crear un gráfico de líneas de cómo avanzó la deforestación en un área en 
# específico en el año 2022

defor <- defor_2022 %>% 
  filter(anp_codi %in% c("RC02", "BP06", "RC01")) %>% 
  mutate(mes = format(md_fecimg, "%m")) %>% 
  group_by(mes, anp_codi) %>% 
  summarise(area = sum(md_sup))

ggplot(defor_rc02, aes(x = mes, y = area, group=1)) + 
  geom_line(color="green") +
  geom_point(color="green") +
  labs(title = "Deforestación en la RC02",
       subtitle = "Reserva Comunal El Sira") +
  xlab("") + ylab("Área (ha)") +
  theme_bw()


crear_grafico <- function(codigo){
  defor <- defor_2022 %>% 
    filter(anp_codi == codigo) %>% 
    mutate(mes = format(md_fecimg, "%m")) %>% 
    group_by(mes, anp_codi) %>% 
    summarise(area = sum(md_sup))
  
  ggplot(defor, aes(x = mes, y = area, group=1)) + 
    geom_line(color="green") +
    geom_point(color="green") +
    labs(title = "Deforestación en la RC02",
         subtitle = "Reserva Comunal El Sira") +
    xlab("") + ylab("Área (ha)") +
    theme_bw()
}

ggsave("grafico1.png", plot = grafico1, width = 2000, height = 1600, units = "px")

crear_grafico("BP06")


write_excel_csv(defor, "defor_3areas")
