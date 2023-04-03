library(readxl)
library(forcats)
library(reshape2)

defor = read_excel("data/deforestacion_2022.xlsx")

defor_rc02 <- defor %>% 
  mutate(mes = format(as.Date(md_fecimg),"%m")) %>% 
  filter(md_fecimg >= as.Date("2022-06-01") & md_fecimg <= as.Date("2022-06-30")) %>% 
  filter(anp_codi == "RC02") %>% 
  group_by(md_zonif) %>% 
  summarise(area = sum(md_sup)) %>% 
  arrange(desc(area))

# 1. Leer la data de deforestación del 2022
# 2. Filtrar un ANP y unas fechas en específico (RC02, entre Abril y Julio)
# 3. Agrupar por grillas de md_exa
# 4. Calcular la suma de deforestación por cada grilla afectada
# 5. Ordernar por area (descendente)

ggplot(defor_rc02, aes(md_zonif, area)) +
  geom_col()

# 1. 
defor_2022 <- defor %>% 
  group_by(anp_codi) %>% 
  summarise(area = sum(md_sup)) %>% 
  arrange(desc(area)) %>% 
  head(10)

ggplot(defor_2022, aes(fct_rev(fct_reorder(anp_codi, area)), area)) +
  geom_col()

sum(defor %>% filter(anp_codi == "BP06") %>% summarise(sum(md_sup)))


# HISTORICO DE ANP
# 
historico = read_excel("data/Historico_oficial_ANP_SIG.xls")

anp_h <- historico %>% 
  mutate(CÓDIGO = gsub(" ", "", CÓDIGO)) %>% 
  pivot_longer(cols = -c(CÓDIGO, NOMBRES, CATEGORIAS, AÑO, `UBICACIÓN POLITICA`), 
               names_to = "año", 
               values_to = "valor")

View(anp_h)
anp_h %>% 
  filter(!is.na(valor) & CATEGORIAS == "ZR") %>% 
  group_by(año) %>% 
  summarise(n()) %>% 
  t() %>% 
  as.data.frame() %>% 
  View()

table(anp_h$CATEGORIAS)

