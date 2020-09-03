library(tidyr)    
library(ggplot2)   
library(readxl)
library(dplyr)
library(geobr)
corona = read_excel("C:\\Users\\bispo\\Downloads\\covid1.xlsx", 
                    +                     col_types = c("text",    # Regiao
                                                        +                                   "text",    # Estado
                                                        +                                   "text",    # Municipio
                                                        +                                   "text",    # coduf
                                                        +                                   "numeric", # codmun
                                                        +                                   "numeric", # codRegiaoSaude
                                                        +                                   "text",    # nomeRegiaoSaude
                                                        +                                   "date",    # data
                                                        +                                   "numeric", # semanaEpi
                                                        +                                   "numeric",    # populacaoTCU2019
                                                        +                                   "numeric", # casosAcumulado
                                                        +                                   "numeric", # casosNovos
                                                        +                                   "numeric", # obitosAcumulado
                                                        +                                   "numeric", # obitosNovos
                                                        +                                   "numeric", # Recuperadosnovos
                                                        +                                   "numeric","numeric"  # emAcompanhamentoNovos
                                                        +                     ))



states <- read_state(year=2014)

no_axis <- theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())

ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() + no_axis


populacao <- corona %>% group_by(estado) %>% summarise(populacao = max(populacaoTCU2019[is.finite(populacaoTCU2019)]))
n_obitos_estado <- corona %>% group_by(estado) %>% summarise(obitos_uf = max(obitosAcumulado))
n_obitos_cidade <- corona %>% group_by(municipio) %>% summarise(obitos_cit = max(obitosAcumulado))

n_casos_estado <- corona %>% group_by(estado) %>% summarise(casos_uf = max(casosAcumulado))
n_casos_cidade <- corona %>% group_by(municipio) %>% summarise(casos_cit = max(casosAcumulado))

states <- left_join(states,n_obitos_estado,by=c("abbrev_state"= "estado"))
states <- left_join(states,n_casos_estado,by=c("abbrev_state"= "estado"))
states <- left_join(states,populacao,by=c("abbrev_state"= "estado"))

states1 <- states  %>% mutate(obitos_x_pop = obitos_uf/populacao*100,casos_x_pop = casos_uf/populacao*100,obito_x_covid= obitos_uf/casos_uf)



ggplot() + 
  geom_sf(data=states1,aes(fill=obitos_uf),color="black",size=.05)+
  scale_fill_distiller(direction = 1,palette = "OrRd", name=" ",breaks = c(1000,5000,10000,15000,20000))+
  theme_minimal() +no_axis+
  labs(title="Número óbitos por Covid-19 no Brasil")+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 25))

ggplot() + 
  geom_sf(data=states1,aes(fill=obitos_x_pop),color="black",size=.05)+
  scale_fill_distiller(direction = 1,palette = "OrRd", name=" ")+
  theme_minimal() +no_axis+
  labs(title="Porcentagem de óbitos por Covid-19 em relação à população estadual")+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 25))

ggplot() + 
  geom_sf(data=states1,aes(fill=casos_uf/1000),color="black",size=.05)+
  scale_fill_distiller(direction = 1,palette = "Greens", name="",breaks = c(50,100,150,200,250,300,350))+
  theme_minimal() +no_axis+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 25))+
  labs(title="Número de pessoas diagnosticadas com Covid-19 no Brasil")

ggplot() + 
  geom_sf(data=states1,aes(fill=casos_x_pop),color="black",size=.05)+
  scale_fill_distiller(direction = 1,palette = "Greens", name="",breaks = c(0.5,1,1.5,2,2.5,3,3.5))+
  theme_minimal() +no_axis+
  labs(title="Porcentagem de pessoas diagnosticadas com Covid-19 em relação à população estadual")+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 25))


ggplot() + 
  geom_sf(data=states1,aes(fill=obito_x_covid*100),color="black",size=.05)+
  scale_fill_distiller(direction = 1, name="")+
  theme_minimal() +no_axis+
  labs(title="Porcentagem de pessoas diagnosticadas com Covid-19 que foram a óbito\nem relação à população estadual")+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 25))+ geom_sf_text(data=states1,aes(label = abbrev_state))
