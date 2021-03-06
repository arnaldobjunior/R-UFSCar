library(coronavirus)
library(dplyr)
library(ggplot2)
data(coronavirus)

brazil <-coronavirus %>% select(cases,country,date,type)%>% filter(country=='Brazil')

total <- group_by_at(coronavirus,vars(country,type)) %>% summarise(total = sum(cases))
maiores_casos <- group_by_at(total,vars(country,type)) %>% filter(type=="confirmed")

maiores_casos <- maiores_casos[order(maiores_casos$total,decreasing=TRUE),]
maiores_casos[8,1] <- "U.K"
maiores_casos$total <- as.numeric(as.character(maiores_casos$total))
total_type <- total %>% group_by(type) %>% summarise(soma = sum(total))

cinco_maiores <- total %>% select(country, type,total) %>% filter(country %in% c("US","Brazil","India","Russia","Peru"))

cinco_maiores$total = as.numeric(as.character(cinco_maiores$total))
total<- total %>% mutate(porcentagem = case_when(type == "confirmed"~total/12910357,type=="death"~total/569128,type=="recovered"~total/7116957))
total$porcentagem <- format(total$porcentagem,scientific = FALSE)
dez_maiores <- total %>% select(country, type,total,porcentagem) %>% filter(country %in% c("US","Brazil","India","Russia","Peru","Chile","Mexico","U.K","South Africa","Iran"))

dez_maiores$porcentagem = as.numeric(as.character(dez_maiores$porcentagem))
dez_maiores[,4] <- round(dez_maiores[,4],4)
dez_maiores_conf <- dez_maiores %>% filter(type=="confirmed")
dez_maiores_death <- dez_maiores %>% filter(type=="death")
dez_maiores_rec <- dez_maiores %>% filter(type=="recovered")


ggplot(dez_maiores_conf,aes(x=country,y=porcentagem,fill=country))+
  geom_bar(stat = "identity",color="black")+
  scale_fill_brewer(palette="Spectral",name="Pa�ses")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Pa�s",y="Confirmados")+
  theme_bw()

ggplot(cinco_maiores,aes(x=country,y=total/1000, fill=type))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(label = c("0","1 Mi","2 Mi","3 Mi"))+
  scale_fill_manual(values=c("#9A3B5C", "#999999", "#FF7373"),name=" ",labels=c("Confirmados","Mortes","Recuperados"))+
  labs(x="Pa�s",y="Total")+
  theme_bw()

ggplot(total_type,aes(x=type,y=soma/100))+
  geom_bar(stat = "identity",aes(fill=type))+
  scale_y_continuous(labels = c("0","1 mi","10 mi"), breaks = c(10000,50000,100000))+
  scale_x_discrete(labels = c(" "," "," "))+
  scale_fill_manual(values = c("#A2A633","#210140","#A67D65"),name = " ",labels=c("Confirmados","Mortes","Recuperados"))+
  labs(x="Pa�s",y="Total")+
  theme_bw()

ggplot(maiores_casos[0:10,])+
  geom_col(aes(x=country,y=total,fill=country))+
  scale_fill_brewer(palette = "Set3",name="Pa�ses")+
  labs(y="Confirmados",x="Pa�ses")+
  scale_y_continuous(breaks=c(0,200000,1000000,1800000,3500000),labels = c("0","200 Mil","1 Mi","1.8 Mi","4 Mi"),limits = c(0,4000000))+
  theme_bw()
ggplot(brazil,aes(x=date,y=cases,colour=type))+
  geom_line(size=0.6)+
  scale_colour_manual(values=c("red","black","blue"),labels=c("Confirmados","Mortes","Recuperados"),name=" ")+
  scale_y_continuous(labels = c("0","25 mil","50 mil","75 mil","100 mil","125 mil"),breaks = c(0,25000,50000,75000,100000,125000))+
  scale_x_date(breaks=as.Date(c("2020-02-08","2020-02-28","2020-03-19","2020-04-08","2020-04-28","2020-05-18","2020-06-07","2020-06-27","2020-07-17")),limits = as.Date(c("2020-02-01","2020-07-20")),position = "top",labels = c("02-08","02-28","03-19","04-08","04-28","05-18","06-07","06-27","07-17"))+
  labs(x="M�s-Dia",y=" ")+
  theme_bw()
  
  