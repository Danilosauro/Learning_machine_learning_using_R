#curso R EVG 

mensagem<-("Olá mundo") #vetor
mensagem
print(mensagem)

## comentários ## 

?print  #ajuda

install.packages() #instala pacotes 
library() #carrega o pacote instalado e que está na biblioteca 


#Os dados em R podem ser do tipo vetor, fator, lista, data frame e matriz 

data <- c("East","West","East","North","North","East","West","West","West","East","North")
fator_data<-factor(data)

fator_data #fator 

fator_data[3]<-"East"
fator_data

pessoa1 <-list(sexo="M", cidade="SSA", idade="18")
pessoa1
 


pessoa1[1]<-"F"
pessoa1


for (i in seq(100)){
  print(i)
}


i<-0
while(i<=10){
  print(i)
  i=i+1
}


x=1

if (x>0){
  print("Número positivo ")
}

nota=4

if (nota>5){
  print("Aprovado")
} else 
  print("reprovado")
?print

install.packages("tidyr")
library(tidyr)

?arrange


install.packages("dplyr")
library(dplyr)
?glimpse
?mutate
?select
?separate
?gather


viagens<-read.csv(file ="C:/Users/decer/Downloads/2019_Viagem.csv" ,sep =';' ,dec =',' )
head(viagens) #primeiras linhas 
view(viagens) #melhor visualização 
dim(viagens)  

summary(viagens$Valor.passagens)
summary(viagens)

library(dplyr)

glimpse(viagens)

viagens$data.início<-as.Date(viagens$Período...Data.de.início,"%d/%m/%y")
viagens$data.início





viagens$data.início.formatada<-format(viagens$data.início,"%y-%m")
viagens$data.início.formatada




#histograma


hist(viagens$Valor.passagens)

summary(viagens$Valor.passagens)

boxplot(viagens$Valor.passagens)

sd(viagens$Valor.passagens)

is.na(viagens$Valor.passagens)


colSums(is.na(viagens))

str(viagens$Valor.passagens)

table(viagens$Valor.passagens)

viagens$Nome.do.órgão.superior



parte1<-viagens %>% group_by(Nome.do.órgão.superior) %>%
  summarise(n=sum(Valor.passagens)) %>% arrange(desc(n)) %>%
  top_n(15)

names(parte1)<-c("orgao","valor")

parte1

library(ggplot2)

ggplot(parte1,aes(x=reorder(orgao,valor),y=valor)+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="valor",y="órgãos") 
  
  
  parte2<-viagens %>% group_by(Destinos)%>% 
    summarise(n=sum(Valor.passagens)) %>% 
    arrange(desc(n))%>%
    top_n(15)

  names(parte2)<-c("destino","valor")  

  ggplot(parte2,aes(x=reorder(destino,valor),y=valor))+
    geom_bar(stat = "identity",fill=3)+
    coord_flip()+
    labs(x="valor",y="destino")
  
  
  
  #diabetes 
  



install.packages("caTools")
library(caTools)

diabetes<-read.csv(file='C:/Users/decer/OneDrive/Área de Trabalho/ESTUDOS_SELEÇÃO/dio.csv',sep=',',dec='.')
head(diabetes)
str(diabetes) 

colSums(is.na(diabetes)) #verificando se há nas 

diabetes$Outcome <- factor(diabetes$Outcome)

summary(diabetes$Insulin)
boxplot(diabetes$Insulin)


boxplot(diabetes)

hist(diabetes$Pregnancies)
hist(diabetes$Glucose)
hist(diabetes$SkinThickness)
hist(diabetes$Age)

diabetes2<- diabetes %>% 
  filter(Insulin<=250) #redução de outliers

boxplot(diabetes2)

set.seed(123)

index=sample.split(diabetes2$Pregnancies,SplitRatio=.70)

index

treino = subset(diabetes2,index==T)
teste  = subset(diabetes2,index==F)

dim(treino)
dim(teste)

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)




modelo<-train(Outcome~.,data=treino,method="knn")
modelo
plot(modelo)

modelo$results
modelo$bestTune
