


#####################    Mohamed Ben Yassine            ##################
#####################    Project Programmation en R     ##################


#################################
####  Importation de données ####
#################################

rm(list = ls())

# Le fichier titanic_original est lu est stocké dans un nouveau objet R nommé Data
# Le séparateur utilisé dans le fichier délimité est la virgule
# Le changement seront faits sur l'obejt "DATA", l'objet"titanic" va servir pour faire des comparaisons


Titanic<-read.table(file.choose(),header = TRUE, sep = ",")
DATA<-read.table(file.choose(),header = TRUE, sep = ",")

View(DATA)

############################################
#### Nettoyer et traiter les données########
############################################


#On commence par le nettoyage des données pour cela on va installer le package dplyr

install.packages("dplyr",dependencies = TRUE)
library(dplyr)

#Commençant par une inspection du jeu de données

names(DATA)

str(DATA)

################################################
####### Traitement des valeurs manquantes ######
################################################


#On aura besoin des librairies VIM et lattice pour voir le pourcentage de valeur manquantes et 
#décider comment faire pour ces valeurs manquantes

install.packages("VIM", dependencies = TRUE)
library(VIM)
require(VIM)

install.packages("lattice")
install.packages("mice")

library(mice)
require(mice)


######## Détection des valeurs manquantes #########

#Avec la fonction summary on pourra voir le nombre des valeurs manquantes pour chaque variables

summary(DATA)

#la fonction md.pattern() nous permets de voir les nombres d'observations qui ont des valeurs manquantes
# et ceci pour chaque variable 
md.pattern(DATA)

#une visualisation des valeurs manquantes va nous montrer plus clairement le  pourcentage des valeurs
#manquantes pour chaque variables, et donc si pour une variable le pourcentage dépasse 5% il faut 
#les remplacer par la moyenne ou par un autre moyen

NAPlot <- aggr(DATA, col=c('navyblue','pink'), numbers=TRUE, sortVars=TRUE,
               labels=names(data), cex.axis=.7, gap=3,
               ylab=c("Histogram des valeurs manquantes","Pattern"))

###  Traitement des variables cabin body and boat ######

#Création des vraiables "have_cabine" et "have_body"
#Création de la variable have_cabin qui a 1 comme valeur pour les passagers qui avaient une cabine, 0 sinon

install.packages("dplR",dependencies = TRUE)
install.packages("magrittr",dependencies = TRUE) 

library("dplyr") 
library("magrittr")
require(magrittr)
require(dplyr)

names(DATA)


DATA<-mutate(DATA, have_cabin=if_else(DATA$cabin=="", 0,1))

#Création de la variable have_body

DATA<-mutate(DATA,have_body=if_else(DATA$body!="NA",1 ,0))

#Remplacer les Na dans la variable have_body par des zéros

DATA$have_body[is.na(DATA$have_body)] <- 0

#Dans le but de simplifier l'analyse de cette base de données on va créer la variable have_boat même 
#Si cette variable n'a pas de valeurs manquantes
#Création de la variable have_boat qui a 1 comme valeur pour ceux qui ont pu trouver un bateau lors du naufrage 0 sinon


DATA<-mutate(DATA,have_boat=if_else(DATA$boat=="",0 ,1))

#view data va nous permettre de voir les nouvelles variables crées
View(DATA)


#IMPUTATION OF THE MISSING VALUES of the variables ("AGE","fare") WITH KNN algorithme
install.packages("class")
library(class)
require(class)

DATA<-kNN(DATA,variable=c("age","fare"),k=5)


md.pattern(DATA)

NAPlot <- aggr(DATA, col=c('navyblue','pink'), numbers=TRUE, sortVars=TRUE,
               labels=names(data), cex.axis=.7, gap=3,
               ylab=c("Histogram des valeurs manquantes","Pattern"))

#Comparaison entre la moyenne avant et après remplacement des valeurs manquantes de la variables age.

mean(DATA$age)
mean(Titanic$age,na.rm=T)

#sachant que l'age ne peut pas être négative ou très grand
#on va donc soustrire un sous ensemble de la base DATA dont l'age est inférieur à zéro
delete_age_negative<-subset(DATA,DATA$age<0) 

#la table contient zéro observation donc on a pas de valeurs négative pour la variable age

#avec les lignes de commandes suivantes on trouve qu'on a 0% de valeurs manquantes pour les 
#variables age et fare

md.pattern(DATA)

NAPlot <- aggr(DATA, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE,
               labels=names(data), cex.axis=.7, gap=3,
               ylab=c("Histogram des valeurs manquantes","Pattern"))


# Traitement des variables BODY
#we may do a knn regression yet we will fine body's with same number wich does not make sens
#so we will keep the variable have_body created before
#The embarked column has some missing values, which are known to correspond to passengers 
#who actually embarked at Southampton. WE WILL Find the missing values and replace them with S


##################transformation de type de données
######################################################
#On va convertir les types de variables à un type plus convenable

DATA = transform ( DATA, boat = as.numeric(boat), cabin = as.numeric(cabin))


#################################
#####  Data visualisation #######
#################################

#On aura besoin de la librairie ggplot2 pour la visualisation des données

install.packages("ggplot2")
library(ggplot2)

View(DATA)

#Histogramme de la variable pclass

hist(DATA$pclass,main = paste("Histogram de la variable pclass"))

#Histogramme de la variable sex
#il faut que la variable DATA$sex soit numérique, on va donc coder cette variable
#la variable num_sex a comme valeur 1 pour femme et zéro sinon

num_sex=if_else(DATA$sex=="male",0,1)

hist(num_sex,main = paste("Histogram de la variable sex"))

pie(table(DATA$sex),main = "sex des passagers",col = c("pink","blue") )

pie(table(DATA$have_boat),main = "boat")
pie(table(DATA$have_body),main = "body")
pie(table(DATA$survived),, labels = c( "notsurvived","survived"),main = "Taux des survivants")
#CRéation de "DATAnum" qui ne contient que les variables de type numérique
DATANUM<-subset(DATA,DATA$age>0,select= c(pclass,survived,age,sibsp,parch,fare,boat,sex))
#supression d'une seul observation avec des NA
DATANUM<-subset(DATANUM,DATANUM$pclass!="NA")
summary(DATANUM)

###### Visualtion de données

#visualisation des variables "sex","pclass", "survived"

ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = sex))
c<-as.factor(DATA$pclass) 
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = c))
s<-as.factor(DATA$survived)
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = s))

#VARIABLE SEX EN FONCTION DES VIVANTS

ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = sex, fill = s))

ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = sex, fill = s), position = "dodge")

#Sex and have body

DATA$have_body<-as.factor(DATA$have_body)
class(DATA$have_body)
levels(DATA$have_body)
have_body=DATA$have_body
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = sex, fill = have_body))



# Sex et class
class<-as.factor(DATA$pclass)
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = sex, fill = class))
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = sex, fill = class), position = "dodge")

#Survived et class
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = s, fill = class))
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = s, fill = class), position = "dodge")

#survived et sex 
  
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = s, fill = sex))
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = s, fill = sex), position = "dodge")

# Création d'une variable adulte qui a 1 comme valeur si age>18 , 0 sinon

DATA<-mutate(DATA,adulte=if_else(DATA$age>18,1 ,0))
DATA$adulte<-as.factor(DATA$adulte)
adulte<-DATA$adulte

ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = adulte, fill = sex))
ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = adulte, fill = sex), position = "dodge")

ggplot(data = DATA) + 
  geom_bar(mapping = aes(x = adulte, fill = s), position = "dodge")

