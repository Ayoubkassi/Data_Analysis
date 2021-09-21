library(readxl)
data <- read_excel("Downloads/demotivation.xlsx")
View(data)


#preprocessing 

#1-convertion

data$Sexe <- as.factor(data$Sexe)
levels(data$Sexe) =  c("M", "F")
data$Sexe

data$Fonction <- as.factor(data$Fonction)
levels(data$Fonction) = c("Responsable","Cadre","Opérateur")
data$Fonction


data$Age <- as.numeric(data$Age)
data$Age


data$Revenu <- as.numeric(data$Revenu)
data$Revenu

summary(data)


#Nettoyage de donne

#1-on a pas de valeurs manquantes alors on va juste se focaliser sur les valeurs aberrantes

#1-commençons par l'Age
boxplot(data$Age)
#d'apres le boxplot il n'ya pas de valeurs aberrante on Age
age_aberrante <- boxplot(data$Age)$out
age_aberrante


#aussi la liste des valeurs aberrantes est nule.

#2-scolarite

boxplot(data$Scolarite)
scolarite_aberrante <- boxplot(data$Scolarite)$out
scolarite_aberrante

#alors d'apres la fonction boxplot out c'est facile de voir qu'il ya une seul valeur aberrante c'est 2
#alors on va calculer la moyene et la remplacer par sa valeur

#fonction pour calculer la moyenne sans valeur aberrante

count <- 0
function.moyen <- function(list){
  for(i in list){
    if(i>3){
      count <- count + i
    }
  }
  result <- count/(length(list)-1)
  print(result)
}

scolarite_moyen <- function.moyen(data$Scolarite)

#prenons la partie entiere c'est 16 alors on vat juste la remplcer dans notre donnes

data$Scolarite <- replace(data$Scolarite , data$Scolarite == 2,16)
data$Scolarite

#3pour la variable revenue

boxplot(data$Revenu)
#alors il n'ya pas de valeurs aberrante .







#Etape 2 L'analyse de donnes

  #1-analyse univarie : consiste a faire un plot pour les variables qualitatives et un hist pour 
  #les variables quantitatives et faire un summary .

plot(data$Sexe)
hist(data$Age)
hist(data$Scolarite)
plot(data$Fonction)
hist(data$Revenu)


summary(data$Sexe)
summary(data$Age)
summary(data$Scolarite)
summary(data$Fonction)
summary(data$Revenu)



#test de normalite c'est juste pour les variables quantitatives

#1 variable Age

shapiro.test(data$Age)
# alors on n'a la valeurs de p-value < 0.5% alors on a pas de normalite passons a la casinormalite

library(moments)
skewness(data$Age)
kurtosis(data$Age)


#alors on 'a notre valeur n'est pas  entre 2.5 et -2.5 alors ne respecte pas la casinormalite


#2 variable Scolarite 

shapiro.test(data$Scolarite)
#On a la valeurs de p-value >5% alors il respecte la normalite , il ny'a pas de difference entre notre distrbution et la loi normale


#3 variable Revenue

shapiro.test(data$Revenu)
#On a la valeurs de p-value >5% alors il respecte la normalite , il ny'a pas de difference entre notre distrbution et la loi normale



#alors on a comme variables suit la loi normale : Scolarite et Revenue .





#2 analyse bivarie : entre 2 varible
#1 entre Age et Revenue sont deux variable quantitative alors effectuons test de correlation

cor.test(data$Revenu,data$Age)
#alors on a p-value = 0.04517 inferieur a 5% alors il y'a une relation entre l'age et le Revenue 

#2entre genre et revenue : alors on'a genre variable qualitative et revenue variables quanti suit la loi normale
#alors effectuons un test de nova 


summary(aov(data$Revenu~data$Sexe))[[1]][["Pr(>F)"]]

#alors on'a une valeur inferieur a 5% alors ily'a une association entre Sexe et Revnue

#3annee d'etude et revenue : on'a pas annee d'etude

#4entre fonction et revenue : de meme quanti suit la loi normale avec variable quali -> la nova

summary(aov(data$Revenu~data$Fonction))[[1]][["Pr(>F)"]]

#on 'a une valeur inferieur a 5% alors il y'a une association entre Revenue et Fonction ce qui est normale
    # le revenue d'un Responsable c'est pas d'un cadre ect .

#5 entre genre et Scolarite : aussi genre variable quali et Scolarite quanti suit la loi normale -> la nova

summary(aov(data$Scolarite~data$Sexe))[[1]][["Pr(>F)"]]

#ON a la valeur de p-value superieur a 5% alors il ny'a pas d'asscociation entre sexe et Scolarite 

#5 entre genre et focntion : 2 variables qualitatives alors chisq test 

chisq.test(data$Fonction,data$Sexe)

#on 'a la valeur de p-value > a 5£ alors il n'ya pas une relation entre Sexe et Fonction







#4-1 ALORS D'APRES Les test effectuer , on peut dire qu'il y'a une certain association entre le revenue d'un homme et femme
 #qui suit le meme parcours , alors il n'ya pas de diffrence .


#4-2 on peut rien dire parce qu'on a pas de donnes suffisantes .




#linear regression between Revnue et Age 


plot(data$Age, data$Revenu)
plot(data$Age, data$Revenu, pch = 16, cex = 1.3, col = "blue", main = "Relation entre Revnue et Age", xlab = "Age", ylab = "Revenue")
lm(data$Revenu ~ data$Age)
abline(32951.2, 592.3 ,col="red")



#linear regression between Revenue et Scolarite


plot(data$Scolarite, data$Revenu)
plot(data$Scolarite, data$Revenu, pch = 16, cex = 1.3, col = "blue", main = "Relation entre Revenue et Scolarite", xlab = "Scolarite", ylab = "Revenue")
lm(data$Revenu ~ data$Scolarite)
abline(13948,  2293,col="red")




