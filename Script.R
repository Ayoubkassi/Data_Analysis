#importation of the data
library(readxl)
data <- read_excel("Downloads/data.xlsx")
View(data)
# Pretraitement  : 

# 1 - Conversion : 

sexe <- data$Genre
sexe<- as.factor(sexe)
levels(sexe) =  c("M", "F")
sexe

Age <- data$Age
Age <- as.numeric(data$Age)
Age

echelle = c("Yes","No")

Parascolaire <- as.factor(data$Parascolaire)  
levels(Parascolaire) = echelle
Parascolaire  

Autoformation <- as.factor(data$Autoformation)
levels(Autoformation) = echelle
Autoformation

echelle = c("Tout à fait d'accord", "D'accord", "Neutre", "Pas d'accord", "Pas du tout d'accord")

M1 <- as.factor(data$M1)
M2 <- as.factor(data$M2)
M3 <- as.factor(data$M3)
M4 <- as.factor(data$M4)
M5 <- as.factor(data$M5)
M6 <- as.factor(data$M6)
D1 <- as.factor(data$D1)
D2 <- as.factor(data$D2)
D3 <- as.factor(data$D3)
D4 <- as.factor(data$D4)
D5 <- as.factor(data$D5) 
levels(M1) = levels(M2) = levels(M3) = levels(M4) = levels(M5) = levels(M6) = levels(D1) = levels(D2) = levels(D3) = levels(D4) = levels(D5)  = echelle
summary(M1)


#comme donnes
# ona 2 types 
# variables a explique :
#     -c'est notre difficultes': de D1,...,D6
# variable explicatif : 
#     -are features of our Student Model and Job Market Model



# 2 - Nettoyage :
#this algorithm used to get Our data clean and clear:

aberrante <- boxplot(age)$out

count <- 0
function.moyen <- function(list){
  for(i in list){
    if(i<23){
      count <- count + i
    }
  }
  result <- count/(length(list)-4)
  print(result)
}


function.aburante <- function(list,age){
  for(i in list){
    print(i)
    age <- replace(age,age == i, NA)
  }
}






#Nous n'avons pas de valeurs manquantes car dans notre formulaire toutes les questions étaient obligatoires



#abr = boxplot(Age)$out
# nous avons trouvé 4 valeur abr dont : 60, 35, 23, 45
# Nous allons les remplacer par NA et calculer la moyenne d'age
summary(data)
#  Nous avons obtenu une moyenne = 20,56 donc nous allons remplacer les valeurs abr par 21
mean(Age)
# On a actualiser les données 
hist(Age)
summary(Age)
# on a age est une variable quantitative 
summary(data)


# 4 - Teste de quasi normalité 

# Nous allons maintenant tester la quasi-normalité de Age :
library(moments)
# pour etudier la quasi-normalité nous allons utiliser le Skezness et le Kurtosis
# il doivent être inclus entre -2.5 et 2.5
skewness(Age)
kurtosis(Age)
# Donc Age est quasi-normal
# On utilise le test non parametrique 
# 5 - test non-parametrique :
# test quali avec quali => chisq
# test quali avec quanti => aov , kruskal 
#### NIVEAU ETUDIANT ######
# etude univariée : 
# entre une variable quanti et une quali 
#--- entre sexe et age ---- 
# on a sexe forme deux groupes(F, M) => t student 


#analyse univarie 
#c'est juste summary avec plot
#plot pour quali et pour qunati hist


#function to plot all our data

# function.plot <- function(data){
#   for(i in 2:length(data)){
#     if(class(data[i]) == "numeric")
#         print(hist(data[i]))
#     else
#         print(plot(data[i]))
#   }
# }
# 
# function.plot(data)


hist(Age)
class(Age)
plot(sexe)
plot(M1)
plot(M2)
plot(M3)
plot(M4)
plot(M5)
plot(M6)
plot(D1)
plot(D2)
plot(D3)
plot(D4)
plot(D5)




#function to whatch summary of all our data 

# function.summary <- function(data){
#   for(i in 2:length(data)){
#     print(summary(data[i]))
#     print("------------------------------")
#   }
# }
# 
# function.summary(data)


summary(Age)
summary(sexe)
summary(M1)
summary(M2)
summary(M3)
summary(M4)
summary(M5)
summary(M6)
summary(D1)
summary(D2)
summary(D3)
summary(D4)
summary(D5)

summary(data)
  

#test bivarie

# 3 - test de normalité  :

# Nous avons une population connue => stratifier 
# On a H0 : Il n'y a pas de difference significative entre la loi normale et notre distribution
# Et H1 : il y a une difference significative entre la loi normale et notre distribution
# Nous avons age est une variable quali => SHAPIRO test
# si pvalue>= 5% => H0 / Sinon pvalue < 5% => H1
#Nous avons pvalue = 7.379e-07 < 5% => donc H1 est verifiée d'ou l'Age ne suit pas la loi normale 

shapiro.test(Age)

    
# 4 - Teste de quasi normalité 

# Nous allons maintenant tester la quasi-normalité de Age :
library(moments)
# pour etudier la quasi-normalité nous allons utiliser le Skezness et le Kurtosis
# il doivent être inclus entre -2.5 et 2.5
#alors notre kurtosis entre 2.5 et -2.5 alors valide aussi on a notre skewness proche de 0
#kurtosis pour platissement et symetrie skewness

skewness(Age) #si proche de 0 mziana
kurtosis(Age) #si entre 2.5 et -2.5


#alors effectuons un test mtn entre(age,sexe)
#alors on a notre variable age casinormale
#-> on va utiliser aov sinon si c'etait non normale -> wilcox


#here we have other function that calcul the association using nova method
#the new and the effecient method

data <- as.data.frame(data)

function.nova <- function(data){
  for(i in 2:length(data)){
    print(summary(aov(as.formula(paste('Age','~',data[i]))))[[1]][["Pr(>F)"]])
    print("-------------------------------------------")
  }
}

function.nova(data)

#the classic and the old method

summary(aov(Age~M1))
summary(aov(Age~M2))
summary(aov(Age~M3))
summary(aov(Age~M4))
summary(aov(Age~M5))
summary(aov(Age~M6))
summary(aov(Age~D1))
summary(aov(Age~D2))
summary(aov(Age~D3))
summary(aov(Age~D4))
summary(aov(Age~D5))

#alors pour tous ces variables il n'ya aucune association 




#alors mtn pour les variables quali entre eux
#chisq.test
#D-> Difficultes
#M-> MARCHE DE Travail
#1 association entre les D avec D
#2 association de M avec M
#3 association de M avec D


#a single function that calcul the all chisq test
function.chis <- function(data){
  for(i in 2:length(data)){
      for(j in i+1:length(data)){
        if(i != j){
          a <- chisq.test(data[,i:i],data[,j:j])$p.value
          print(paste0("P-value is :", a))
          #print(chisq.test(data[,i],data[,j])$p.value)
          print("-------------------------------------------------")
        }
        
      }
  }
}



function.chis(data)


#1 D WITH D
class(chisq.test(D1,D2)$p.value) #rien
chisq.test(D1,D3) #rien
chisq.test(D1,D4) #rien
chisq.test(D1,D5) #rien
chisq.test(D2,D3) #rien
chisq.test(D2,D4) #0.6% chwiiiya mais non
chisq.test(D2,D5) #rien
chisq.test(D3,D4) #rien
chisq.test(D3,D5) #rien
chisq.test(D4,D5) #rien


#2 M WITH M

chisq.test(M1,M2) #INFR a 5 -> il y'a association entre parascolarité at autoformation
chisq.test(M1,M3) #INF a 5 -> il y'a une association entre parascolarite et la demande de competence
chisq.test(M1,M4) #INF a 5 -> il y'a une association entre parascolarite et softskills and hardskills
chisq.test(M1,M5) #rien ->
chisq.test(M1,M6) #rien
chisq.test(M2,M3) #INF -> il y'a une association entre l'autoformation et demande de comptence
chisq.test(M2,M4) #rien
chisq.test(M2,M5) #rien
chisq.test(M2,M6) #rien
chisq.test(M3,M4) #rien
chisq.test(M3,M5) #rien
chisq.test(M3,M6)#rien
chisq.test(M4,M5) #INF -> tech avec hard and soft skills
chisq.test(M4,M6) #rien
chisq.test(M5,M6) #rien

#3  M WITH D

chisq.test(M3,D1)
chisq.test(M3,D2)
chisq.test(M3,D3) #il y'a qlq entre competence et manque d'experience
chisq.test(M3,D4)
chisq.test(M3,D5) # il y'a competence et softskills and communication
chisq.test(M4,D1) 
chisq.test(M4,D2)
chisq.test(M4,D3) # il y'a entre profiles pointus et experience
chisq.test(M4,D4)
chisq.test(M4,D5)
chisq.test(M5,D1)
chisq.test(M5,D2)
chisq.test(M5,D3) #RIEN
chisq.test(M5,D4)
chisq.test(M5,D5) # RIEN
chisq.test(M6,D1)
chisq.test(M6,D2) #IL Ya entre reseaux et formation non a jour 
chisq.test(M6,D3) #il y'a entre reseaux et manque d'experience
chisq.test(M6,D4)
chisq.test(M6,D5) # RIEN










#krustal si on a non param quanti avec quali



#test d'hypothese d'association 
#hypothese -> il y'a des difficulte

#alors pour difficulte 1 c'est deja valide 
#difficulte 2 si possible  non la meilleire
#difficulte 3 aussi possible mais non la meilleure 
#difficulte 4 alors la plupart est d'accors que le salaire et non motivant
#difficulte 5  alors la plupart et ne pas du tout d'accord avec cette difficulte (communication et softskills) 


#alors la perception que  la formation n'est pas a jour est la difficulte valide .




#Nous avons fait des tests de comparaison de moyennes ou variances

#alors on utilisant 2 boxplot a la fois 
#AUSSI UN test de t.test() c'est un test de moyen 


plot(sexe,Age)
t.test(Age)
library(Rcmdr)

#alors les test de moyennes de variable t age sont aproximatly equal .
#alors selon calculatrice de l'echantillonage on a la taille de l'echantilllon representative 37
#avec une marge d'erreur de 10% et intervalle de confiance de 90%


#alors la regression lineaire nessecite 2 variables quanti et on a que une seule alors on ne peut pas






#**********classification avec idee nouvelles ****************
#algorithme de KNN k nearest neighbor




#converting our data , and meake it valid

data <- as.data.frame(data)
data$Genre <- as.factor(data$Genre)
row_labels = data$Genre
row_labels




data$Genre <- as.numeric(data$Genre)
data <- data[,2:5]
data
data$Parascolaire <- as.factor(data$Parascolaire)
data$Parascolaire <- as.numeric(data$Parascolaire)
data$Autoformation <- as.factor(data$Autoformation)
data$Autoformation <- as.numeric(data$Autoformation)



#here we scale our data , so that we minimize the distance calculated each time .
data[,2:4] <- scale(data[,2:4])
data

set.seed(6)

#so here we scale our data into two sections first one is train_set with 80% , and second one of 20%
size <- floor(0.8 * nrow(data))

size


train_ind <- sample(seq_len(nrow(data)),size = size)
train_labels <- data[train_ind,1]
test_labels <- row_labels[-train_ind]
data_train <- data[train_ind,2:4]
data_test <- data[-train_ind,2:4]

library(class)

predictions <- knn(train = data_train,test=data_test,cl = train_labels,k=round(sqrt(nrow(data_train))))


plot_predictions <- data.frame(
  data_test$Age,
  data_test$Parascolaire,
  data_test$Autoformation,
  predicted = predictions
)

colnames(plot_predictions) <- c("Age","Parascolaire","Autoformation",'predicted')

library(ggplot2)
library(gridExtra)



#voila notre classification est pret
p1 <- ggplot(plot_predictions, aes(Parascolaire,Autoformation,color = predicted, fill = predicted))+
  geom_point(size = 5) + geom_text(aes(label = test_labels), hjust = 1 , vjust=2)+
  ggtitle("Parascolarite avec autoformation")+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position = "none")

p2 <- ggplot(plot_predictions, aes(Age,Parascolaire,color = predicted, fill = predicted))+
  geom_point(size = 5) + geom_text(aes(label = test_labels), hjust = 1 , vjust=2)+
  ggtitle("Age avec Parascolarite")+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position = "none")




p3 <- ggplot(plot_predictions, aes(Age,Autoformation,color = predicted, fill = predicted))+
  geom_point(size = 5) + geom_text(aes(label = test_labels), hjust = 1 , vjust=2)+
  ggtitle("Age avec Autoformation")+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position = "none")

grid.arrange(p1,p2,p3,ncol = 3)







#linear regression
#alors dans notre donnes on a qu'une seule variable quantitative alors on a juste effectuer 2 modeles
#de regression , dans le premier utilisant Taille et poids , et 2eme juste donnes de fleur iris which is co famous

height <- c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
weight <- c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

plot(weight, height)
plot(weight, height, pch = 16, cex = 1.3, col = "blue", main = "BODY AND HEIGHT", xlab = "Weight (kg)", ylab = "Height (cm)")
lm(height ~weight)

abline(98.0054, 0.9528,col="red")


#second linear regression 

data <- iris
fit1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
lm(formula = Sepal.Length ~ Petal.Width, data = iris)

plot(iris$Sepal.Length,iris$Petal.Length,pch = 16, cex = 1.3, col = "blue", main = "BODY AND HEIGHT(Pr Moumen)")
abline(fit1,col="red")


library(ggplot2)

ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")



#representation de notre sexe et autre comme age d'une façon 3D

Genre <- data$Genre
Genre <- as.factor(sexe)
levels(Genre) <- c("M","F")
Genre
x <- c(15,17)
piepercent<- round(100*x/sum(x), 1)
pie(x,labels = piepercent,main="Age Distribution",col = rainbow(length(x)))
legend("topright",c("Male","Female"),cex = 0.8, fill = rainbow(length(x)))

lbl <-  c("Male","Female")

library(plotrix)

pie3D(x,labels = lbl,explode = 0.1, main = "Age Distibution in our Data ")


hist(Age)
qplot(Age,main="Age frequence",binwidth=0.2)
boxplot(age)


#on a effectuer aussi l'analyse factorielle , puisque dans la classification on a effectuer
#un teste sur plusiers variable .




