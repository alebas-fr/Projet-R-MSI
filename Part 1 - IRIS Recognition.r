# Alexandre Lebas - Océane Deletrez 24/09/2021
# Charger les données
data = iris
ind = sample(2,nrow(data),replace = TRUE, prob = c(0.75,0.25))
train.data = iris[ind == 1,]
test.data = iris[ind ==2,]

install.packages("party")

library(party)

install.packages("tidyverse")
install.packages("caret")
install.packages("e1071")

library(caret)


# Paramétrer le 5-fold avec la librairie caret
train_control = trainControl(method = "cv",number = 5)

# Entrainer le modèle avec les données, la méthode et mettre le 5-fold en place avec trControl
myFormula = Species ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width
model = train(myFormula,data = train.data,method = "ctree",trControl = train_control)

print(model)

# Party est déjà installé
install.packages("MLmetrics")

library(MLmetrics)

# Récupérer l'arbre final du modèle entrainé avec le 5-fold
Arbre = model$finalModel
Arbre

plot(Arbre)

# Prédire les espèces du jeu de test et les comparé aux vraies valeurs
predictTest = predict(Arbre,newdata = test.data)
table(predictTest,test.data$Species)

# Prédire les espèces du jeu d'entrainement et les comparé aux vraies valeurs
predictTrain = predict(Arbre,newdata = train.data)
table(predictTrain,train.data$Species)

# Convertir les valeurs des espéces predite en numique pour pouvoir calculer le MAE
yTePred = as.numeric(predictTest)
yTePred

yTeReel = as.numeric(test.data$Species)

MAE(yTePred,yTeReel)

yTrPred = as.numeric(predictTrain)

yTrReel = as.numeric(train.data$Species)

MAE(yTrPred,yTrReel)
