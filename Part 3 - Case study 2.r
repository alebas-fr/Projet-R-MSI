# Alexandre Lebas - Océane Deletrez
# Inspiré du livre de Yanchang Zhao

data.train = read.csv(file = "cup98LRN.txt")
dim(data.train)

data.val = read.csv(file = "cup98VAL.txt")
dim(data.val)

# Nous permet d'avoir une description de toutes les variables
install.packages("Hmisc")

library(Hmisc)
describe(data.train) 

describe(data.val)

# Trouver où sont les deux classes dans les données
# Pour cela on va regarder un nombre défini (exemple 100) de variables avec describe
# On regarde si jamais les premières et les dernières sont les mêmes
# Si c'est le cas alors les classes ne sont pas dans ces variables
describe(data.train[,1:100])

describe(data.val[,1:100])

# Pas dans les 100 premières
describe(data.train[,101:200])

describe(data.val[,101:200])

# Pas dans les 200 premières variables
describe(data.train[,201:300])

describe(data.val[,201:300])

# Pas dans les 300 premières
describe(data.train[,301:400])

describe(data.val[,301:400])

# Pas dans les 400 premières 
# Elles se situent donc faire la fin du jeu d'entrainement essayons donc de partir des dernières variables
describe(data.train[,470:481])

describe(data.val[,470:479])

# On remarque donc que les deux classes sont TargetB et TargetD
# TargetB a deux valeurs certainement que cela doit être un binaire pour savoir si une personne répond ou non 
# Target D doit représenter la somme donnée si celui-ci à donné
unique(data.train$TARGET_B)

# 0 doit être le nombre de gens qui n'ont pas donné et 1 les gens qui ont donné
proportionB = prop.table(table(data.train$TARGET_B))
proportionB

personneQuiOntDonne = data.train[data.train$TARGET_D >0,]
# On regarde le nombre de dons au cas ou TARGETB aurait été mal rempli
donsDesPersonnes = personneQuiOntDonne$TARGET_D
# Regardons maintenant combien de personnes ont donné cela correspond bien à la proportion des gens qui ont donné
dim(personneQuiOntDonne)


summary(donsDesPersonnes)

boxplot(donsDesPersonnes)

barplot(table(round(donsDesPersonnes)))

# Avant de commencer à travailler avec les données nous voyons que beaucoup de données sont manquantes
# Nous allons donc les remplacer par leurs valeurs médianes

for(i in 1:ncol(data.train)){
    data.train[is.na(data.train[,i]), i] <- median(data.train[,i], na.rm = TRUE)
}


variablesNumerique = which(sapply(data.train,is.numeric))
myHist <- function(x){
    hist(data.train[,x],main=NULL,xlab=x)
}
sapply(names(variablesNumerique),myHist)

boxplot(data.train$HIT)

valeursAberante = data.train$HIT[data.train$HIT>100]
valeursAberante

valeursNonAberante = data.train$HIT[data.train$HIT<100]
mean(valeursNonAberante)

data.train$HIT[data.train$HIT>100] = mean(valeursNonAberante)
boxplot(data.train$HIT)

AgeNumeriser <- cut(personneQuiOntDonne$AGE, right=F, breaks=seq(0, 100, by=5))
boxplot(personneQuiOntDonne$TARGET_D ∼ AgeNumeriser,ylim=c(0,40))

boxplot(personneQuiOntDonne$TARGET_D ∼ personneQuiOntDonne$GENDER, ylim=c(0,80))

correlation = cor(data.train$TARGET_D,data.train[,variablesNumerique],use = "pairwise.complete.obs")
# pairwise.complete.obs permet d'éviter une erreur en cas de valeurs manquantes
correlation = abs(correlation) 
# Étudier les valeurs absolues nous permettent de voir lesquels 
# sont les plus corrélés négativement ou positivement.
(correlation <- correlation[,order(correlation, decreasing=T)])

install.packages("party")

library(party)

set.seed(1234)
ind = sample(2,nrow(data.train),replace = TRUE, prob = c(0.8,0.2))

train.data = data.train[ind == 1,]
test.data = data.train[ind ==2,]

# Paramétre de l'arbre 
MinSplit = 200
MinBucket = 40
MaxDepth = 10

# Variables à garder choisi en fonction des corrélations et d'autres variables qu'il semble logique d'utiliser
VariableAUtiliser <- c("HV1","HV2","HV3","HVP4","ETH10","IC12","IC1","HV4","IC3","IC2","HVP3","HVP6","IC4",
"AGE", "AVGGIFT", "CARDGIFT", "CARDGIFT",
"CLUSTER2","HIT","HPHONE_D", "INCOME", "LASTGIFT", "MAXRAMNT",
"MINRAMNT", "NGIFTALL", "NUMPRM12", "RAMNTALL","RFA_2F", "TIMELAG")
DonneeEntrainement = train.data[,c("TARGET_D",VariableAUtiliser)]
dim(DonneeEntrainement)

describe(DonneeEntrainement)
# Remplacons les valeurs manquantes dans chaque colonne par la médianne des autres valeurs 
# (pas la moyenne car il y a des données avec un nombre finie de valeurs)

formula = TARGET_D ~ HV1+HV2+HV3+HVP4+ETH10+IC12+IC1+HV4+IC3+IC2+HVP3+HVP6+IC4+AGE+AVGGIFT+CARDGIFT+CARDGIFT+CLUSTER2+HIT+HPHONE_D+INCOME+LASTGIFT+MAXRAMNT+MINRAMNT+NGIFTALL+NUMPRM12+RAMNTALL+RFA_2F+TIMELAG

Arbre <- ctree(formula,data=DonneeEntrainement,
controls=ctree_control(minsplit=MinSplit, minbucket=MinBucket, maxdepth=MaxDepth))

plot(Arbre, type="simple",
ip_args=list(pval=FALSE), ep_args=list(digits=0,abbreviate=TRUE),
tp_args=list(digits=2))

testPred = predict(Arbre,newdata = test.data)

plot(testPred)

plot(test.data$TARGET_D)

table(testPred, test.data$TARGET_D)
