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

# Trouver ou sont les deux classe dans les données
# POur cela on va regarder un nombre défini (exemple 100) de vriable avec describe
# On va voir si jamais les premières et les dernières sont les mêmes
# Si c'est le cas alors les classes ne sont pas dans ces variables
describe(data.train[,1:100])

describe(data.val[,1:100])

# Pas dans 100
describe(data.train[,101:200])

describe(data.val[,101:200])

# Pas dans les 200 premières variables
describe(data.train[,201:300])

describe(data.val[,201:300])

# Pas dans les 300 premières
describe(data.train[,301:400])

describe(data.val[,301:400])

# Pas dans les 400 premières 
# Elle se situe donc faire la fin du deu d'entrainement esseyons donc de partir des dernières variables
describe(data.train[,470:481])

describe(data.val[,470:479])

# On remarque donc que les deux classes sont TargetB et TargetD
# TargetB a deux valeurs certainement que cela doit être un binaire pour savoir si une personne répond ou non 
# Target D doit representer la somme donnée si celui-ci à donné
unique(data.train$TARGET_B)

# 0 doit être le nombre de gens qui n'ont pas donné et 1 les gens qui ont donnée
proportionB = prop.table(table(data.train$TARGET_B))
proportionB

personneQuiOntDonne = data.train[data.train$TARGET_D >0,]
# On regarde le nombre de dons au cas ou TARGETB aurait été mal rempli
donsDesPersonnes = personneQuiOntDonne$TARGET_D
# Regardons maintenant combien de personnes ont données cela correspond bien à la proportion des gens qui ont donnée
dim(personneQuiOntDonne)


summary(donsDesPersonnes)

boxplot(donsDesPersonnes)

barplot(table(round(donsDesPersonnes)))

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
# Etudier la valeurs absolues nous permet de voir lesquels 
# sont les plus corrélé négativement ou positivement.
(correlation <- correlation[,order(correlation, decreasing=T)])

install.packages("party")

library(party)

 # Paramétre de l'arbre 
MinSplit = 400
MinBucket = 40
MaxDepth = 10
VariableAUtiliser <- c("AGE", "AVGGIFT", "CARDGIFT", "CARDGIFT",
"CLUSTER2", "DOMAIN", "GEOCODE2", "HIT",
"HOMEOWNR", "HPHONE_D", "INCOME", "LASTGIFT", "MAXRAMNT",
"MDMAUD_F", "MDMAUD_R", "MINRAMNT", "NGIFTALL", "NUMPRM12",
"PCOWNERS", "PEPSTRFL", "PETS", "RAMNTALL", "RECINHSE",
"RFA_2A", "RFA_2F", "STATE", "TIMELAG")
DonneeEntrainement = data.train[,c("TARGET_D",VariableAUtiliser)]
dim(DonneeEntrainement)

formula = TARGET_D ~ AGE+AVGGIFT+CARDGIFT+CARDGIFT+CLUSTER2+DOMAIN+GEOCODE2+HIT+HOMEOWNR+HPHONE_D+INCOME+LASTGIFT+MAXRAMNT+MDMAUD_F+MDMAUD_R+MINRAMNT+NGIFTALL+NUMPRM12+PCOWNERS+PEPSTRFL+PETS+RAMNTALL+RECINHSE+RFA_2A+RFA_2F+STATE+TIMELAG

myCtree <- ctree(formula,data=DonneeEntrainement,
controls=ctree_control(minsplit=MinSplit, minbucket=MinBucket, maxdepth=MaxDepth))


