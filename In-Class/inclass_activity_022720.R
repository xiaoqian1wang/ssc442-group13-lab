# prep data

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

install.packages('tree')
install.packages('rpart')
install.packages('caret')
install.packages('lattice')
install.packages('partykit')
install.packages('grid')
install.packages('libcoin')
install.packages('mvtnorm')


library(tree)
library(rpart)
library(caret)
library(partykit)

str(ameslist)

ameslist$Fireplaces <- factor(ameslist$Fireplaces)


# partition data, training/validation datasets
set.seed(1234)

part_data<-sample(2, nrow(ameslist), replace=TRUE, prob = c(0.5, 0.5))

train<-ameslist[part_data == 1,]

val<- ameslist[part_data==2,]

# decision tree with party package 

install.packages('party')

tree <- ctree(Fireplaces~ Id + LotArea + LotFrontage, data = train)
plot(tree)

### PART 2 -- CONFUSION MATRIX (i too, am confused)


