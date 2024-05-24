# Librerie utili
library(dplyr)
library(ggplot2)
library(moments)

# ------------------------------------------------ #

### HOUSEHOLD PRICES dataset

# Carico il dataset
houses <- read.csv("house_price.csv", stringsAsFactors = TRUE)
dim(houses)

# Operazioni preliminari:
# 1. Rimuovo le colonne con troppi valori mancanti
# 2. Divido il dataset in due parti: una con le variabili numeriche e una con le variabili categoriche
too_much_na = sapply(houses, function(x) sum(is.na(x))) > 0.25 * nrow(houses)
houses = houses[, !too_much_na]
dim(houses)

numerical_houses = houses[, sapply(houses, is.numeric)]
categorical_houses = houses[, sapply(houses, is.factor)]


# ---------------------------------------------------------- #
## Analisi UNIVARIATA

## Variabili numeriche

# Funzione helper per printare tutti i dati che mi servono
info = function (x) {
  print(summary(x))
  print(skewness(x))
  print(kurtosis(x))
  par(nrow(c(1, 2)))
  boxplot(x, main = colnames(x))
  hist(x, main = colnames(x))
}

# Applico la funzione a tutte le variabili numeriche
for (i in 2:ncol(numerical_houses)) {
  info(numerical_houses[, i])
}

## Variabili categoriche


# ---------------------------------------------------------- #
## Analisi BIVARIATA

## Variabili numeriche

## Variabili categoriche



# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
# ---------------------------------------------------------- #




