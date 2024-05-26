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
# 1. Verifico quali righe e colonne hanno troppi valori mancanti
# 2. Rimuovo righe e colonne con troppi valori mancanti
# 3. Divido il dataset in due parti: una con le variabili numeriche e una con le variabili categoriche
na_rows = sapply(houses, function(x) sum(is.na(x))) > 0.25 * ncol(houses)
na_cols = sapply(houses, function(x) sum(is.na(x))) > 0.25 * nrow(houses)

houses = houses[!na_rows, ]
houses = houses[, !na_cols]
dim(houses)

num_but_factor_cols = c("MSSubClass", "OverallQual", "OverallCond")
houses[, num_but_factor_cols] = lapply(houses[, num_but_factor_cols], as.factor)

numerical_houses = houses[, sapply(houses, is.numeric)]
categorical_houses = houses[, sapply(houses, is.factor)]




# ---------------------------------------------------------- #
## Analisi UNIVARIATA

## Variabili numeriche

# Funzione helper per printare tutti i dati che mi servono
num_info = function (x) {
  print(summary(x))
  print(skewness(x))
  print(kurtosis(x))
  par(mfrow = (c(2, 1)))
  boxplot(x, main = colnames(x), xlab = "Value", horizontal = TRUE)
  hist(x, main = colnames(x), xlab = "Value", ylab = "Frequency")
}

# Applico la funzione a tutte le variabili numeriche
for (i in 2:ncol(numerical_houses)) {
  num_info(numerical_houses[, i])
}


## Variabili categoriche

# Funzione helper per printare tutti i dati che mi servono
cat_info = function (x) {
  tab = table(x)
  print(tab)
  print(prop.table(tab))
  barplot(table(x), main = colnames(x), xlab = "Value", ylab = "Frequency")
  barplot(prop.table(table(x)), main = colnames(x), xlab = "Value", ylab = "Frequency")
}

# Applico la funzione a tutte le variabili categoriche
for (i in 1:ncol(categorical_houses)) {
  cat_info(categorical_houses[, i])
}
cat_info(categorical_houses[, "MSSubClass"])


# ---------------------------------------------------------- #
## Analisi BIVARIATA

## Variabili numeriche

## Variabili categoriche



# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
# ---------------------------------------------------------- #




