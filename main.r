# Librerie utili
library(dplyr)
library(ggplot2)
library(moments)
options(scipen = 999)


# ------------------------------------------------ #

### HOUSEHOLD PRICES dataset

# Carico il dataset
houses = read.csv("house_price.csv", stringsAsFactors = TRUE)
dim(houses)

# Operazioni preliminari:
# 1. Verifico quali righe e colonne hanno troppi valori mancanti
# 2. Rimuovo righe e colonne con troppi valori mancanti
# 3. Divido il dataset in due parti: una con le variabili numeriche e una con le variabili categoriche

quant_cont_cols = c("LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch")
qual_cols = c("MSSubClass", "MSZoning", "Street", "LotShape", "LandContour", "Utilities", "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual", "Functional", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PavedDrive", "SaleType", "SaleCondition")
quant_disc_cols = c("BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars")
year_cols = c("YearBuilt", "YearRemodAdd", "GarageYrBlt")
mark_cols = c("OverallQual", "OverallCond")

houses[, qual_cols[1]] = as.factor(houses[, qual_cols[1]])

quant_cont_vars = houses[, quant_cont_cols]
qual_vars = houses[, qual_cols]
quant_discr_vars = houses[, quant_disc_cols]
year_vars = houses[, year_cols]
mark_vars = houses[, mark_cols]





# ---------------------------------------------------------- #
## Analisi UNIVARIATA




## Variabili numeriche continue

# Funzione helper per printare tutti i dati che mi servono
cont_info = function (x, i) {
  if (colnames(quant_cont_vars[i]) != "LotFrontage" & colnames(quant_cont_vars[i]) != "LotArea") {
    x = x[x != 0]
  }
  print(colnames(quant_cont_vars[i]))
  print(summary(x))
  print("Skewness")
  print(skewness(x, na.rm = TRUE))
  print("Curtosi")
  print(kurtosis(x, na.rm = TRUE))
  par(mfrow = (c(1, 3)))
  boxplot(x, main = "Boxplot", xlab = "Value", horizontal = TRUE)
  plot(density(x, na.rm = T), main="Frequency", xlab = "Frequency", ylab = "Frequency")
  qqnorm(x, main = "QQ Plot")
  qqline(x)
  mtext(text=colnames(quant_cont_vars[i]), line = -1.75, outer = T, cex = 1.5)
}

# Applico la funzione a tutte le variabili numeriche continue
for (i in seq_along(quant_cont_vars)) {
  cont_info(quant_cont_vars[, i], i)
}

## Variabili numeriche discrete

# Funzione helper per printare tutti i dati che mi servono
disc_info = function(x, i) {
  print(colnames(quant_discr_vars[i]))
  print(summary(x, na.rm = T))
  par(mfrow = (c(1, 1)))
  barplot(prop.table(table(x)), main = colnames(quant_discr_vars[i]), xlab = "Value", ylab = "Frequency")
}

# Applico la funzione a tutte le variabili numeriche discrete
for (i in seq_along(quant_discr_vars)) {
  disc_info(quant_discr_vars[, i], i)
}




## Variabili anno

year_info = function(x, i) {
  par(mfrow = (c(1, 1)))
  print("Minimo")
  print(min(x, na.rm=T))
  print("Massimo")
  print(max(x, na.rm=T))
  print("Quantili")
  print(quantile(x, na.rm=T))
  hist(x, main = colnames(year_vars[i]), xlab = "Value", ylab = "Frequency")
}

for (i in seq_along(year_vars)) {
  year_info(year_vars[, i], i)
}



## Variabili voto

mark_info = function(x, i) {
  par(mfrow = (c(1, 1)))
  print(colnames(mark_vars[i]))
  print("Minimo")
  print(min(x, na.rm=T))
  print("Massimo")
  print(max(x, na.rm=T))
  print("Quantili")
  print(quantile(x, na.rm=T))
  barplot(prop.table(table(x)), main = colnames(mark_vars[i]), xlab = "Value", ylab = "Frequency")
}

for (i in seq_along(mark_vars)) {
  mark_info(mark_vars[, i], i)
}





## Variabili categoriche

# Funzione helper per printare tutti i dati che mi servono
qual_info = function (x, i) {
  print(colnames(quant_cont_vars[i]))
  print(table(x))
  print(prop.table(table(x)))
  par(mfrow = (c(1, 2)))
  barplot(table(x), main = "Frequenze assolute", xlab = "Value", ylab = "Frequenze")
  barplot(prop.table(table(x)), main = "Frequenze relative", xlab = "Value", ylab = "Frequenze")
  mtext(text=colnames(quant_cont_vars[i]), line = -1.75, outer = T, cex = 1.5)
}

# Applico la funzione a tutte le variabili categoriche
for (i in seq_along(qual_vars)) {
  qual_info(qual_vars[, i], i)
}









### ANALISI BIVARIATA



## Variabili QUALITATIVE
par(mfrow = c(1, 1))
biv_cat = function(a, i){
  print(colnames(qual_vars)[i])
  boxplot(log(houses$SalePrice) ~ a, main = c("Prezzo e ", colnames(qual_vars)[i]), xlab= colnames(qual_vars)[i], ylab="Prezzo")
  plot(ecdf(houses$SalePrice[a == levels(a)[1]]), xlab="Prezzo", ylab="Frequenza", verticals=T, main="funzione empirica")

  for (j in 2: nlevels(a)){
    if (sum(houses$SalePrice[a == levels(a)[j]], na.rm=T)>0)
      plot(ecdf(houses$SalePrice[a == levels(a)[j]]), verticals=T, add=T, col= j)
  }
  legend("topright", inset=c(0,0.1), legend = levels(a), col = 1:nlevels(a), lty = 1, cex=0.35)

  p3 <- ggplot(houses, aes(x = a, y = SalePrice)) +
    geom_violin() +
    labs(title = paste("Prezzo e ", colnames(qual_vars)[i]), x = colnames(qual_vars)[i], y = "Prezzo")

  print(p3)
  testanova<- aov(SalePrice ~ a, data=houses)
  print(summary(testanova))

}


for (i in 1:seq_along(qual_vars)){
  biv_cat(houses[,colnames(qual_vars)[i]],i)
}



