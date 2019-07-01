library(dplyr)
library(mlr)

df <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")
print(df)
v <- as.data.frame(df)

colName <- v$Color

print(colName)
"Color"
"Smokes"
col <- v[,c("Color")]
print(col)
colNA <- is.na(col)
print(colNA)

df <- v[!colNA,]
print(df)

for (i in names(df)) {
  if (is.logical(df[,i])) {
    print("yes")
    df[,i] <- as.factor(df[,i])
    print(df[,i])
  }
  else{
    print(i)
  }
}
class(df$Smokes)
print(df$Smokes)

task = makeClassifTask(data = df, target = "Color")
selected_model = makeLearner("classif.naiveBayes")
NB_mlr = mlr::train(selected_model, task)

NB_mlr$learner.model
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = df[,!names(df) %in% c("Color")]))
tab <- table(predictions_mlr[,1],df[,c("Color")])
print(tab)
cost <- 0
data.frame(as.data.frame(tab)[,-3],cost)
