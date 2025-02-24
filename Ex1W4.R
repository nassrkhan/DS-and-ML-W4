#Calculate correlations between variables
cor.test(iris$Sepal.Length, iris$Sepal.Width, method="pearson")

#Now also create a scatter plot
plot(iris$Sepal.Length, iris$Sepal.Width)

#Create an adjacency matrix and interpret the results

cor(iris[c("Sepal.Length","Sepal.Width", "Petal.Length","Petal.Width")])

#Now create SPLOM graphics
pairs(iris[c("Sepal.Length","Sepal.Width", "Petal.Length","Petal.Width")])

install.packages("psych")

library(psych)

pairs.panels(iris[c("Sepal.Length","Sepal.Width", "Petal.Length","Petal.Width")])

